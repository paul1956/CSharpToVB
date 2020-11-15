' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter
    Public Module INamedTypeSymbolExtensons

        Private Function GetAbstractClassesToImplement(abstractClasses As IEnumerable(Of INamedTypeSymbol)) As ImmutableArray(Of INamedTypeSymbol)
            Return abstractClasses.SelectMany(Function(a As INamedTypeSymbol) a.GetBaseTypesAndThis()).Where(Function(t As INamedTypeSymbol) t.IsAbstractClass()).ToImmutableArray()
        End Function

        Private Function GetAllImplementedMembers(classOrStructType As INamedTypeSymbol,
                                                  interfacesOrAbstractClasses As IEnumerable(Of INamedTypeSymbol),
                                                  isImplemented As Func(Of INamedTypeSymbol,
                                                  ISymbol,
                                                  Func(Of INamedTypeSymbol, ISymbol, Boolean), CancellationToken, Boolean),
                                                  isValidImplementation As Func(Of INamedTypeSymbol, ISymbol, Boolean),
                                                  interfaceMemberGetter As Func(Of INamedTypeSymbol, ISymbol, ImmutableArray(Of ISymbol)),
                                                  allowReimplementation As Boolean,
                                                  CancelToken As CancellationToken
                                                 ) As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))
            If classOrStructType.TypeKind <> TypeKind.Class AndAlso classOrStructType.TypeKind <> TypeKind.Struct Then
                Return ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))).Empty
            End If

            If Not interfacesOrAbstractClasses.Any() Then
                Return ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))).Empty
            End If

            If Not interfacesOrAbstractClasses.All(Function(i As INamedTypeSymbol) i.TypeKind = TypeKind.Interface) AndAlso Not interfacesOrAbstractClasses.All(Function(i As INamedTypeSymbol) i.IsAbstractClass()) Then
                Return ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))).Empty
            End If

            Dim typesToImplement As ImmutableArray(Of INamedTypeSymbol) = GetTypesToImplement(classOrStructType, interfacesOrAbstractClasses, allowReimplementation, CancelToken)
            Return typesToImplement.SelectAsArray(Function(s As INamedTypeSymbol) (s, members:=GetImplementedMembers(classOrStructType, s, isImplemented, isValidImplementation, interfaceMemberGetter, CancelToken))).WhereAsArray(Function(t As (s As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))) t.members.Any)
        End Function

        Private Function GetImplementedMembers(classOrStructType As INamedTypeSymbol, interfaceType As INamedTypeSymbol, isImplemented As Func(Of INamedTypeSymbol, ISymbol, Func(Of INamedTypeSymbol, ISymbol, Boolean), CancellationToken, Boolean), isValidImplementation As Func(Of INamedTypeSymbol, ISymbol, Boolean), interfaceMemberGetter As Func(Of INamedTypeSymbol, ISymbol, ImmutableArray(Of ISymbol)), CancelToken As CancellationToken) As ImmutableArray(Of ISymbol)
            Dim q As IEnumerable(Of ISymbol) = From m In interfaceMemberGetter(interfaceType, classOrStructType)
                                               Where m.Kind <> SymbolKind.NamedType
                                               Where m.Kind <> SymbolKind.Method OrElse DirectCast(m, IMethodSymbol).MethodKind = MethodKind.Ordinary
                                               Where m.Kind <> SymbolKind.Property OrElse DirectCast(m, IPropertySymbol).IsIndexer OrElse DirectCast(m, IPropertySymbol).CanBeReferencedByName
                                               Where m.Kind <> SymbolKind.Event OrElse DirectCast(m, IEventSymbol).CanBeReferencedByName
                                               Where isImplemented(classOrStructType, m, isValidImplementation, CancelToken)
                                               Select m

            Return q.ToImmutableArray()
        End Function

        Private Function GetInterfacesToImplement(classOrStructType As INamedTypeSymbol, interfaces As IEnumerable(Of INamedTypeSymbol), allowReimplementation As Boolean, CancelToken As CancellationToken) As ImmutableArray(Of INamedTypeSymbol)
            ' We need to not only implement the specified interface, but also everything it
            ' inherits from.
            CancelToken.ThrowIfCancellationRequested()
            Dim interfacesToImplement As New List(Of INamedTypeSymbol)(interfaces.SelectMany(Function(i As INamedTypeSymbol) i.GetAllInterfacesIncludingThis()).Distinct())

            ' However, there's no need to re-implement any interfaces that our base types already
            ' implement.  By definition they must contain all the necessary methods.
            Dim baseType As INamedTypeSymbol = classOrStructType.BaseType
            Dim alreadyImplementedInterfaces As ImmutableArray(Of INamedTypeSymbol) = If(baseType Is Nothing OrElse allowReimplementation, New ImmutableArray(Of INamedTypeSymbol), baseType.AllInterfaces)

            CancelToken.ThrowIfCancellationRequested()
            interfacesToImplement.RemoveRange(alreadyImplementedInterfaces)
            Return interfacesToImplement.ToImmutableArray()
        End Function

        Private Function GetTypesToImplement(classOrStructType As INamedTypeSymbol, interfacesOrAbstractClasses As IEnumerable(Of INamedTypeSymbol), allowReimplementation As Boolean, CancelToken As CancellationToken) As ImmutableArray(Of INamedTypeSymbol)
            Return If(interfacesOrAbstractClasses.First().TypeKind = TypeKind.Interface, GetInterfacesToImplement(classOrStructType, interfacesOrAbstractClasses, allowReimplementation, CancelToken), GetAbstractClassesToImplement(interfacesOrAbstractClasses))
        End Function

        Private Function ImplementationExists(classOrStructType As INamedTypeSymbol, member As ISymbol) As Boolean
            Return classOrStructType.FindImplementationForInterfaceMember(member) IsNot Nothing
        End Function

        Private Function IsAbstractPropertyImplemented(classOrStructType As INamedTypeSymbol,
                        propertySymbol As IPropertySymbol) As Boolean
            ' A property is only fully implemented if both it's setter and getter is implemented.
            If propertySymbol.GetMethod IsNot Nothing Then
                If classOrStructType.FindImplementationForAbstractMember(propertySymbol.GetMethod) Is Nothing Then
                    Return False
                End If
            End If

            If propertySymbol.SetMethod IsNot Nothing Then
                If classOrStructType.FindImplementationForAbstractMember(propertySymbol.SetMethod) Is Nothing Then
                    Return False
                End If
            End If

            Return True
        End Function

        Private Function IsAccessorImplemented(accessor As IMethodSymbol, classOrStructType As INamedTypeSymbol) As Boolean
            Return accessor Is Nothing OrElse Not IsImplementable(accessor) OrElse classOrStructType.FindImplementationForInterfaceMember(accessor) IsNot Nothing
        End Function

        Private Function IsImplementable(m As ISymbol) As Boolean
            Return m.IsVirtual OrElse m.IsAbstract
        End Function

        Private Function IsImplemented(classOrStructType As INamedTypeSymbol, member As ISymbol, isValidImplementation As Func(Of INamedTypeSymbol, ISymbol, Boolean), CancelToken As CancellationToken) As Boolean
            If member.ContainingType.TypeKind = TypeKind.Interface Then
                If member.Kind = SymbolKind.Property Then
                    Return IsInterfacePropertyImplemented(classOrStructType, DirectCast(member, IPropertySymbol))
                Else
                    Return isValidImplementation(classOrStructType, member)
                End If
            End If

            If member.IsAbstract Then
                If member.Kind = SymbolKind.Property Then
                    Return IsAbstractPropertyImplemented(classOrStructType, DirectCast(member, IPropertySymbol))
                Else
                    Return classOrStructType.FindImplementationForAbstractMember(member) IsNot Nothing
                End If
            End If

            Return True
        End Function

        Private Function IsInterfacePropertyImplemented(classOrStructType As INamedTypeSymbol, propertySymbol As IPropertySymbol) As Boolean
            ' A property is only fully implemented if both it's setter and getter is implemented.

            Return IsAccessorImplemented(propertySymbol.GetMethod, classOrStructType) AndAlso IsAccessorImplemented(propertySymbol.SetMethod, classOrStructType)

        End Function

        Private Function IsNonNestedTypeAccessible(assembly As IAssemblySymbol, declaredAccessibility As Accessibility, within As ISymbol) As Boolean
            Dim withinAssembly As IAssemblySymbol = If(TryCast(within, IAssemblySymbol), DirectCast(within, INamedTypeSymbol).ContainingAssembly)
            Select Case declaredAccessibility
                Case Accessibility.NotApplicable, Accessibility.Public
                    ' Public symbols are always accessible from any context
                    Return True

                Case Accessibility.Private, Accessibility.Protected, Accessibility.ProtectedAndInternal
                    ' Shouldn't happen except in error cases.
                    Return False

                Case Accessibility.Internal, Accessibility.ProtectedOrInternal
                    ' An internal type is accessible if we're in the same assembly or we have
                    ' friend access to the assembly it was defined in.
                    Return withinAssembly.IsSameAssemblyOrHasFriendAccessTo(assembly)

                Case Else
                    Throw UnexpectedValue(declaredAccessibility)
            End Select
        End Function

        Private Function IsNonPublicImplementableAccessor(accessor As IMethodSymbol) As Boolean
            Return accessor IsNot Nothing AndAlso IsImplementable(accessor) AndAlso accessor.DeclaredAccessibility <> Accessibility.Public
        End Function

        Private Function IsPropertyWithNonPublicImplementableAccessor(member As ISymbol) As Boolean
            If member.Kind <> SymbolKind.Property Then
                Return False
            End If

            Dim [property] As IPropertySymbol = DirectCast(member, IPropertySymbol)

            Return IsNonPublicImplementableAccessor([property].GetMethod) OrElse IsNonPublicImplementableAccessor([property].SetMethod)
        End Function

        ''' <summary>
        ''' Checks if 'symbol' is accessible from within 'within', which must be a INamedTypeSymbol
        ''' or an IAssemblySymbol.  If 'symbol' is accessed off of an expression then
        ''' "throughTypeOpt" is the type of that expression. This is needed to properly do protected
        ''' access checks. Sets "failedThroughTypeCheck" to true if this protected check failed.
        ''' </summary>
        ''' <remarks>
        ''' NOTE: I expect this function to be called a lot.  As such, I do not do any memory
        ''' allocations in the function itself (including not making any iterators).  This does mean
        ''' that certain helper functions that we'd like to call are in-lined in this method to
        ''' prevent the overhead of returning collections or enumerators.
        '''</remarks>
        Private Function IsSymbolAccessibleCore(symbol As ISymbol, Within As ISymbol, throughTypeOpt As ITypeSymbol, ByRef failedThroughTypeCheck As Boolean) As Boolean ' must be assembly or named type symbol
            failedThroughTypeCheck = False
            Dim withinAssembly As IAssemblySymbol = If(TryCast(Within, IAssemblySymbol), DirectCast(Within, INamedTypeSymbol).ContainingAssembly)

            Select Case symbol.Kind
                Case SymbolKind.Alias
                    Return IsSymbolAccessibleCore(symbol:=DirectCast(symbol, IAliasSymbol).Target,
                                                      Within:=Within,
                                                      throughTypeOpt:=throughTypeOpt,
                                                      failedThroughTypeCheck:=failedThroughTypeCheck)

                Case SymbolKind.ArrayType
                    Return IsSymbolAccessibleCore(symbol:=DirectCast(symbol, IArrayTypeSymbol).ElementType,
                                                      Within:=Within,
                                                      throughTypeOpt:=Nothing,
                                                      failedThroughTypeCheck:=failedThroughTypeCheck)

                Case SymbolKind.PointerType
                    Return IsSymbolAccessibleCore(symbol:=DirectCast(symbol, IPointerTypeSymbol).PointedAtType,
                                                      Within:=Within,
                                                      throughTypeOpt:=Nothing,
                                                      failedThroughTypeCheck:=failedThroughTypeCheck)

                Case SymbolKind.NamedType
                    Return IsNamedTypeAccessible(DirectCast(symbol, INamedTypeSymbol), Within)

                Case SymbolKind.ErrorType, SymbolKind.Discard
                    Return True

                Case SymbolKind.TypeParameter, SymbolKind.Parameter, SymbolKind.Local, SymbolKind.Label, SymbolKind.Namespace, SymbolKind.DynamicType, SymbolKind.Assembly, SymbolKind.NetModule, SymbolKind.RangeVariable
                    ' These types of symbols are always accessible (if visible).
                    Return True

                Case SymbolKind.Method, SymbolKind.Property, SymbolKind.Field, SymbolKind.Event
                    If symbol.IsStatic Then
                        ' static members aren't accessed "through" an "instance" of any type.  So we
                        ' null out the "through" instance here.  This ensures that we'll understand
                        ' accessing protected statics properly.
                        throughTypeOpt = Nothing
                    End If

                    ' If this is a synthesized operator of dynamic, it's always accessible.
                    If symbol.IsKind(SymbolKind.Method) AndAlso
                            DirectCast(symbol, IMethodSymbol).MethodKind = MethodKind.BuiltinOperator AndAlso
                            symbol.ContainingSymbol.IsKind(SymbolKind.DynamicType) Then
                        Return True
                    End If

                    ' If it's a synthesized operator on a pointer, use the pointer's PointedAtType.
                    If symbol.IsKind(SymbolKind.Method) AndAlso
                            DirectCast(symbol, IMethodSymbol).MethodKind = MethodKind.BuiltinOperator AndAlso
                            symbol.ContainingSymbol.IsKind(SymbolKind.PointerType) Then
                        Return IsSymbolAccessibleCore(symbol:=DirectCast(symbol.ContainingSymbol, IPointerTypeSymbol).PointedAtType,
                                                          Within:=Within,
                                                          throughTypeOpt:=Nothing,
                                                          failedThroughTypeCheck:=failedThroughTypeCheck)
                    End If

                    Return IsMemberAccessible(containingType:=symbol.ContainingType,
                                                  declaredAccessibility:=symbol.DeclaredAccessibility,
                                                  within:=Within,
                                                  throughTypeOpt:=throughTypeOpt,
                                                  failedThroughTypeCheck:=failedThroughTypeCheck)

                Case Else
                    Throw UnexpectedValue(symbol.Kind)
            End Select
        End Function

        <Extension>
        Friend Function FindImplementationForAbstractMember(type As INamedTypeSymbol, symbol As ISymbol) As ISymbol
            If symbol.IsAbstract Then
                Return type.GetBaseTypesAndThis() _
                    .SelectMany(Function(t As INamedTypeSymbol) t.GetMembers(symbol.Name)) _
                    .FirstOrDefault(Function(s As ISymbol) SymbolEqualityComparer.Default.Equals(symbol, GetOverriddenMember(s)))
            End If

            Return Nothing
        End Function

        <Extension>
        Friend Function GetAllImplementedMembers(classOrStructType As INamedTypeSymbol,
                                                   interfacesOrAbstractClasses As IEnumerable(Of INamedTypeSymbol),
                                                   CancelToken As CancellationToken) As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))
            Return GetAllImplementedMembers(classOrStructType,
                                            interfacesOrAbstractClasses,
                                            AddressOf IsImplemented,
                                            AddressOf ImplementationExists,
                                            Function(type As INamedTypeSymbol, within As ISymbol)
                                                If type.TypeKind = TypeKind.Interface Then
                                                    Return type.GetMembers().WhereAsArray(
                                                        Function(m As ISymbol) m.DeclaredAccessibility = Microsoft.CodeAnalysis.Accessibility.Public AndAlso
                                                                    m.Kind <> SymbolKind.NamedType AndAlso
                                                                    IsImplementable(m) AndAlso
                                                                    Not IsPropertyWithNonPublicImplementableAccessor(m))
                                                End If
                                                Return type.GetMembers()
                                            End Function,
                                            allowReimplementation:=False,
                                            CancelToken)
        End Function

        <Extension>
        Friend Function GetValidAnonymousTypeProperties(symbol As INamedTypeSymbol) As IEnumerable(Of IPropertySymbol)
            If symbol Is Nothing Then
                Throw New ArgumentNullException(NameOf(symbol))
            End If
            Return symbol.GetMembers().OfType(Of IPropertySymbol)().Where(Function(p As IPropertySymbol) p.CanBeReferencedByName)
        End Function

        Friend Function ImplementsMethodOrProperty(Of T As ISymbol)(csMethodOrProperty As T, interfaceMethodOrProperty As T, ByRef SimpleName As VBS.SimpleNameSyntax) As Boolean
            If csMethodOrProperty.Name <> interfaceMethodOrProperty.Name Then
                Return False
            End If
            If TypeOf csMethodOrProperty Is IMethodSymbol Then
                Dim csMethod As IMethodSymbol = CType(csMethodOrProperty, IMethodSymbol)
                Dim interfaceMethod As IMethodSymbol = CType(interfaceMethodOrProperty, IMethodSymbol)
                If csMethod.Parameters.Length <> interfaceMethod.Parameters.Length Then
                    Return False
                End If
                For Each e As IndexClass(Of IParameterSymbol) In csMethod.Parameters.WithIndex
                    If e.Value.Type.Name <> interfaceMethod.Parameters(e.index).Type.Name Then
                        Return False
                    End If
                Next
            End If
            SimpleName = Factory.IdentifierName(interfaceMethodOrProperty.Name)
            Return True
        End Function

        ' Is the named type "type" accessible from within "within", which must be a named type or
        ' an assembly.
        <Extension>
        Friend Function IsNamedTypeAccessible(type As INamedTypeSymbol, within As ISymbol) As Boolean
            Debug.Assert(TypeOf within Is INamedTypeSymbol OrElse TypeOf within Is IAssemblySymbol)
            If type Is Nothing Then
                Throw New ArgumentNullException(NameOf(type))
            End If

            If type.IsErrorType() Then
                ' Always assume that error types are accessible.
                Return True
            End If

            If Not type.IsDefinition Then
                ' All type argument must be accessible.
                For Each typeArg As ITypeSymbol In type.TypeArguments
                    ' type parameters are always accessible, so don't check those (so common it's
                    ' worth optimizing this).
                    If typeArg.Kind <> SymbolKind.TypeParameter AndAlso
                        typeArg.TypeKind <> TypeKind.Error AndAlso
                        Not IsSymbolAccessibleCore(symbol:=typeArg,
                                                   Within:=within,
                                                   throughTypeOpt:=Nothing,
                                                   failedThroughTypeCheck:=Nothing) Then
                        Return False
                    End If
                Next typeArg
            End If

            Dim containingType As INamedTypeSymbol = type.ContainingType
            Return If(containingType Is Nothing,
                          IsNonNestedTypeAccessible(assembly:=type.ContainingAssembly, declaredAccessibility:=type.DeclaredAccessibility, within:=within),
                          IsMemberAccessible(containingType:=type.ContainingType, declaredAccessibility:=type.DeclaredAccessibility, within:=within, throughTypeOpt:=Nothing, failedThroughTypeCheck:=Nothing))
        End Function

        <Extension>
        Public Iterator Function GetBaseTypesAndThis(namedType As INamedTypeSymbol) As IEnumerable(Of INamedTypeSymbol)
            Dim current As INamedTypeSymbol = namedType
            Do While current IsNot Nothing
                Yield current
                current = current.BaseType
            Loop
        End Function

    End Module
End Namespace
