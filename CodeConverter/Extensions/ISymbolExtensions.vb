' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Namespace CSharpToVBCodeConverter.Util

    ' All type argument must be accessible.

    Public Module ISymbolExtensions

        Public Enum SymbolVisibility
            [Public]
            Internal
            [Private]
        End Enum

        Private Function IsNonNestedTypeAccessible(assembly As IAssemblySymbol, declaredAccessibility As Accessibility, within As ISymbol) As Boolean
            Dim withinAssembly As IAssemblySymbol = If((TryCast(within, IAssemblySymbol)), DirectCast(within, INamedTypeSymbol).ContainingAssembly)
            Select Case declaredAccessibility
                Case Microsoft.CodeAnalysis.Accessibility.NotApplicable, Microsoft.CodeAnalysis.Accessibility.Public
                    ' Public symbols are always accessible from any context
                    Return True

                Case Microsoft.CodeAnalysis.Accessibility.Private, Microsoft.CodeAnalysis.Accessibility.Protected, Microsoft.CodeAnalysis.Accessibility.ProtectedAndInternal
                    ' Shouldn't happen except in error cases.
                    Return False

                Case Microsoft.CodeAnalysis.Accessibility.Internal, Microsoft.CodeAnalysis.Accessibility.ProtectedOrInternal
                    ' An internal type is accessible if we're in the same assembly or we have
                    ' friend access to the assembly it was defined in.
                    Return withinAssembly.IsSameAssemblyOrHasFriendAccessTo(assembly)

                Case Else
                    Throw UnexpectedValue(declaredAccessibility)
            End Select
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
            Dim withinAssembly As IAssemblySymbol = If((TryCast(Within, IAssemblySymbol)), DirectCast(Within, INamedTypeSymbol).ContainingAssembly)

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

        ''' <summary>
        ''' Checks if 'symbol' is accessible from within 'within'.
        ''' </summary>
        <Extension>
        Friend Function IsAccessibleWithin(symbol As ISymbol, within As ISymbol, Optional throughTypeOpt As ITypeSymbol = Nothing) As Boolean
            Dim tempVar As Boolean = TypeOf within Is IAssemblySymbol
            Dim assembly As IAssemblySymbol = If(tempVar, CType(within, IAssemblySymbol), Nothing)
            If tempVar Then
                Return symbol.IsAccessibleWithin(assembly, throughTypeOpt)
            Else
                Dim tempVar2 As Boolean = TypeOf within Is INamedTypeSymbol
                Dim namedType As INamedTypeSymbol = If(tempVar2, CType(within, INamedTypeSymbol), Nothing)
                If tempVar2 Then
                    Return symbol.IsAccessibleWithin(namedType, throughTypeOpt)
                Else
                    Throw New ArgumentException($"TypeOf {NameOf(within)} is not {NameOf(INamedTypeSymbol)}")
                End If
            End If
        End Function

        <Extension>
        Public Function ConvertISymbolToType(symbol As ISymbol, compilation As Compilation, Optional extensionUsedAsInstance As Boolean = False) As ITypeSymbol
            If compilation Is Nothing Then
                Throw New ArgumentNullException(NameOf(compilation))
            End If
            Dim _type As ITypeSymbol = TryCast(symbol, ITypeSymbol)
            If _type IsNot Nothing Then
                Return _type
            End If
            Dim method As IMethodSymbol = TryCast(symbol, IMethodSymbol)
            If method Is Nothing OrElse method.Parameters.Any(Function(p As IParameterSymbol) p.RefKind <> RefKind.None) Then
                ' Otherwise, just default to object.
                Return compilation.ObjectType
            End If
            ' Convert the symbol to Func<...> or Action<...>
            If method.ReturnsVoid Then
                Dim count As Integer = If(extensionUsedAsInstance, method.Parameters.Length - 1, method.Parameters.Length)
                Dim skip As Integer = If(extensionUsedAsInstance, 1, 0)
                count = Math.Max(0, count)
                If count = 0 Then
                    ' Action
                    Return compilation.ActionType()
                Else
                    ' Action<TArg1, ..., TArgN>
                    Dim actionName As String = "System.Action`" & count
                    Dim _ActionType As INamedTypeSymbol = compilation.GetTypeByMetadataName(actionName)

                    If _ActionType IsNot Nothing Then
                        Dim types() As ITypeSymbol = method.Parameters.
                        Skip(skip).
                        Select(Function(p As IParameterSymbol) If(p.Type, compilation.GetSpecialType(SpecialType.System_Object))).
                        ToArray()
                        Return _ActionType.Construct(types)
                    End If
                End If
            Else
                ' Func<TArg1,...,TArgN,TReturn>
                '
                ' +1 for the return type.
                Dim count As Integer = If(extensionUsedAsInstance, method.Parameters.Length - 1, method.Parameters.Length)
                Dim skip As Integer = If(extensionUsedAsInstance, 1, 0)
                Dim functionName As String = "System.Func`" & (count + 1)
                Dim functionType As INamedTypeSymbol = compilation.GetTypeByMetadataName(functionName)

                If functionType IsNot Nothing Then
                    Try
                        Dim CSharpTypes() As ITypeSymbol = method.Parameters.
                        Skip(skip).Select(Function(p As IParameterSymbol) If(p.Type.IsErrorType, compilation.GetSpecialType(SpecialType.System_Object), p.Type)).
                        Concat({method.ReturnType}).
                        Select(Function(t As ITypeSymbol) If(t Is Nothing OrElse t.IsErrorType, compilation.GetSpecialType(SpecialType.System_Object), t)).
                        ToArray()
                        Return functionType.Construct(CSharpTypes)
                    Catch ex As OperationCanceledException
                        Throw
                    Catch ex As Exception
                        Stop
                    End Try
                End If
            End If

            ' Otherwise, just default to object.
            Return compilation.ObjectType
        End Function

        <Extension>
        Public Function GetReturnType(symbol As ISymbol) As ITypeSymbol
            If symbol Is Nothing Then
                Throw New ArgumentNullException(NameOf(symbol))
            End If
            Select Case symbol.Kind
                Case SymbolKind.Field
                    Dim field As IFieldSymbol = DirectCast(symbol, IFieldSymbol)
                    Return field.Type
                Case SymbolKind.Method
                    Dim method As IMethodSymbol = DirectCast(symbol, IMethodSymbol)
                    If method.MethodKind = MethodKind.Constructor Then
                        Return method.ContainingType
                    End If
                    Return method.ReturnType
                Case SymbolKind.Property
                    Dim [property] As IPropertySymbol = DirectCast(symbol, IPropertySymbol)
                    Return [property].Type
                Case SymbolKind.Event
                    Dim evt As IEventSymbol = DirectCast(symbol, IEventSymbol)
                    Return evt.Type
                Case SymbolKind.Parameter
                    Dim param As IParameterSymbol = DirectCast(symbol, IParameterSymbol)
                    Return param.Type
                Case SymbolKind.Local
                    Dim local As ILocalSymbol = DirectCast(symbol, ILocalSymbol)
                    Return local.Type
            End Select
            Return Nothing
        End Function

        <Extension()>
        Public Function IsInterfaceType(symbol As ISymbol) As Boolean
            If symbol Is Nothing OrElse TryCast(symbol, ITypeSymbol) Is Nothing Then
                Return False
            End If
            Return DirectCast(symbol, ITypeSymbol).IsInterfaceType() = True
        End Function

        ' Is the named type "type" accessible from within "within", which must be a named type or
        ' an assembly.
        Public Function IsNamedTypeAccessible(type As INamedTypeSymbol, within As ISymbol) As Boolean
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

    End Module
End Namespace
