' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices
Imports System.Threading

Imports CSharpToVBCodeConverter.DestVisualBasic

Imports Microsoft.CodeAnalysis

Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.Util

    Public Module INamedTypeSymbolExtensons

        Private Function ConvertISymbolToNameSyntaxInterfaceName(interfaceMethod As ISymbol) As VBS.NameSyntax
            Dim TypeString As String = interfaceMethod.ContainingSymbol.ToString
            TypeString = TypeString.Replace("<", "(Of ", StringComparison.Ordinal).
                                    Replace(">", ")", StringComparison.Ordinal).
                                    Replace("[", "(", StringComparison.Ordinal).
                                    Replace("]", ")", StringComparison.Ordinal)
            Dim FirstTupleIndex As Integer = TypeString.IndexOf("(Of (", StringComparison.Ordinal)
            If FirstTupleIndex < 0 Then
                Return VBFactory.ParseName(TypeString)
            End If
            FirstTupleIndex += 4
            Dim Result As String = TypeString.Substring(0, FirstTupleIndex)
            Dim OpenIndex As Integer = FirstTupleIndex
            Dim OpenParenCount As Integer = 0
            Dim CloseIndex As Integer = FirstTupleIndex
            Dim TupleList As New List(Of String)
            While CloseIndex < TypeString.Length - 1
                Select Case TypeString.Substring(CloseIndex, 1)
                    Case "("
                        OpenParenCount += 1
                    Case ")"
                        OpenParenCount -= 1
                        If OpenParenCount = 0 Then
                            Dim TupleString As String = TypeString.Substring(OpenIndex, (CloseIndex - OpenIndex) + 1)
                            Result &= CSharpConverter.ExtractConvertedTuple(TupleString)
                            If CloseIndex < TypeString.Length - 2 Then
                                Stop
                            Else
                                Exit While
                            End If
                        End If
                    Case Else
                End Select
                CloseIndex += 1
            End While
            Result = $"{Result})"
            Return VBFactory.ParseName(Result)
        End Function

        Private Function GetAbstractClassesToImplement(abstractClasses As IEnumerable(Of INamedTypeSymbol)) As ImmutableArray(Of INamedTypeSymbol)
            Return abstractClasses.SelectMany(Function(a As INamedTypeSymbol) a.GetBaseTypesAndThis()).Where(Function(t As INamedTypeSymbol) t.IsAbstractClass()).ToImmutableArray()
        End Function

        <Extension>
        Private Function GetAllImplementedMembers(classOrStructType As INamedTypeSymbol,
                                                    interfacesOrAbstractClasses As IEnumerable(Of INamedTypeSymbol),
                                                    isImplemented As Func(Of INamedTypeSymbol,
                                                    ISymbol,
                                                    Func(Of INamedTypeSymbol, ISymbol, Boolean), CancellationToken, Boolean),
                                                    isValidImplementation As Func(Of INamedTypeSymbol, ISymbol, Boolean),
                                                    interfaceMemberGetter As Func(Of INamedTypeSymbol, ISymbol, ImmutableArray(Of ISymbol)),
                                                    allowReimplementation As Boolean,
                                                    CancelToken As CancellationToken) As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))
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
            Return typesToImplement.SelectAsArray(Function(s As INamedTypeSymbol) (s, members:=GetImplementedMembers(classOrStructType, s, isImplemented, isValidImplementation, interfaceMemberGetter, CancelToken))).WhereAsArray(Function(t As (s As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))) t.members.Length > 0)
        End Function

        <Extension>
        Private Function GetAllUnimplementedMembers(classOrStructType As INamedTypeSymbol,
                                                    interfacesOrAbstractClasses As IEnumerable(Of INamedTypeSymbol),
                                                    isImplemented As Func(Of INamedTypeSymbol,
                                                    ISymbol,
                                                    Func(Of INamedTypeSymbol, ISymbol, Boolean), CancellationToken, Boolean),
                                                    isValidImplementation As Func(Of INamedTypeSymbol, ISymbol, Boolean),
                                                    interfaceMemberGetter As Func(Of INamedTypeSymbol, ISymbol, ImmutableArray(Of ISymbol)),
                                                    allowReimplementation As Boolean,
                                                    CancelToken As CancellationToken) As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))

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
            Return typesToImplement.SelectAsArray(Function(s As INamedTypeSymbol) (s, members:=GetUnimplementedMembers(classOrStructType, s, isImplemented, isValidImplementation, interfaceMemberGetter, CancelToken))).WhereAsArray(Function(t As (s As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))) t.members.Length > 0)
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

        Private Function GetUnimplementedMembers(classOrStructType As INamedTypeSymbol, interfaceType As INamedTypeSymbol, isImplemented As Func(Of INamedTypeSymbol, ISymbol, Func(Of INamedTypeSymbol, ISymbol, Boolean), CancellationToken, Boolean), isValidImplementation As Func(Of INamedTypeSymbol, ISymbol, Boolean), interfaceMemberGetter As Func(Of INamedTypeSymbol, ISymbol, ImmutableArray(Of ISymbol)), CancelToken As CancellationToken) As ImmutableArray(Of ISymbol)
            Dim q As IEnumerable(Of ISymbol) = From m In interfaceMemberGetter(interfaceType, classOrStructType)
                                               Where m.Kind <> SymbolKind.NamedType
                                               Where m.Kind <> SymbolKind.Method OrElse DirectCast(m, IMethodSymbol).MethodKind = MethodKind.Ordinary
                                               Where m.Kind <> SymbolKind.Property OrElse DirectCast(m, IPropertySymbol).IsIndexer OrElse DirectCast(m, IPropertySymbol).CanBeReferencedByName
                                               Where m.Kind <> SymbolKind.Event OrElse DirectCast(m, IEventSymbol).CanBeReferencedByName
                                               Where Not isImplemented(classOrStructType, m, isValidImplementation, CancelToken)
                                               Select m

            Return q.ToImmutableArray()
        End Function

        Private Function ImplementationExists(classOrStructType As INamedTypeSymbol, member As ISymbol) As Boolean
            Return classOrStructType.FindImplementationForInterfaceMember(member) IsNot Nothing
        End Function

        Private Function ImplementsMethodOrProperty(Of T As ISymbol)(csMethodOrProperty As T, interfaceMethodOrProperty As T, ByRef SimpleName As VBS.SimpleNameSyntax) As Boolean
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
                    If e.Value.[Type].Name <> interfaceMethod.Parameters(e.Index).[Type].Name Then
                        Return False
                    End If
                Next
            End If
            SimpleName = VBFactory.IdentifierName(interfaceMethodOrProperty.Name)
            Return True
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

        Private Function IsNonPublicImplementableAccessor(accessor As IMethodSymbol) As Boolean
            Return accessor IsNot Nothing AndAlso IsImplementable(accessor) AndAlso accessor.DeclaredAccessibility <> Microsoft.CodeAnalysis.Accessibility.Public
        End Function

        Private Function IsPropertyWithNonPublicImplementableAccessor(member As ISymbol) As Boolean
            If member.Kind <> SymbolKind.Property Then
                Return False
            End If

            Dim [property] As IPropertySymbol = DirectCast(member, IPropertySymbol)

            Return IsNonPublicImplementableAccessor([property].GetMethod) OrElse IsNonPublicImplementableAccessor([property].SetMethod)
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
            Return classOrStructType.GetAllImplementedMembers(
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
        Friend Function GetAllUnimplementedMembers(classOrStructType As INamedTypeSymbol,
                                               interfacesOrAbstractClasses As IEnumerable(Of INamedTypeSymbol),
                                               CancelToken As CancellationToken) As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))
            Return classOrStructType.GetAllUnimplementedMembers(
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
        Friend Function GetImplementsClauseForMethod(ListOfRequiredInterfaces As ImmutableArray(Of (InterfaceName As INamedTypeSymbol, MethodList As ImmutableArray(Of ISymbol))), csMethod As IMethodSymbol) As VBS.ImplementsClauseSyntax
            If Not ListOfRequiredInterfaces.Any Then
                Return Nothing
            End If
            Dim SeparatedList As New List(Of VBS.QualifiedNameSyntax)
            For Each entry As (InterfaceName As INamedTypeSymbol, MethodList As ImmutableArray(Of ISymbol)) In ListOfRequiredInterfaces
                Dim InterfaceName As VBS.NameSyntax = VBFactory.IdentifierName(entry.InterfaceName.ToString)
                For Each InterfaceMethod As ISymbol In entry.MethodList
                    Dim _Right As VBS.SimpleNameSyntax = Nothing
                    If TypeOf InterfaceMethod Is IMethodSymbol Then
                        If ImplementsMethodOrProperty(csMethod, CType(InterfaceMethod, IMethodSymbol), _Right) Then
                            Dim QualifiedName As VBS.QualifiedNameSyntax = CType(ConvertISymbolToNameSyntaxInterfaceName(InterfaceMethod), VBS.QualifiedNameSyntax)
                            SeparatedList.Add(VBFactory.QualifiedName(QualifiedName, _Right))
                            Exit For
                        End If
                    End If
                Next
            Next
            If SeparatedList.Count = 0 Then
                Return Nothing
            End If
            Return VBFactory.ImplementsClause(VBFactory.SeparatedList(SeparatedList))
        End Function

        <Extension>
        Friend Function GetImplementsClauseForProperty(ListOfRequiredInterfaces As ImmutableArray(Of (InterfaceName As INamedTypeSymbol, MethodList As ImmutableArray(Of ISymbol))), csProperty As IPropertySymbol) As VBS.ImplementsClauseSyntax
            If Not ListOfRequiredInterfaces.Any Then
                Return Nothing
            End If
            Dim SeparatedList As New List(Of VBS.QualifiedNameSyntax)
            For Each entry As (InterfaceName As INamedTypeSymbol, MethodList As ImmutableArray(Of ISymbol)) In ListOfRequiredInterfaces
                Dim InterfaceName As VBS.NameSyntax = VBFactory.IdentifierName(entry.InterfaceName.ToString)
                For Each InterfaceProperty As ISymbol In entry.MethodList
                    Dim _Right As VBS.SimpleNameSyntax = Nothing
                    If TypeOf InterfaceProperty Is IPropertySymbol Then
                        If ImplementsMethodOrProperty(csProperty, CType(InterfaceProperty, IPropertySymbol), _Right) Then
                            Dim QualifiedName As VBS.QualifiedNameSyntax = CType(ConvertISymbolToNameSyntaxInterfaceName(InterfaceProperty), VBS.QualifiedNameSyntax)
                            SeparatedList.Add(VBFactory.QualifiedName(QualifiedName, _Right))
                            Exit For
                        End If
                    End If
                Next
            Next
            If SeparatedList.Count = 0 Then
                Return Nothing
            End If
            Return VBFactory.ImplementsClause(VBFactory.SeparatedList(SeparatedList))
        End Function

        <Extension()>
        Friend Function GetOverriddenMember(symbol As ISymbol) As ISymbol
            Select Case True
                Case TypeOf symbol Is IMethodSymbol
                    Dim method As IMethodSymbol = CType(symbol, IMethodSymbol)
                    Return method.OverriddenMethod
                Case TypeOf symbol Is IPropertySymbol
                    Dim [property] As IPropertySymbol = CType(symbol, IPropertySymbol)
                    Return [property].OverriddenProperty
                Case TypeOf symbol Is IEventSymbol
                    Dim [event] As IEventSymbol = CType(symbol, IEventSymbol)
                    Return [event].OverriddenEvent
            End Select

            Return Nothing
        End Function

        <Extension>
        Public Iterator Function GetBaseTypesAndThis(namedType As INamedTypeSymbol) As IEnumerable(Of INamedTypeSymbol)
            Dim current As INamedTypeSymbol = namedType
            Do While current IsNot Nothing
                Yield current
                current = current.BaseType
            Loop
        End Function

        <Extension>
        Public Function GetValidAnonymousTypeProperties(symbol As INamedTypeSymbol) As IEnumerable(Of IPropertySymbol)
            If symbol Is Nothing Then
                Throw New ArgumentNullException(NameOf(symbol))
            End If
            Return symbol.GetMembers().OfType(Of IPropertySymbol)().Where(Function(p As IPropertySymbol) p.CanBeReferencedByName)
        End Function

        <Extension>
        Public Function IsKind(symbol As ISymbol, kind As SymbolKind) As Boolean
            If symbol Is Nothing Then
                Return False
            End If
            Return symbol.Kind = kind
        End Function

    End Module
End Namespace
