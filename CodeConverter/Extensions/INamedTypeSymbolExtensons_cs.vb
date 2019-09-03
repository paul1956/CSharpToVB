' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices
Imports System.Threading
Imports IVisualBasicCode.CodeConverter.Visual_Basic.CSharpConverter
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace IVisualBasicCode.CodeConverter.Util

    Public Module INamedTypeSymbolExtensons

        Private Function ConvertISymbolToNameSyntaxInterfaceName(interfaceMethod As ISymbol) As NameSyntax
            Dim TypeString As String = interfaceMethod.ContainingSymbol.ToString
            TypeString = TypeString.Replace("<", "(Of ").Replace(">", ")").Replace("[", "(").Replace("]", ")")
            Return VisualBasic.SyntaxFactory.ParseName(TypeString)
        End Function

        Private Function GetAbstractClassesToImplement(_classOrStructType As INamedTypeSymbol, abstractClasses As IEnumerable(Of INamedTypeSymbol)) As ImmutableArray(Of INamedTypeSymbol)
            Return abstractClasses.SelectMany(Function(a) a.GetBaseTypesAndThis()).Where(Function(t) t.IsAbstractClass()).ToImmutableArray()
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
                                                    cancellationToken As CancellationToken) As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))
            Contract.ThrowIfNull(classOrStructType)
            Contract.ThrowIfNull(interfacesOrAbstractClasses)
            Contract.ThrowIfNull(isImplemented)

            If classOrStructType.TypeKind <> TypeKind.Class AndAlso classOrStructType.TypeKind <> TypeKind.Struct Then
                Return ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))).Empty
            End If

            If Not interfacesOrAbstractClasses.Any() Then
                Return ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))).Empty
            End If

            If Not interfacesOrAbstractClasses.All(Function(i) i.TypeKind = TypeKind.Interface) AndAlso Not interfacesOrAbstractClasses.All(Function(i) i.IsAbstractClass()) Then
                Return ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))).Empty
            End If

            Dim typesToImplement As ImmutableArray(Of INamedTypeSymbol) = GetTypesToImplement(classOrStructType, interfacesOrAbstractClasses, allowReimplementation, cancellationToken)
            Return typesToImplement.SelectAsArray(Function(s) (s, members:=GetImplementedMembers(classOrStructType, s, isImplemented, isValidImplementation, interfaceMemberGetter, cancellationToken))).WhereAsArray(Function(t) t.members.Length > 0)
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
                                                    cancellationToken As CancellationToken) As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))
            Contract.ThrowIfNull(classOrStructType)
            Contract.ThrowIfNull(interfacesOrAbstractClasses)
            Contract.ThrowIfNull(isImplemented)

            If classOrStructType.TypeKind <> TypeKind.Class AndAlso classOrStructType.TypeKind <> TypeKind.Struct Then
                Return ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))).Empty
            End If

            If Not interfacesOrAbstractClasses.Any() Then
                Return ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))).Empty
            End If

            If Not interfacesOrAbstractClasses.All(Function(i) i.TypeKind = TypeKind.Interface) AndAlso Not interfacesOrAbstractClasses.All(Function(i) i.IsAbstractClass()) Then
                Return ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))).Empty
            End If

            Dim typesToImplement As ImmutableArray(Of INamedTypeSymbol) = GetTypesToImplement(classOrStructType, interfacesOrAbstractClasses, allowReimplementation, cancellationToken)
            Return typesToImplement.SelectAsArray(Function(s) (s, members:=GetUnimplementedMembers(classOrStructType, s, isImplemented, isValidImplementation, interfaceMemberGetter, cancellationToken))).WhereAsArray(Function(t) t.members.Length > 0)
        End Function

        Private Function GetImplementedMembers(classOrStructType As INamedTypeSymbol, interfaceType As INamedTypeSymbol, isImplemented As Func(Of INamedTypeSymbol, ISymbol, Func(Of INamedTypeSymbol, ISymbol, Boolean), CancellationToken, Boolean), isValidImplementation As Func(Of INamedTypeSymbol, ISymbol, Boolean), interfaceMemberGetter As Func(Of INamedTypeSymbol, ISymbol, ImmutableArray(Of ISymbol)), cancellationToken As CancellationToken) As ImmutableArray(Of ISymbol)
            Dim q As IEnumerable(Of ISymbol) = From m In interfaceMemberGetter(interfaceType, classOrStructType)
                                               Where m.Kind <> SymbolKind.NamedType
                                               Where m.Kind <> SymbolKind.Method OrElse DirectCast(m, IMethodSymbol).MethodKind = MethodKind.Ordinary
                                               Where m.Kind <> SymbolKind.Property OrElse DirectCast(m, IPropertySymbol).IsIndexer OrElse DirectCast(m, IPropertySymbol).CanBeReferencedByName
                                               Where m.Kind <> SymbolKind.Event OrElse DirectCast(m, IEventSymbol).CanBeReferencedByName
                                               Where isImplemented(classOrStructType, m, isValidImplementation, cancellationToken)
                                               Select m

            Return q.ToImmutableArray()
        End Function

        Private Function GetInterfacesToImplement(classOrStructType As INamedTypeSymbol, interfaces As IEnumerable(Of INamedTypeSymbol), allowReimplementation As Boolean, cancellationToken As CancellationToken) As ImmutableArray(Of INamedTypeSymbol)
            ' We need to not only implement the specified interface, but also everything it
            ' inherits from.
            cancellationToken.ThrowIfCancellationRequested()
            Dim interfacesToImplement As New List(Of INamedTypeSymbol)(interfaces.SelectMany(Function(i) i.GetAllInterfacesIncludingThis()).Distinct())

            ' However, there's no need to re-implement any interfaces that our base types already
            ' implement.  By definition they must contain all the necessary methods.
            Dim baseType As INamedTypeSymbol = classOrStructType.BaseType
            Dim alreadyImplementedInterfaces As ImmutableArray(Of INamedTypeSymbol) = If(baseType Is Nothing OrElse allowReimplementation, New ImmutableArray(Of INamedTypeSymbol), baseType.AllInterfaces)

            cancellationToken.ThrowIfCancellationRequested()
            interfacesToImplement.RemoveRange(alreadyImplementedInterfaces)
            Return interfacesToImplement.ToImmutableArray()
        End Function

#Disable Warning IDE0051 ' Remove unused private members
        Private Function GetMembers(type As INamedTypeSymbol, _within As ISymbol) As ImmutableArray(Of ISymbol)
#Enable Warning IDE0051 ' Remove unused private members
            Return type.GetMembers()
        End Function

        Private Function GetTypesToImplement(classOrStructType As INamedTypeSymbol, interfacesOrAbstractClasses As IEnumerable(Of INamedTypeSymbol), allowReimplementation As Boolean, cancellationToken As CancellationToken) As ImmutableArray(Of INamedTypeSymbol)
            Return If(interfacesOrAbstractClasses.First().TypeKind = TypeKind.Interface, GetInterfacesToImplement(classOrStructType, interfacesOrAbstractClasses, allowReimplementation, cancellationToken), GetAbstractClassesToImplement(classOrStructType, interfacesOrAbstractClasses))
        End Function

        Private Function GetUnimplementedMembers(classOrStructType As INamedTypeSymbol, interfaceType As INamedTypeSymbol, isImplemented As Func(Of INamedTypeSymbol, ISymbol, Func(Of INamedTypeSymbol, ISymbol, Boolean), CancellationToken, Boolean), isValidImplementation As Func(Of INamedTypeSymbol, ISymbol, Boolean), interfaceMemberGetter As Func(Of INamedTypeSymbol, ISymbol, ImmutableArray(Of ISymbol)), cancellationToken As CancellationToken) As ImmutableArray(Of ISymbol)
            Dim q As IEnumerable(Of ISymbol) = From m In interfaceMemberGetter(interfaceType, classOrStructType)
                                               Where m.Kind <> SymbolKind.NamedType
                                               Where m.Kind <> SymbolKind.Method OrElse DirectCast(m, IMethodSymbol).MethodKind = MethodKind.Ordinary
                                               Where m.Kind <> SymbolKind.Property OrElse DirectCast(m, IPropertySymbol).IsIndexer OrElse DirectCast(m, IPropertySymbol).CanBeReferencedByName
                                               Where m.Kind <> SymbolKind.Event OrElse DirectCast(m, IEventSymbol).CanBeReferencedByName
                                               Where Not isImplemented(classOrStructType, m, isValidImplementation, cancellationToken)
                                               Select m

            Return q.ToImmutableArray()
        End Function

        Private Function ImplementationExists(classOrStructType As INamedTypeSymbol, member As ISymbol) As Boolean
            Return classOrStructType.FindImplementationForInterfaceMember(member) IsNot Nothing
        End Function

        Private Function ImplementsMethodOrProperty(Of T As ISymbol)(cS_MethodOrProperty As T, interfaceMethodOrProperty As T, ByRef SimpleName As SimpleNameSyntax) As Boolean
            If cS_MethodOrProperty.Name <> interfaceMethodOrProperty.Name Then
                Return False
            End If
            If TypeOf cS_MethodOrProperty Is IMethodSymbol Then
                Dim CS_Method As IMethodSymbol = CType(cS_MethodOrProperty, IMethodSymbol)
                Dim interfaceMethod As IMethodSymbol = CType(interfaceMethodOrProperty, IMethodSymbol)
                If CS_Method.Parameters.Count <> interfaceMethod.Parameters.Count Then
                    Return False
                End If
                For i As Integer = 0 To CS_Method.Parameters.Count - 1
                    If CS_Method.Parameters(i).[Type].Name <> interfaceMethod.Parameters(i).[Type].Name Then
                        Return False
                    End If
                Next
            End If
            SimpleName = VisualBasic.SyntaxFactory.IdentifierName(interfaceMethodOrProperty.Name)
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

#Disable Warning IDE0051 ' Remove unused private members
        Private Function IsExplicitlyImplemented(
            classOrStructType As INamedTypeSymbol,
            member As ISymbol,
            _isValid As Func(Of INamedTypeSymbol, ISymbol, Boolean)) As Boolean
#Enable Warning IDE0051 ' Remove unused private members

            Dim implementation As ISymbol = classOrStructType.FindImplementationForInterfaceMember(member)

            If implementation?.ContainingType.TypeKind = TypeKind.[Interface] Then
                ' Treat all implementations in interfaces as explicit, even the original declaration with implementation.
                ' There are no implicit interface implementations in derived interfaces and it feels reasonable to treat
                ' original declaration with implementation as an explicit implementation as well, the implementation is
                ' explicitly provided after all. All implementations in interfaces will be treated uniformly.
                Return True
            End If

            Select Case True
                Case TypeOf implementation Is IEventSymbol
                    Dim [event] As IEventSymbol = CType(implementation, IEventSymbol)
                    Return [event].ExplicitInterfaceImplementations.Length > 0
                Case TypeOf implementation Is IMethodSymbol
                    Dim method As IMethodSymbol = CType(implementation, IMethodSymbol)
                    Return method.ExplicitInterfaceImplementations.Length > 0
                Case TypeOf implementation Is IPropertySymbol
                    Dim [property] As IPropertySymbol = CType(implementation, IPropertySymbol)
                    Return [property].ExplicitInterfaceImplementations.Length > 0
                Case Else
                    Return False
            End Select
        End Function

        Private Function IsImplementable(m As ISymbol) As Boolean
            Return m.IsVirtual OrElse m.IsAbstract
        End Function

        Private Function IsImplemented(classOrStructType As INamedTypeSymbol, member As ISymbol, isValidImplementation As Func(Of INamedTypeSymbol, ISymbol, Boolean), cancellationToken As CancellationToken) As Boolean
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

        Private Function IsInaccessibleImplementableAccessor(accessor As IMethodSymbol, within As ISymbol) As Boolean
            Return accessor IsNot Nothing AndAlso IsImplementable(accessor) AndAlso Not accessor.IsAccessibleWithin(within)
        End Function

        Private Function IsInterfacePropertyImplemented(classOrStructType As INamedTypeSymbol, propertySymbol As IPropertySymbol) As Boolean
            ' A property is only fully implemented if both it's setter and getter is implemented.

            Return IsAccessorImplemented(propertySymbol.GetMethod, classOrStructType) AndAlso IsAccessorImplemented(propertySymbol.SetMethod, classOrStructType)

        End Function

        Private Function IsNonPublicImplementableAccessor(accessor As IMethodSymbol) As Boolean
            Return accessor IsNot Nothing AndAlso IsImplementable(accessor) AndAlso accessor.DeclaredAccessibility <> Microsoft.CodeAnalysis.Accessibility.Public
        End Function

#Disable Warning IDE0051 ' Remove unused private members
        Private Function IsPropertyWithInaccessibleImplementableAccessor(member As ISymbol, within As ISymbol) As Boolean
#Enable Warning IDE0051 ' Remove unused private members
            If member.Kind <> SymbolKind.Property Then
                Return False
            End If

            Dim [property] As IPropertySymbol = DirectCast(member, IPropertySymbol)

            Return IsInaccessibleImplementableAccessor([property].GetMethod, within) OrElse IsInaccessibleImplementableAccessor([property].SetMethod, within)
        End Function

        Private Function IsPropertyWithNonPublicImplementableAccessor(member As ISymbol) As Boolean
            If member.Kind <> SymbolKind.Property Then
                Return False
            End If

            Dim [property] As IPropertySymbol = DirectCast(member, IPropertySymbol)

            Return IsNonPublicImplementableAccessor([property].GetMethod) OrElse IsNonPublicImplementableAccessor([property].SetMethod)
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
        Public Function FindImplementationForAbstractMember(type As INamedTypeSymbol, symbol As ISymbol) As ISymbol
            If symbol.IsAbstract Then
                Return type.GetBaseTypesAndThis().SelectMany(Function(t) t.GetMembers(symbol.Name)).FirstOrDefault(Function(s) DirectCast(symbol, Object).Equals(GetOverriddenMember(s)))
            End If

            Return Nothing
        End Function

        <Extension>
        Public Function GetAllImplementedMembers(classOrStructType As INamedTypeSymbol,
                                               interfacesOrAbstractClasses As IEnumerable(Of INamedTypeSymbol),
                                               cancellationToken As CancellationToken) As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))
            Return classOrStructType.GetAllImplementedMembers(
                            interfacesOrAbstractClasses,
                            AddressOf IsImplemented,
                            AddressOf ImplementationExists,
                            Function(type As INamedTypeSymbol, within As ISymbol)
                                If type.TypeKind = TypeKind.Interface Then
                                    Return type.GetMembers().WhereAsArray(
                                        Function(m) m.DeclaredAccessibility = Microsoft.CodeAnalysis.Accessibility.Public AndAlso
                                                    m.Kind <> SymbolKind.NamedType AndAlso
                                                    IsImplementable(m) AndAlso
                                                    Not IsPropertyWithNonPublicImplementableAccessor(m))
                                End If
                                Return type.GetMembers()
                            End Function,
                            allowReimplementation:=False,
                            cancellationToken:=cancellationToken)
        End Function

        '<Extension>
        'Public Function GetAllUnimplementedExplicitMembers(classOrStructType As INamedTypeSymbol,
        '                                               interfaces As IEnumerable(Of INamedTypeSymbol),
        '                                               cancellationToken As CancellationToken) As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))
        '    Return classOrStructType.GetAllUnimplementedMembers(
        '                    interfaces,
        '                    AddressOf IsExplicitlyImplemented,
        '                    AddressOf ImplementationExists,
        '                    Function(type As INamedTypeSymbol, within As ISymbol)
        '                        If type.TypeKind = TypeKind.Interface Then
        '                            Return type.GetMembers().WhereAsArray(Function(m) m.Kind <> SymbolKind.NamedType AndAlso
        '                                                                        IsImplementable(m) AndAlso
        '                                                                        m.IsAccessibleWithin(within) AndAlso
        '                                                                        Not IsPropertyWithInaccessibleImplementableAccessor(m, within))
        '                        End If
        '                        Return type.GetMembers()
        '                    End Function,
        '                    allowReimplementation:=False,
        '                    cancellationToken:=cancellationToken)

        'End Function

        <Extension>
        Public Function GetAllUnimplementedMembers(classOrStructType As INamedTypeSymbol,
                                               interfacesOrAbstractClasses As IEnumerable(Of INamedTypeSymbol),
                                               cancellationToken As CancellationToken) As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))
            Return classOrStructType.GetAllUnimplementedMembers(
                            interfacesOrAbstractClasses,
                            AddressOf IsImplemented,
                            AddressOf ImplementationExists,
                            Function(type As INamedTypeSymbol, within As ISymbol)
                                If type.TypeKind = TypeKind.Interface Then
                                    Return type.GetMembers().WhereAsArray(
                                        Function(m) m.DeclaredAccessibility = Microsoft.CodeAnalysis.Accessibility.Public AndAlso
                                                    m.Kind <> SymbolKind.NamedType AndAlso
                                                    IsImplementable(m) AndAlso
                                                    Not IsPropertyWithNonPublicImplementableAccessor(m))
                                End If
                                Return type.GetMembers()
                            End Function,
                            allowReimplementation:=False,
                            cancellationToken:=cancellationToken)
        End Function

        '<Extension>
        'Public Function GetAllUnimplementedMembersInThis(classOrStructType As INamedTypeSymbol,
        '                                             interfacesOrAbstractClasses As IEnumerable(Of INamedTypeSymbol),
        '                                             cancellationToken As CancellationToken) As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))
        '    Return classOrStructType.GetAllUnimplementedMembers(
        '    interfacesOrAbstractClasses,
        '    AddressOf IsImplemented,
        '    Function(t, m)
        '        Dim implementation As ISymbol = classOrStructType.FindImplementationForInterfaceMember(m)
        '        Return implementation IsNot Nothing AndAlso
        '               Equals(implementation.ContainingType, classOrStructType)
        '    End Function,
        '    AddressOf GetMembers,
        '    allowReimplementation:=True,
        '    cancellationToken:=cancellationToken)
        'End Function

        '<Extension>
        'Public Function GetAllUnimplementedMembersInThis(classOrStructType As INamedTypeSymbol,
        '                                             interfacesOrAbstractClasses As IEnumerable(Of INamedTypeSymbol),
        '                                             interfaceMemberGetter As Func(Of INamedTypeSymbol, ISymbol, ImmutableArray(Of ISymbol)),
        '                                             cancellationToken As CancellationToken) As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))
        '    Return classOrStructType.GetAllUnimplementedMembers(
        '    interfacesOrAbstractClasses,
        '    AddressOf IsImplemented,
        '    Function(t, m)
        '        Dim implementation As ISymbol = classOrStructType.FindImplementationForInterfaceMember(m)
        '        Return implementation IsNot Nothing AndAlso Equals(implementation.ContainingType, classOrStructType)
        '    End Function,
        '    interfaceMemberGetter,
        '    allowReimplementation:=True,
        '    cancellationToken:=cancellationToken)
        'End Function

        <Extension>
        Public Iterator Function GetBaseTypesAndThis(namedType As INamedTypeSymbol) As IEnumerable(Of INamedTypeSymbol)
            Dim current As INamedTypeSymbol = namedType
            Do While current IsNot Nothing
                Yield current
                current = current.BaseType
            Loop
        End Function

        <Extension>
        Public Function GetImplementsClauseForMethod(ListOfRequiredInterfaces As ImmutableArray(Of (InterfaceName As INamedTypeSymbol, MethodList As ImmutableArray(Of ISymbol))), CS_Method As IMethodSymbol) As ImplementsClauseSyntax
            If ListOfRequiredInterfaces.Count = 0 Then
                Return Nothing
            End If
            Dim SeparatedList As New List(Of QualifiedNameSyntax)
            For Each entry As (InterfaceName As INamedTypeSymbol, MethodList As ImmutableArray(Of ISymbol)) In ListOfRequiredInterfaces
                Dim InterfaceName As NameSyntax = VisualBasic.SyntaxFactory.IdentifierName(entry.InterfaceName.ToString)
                For Each InterfaceMethod As ISymbol In entry.MethodList
                    Dim _Right As SimpleNameSyntax = Nothing
                    If TypeOf InterfaceMethod Is IMethodSymbol Then
                        If ImplementsMethodOrProperty(CS_Method, CType(InterfaceMethod, IMethodSymbol), _Right) Then
                            Dim QualifiedName As QualifiedNameSyntax = CType(ConvertISymbolToNameSyntaxInterfaceName(InterfaceMethod), QualifiedNameSyntax)
                            SeparatedList.Add(VisualBasic.SyntaxFactory.QualifiedName(QualifiedName, _Right))
                            Exit For
                        End If
                    End If
                Next
            Next
            If SeparatedList.Count = 0 Then
                Return Nothing
            End If
            Return VisualBasic.SyntaxFactory.ImplementsClause(VisualBasic.SyntaxFactory.SeparatedList(SeparatedList))
        End Function
        <Extension>
        Public Function GetImplementsClauseForProperty(ListOfRequiredInterfaces As ImmutableArray(Of (InterfaceName As INamedTypeSymbol, MethodList As ImmutableArray(Of ISymbol))), CS_Property As IPropertySymbol) As ImplementsClauseSyntax
            If ListOfRequiredInterfaces.Count = 0 Then
                Return Nothing
            End If
            Dim SeparatedList As New List(Of QualifiedNameSyntax)
            For Each entry As (InterfaceName As INamedTypeSymbol, MethodList As ImmutableArray(Of ISymbol)) In ListOfRequiredInterfaces
                Dim InterfaceName As NameSyntax = VisualBasic.SyntaxFactory.IdentifierName(entry.InterfaceName.ToString)
                For Each InterfaceProperty As ISymbol In entry.MethodList
                    Dim _Right As SimpleNameSyntax = Nothing
                    If TypeOf InterfaceProperty Is IPropertySymbol Then
                        If ImplementsMethodOrProperty(CS_Property, CType(InterfaceProperty, IPropertySymbol), _Right) Then
                            Dim QualifiedName As QualifiedNameSyntax = CType(ConvertISymbolToNameSyntaxInterfaceName(InterfaceProperty), QualifiedNameSyntax)
                            SeparatedList.Add(VisualBasic.SyntaxFactory.QualifiedName(QualifiedName, _Right))
                            Exit For
                        End If
                    End If
                Next
            Next
            If SeparatedList.Count = 0 Then
                Return Nothing
            End If
            Return VisualBasic.SyntaxFactory.ImplementsClause(VisualBasic.SyntaxFactory.SeparatedList(SeparatedList))
        End Function
        <Extension>
        Public Function GetValidAnonymousTypeProperties(symbol As INamedTypeSymbol) As IEnumerable(Of IPropertySymbol)
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
