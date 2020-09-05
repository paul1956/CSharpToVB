' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Module ISymbolExtensions
    <Extension>
    Private Function ActionType(compilation As Compilation) As INamedTypeSymbol
        Return compilation.GetTypeByMetadataName(GetType(Action).FullName)
    End Function

    <Extension>
    Friend Function GetImplementsClauseForProperty(csProperty As IPropertySymbol, Node As CSharp.CSharpSyntaxNode, Model As SemanticModel, ListOfRequiredInterfaces As ImmutableArray(Of (InterfaceName As INamedTypeSymbol, MethodList As ImmutableArray(Of ISymbol)))) As VBS.ImplementsClauseSyntax
        If Not ListOfRequiredInterfaces.Any Then
            Return Nothing
        End If
        Dim SeparatedList As New List(Of VBS.QualifiedNameSyntax)
        For Each entry As (InterfaceName As INamedTypeSymbol, MethodList As ImmutableArray(Of ISymbol)) In ListOfRequiredInterfaces
            Dim InterfaceName As VBS.NameSyntax = Factory.IdentifierName(entry.InterfaceName.ToString)
            For Each InterfaceProperty As ISymbol In entry.MethodList
                Dim _Right As VBS.SimpleNameSyntax = Nothing
                If TypeOf InterfaceProperty Is IPropertySymbol Then
                    If ImplementsMethodOrProperty(csProperty, CType(InterfaceProperty, IPropertySymbol), _Right) Then
                        Dim name As VBS.NameSyntax = ConvertISymbolToNameSyntaxInterfaceName(Node, InterfaceProperty, Model)
                        If TypeOf name Is VBS.QualifiedNameSyntax Then
                            Dim qualifiedName As VBS.QualifiedNameSyntax = CType(name, VBS.QualifiedNameSyntax)
                            SeparatedList.Add(Factory.QualifiedName(qualifiedName, _Right))
                        ElseIf TypeOf name Is VBS.SimpleNameSyntax Then
                            Dim simpleName As VBS.SimpleNameSyntax = CType(name, VBS.SimpleNameSyntax)
                            SeparatedList.Add(Factory.QualifiedName(simpleName, _Right))
                        ElseIf TypeOf name Is VBS.IdentifierNameSyntax Then
                            Dim identifier As VBS.IdentifierNameSyntax = CType(name, VBS.IdentifierNameSyntax)
                            SeparatedList.Add(Factory.QualifiedName(identifier, _Right))
                        Else
                            Stop
                        End If
                        Exit For
                    End If
                End If
            Next
        Next
        If SeparatedList.Count = 0 Then
            Return Nothing
        End If
        Return Factory.ImplementsClause(Factory.SeparatedList(SeparatedList))
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

    <Extension()>
    Public Function ExplicitInterfaceImplementations(symbol As ISymbol) As ImmutableArray(Of ISymbol)
        If symbol Is Nothing Then
            Throw New ArgumentNullException(NameOf(symbol))
        End If

        Return symbol.TypeSwitch(
        Function([event] As IEventSymbol) [event].ExplicitInterfaceImplementations.As(Of ISymbol)(),
        Function(method As IMethodSymbol) method.ExplicitInterfaceImplementations.As(Of ISymbol)(),
        Function([property] As IPropertySymbol) [property].ExplicitInterfaceImplementations.As(Of ISymbol)(),
        Function(__) ImmutableArray.Create(Of ISymbol)())
    End Function

    <Extension>
    Public Function ExtractBestMatch(Of TSymbol As {Class, ISymbol})(info As SymbolInfo, Optional isMatch As Func(Of TSymbol, Boolean) = Nothing) As TSymbol
        isMatch = If(isMatch, Function(_1) True)
        If info.Symbol Is Nothing AndAlso info.CandidateSymbols.IsEmpty Then
            Return Nothing
        End If
        If info.Symbol IsNot Nothing Then
            Return TryCast(info.Symbol, TSymbol)
        End If
        Dim matches As List(Of TSymbol) = info.CandidateSymbols.OfType(Of TSymbol)().Where(isMatch).ToList()
        If matches.Count = 1 Then
            Return matches.Single()
        End If

        Return Nothing
    End Function

    <Extension()>
    Public Function GetArity(symbol As ISymbol) As Integer
        If symbol Is Nothing Then
            Throw New ArgumentNullException(NameOf(symbol))
        End If

        Select Case symbol.Kind
            Case SymbolKind.NamedType
                Return CType(symbol, INamedTypeSymbol).Arity
            Case SymbolKind.Method
                Return CType(symbol, IMethodSymbol).Arity
            Case Else
                Return 0
        End Select
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
    Public Function IsDefinedInSource(symbol As ISymbol) As Boolean
        If symbol Is Nothing Then
            Throw New ArgumentNullException(NameOf(symbol))
        End If

        Return symbol.Locations.Any(Function(loc) loc.IsInSource)
    End Function

    <Extension()>
    Public Function IsInterfaceType(symbol As ISymbol) As Boolean
        If symbol Is Nothing OrElse TryCast(symbol, ITypeSymbol) Is Nothing Then
            Return False
        End If
        Return DirectCast(symbol, ITypeSymbol).IsInterfaceType() = True
    End Function

    <Extension>
    Public Function IsKind(symbol As ISymbol, kind As SymbolKind) As Boolean
        If symbol Is Nothing Then
            Return False
        End If
        Return symbol.Kind = kind
    End Function

    <Extension>
    Public Function MatchesKind(symbol As ISymbol, ParamArray kinds() As SymbolKind) As Boolean
        If symbol Is Nothing Then
            Return False
        End If
        Return kinds.Contains(symbol.Kind)
    End Function

End Module
