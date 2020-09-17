' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis

Namespace CSharpToVBConverter

    Public Module ISymbolExtensions

        <Extension>
        Friend Function ExplicitInterfaceImplementations(symbol As ISymbol) As ImmutableArray(Of ISymbol)
            If symbol Is Nothing Then
                Throw New ArgumentNullException(NameOf(symbol))
            End If

            Return symbol.TypeSwitch(
                Function([event] As IEventSymbol)
                    Return [event].ExplicitInterfaceImplementations.As(Of ISymbol)
                End Function,
                Function(method As IMethodSymbol)
                    Return method.ExplicitInterfaceImplementations.As(Of ISymbol)
                End Function,
                Function([property] As IPropertySymbol)
                    Return [property].ExplicitInterfaceImplementations.As(Of ISymbol)
                End Function,
                Function(__) ImmutableArray.Create(Of ISymbol)())
        End Function

        <Extension>
        Friend Function ExtractBestMatch(Of TSymbol As {Class, ISymbol})(info As SymbolInfo, Optional isMatch As Func(Of TSymbol, Boolean) = Nothing) As TSymbol
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

        <Extension>
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
        Friend Function GetReturnType(symbol As ISymbol) As ITypeSymbol
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

        ''' <summary>
        ''' Checks if 'symbol' is accessible from within 'within'.
        ''' </summary>
        <Extension>
        Friend Function IsAccessibleWithin(symbol As ISymbol, within As ISymbol, Optional throughTypeOpt As ITypeSymbol = Nothing) As Boolean
            Dim isIAssembly As Boolean = TypeOf within Is IAssemblySymbol
            Dim assembly As IAssemblySymbol = If(isIAssembly, CType(within, IAssemblySymbol), Nothing)
            If isIAssembly Then
                Return symbol.IsAccessibleWithin(assembly, throughTypeOpt)
            Else
                Dim isINamedType As Boolean = TypeOf within Is INamedTypeSymbol
                Dim namedType As INamedTypeSymbol = If(isINamedType, CType(within, INamedTypeSymbol), Nothing)
                If isINamedType Then
                    Return symbol.IsAccessibleWithin(namedType, throughTypeOpt)
                Else
                    Throw New ArgumentException($"TypeOf {NameOf(within)} is not {NameOf(INamedTypeSymbol)}")
                End If
            End If
        End Function

        <Extension>
        Friend Function IsDefinedInSource(symbol As ISymbol) As Boolean
            If symbol Is Nothing Then
                Throw New ArgumentNullException(NameOf(symbol))
            End If

            Return symbol.Locations.Any(Function(loc) loc.IsInSource)
        End Function

        <Extension>
        Friend Function IsInterfaceType(symbol As ISymbol) As Boolean
            If symbol Is Nothing OrElse TryCast(symbol, ITypeSymbol) Is Nothing Then
                Return False
            End If
            Return DirectCast(symbol, ITypeSymbol).IsInterfaceType() = True
        End Function

        <Extension>
        Friend Function IsKind(symbol As ISymbol, kind As SymbolKind) As Boolean
            If symbol Is Nothing Then
                Return False
            End If
            Return symbol.Kind = kind
        End Function

        <Extension>
        Friend Function MatchesKind(symbol As ISymbol, ParamArray kinds() As SymbolKind) As Boolean
            If symbol Is Nothing Then
                Return False
            End If
            Return kinds.Contains(symbol.Kind)
        End Function

    End Module
End Namespace
