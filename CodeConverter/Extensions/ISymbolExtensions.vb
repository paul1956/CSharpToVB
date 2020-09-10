' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis

Namespace CSharpToVBConverter

    Public Module ISymbolExtensions

        <Extension>
        Private Function ActionType(compilation As Compilation) As INamedTypeSymbol
            Return compilation.GetTypeByMetadataName(GetType(Action).FullName)
        End Function

        <Extension>
        Friend Function ConvertISymbolToType(symbol As ISymbol, compilation As Compilation, Optional extensionUsedAsInstance As Boolean = False) As ITypeSymbol
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
        Friend Function ExplicitInterfaceImplementations(symbol As ISymbol) As ImmutableArray(Of ISymbol)
            If symbol Is Nothing Then
                Throw New ArgumentNullException(NameOf(symbol))
            End If

            Return symbol.TypeSwitch(
        Function([event] As IEventSymbol) [event].ExplicitInterfaceImplementations.As(Of ISymbol)(),
        Function(method As IMethodSymbol) method.ExplicitInterfaceImplementations.As(Of ISymbol)(),
        Function([property] As IPropertySymbol) [property].ExplicitInterfaceImplementations.As(Of ISymbol)(),
        Function(__) ImmutableArray.Create(Of ISymbol)())
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

        <Extension()>
        Friend Function IsDefinedInSource(symbol As ISymbol) As Boolean
            If symbol Is Nothing Then
                Throw New ArgumentNullException(NameOf(symbol))
            End If

            Return symbol.Locations.Any(Function(loc) loc.IsInSource)
        End Function

        <Extension()>
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
