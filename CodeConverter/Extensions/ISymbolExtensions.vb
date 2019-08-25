' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Diagnostics.CodeAnalysis
Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Namespace IVisualBasicCode.CodeConverter.Util

    ' All type argument must be accessible.

    Partial Public Module ISymbolExtensions

        Public Enum SymbolVisibility
            [Public]
            Internal
            [Private]
        End Enum

        <ExcludeFromCodeCoverage>
        <Extension>
        Private Function GetResultantVisibility(symbol As ISymbol) As SymbolVisibility
            ' Start by assuming it's visible.
            Dim visibility As SymbolVisibility = SymbolVisibility.Public

            Select Case symbol.Kind
                Case SymbolKind.Alias
                    ' Aliases are uber private.  They're only visible in the same file that they
                    ' were declared in.
                    Return SymbolVisibility.Private

                Case SymbolKind.Parameter
                    ' Parameters are only as visible as their containing symbol
                    Return GetResultantVisibility(symbol.ContainingSymbol)

                Case SymbolKind.TypeParameter
                    ' Type Parameters are private.
                    Return SymbolVisibility.Private
            End Select

            Do While symbol IsNot Nothing AndAlso symbol.Kind <> SymbolKind.Namespace
                Select Case symbol.DeclaredAccessibility
                    ' If we see anything private, then the symbol is private.
                    Case Microsoft.CodeAnalysis.Accessibility.NotApplicable, Microsoft.CodeAnalysis.Accessibility.Private
                        Return SymbolVisibility.Private

                    ' If we see anything internal, then knock it down from public to
                    ' internal.
                    Case Microsoft.CodeAnalysis.Accessibility.Internal, Microsoft.CodeAnalysis.Accessibility.ProtectedAndInternal
                        visibility = SymbolVisibility.Internal

                        ' For anything else (Public, Protected, ProtectedOrInternal), the
                        ' symbol stays at the level we've gotten so far.
                End Select

                symbol = symbol.ContainingSymbol
            Loop

            Return visibility
        End Function

        <Extension>
        Public Function ConvertISymbolToType(ByVal symbol As ISymbol, ByVal compilation As Compilation, Optional ByVal extensionUsedAsInstance As Boolean = False) As ITypeSymbol
            Dim _type As ITypeSymbol = TryCast(symbol, ITypeSymbol)
            If _type IsNot Nothing Then
                Return _type
            End If

            Dim method As IMethodSymbol = TryCast(symbol, IMethodSymbol)
            If method IsNot Nothing AndAlso Not method.Parameters.Any(Function(p As IParameterSymbol) p.RefKind <> RefKind.None) Then
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
                        Catch ex As Exception
                            Stop
                        End Try
                    End If
                End If
            End If

            ' Otherwise, just default to object.
            Return compilation.ObjectType
        End Function

        <ExcludeFromCodeCoverage>
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

    End Module
End Namespace