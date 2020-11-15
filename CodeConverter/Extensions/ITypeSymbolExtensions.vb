' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.ComponentModel
Imports System.Runtime.CompilerServices
Imports CSharpToVBConverter
Imports Microsoft.CodeAnalysis

Namespace CSharpToVBConverter

    <EditorBrowsable(EditorBrowsableState.Never)>
    Public Module ITypeSymbolExtensions

        ' Is the type "withinType" nested within the original type "originalContainingType".
        <Extension>
        Private Function IsNestedWithinOriginalContainingType(withinType As INamedTypeSymbol, originalContainingType As INamedTypeSymbol) As Boolean
            ' Walk up my parent chain and see if I eventually hit the owner.  If so then I'm a
            ' nested type of that owner and I'm allowed access to everything inside of it.
            Dim current As INamedTypeSymbol = withinType.OriginalDefinition
            Do While current IsNot Nothing
                If SymbolEqualityComparer.Default.Equals(current, originalContainingType) Then
                    Return True
                End If

                current = current.ContainingType
            Loop

            Return False
        End Function

        ' Is a private symbol access
        Private Function IsPrivateSymbolAccessible(within As ISymbol, originalContainingType As INamedTypeSymbol) As Boolean
            Dim withinType As INamedTypeSymbol = TryCast(within, INamedTypeSymbol)
            If withinType Is Nothing Then
                ' If we're not within a type, we can't access a private symbol
                Return False
            End If

            ' A private symbol is accessible if we're (optionally nested) inside the type that it
            ' was defined in.
            Return withinType.IsNestedWithinOriginalContainingType(originalContainingType)
        End Function

        ' Is a protected symbol inside "originalContainingType" accessible from within "within",
        ' which much be a named type or an assembly.
        Private Function IsProtectedSymbolAccessible(withinType As INamedTypeSymbol, withinAssembly As IAssemblySymbol, throughTypeOpt As ITypeSymbol, originalContainingType As INamedTypeSymbol, ByRef failedThroughTypeCheck As Boolean) As Boolean
            failedThroughTypeCheck = False

            ' It is not an error to define protected member in a sealed Script class,
            ' it's just a warning. The member behaves like a private one - it is visible
            ' in all subsequent submissions.
            If withinAssembly.IsInteractive AndAlso originalContainingType.IsScriptClass Then
                Return True
            End If

            If withinType Is Nothing Then
                ' If we're not within a type, we can't access a protected symbol
                Return False
            End If

            ' A protected symbol is accessible if we're (optionally nested) inside the type that it
            ' was defined in.

            ' NOTE: It is helpful to consider 'protected' as *increasing* the
            ' accessibility domain of a private member, rather than *decreasing* that of a public
            ' member. members are naturally private; the protected, internal and public access
            ' modifiers all increase the accessibility domain. Since private members are accessible
            ' to nested types, so are protected members.

            ' NOTE(cyrusn): We do this check up front as it is very fast and easy to do.
            If withinType.IsNestedWithinOriginalContainingType(originalContainingType) Then
                Return True
            End If

            ' Protected is really confusing.  Check out 3.5.3 of the language spec "protected access
            ' for instance members" to see how it works.  I actually got the code for this from
            ' LangCompiler::CheckAccessCore
            If True Then
                Dim current As INamedTypeSymbol = withinType.OriginalDefinition
                Dim originalThroughTypeOpt As ITypeSymbol = throughTypeOpt?.OriginalDefinition
                Do While current IsNot Nothing
                    If current.InheritsFromOrEqualsIgnoringConstruction(originalContainingType) Then
                        ' NOTE(cyrusn): We're continually walking up the 'throughType's inheritance
                        ' chain.  We could compute it up front and cache it in a set.  However, i
                        ' don't want to allocate memory in this function.  Also, in practice
                        ' inheritance chains should be very short.  As such, it might actually be
                        ' slower to create and check inside the set versus just walking the
                        ' inheritance chain.
                        If originalThroughTypeOpt Is Nothing OrElse originalThroughTypeOpt.InheritsFromOrEqualsIgnoringConstruction(current) Then
                            Return True
                        Else
                            failedThroughTypeCheck = True
                        End If
                    End If

                    ' NOTE(cyrusn): The container of an original type is always original.
                    current = current.ContainingType
                Loop
            End If

            Return False
        End Function

        <Extension>
        Friend Function GetAllInterfacesIncludingThis(type As ITypeSymbol) As IList(Of INamedTypeSymbol)
            Dim allInterfaces As ImmutableArray(Of INamedTypeSymbol) = type.AllInterfaces
            Dim isINamedType As Boolean = TypeOf type Is INamedTypeSymbol
            Dim namedType As INamedTypeSymbol = If(isINamedType, CType(type, INamedTypeSymbol), Nothing)
            If isINamedType AndAlso namedType.TypeKind = Microsoft.CodeAnalysis.TypeKind.Interface AndAlso Not allInterfaces.Contains(namedType) Then
                Dim result As New List(Of INamedTypeSymbol)(allInterfaces.Length + 1) From {
                namedType
            }
                result.AddRange(allInterfaces)
                Return result
            End If

            Return allInterfaces
        End Function

        ' Determine if "type" inherits from "baseType", ignoring constructed types, and dealing
        ' only with original types.
        <Extension>
        Friend Function InheritsFromOrEqualsIgnoringConstruction(type As ITypeSymbol, baseType As ITypeSymbol) As Boolean
            Dim originalBaseType As ITypeSymbol = baseType.OriginalDefinition
            Return type.GetBaseTypesAndThis.Contains(Function(t As ITypeSymbol) SymbolEquivalenceComparer.s_instance.Equals(t.OriginalDefinition, originalBaseType))
        End Function

        <Extension>
        Friend Function IsAbstractClass(symbol As ITypeSymbol) As Boolean
            Return CBool(symbol?.TypeKind = Microsoft.CodeAnalysis.TypeKind.Class AndAlso symbol.IsAbstract)
        End Function

        <Extension>
        Friend Function IsDelegateType(symbol As ITypeSymbol) As Boolean
            If symbol Is Nothing Then
                Return False
            End If
            Return symbol.TypeKind = TypeKind.Delegate
        End Function

        <Extension>
        Friend Function IsErrorType(symbol As ITypeSymbol) As Boolean
            Return CBool(symbol?.TypeKind = TypeKind.Error)
        End Function

        <Extension>
        Friend Function IsInterfaceType(symbol As ITypeSymbol) As Boolean
            If symbol Is Nothing Then
                Return False
            End If

            Return symbol.TypeKind = TypeKind.Interface
        End Function

        ' Is a member with declared accessibility "declaredAccessiblity" accessible from within
        ' "within", which must be a named type or an assembly.
        Friend Function IsMemberAccessible(containingType As INamedTypeSymbol, declaredAccessibility As Accessibility, within As ISymbol, throughTypeOpt As ITypeSymbol, ByRef failedThroughTypeCheck As Boolean) As Boolean
            failedThroughTypeCheck = False

            Dim originalContainingType As INamedTypeSymbol = containingType.OriginalDefinition
            Dim withinNamedType As INamedTypeSymbol = TryCast(within, INamedTypeSymbol)
            Dim withinAssembly As IAssemblySymbol = If(TryCast(within, IAssemblySymbol), DirectCast(within, INamedTypeSymbol).ContainingAssembly)

            ' A nested symbol is only accessible to us if its container is accessible as well.
            If Not IsNamedTypeAccessible(containingType, within) Then
                Return False
            End If

            Select Case declaredAccessibility
                Case Accessibility.NotApplicable
                    ' TODO(cyrusn): Is this the right thing to do here?  Should the caller ever be
                    ' asking about the accessibility of a symbol that has "NotApplicable" as its
                    ' value?  For now, I'm preserving the behavior of the existing code.  But perhaps
                    ' we should fail here and require the caller to not do this?
                    Return True

                Case Accessibility.Public
                    ' Public symbols are always accessible from any context
                    Return True

                Case Accessibility.Private
                    ' All expressions in the current submission (top-level or nested in a method or
                    ' type) can access previous submission's private top-level members. Previous
                    ' submissions are treated like outer classes for the current submission - the
                    ' inner class can access private members of the outer class.
                    If withinAssembly.IsInteractive AndAlso containingType.IsScriptClass Then
                        Return True
                    End If

                    ' private members never accessible from outside a type.
                    Return withinNamedType IsNot Nothing AndAlso IsPrivateSymbolAccessible(withinNamedType, originalContainingType)

                Case Accessibility.Internal
                    ' An internal type is accessible if we're in the same assembly or we have
                    ' friend access to the assembly it was defined in.
                    Return withinAssembly.IsSameAssemblyOrHasFriendAccessTo(containingType.ContainingAssembly)

                Case Accessibility.ProtectedAndInternal
                    If Not withinAssembly.IsSameAssemblyOrHasFriendAccessTo(containingType.ContainingAssembly) Then
                        ' We require internal access.  If we don't have it, then this symbol is
                        ' definitely not accessible to us.
                        Return False
                    End If

                    ' We had internal access.  Also have to make sure we have protected access.
                    Return IsProtectedSymbolAccessible(withinNamedType, withinAssembly, throughTypeOpt, originalContainingType, failedThroughTypeCheck)

                Case Accessibility.ProtectedOrInternal
                    If withinAssembly.IsSameAssemblyOrHasFriendAccessTo(containingType.ContainingAssembly) Then
                        ' If we have internal access to this symbol, then that's sufficient.  no
                        ' need to do the complicated protected case.
                        Return True
                    End If

                    ' We don't have internal access.  But if we have protected access then that's
                    ' sufficient.
                    Return IsProtectedSymbolAccessible(withinNamedType, withinAssembly, throughTypeOpt, originalContainingType, failedThroughTypeCheck)

                Case Accessibility.Protected
                    Return IsProtectedSymbolAccessible(withinNamedType, withinAssembly, throughTypeOpt, originalContainingType, failedThroughTypeCheck)
                Case Else
                    Stop
                    Throw UnreachableException
            End Select
        End Function

        <Extension>
        Friend Function IsSameAssemblyOrHasFriendAccessTo(assembly As IAssemblySymbol, toAssembly As IAssemblySymbol) As Boolean
            Return SymbolEqualityComparer.Default.Equals(assembly, toAssembly) OrElse (assembly.IsInteractive AndAlso toAssembly.IsInteractive) OrElse toAssembly.GivesAccessTo(assembly)
        End Function

        <Extension>
        Public Iterator Function GetBaseTypesAndThis(type As ITypeSymbol) As IEnumerable(Of ITypeSymbol)
            Dim current As ITypeSymbol = type
            While current IsNot Nothing
                Yield current
                current = current.BaseType
            End While
        End Function

    End Module
End Namespace
