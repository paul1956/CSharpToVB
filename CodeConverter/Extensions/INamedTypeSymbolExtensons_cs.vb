Option Explicit On
Option Infer Off
Option Strict On

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Namespace IVisualBasicCode.CodeConverter.Util

    Public Module INamedTypeSymbolExtensons

        Private Function IsNonNestedTypeAccessible(assembly As IAssemblySymbol, declaredAccessibility As Microsoft.CodeAnalysis.Accessibility, within As ISymbol) As Boolean
            Contract.Requires(TypeOf within Is INamedTypeSymbol OrElse TypeOf within Is IAssemblySymbol)
            Contract.ThrowIfNull(assembly)
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
                    Throw ExceptionUtilities.UnexpectedValue(declaredAccessibility)
            End Select
        End Function

        ''' <summary>
        ''' Checks if 'symbol' is accessible from within 'within', which must be a INamedTypeSymbol
        ''' or an IAssemblySymbol.  If 'symbol' is accessed off of an expression then
        ''' 'throughTypeOpt' is the type of that expression. This is needed to properly do protected
        ''' access checks. Sets "failedThroughTypeCheck" to true if this protected check failed.
        ''' </summary>
        '// NOTE(cyrusn): I expect this function to be called a lot.  As such, I do not do any memory
        '// allocations in the function itself (including not making any iterators).  This does mean
        '// that certain helper functions that we'd like to call are inlined in this method to
        '// prevent the overhead of returning collections or enumerators.
        <Extension>
        Private Function IsSymbolAccessibleCore(symbol As ISymbol, Within As ISymbol, throughTypeOpt As ITypeSymbol, ByRef failedThroughTypeCheck As Boolean) As Boolean ' must be assembly or named type symbol
            Contract.ThrowIfNull(symbol)
            Contract.ThrowIfNull(Within)
            Contract.Requires(TypeOf Within Is INamedTypeSymbol OrElse TypeOf Within Is IAssemblySymbol)

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
        ' Is the named type "type" accessible from within "within", which must be a named type or
        ' an assembly.
        <Extension>
        Public Function IsNamedTypeAccessible(type As INamedTypeSymbol, within As ISymbol) As Boolean
            Contract.Requires(TypeOf within Is INamedTypeSymbol OrElse TypeOf within Is IAssemblySymbol)
            Contract.ThrowIfNull(type)

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