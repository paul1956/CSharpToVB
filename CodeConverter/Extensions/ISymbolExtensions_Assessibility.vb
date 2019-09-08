Imports Microsoft.CodeAnalysis

Namespace CSharpToVBCodeConverter.Util

    Partial Public Module ISymbolExtensions

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
        Private Function IsSymbolAccessibleCore(symbol As ISymbol, Within As ISymbol, throughTypeOpt As ITypeSymbol, ByRef failedThroughTypeCheck As Boolean) As Boolean ' must be assembly or named type symbol
            Contracts.Contract.Requires(symbol IsNot Nothing)
            Contracts.Contract.Requires(Within IsNot Nothing)
            Contracts.Contract.Assume(TypeOf Within Is INamedTypeSymbol OrElse TypeOf Within Is IAssemblySymbol)

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

    End Module
End Namespace
