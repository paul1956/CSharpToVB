' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Shared.Extensions

Namespace CSharpToVBConverter

    Partial Friend Class SymbolEquivalenceComparer

        Private Class EquivalenceVisitor
            Private ReadOnly _compareMethodTypeParametersByIndex As Boolean
            Private ReadOnly _objectAndDynamicCompareEqually As Boolean
            Private ReadOnly _symbolEquivalenceComparer As SymbolEquivalenceComparer

            Friend Sub New(symbolEquivalenceComparer As SymbolEquivalenceComparer, compareMethodTypeParametersByIndex As Boolean, objectAndDynamicCompareEqually As Boolean)
                _symbolEquivalenceComparer = symbolEquivalenceComparer
                _compareMethodTypeParametersByIndex = compareMethodTypeParametersByIndex
                _objectAndDynamicCompareEqually = objectAndDynamicCompareEqually
            End Sub

            Private Shared Function AreCompatibleMethodKinds(kind1 As MethodKind, kind2 As MethodKind) As Boolean
                If kind1 = kind2 Then
                    Return True
                End If

                If (kind1 = MethodKind.Ordinary AndAlso IsPropertyAccessor(kind2)) OrElse (IsPropertyAccessor(kind1) AndAlso kind2 = MethodKind.Ordinary) Then
                    Return True
                End If

                Return False
            End Function

            Private Shared Function DynamicTypesAreEquivalent(x As IDynamicTypeSymbol, y As IDynamicTypeSymbol) As Boolean
                If SymbolEqualityComparer.Default.Equals(x, y) Then Return True
                Return True
            End Function

            Private Shared Function HaveSameLocation(x As ISymbol, y As ISymbol) As Boolean
                Return x.Locations.Length = 1 AndAlso y.Locations.Length = 1 AndAlso x.Locations.First().Equals(y.Locations.First())
            End Function

            Private Shared Function LabelsAreEquivalent(x As ILabelSymbol, y As ILabelSymbol) As Boolean
                Return x.Name = y.Name AndAlso HaveSameLocation(x, y)
            End Function

            Private Shared Function LocalsAreEquivalent(x As ILocalSymbol, y As ILocalSymbol) As Boolean
                Return HaveSameLocation(x, y)
            End Function

            Private Shared Function PreprocessingSymbolsAreEquivalent(x As IPreprocessingSymbol, y As IPreprocessingSymbol) As Boolean
                Return x.Name = y.Name
            End Function

            Private Shared Function RangeVariablesAreEquivalent(x As IRangeVariableSymbol, y As IRangeVariableSymbol) As Boolean
                Return HaveSameLocation(x, y)
            End Function

            Private Function AreEquivalentWorker(x As ISymbol, y As ISymbol, k As SymbolKind, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                Select Case k
                    Case SymbolKind.ArrayType
                        Return Me.ArrayTypesAreEquivalent(DirectCast(x, IArrayTypeSymbol), DirectCast(y, IArrayTypeSymbol), equivalentTypesWithDifferingAssemblies)
                    Case SymbolKind.Assembly
                        Return Me.AssembliesAreEquivalent(DirectCast(x, IAssemblySymbol), DirectCast(y, IAssemblySymbol))
                    Case SymbolKind.DynamicType
                        Return DynamicTypesAreEquivalent(DirectCast(x, IDynamicTypeSymbol), DirectCast(y, IDynamicTypeSymbol))
                    Case SymbolKind.Event
                        Return Me.EventsAreEquivalent(DirectCast(x, IEventSymbol), DirectCast(y, IEventSymbol), equivalentTypesWithDifferingAssemblies)
                    Case SymbolKind.Field
                        Return Me.FieldsAreEquivalent(DirectCast(x, IFieldSymbol), DirectCast(y, IFieldSymbol), equivalentTypesWithDifferingAssemblies)
                    Case SymbolKind.Label
                        Return LabelsAreEquivalent(DirectCast(x, ILabelSymbol), DirectCast(y, ILabelSymbol))
                    Case SymbolKind.Local
                        Return LocalsAreEquivalent(DirectCast(x, ILocalSymbol), DirectCast(y, ILocalSymbol))
                    Case SymbolKind.Method
                        Return Me.MethodsAreEquivalent(DirectCast(x, IMethodSymbol), DirectCast(y, IMethodSymbol), equivalentTypesWithDifferingAssemblies)
                    Case SymbolKind.NetModule
                        Return Me.ModulesAreEquivalent(DirectCast(x, IModuleSymbol), DirectCast(y, IModuleSymbol))
                    Case SymbolKind.NamedType, SymbolKind.ErrorType
                        Return Me.NamedTypesAreEquivalent(DirectCast(x, INamedTypeSymbol), DirectCast(y, INamedTypeSymbol), equivalentTypesWithDifferingAssemblies)
                    Case SymbolKind.Namespace
                        Return Me.NamespacesAreEquivalent(DirectCast(x, INamespaceSymbol), DirectCast(y, INamespaceSymbol), equivalentTypesWithDifferingAssemblies)
                    Case SymbolKind.PointerType
                        Return Me.PointerTypesAreEquivalent(DirectCast(x, IPointerTypeSymbol), DirectCast(y, IPointerTypeSymbol), equivalentTypesWithDifferingAssemblies)
                    Case SymbolKind.Property
                        Return Me.PropertiesAreEquivalent(DirectCast(x, IPropertySymbol), DirectCast(y, IPropertySymbol), equivalentTypesWithDifferingAssemblies)
                    Case SymbolKind.RangeVariable
                        Return RangeVariablesAreEquivalent(DirectCast(x, IRangeVariableSymbol), DirectCast(y, IRangeVariableSymbol))
                    Case SymbolKind.TypeParameter
                        Return Me.TypeParametersAreEquivalent(DirectCast(x, ITypeParameterSymbol), DirectCast(y, ITypeParameterSymbol), equivalentTypesWithDifferingAssemblies)
                    Case SymbolKind.Preprocessing
                        Return PreprocessingSymbolsAreEquivalent(DirectCast(x, IPreprocessingSymbol), DirectCast(y, IPreprocessingSymbol))
                    Case Else
                        Return False
                End Select
            End Function

            Private Function ArrayTypesAreEquivalent(x As IArrayTypeSymbol, y As IArrayTypeSymbol, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                Return x.Rank = y.Rank AndAlso Me.AreEquivalent(x.CustomModifiers, y.CustomModifiers, equivalentTypesWithDifferingAssemblies) AndAlso Me.AreEquivalent(x.ElementType, y.ElementType, equivalentTypesWithDifferingAssemblies)
            End Function

            Private Function AssembliesAreEquivalent(x As IAssemblySymbol, y As IAssemblySymbol) As Boolean
                Return If(_symbolEquivalenceComparer._assemblyComparerOpt?.Equals(x, y), True)
            End Function

            Private Function EventsAreEquivalent(x As IEventSymbol, y As IEventSymbol, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                Return x.Name = y.Name AndAlso Me.AreEquivalent(x.ContainingSymbol, y.ContainingSymbol, equivalentTypesWithDifferingAssemblies)
            End Function

            Private Function FieldsAreEquivalent(x As IFieldSymbol, y As IFieldSymbol, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                Return x.Name = y.Name AndAlso Me.AreEquivalent(x.CustomModifiers, y.CustomModifiers, equivalentTypesWithDifferingAssemblies) AndAlso Me.AreEquivalent(x.ContainingSymbol, y.ContainingSymbol, equivalentTypesWithDifferingAssemblies)
            End Function

            Private Function HandleAnonymousTypes(x As INamedTypeSymbol, y As INamedTypeSymbol, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                If x.TypeKind = TypeKind.Delegate Then
                    Return Me.AreEquivalent(x.DelegateInvokeMethod, y.DelegateInvokeMethod, equivalentTypesWithDifferingAssemblies)
                Else
                    Dim xMembers As IEnumerable(Of IPropertySymbol) = x.GetValidAnonymousTypeProperties()
                    Dim yMembers As IEnumerable(Of IPropertySymbol) = y.GetValidAnonymousTypeProperties()

                    Dim xMembersEnumerator As IEnumerator(Of IPropertySymbol) = xMembers.GetEnumerator()
                    Dim yMembersEnumerator As IEnumerator(Of IPropertySymbol) = yMembers.GetEnumerator()

                    Do While xMembersEnumerator.MoveNext()
                        If Not yMembersEnumerator.MoveNext() Then
                            Return False
                        End If

                        Dim p1 As IPropertySymbol = xMembersEnumerator.Current
                        Dim p2 As IPropertySymbol = yMembersEnumerator.Current

                        If p1.Name <> p2.Name OrElse p1.IsReadOnly <> p2.IsReadOnly OrElse Not Me.AreEquivalent(p1.Type, p2.Type, equivalentTypesWithDifferingAssemblies) Then
                            Return False
                        End If
                    Loop

                    Return Not yMembersEnumerator.MoveNext()
                End If
            End Function

            Private Function HandleNamedTypesWorker(x As INamedTypeSymbol, y As INamedTypeSymbol, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                Debug.Assert(GetTypeKind(x) = GetTypeKind(y))

                If x.IsTupleType OrElse y.IsTupleType Then
                    If x.IsTupleType <> y.IsTupleType Then
                        Return False
                    End If

                    Dim xElements As ImmutableArray(Of IFieldSymbol) = x.TupleElements
                    Dim yElements As ImmutableArray(Of IFieldSymbol) = y.TupleElements

                    If xElements.Length <> yElements.Length Then
                        Return False
                    End If

                    For index As Integer = 0 To xElements.Length - 1
                        If Not Me.AreEquivalent(xElements(index).Type, yElements(index).Type, equivalentTypesWithDifferingAssemblies) Then
                            Return False
                        End If
                    Next

                    Return True
                End If

                If x.IsDefinition <> y.IsDefinition OrElse IsConstructedFromSelf(x) <> IsConstructedFromSelf(y) OrElse x.Arity <> y.Arity OrElse x.Name <> y.Name OrElse x.IsAnonymousType <> y.IsAnonymousType OrElse x.IsUnboundGenericType <> y.IsUnboundGenericType Then
                    Return False
                End If

                If Not Me.AreEquivalent(x.ContainingSymbol, y.ContainingSymbol, equivalentTypesWithDifferingAssemblies) Then
                    Return False
                End If

                ' Above check makes sure that the containing assemblies are considered the same by the assembly comparer being used.
                ' If they are in fact not the same (have different name) and the caller requested to know about such types add {x, y}
                ' to equivalentTypesWithDifferingAssemblies map.
                If equivalentTypesWithDifferingAssemblies IsNot Nothing AndAlso x.ContainingType Is Nothing AndAlso x.ContainingAssembly IsNot Nothing AndAlso Not AssemblyIdentityComparer.SimpleNameComparer.Equals(x.ContainingAssembly.Name, y.ContainingAssembly.Name) AndAlso Not equivalentTypesWithDifferingAssemblies.ContainsKey(x) Then
                    equivalentTypesWithDifferingAssemblies.Add(x, y)
                End If

                If x.IsAnonymousType Then
                    Return Me.HandleAnonymousTypes(x, y, equivalentTypesWithDifferingAssemblies)
                End If

                ' They look very similar at this point.  In the case of non constructed types, we're
                ' done.  However, if they are constructed, then their type arguments have to match
                ' as well.
                Return IsConstructedFromSelf(x) OrElse x.IsUnboundGenericType OrElse Me.TypeArgumentsAreEquivalent(x.TypeArguments, y.TypeArguments, equivalentTypesWithDifferingAssemblies)
            End Function

            ''' <summary>
            ''' Worker for comparing two named types for equivalence. Note: The two
            ''' types must have the same TypeKind.
            ''' </summary>
            ''' <param name="x">The first type to compare</param>
            ''' <param name="y">The second type to compare</param>
            ''' <param name="equivalentTypesWithDifferingAssemblies">
            ''' Map of equivalent non-nested types to be populated, such that each key-value pair of named types are equivalent but reside in different assemblies.
            ''' This map is populated only if we are ignoring assemblies for symbol equivalence comparison, i.e. <see cref="_assemblyComparerOpt"/> is true.
            ''' </param>
            ''' <returns>True if the two types are equivalent.</returns>
            Private Function MethodsAreEquivalent(x As IMethodSymbol, y As IMethodSymbol, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                If Not AreCompatibleMethodKinds(x.MethodKind, y.MethodKind) Then
                    Return False
                End If

                If x.MethodKind = MethodKind.ReducedExtension Then
                    Dim rx As IMethodSymbol = x.ReducedFrom
                    Dim ry As IMethodSymbol = y.ReducedFrom

                    ' reduced from symbols are equivalent
                    If Not Me.AreEquivalent(rx, ry, equivalentTypesWithDifferingAssemblies) Then
                        Return False
                    End If

                    ' receiver types are equivalent
                    If Not Me.AreEquivalent(x.ReceiverType, y.ReceiverType, equivalentTypesWithDifferingAssemblies) Then
                        Return False
                    End If
                Else
                    If x.MethodKind = MethodKind.AnonymousFunction OrElse x.MethodKind = MethodKind.LocalFunction Then
                        ' Treat local and anonymous functions just like we do ILocalSymbols.
                        ' They're only equivalent if they have the same location.
                        Return HaveSameLocation(x, y)
                    End If

                    If IsPartialMethodDefinitionPart(x) <> IsPartialMethodDefinitionPart(y) OrElse IsPartialMethodImplementationPart(x) <> IsPartialMethodImplementationPart(y) OrElse x.IsDefinition <> y.IsDefinition OrElse IsConstructedFromSelf(x) <> IsConstructedFromSelf(y) OrElse x.Arity <> y.Arity OrElse x.Parameters.Length <> y.Parameters.Length OrElse x.Name <> y.Name Then
                        Return False
                    End If

                    Dim checkContainingType_ As Boolean = CheckContainingType(x)
                    If checkContainingType_ Then
                        If Not Me.AreEquivalent(x.ContainingSymbol, y.ContainingSymbol, equivalentTypesWithDifferingAssemblies) Then
                            Return False
                        End If
                    End If

                    If Not Me.ParametersAreEquivalent(x.Parameters, y.Parameters, equivalentTypesWithDifferingAssemblies) Then
                        Return False
                    End If

                    If Not Me.ReturnTypesAreEquivalent(x, y, equivalentTypesWithDifferingAssemblies) Then
                        Return False
                    End If
                End If

                ' If it's an unconstructed method, then we don't need to check the type arguments.
                If IsConstructedFromSelf(x) Then
                    Return True
                End If

                Return Me.TypeArgumentsAreEquivalent(x.TypeArguments, y.TypeArguments, equivalentTypesWithDifferingAssemblies)
            End Function

            Private Function ModulesAreEquivalent(x As IModuleSymbol, y As IModuleSymbol) As Boolean
                Return Me.AssembliesAreEquivalent(x.ContainingAssembly, y.ContainingAssembly) AndAlso x.Name = y.Name
            End Function

            Private Function NamedTypesAreEquivalent(x As INamedTypeSymbol, y As INamedTypeSymbol, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                ' PERF: Avoid multiple virtual calls to fetch the TypeKind property
                Dim xTypeKind As TypeKind = GetTypeKind(x)
                Dim yTypeKind As TypeKind = GetTypeKind(y)

                If xTypeKind = TypeKind.Error OrElse yTypeKind = TypeKind.Error Then
                    ' Slow path: x or y is an error type. We need to compare
                    ' all the candidates in both.
                    Return Me.NamedTypesAreEquivalentError(x, y, equivalentTypesWithDifferingAssemblies)
                End If

                ' Fast path: we can compare the symbols directly,
                ' avoiding any allocations associated with the Unwrap()
                ' enumerator.
                Return xTypeKind = yTypeKind AndAlso Me.HandleNamedTypesWorker(x, y, equivalentTypesWithDifferingAssemblies)
            End Function

            Private Function NamedTypesAreEquivalentError(x As INamedTypeSymbol, y As INamedTypeSymbol, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                For Each type1 As INamedTypeSymbol In Unwrap(x)
                    Dim typeKind1 As TypeKind = GetTypeKind(type1)
                    For Each type2 As INamedTypeSymbol In Unwrap(y)
                        Dim typeKind2 As TypeKind = GetTypeKind(type2)
                        If typeKind1 = typeKind2 AndAlso Me.HandleNamedTypesWorker(type1, type2, equivalentTypesWithDifferingAssemblies) Then
                            Return True
                        End If
                    Next type2
                Next type1

                Return False
            End Function

            Private Function NamespacesAreEquivalent(x As INamespaceSymbol, y As INamespaceSymbol, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                If x.IsGlobalNamespace <> y.IsGlobalNamespace OrElse x.Name <> y.Name Then
                    Return False
                End If

                If x.IsGlobalNamespace AndAlso _symbolEquivalenceComparer._assemblyComparerOpt Is Nothing Then
                    ' No need to compare the containers of global namespace when assembly identities are ignored.
                    Return True
                End If

                Return Me.AreEquivalent(x.ContainingSymbol, y.ContainingSymbol, equivalentTypesWithDifferingAssemblies)
            End Function

            Private Function ParametersAreEquivalent(xParameters As ImmutableArray(Of IParameterSymbol), yParameters As ImmutableArray(Of IParameterSymbol), equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol), Optional compareParameterName As Boolean = False, Optional isParameterNameCaseSensitive As Boolean = False) As Boolean
                ' Note the special parameter comparer we pass in.  We do this so we don't end up
                ' infinitely looping between parameters -> type parameters -> methods -> parameters
                Dim count As Integer = xParameters.Length
                If yParameters.Length <> count Then
                    Return False
                End If

                For index As Integer = 0 To count - 1
                    If Not _symbolEquivalenceComparer.ParameterEquivalenceComparer.Equals(xParameters(index), yParameters(index), equivalentTypesWithDifferingAssemblies, compareParameterName, isParameterNameCaseSensitive) Then
                        Return False
                    End If
                Next index

                Return True
            End Function

            Private Function PointerTypesAreEquivalent(x As IPointerTypeSymbol, y As IPointerTypeSymbol, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                Return Me.AreEquivalent(x.CustomModifiers, y.CustomModifiers, equivalentTypesWithDifferingAssemblies) AndAlso Me.AreEquivalent(x.PointedAtType, y.PointedAtType, equivalentTypesWithDifferingAssemblies)
            End Function

            Private Function PropertiesAreEquivalent(x As IPropertySymbol, y As IPropertySymbol, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                If x.ContainingType.IsAnonymousType AndAlso y.ContainingType.IsAnonymousType Then
                    ' We can short circuit here and just use the symbols themselves to determine
                    ' equality.  This will properly handle things like the VB case where two
                    ' anonymous types will be considered the same if they have properties that
                    ' differ in casing.
                    If SymbolEqualityComparer.Default.Equals(x, y) Then
                        Return True
                    End If
                End If

                Return x.IsIndexer = y.IsIndexer AndAlso x.MetadataName = y.MetadataName AndAlso x.Parameters.Length = y.Parameters.Length AndAlso Me.ParametersAreEquivalent(x.Parameters, y.Parameters, equivalentTypesWithDifferingAssemblies) AndAlso Me.AreEquivalent(x.ContainingSymbol, y.ContainingSymbol, equivalentTypesWithDifferingAssemblies)
            End Function

            Private Function TypeArgumentsAreEquivalent(xTypeArguments As ImmutableArray(Of ITypeSymbol), yTypeArguments As ImmutableArray(Of ITypeSymbol), equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                Dim count As Integer = xTypeArguments.Length
                If yTypeArguments.Length <> count Then
                    Return False
                End If

                For index As Integer = 0 To count - 1
                    If Not Me.AreEquivalent(xTypeArguments(index), yTypeArguments(index), equivalentTypesWithDifferingAssemblies) Then
                        Return False
                    End If
                Next

                Return True
            End Function

            Private Function TypeParametersAreEquivalent(x As ITypeParameterSymbol, y As ITypeParameterSymbol, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                If x.Ordinal <> y.Ordinal OrElse x.TypeParameterKind <> y.TypeParameterKind Then
                    Return False
                End If

                ' If this is a method type parameter, and we are in 'non-recurse' mode (because
                ' we're comparing method parameters), then we're done at this point.  The types are
                ' equal.
                If x.TypeParameterKind = TypeParameterKind.Method AndAlso _compareMethodTypeParametersByIndex Then
                    Return True
                End If

                If x.TypeParameterKind = TypeParameterKind.Type AndAlso x.ContainingType.IsAnonymousType Then
                    ' Anonymous type, type parameters compare by index as well to prevent
                    ' recursion.
                    Return True
                End If

                If x.TypeParameterKind = TypeParameterKind.Cref Then
                    Return True
                End If

                Return Me.AreEquivalent(x.ContainingSymbol, y.ContainingSymbol, equivalentTypesWithDifferingAssemblies)
            End Function

            Friend Shared Function IsPropertyAccessor(kind As MethodKind) As Boolean
                Return kind = MethodKind.PropertyGet OrElse kind = MethodKind.PropertySet
            End Function

            Friend Function AreEquivalent(x As CustomModifier, y As CustomModifier, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                Return x.IsOptional = y.IsOptional AndAlso Me.AreEquivalent(x.Modifier, y.Modifier, equivalentTypesWithDifferingAssemblies)
            End Function

            Friend Function AreEquivalent(x As ImmutableArray(Of CustomModifier), y As ImmutableArray(Of CustomModifier), equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                Debug.Assert(Not x.IsDefault AndAlso Not y.IsDefault)
                If x.Length <> y.Length Then
                    Return False
                End If

                For index As Integer = 0 To x.Length - 1
                    If Not Me.AreEquivalent(x(index), y(index), equivalentTypesWithDifferingAssemblies) Then
                        Return False
                    End If
                Next index

                Return True
            End Function

            Friend Function AreEquivalent(x As ISymbol, y As ISymbol, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
                If ReferenceEquals(x, y) Then
                    Return True
                End If

                If x Is Nothing OrElse y Is Nothing Then
                    Return False
                End If

                Dim xKind As SymbolKind = GetKindAndUnwrapAlias(x)
                Dim yKind As SymbolKind = GetKindAndUnwrapAlias(y)

                ' Normally, if they're different types, then they're not the same.
                If xKind <> yKind Then
                    ' Special case.  If we're comparing signatures then we want to compare 'object'
                    ' and 'dynamic' as the same.  However, since they're different types, we don't
                    ' want to bail out using the above check.
                    Return _objectAndDynamicCompareEqually AndAlso ((yKind = SymbolKind.DynamicType AndAlso xKind = SymbolKind.NamedType AndAlso DirectCast(x, ITypeSymbol).SpecialType = SpecialType.System_Object) OrElse (xKind = SymbolKind.DynamicType AndAlso yKind = SymbolKind.NamedType AndAlso DirectCast(y, ITypeSymbol).SpecialType = SpecialType.System_Object))
                End If

                Return Me.AreEquivalentWorker(x, y, xKind, equivalentTypesWithDifferingAssemblies)

            End Function

            Friend Function ReturnTypesAreEquivalent(x As IMethodSymbol, y As IMethodSymbol, Optional equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol) = Nothing) As Boolean
                Return _symbolEquivalenceComparer.SignatureTypeEquivalenceComparer.Equals(x.ReturnType, y.ReturnType, equivalentTypesWithDifferingAssemblies) AndAlso Me.AreEquivalent(x.ReturnTypeCustomModifiers, y.ReturnTypeCustomModifiers, equivalentTypesWithDifferingAssemblies)
            End Function

        End Class

    End Class

End Namespace
