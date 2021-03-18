' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Microsoft.CodeAnalysis

Namespace Extensions
    Public Module NamedTypeSymbolExtensions

        <Extension>
        Private Function FindImplementationForAbstractMember(type As INamedTypeSymbol, symbol As ISymbol) As ISymbol
            If symbol.IsAbstract Then
                Return type.GetBaseTypesAndThis() _
                    .SelectMany(Function(t As INamedTypeSymbol) t.GetMembers(symbol.Name)) _
                    .FirstOrDefault(Function(s As ISymbol) SymbolEqualityComparer.Default.Equals(symbol, s.GetOverriddenMember()))
            End If

            Return Nothing
        End Function

        Private Function GetAbstractClassesToImplement(abstractClasses As IEnumerable(Of INamedTypeSymbol)) As ImmutableArray(Of INamedTypeSymbol)
            Return abstractClasses.SelectMany(Function(a As INamedTypeSymbol) a.GetBaseTypesAndThis()).Where(Function(t As INamedTypeSymbol) t.IsAbstractClass()).ToImmutableArray()
        End Function

        Private Function GetAllImplementedMembers(classOrStructType As INamedTypeSymbol,
                                                                          interfacesOrAbstractClasses As IEnumerable(Of INamedTypeSymbol),
                                                                          isImplemented As Func(Of INamedTypeSymbol,
                                                                          ISymbol,
                                                                          Func(Of INamedTypeSymbol, ISymbol, Boolean), CancellationToken, Boolean),
                                                                          isValidImplementation As Func(Of INamedTypeSymbol, ISymbol, Boolean),
                                                                          interfaceMemberGetter As Func(Of INamedTypeSymbol, ISymbol, ImmutableArray(Of ISymbol)),
                                                                          allowReimplementation As Boolean,
                                                                          cancelToken As CancellationToken
                                                                         ) As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))
            If classOrStructType.TypeKind <> TypeKind.Class AndAlso classOrStructType.TypeKind <> TypeKind.Struct Then
                Return ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))).Empty
            End If

            Dim namedTypeSymbols As IEnumerable(Of INamedTypeSymbol) = If(TryCast(interfacesOrAbstractClasses, INamedTypeSymbol()), interfacesOrAbstractClasses.ToArray())
            If Not namedTypeSymbols.Any() Then
                Return ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))).Empty
            End If

            If Not namedTypeSymbols.All(Function(i As INamedTypeSymbol) i.TypeKind = TypeKind.Interface) AndAlso Not namedTypeSymbols.All(Function(i As INamedTypeSymbol) i.IsAbstractClass()) Then
                Return ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))).Empty
            End If

            Dim typesToImplement As ImmutableArray(Of INamedTypeSymbol) = GetTypesToImplement(classOrStructType, namedTypeSymbols, allowReimplementation, cancelToken)
            Return typesToImplement.SelectAsArray(Function(s As INamedTypeSymbol) (s, members:=GetImplementedMembers(classOrStructType, s, isImplemented, isValidImplementation, interfaceMemberGetter, cancelToken))).WhereAsArray(Function(t As (s As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))) t.members.Any)
        End Function

        <Extension>
        Private Function GetAllInterfacesIncludingThis(type As ITypeSymbol) As IList(Of INamedTypeSymbol)
            Dim allInterfaces As ImmutableArray(Of INamedTypeSymbol) = type.AllInterfaces
            Dim isINamedType As Boolean = TypeOf type Is INamedTypeSymbol
            Dim namedType As INamedTypeSymbol = If(isINamedType, CType(type, INamedTypeSymbol), Nothing)
            If isINamedType AndAlso namedType.TypeKind = TypeKind.Interface AndAlso Not allInterfaces.Contains(namedType) Then
                Dim result As New List(Of INamedTypeSymbol)(allInterfaces.Length + 1) From {
                namedType
            }
                result.AddRange(allInterfaces)
                Return result
            End If

            Return allInterfaces
        End Function

        <Extension>
        Private Iterator Function GetBaseTypesAndThis(namedType As INamedTypeSymbol) As IEnumerable(Of INamedTypeSymbol)
            Dim current As INamedTypeSymbol = namedType
            Do While current IsNot Nothing
                Yield current
                current = current.BaseType
            Loop
        End Function

        Private Function GetImplementedMembers(classOrStructType As INamedTypeSymbol, interfaceType As INamedTypeSymbol, isImplemented As Func(Of INamedTypeSymbol, ISymbol, Func(Of INamedTypeSymbol, ISymbol, Boolean), CancellationToken, Boolean), isValidImplementation As Func(Of INamedTypeSymbol, ISymbol, Boolean), interfaceMemberGetter As Func(Of INamedTypeSymbol, ISymbol, ImmutableArray(Of ISymbol)), cancelToken As CancellationToken) As ImmutableArray(Of ISymbol)
            Dim q As IEnumerable(Of ISymbol) = From m In interfaceMemberGetter(interfaceType, classOrStructType)
                                               Where m.Kind <> SymbolKind.NamedType
                                               Where m.Kind <> SymbolKind.Method OrElse DirectCast(m, IMethodSymbol).MethodKind = MethodKind.Ordinary
                                               Where m.Kind <> SymbolKind.Property OrElse DirectCast(m, IPropertySymbol).IsIndexer OrElse DirectCast(m, IPropertySymbol).CanBeReferencedByName
                                               Where m.Kind <> SymbolKind.Event OrElse DirectCast(m, IEventSymbol).CanBeReferencedByName
                                               Where isImplemented(classOrStructType, m, isValidImplementation, cancelToken)
                                               Select m

            Return q.ToImmutableArray()
        End Function

        Private Function GetInterfacesToImplement(classOrStructType As INamedTypeSymbol, interfaces As IEnumerable(Of INamedTypeSymbol), allowReimplementation As Boolean, cancelToken As CancellationToken) As ImmutableArray(Of INamedTypeSymbol)
            ' We need to not only implement the specified interface, but also everything it
            ' inherits from.
            cancelToken.ThrowIfCancellationRequested()
            Dim interfacesToImplement As New List(Of INamedTypeSymbol)
            For Each e As INamedTypeSymbol In interfaces
                For Each e1 As INamedTypeSymbol In e.GetAllInterfacesIncludingThis()
                    If Not interfacesToImplement.Contains(e1) Then
                        interfacesToImplement.Add(e1)
                    End If
                Next
            Next
            ' However, there's no need to re-implement any interfaces that our base types already implement.
            ' By definition they must contain all the necessary methods.
            Dim baseType As INamedTypeSymbol = classOrStructType.BaseType
            Dim alreadyImplementedInterfaces As ImmutableArray(Of INamedTypeSymbol) = If(baseType Is Nothing OrElse allowReimplementation, New ImmutableArray(Of INamedTypeSymbol), baseType.AllInterfaces)

            cancelToken.ThrowIfCancellationRequested()
            interfacesToImplement.RemoveRange(alreadyImplementedInterfaces)
            Return interfacesToImplement.ToImmutableArray()
        End Function

        <Extension>
        Private Function GetOverriddenMember(symbol As ISymbol) As ISymbol
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

        Private Function GetTypesToImplement(classOrStructType As INamedTypeSymbol, interfacesOrAbstractClasses As IEnumerable(Of INamedTypeSymbol), allowReimplementation As Boolean, cancelToken As CancellationToken) As ImmutableArray(Of INamedTypeSymbol)
            Dim namedTypeSymbols As IEnumerable(Of INamedTypeSymbol) = If(TryCast(interfacesOrAbstractClasses, INamedTypeSymbol()), interfacesOrAbstractClasses.ToArray())
            Return If(namedTypeSymbols.First().TypeKind = TypeKind.Interface, GetInterfacesToImplement(classOrStructType,
                                                                                                       namedTypeSymbols,
                                                                                                       allowReimplementation,
                                                                                                       cancelToken),
                                                                             GetAbstractClassesToImplement(namedTypeSymbols))
        End Function

        Private Function ImplementationExists(classOrStructType As INamedTypeSymbol, member As ISymbol) As Boolean
            Return classOrStructType.FindImplementationForInterfaceMember(member) IsNot Nothing
        End Function

        <Extension>
        Private Function IsAbstractClass(symbol As ITypeSymbol) As Boolean
            Return CBool(symbol?.TypeKind = TypeKind.Class AndAlso symbol.IsAbstract)
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

        Private Function IsImplemented(classOrStructType As INamedTypeSymbol, member As ISymbol, isValidImplementation As Func(Of INamedTypeSymbol, ISymbol, Boolean), cancelToken As CancellationToken) As Boolean
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
            Return accessor IsNot Nothing AndAlso IsImplementable(accessor) AndAlso accessor.DeclaredAccessibility <> Accessibility.Public
        End Function

        Private Function IsPropertyWithNonPublicImplementableAccessor(member As ISymbol) As Boolean
            If member.Kind <> SymbolKind.Property Then
                Return False
            End If

            Dim [property] As IPropertySymbol = DirectCast(member, IPropertySymbol)

            Return IsNonPublicImplementableAccessor([property].GetMethod) OrElse IsNonPublicImplementableAccessor([property].SetMethod)
        End Function

        <Extension>
        Private Sub RemoveRange(Of T)(collection As ICollection(Of T), items As IEnumerable(Of T))
            If collection Is Nothing Then
                Throw New ArgumentNullException(NameOf(collection))
            End If

            If items IsNot Nothing Then
                For Each item As T In items
                    collection.Remove(item)
                Next item
            End If
        End Sub

        ''' <summary>
        ''' Maps an immutable array to another immutable array.
        ''' </summary>
        ''' <typeparam name="TItem"></typeparam>
        ''' <typeparam name="TResult"></typeparam>
        ''' <param name="items">The array to map</param>
        ''' <param name="map">The mapping delegate</param>
        ''' <returns>If the items length is 0, this will return an empty immutable array</returns>
        <Extension>
        Private Function SelectAsArray(Of TItem, TResult)(items As ImmutableArray(Of TItem), map As Func(Of TItem, TResult)) As ImmutableArray(Of TResult)
            Return ImmutableArray.CreateRange(items, map)
        End Function

        ''' <summary>
        ''' Creates a new immutable array based on filtered elements by the predicate. The array must not be null.
        ''' </summary>
        ''' <typeparam name="T"></typeparam>
        ''' <param name="array">The array to process</param>
        ''' <param name="predicate">The delegate that defines the conditions of the element to search for.</param>
        ''' <returns></returns>
        <Extension>
        Private Function WhereAsArray(Of T)(array As ImmutableArray(Of T), predicate As Func(Of T, Boolean)) As ImmutableArray(Of T)
            If predicate Is Nothing Then
                Throw New ArgumentNullException(NameOf(predicate))
            End If

            Debug.Assert(Not array.IsDefault)

            Dim builder As List(Of T) = Nothing
            Dim none As Boolean = True
            Dim all As Boolean = True

            Dim n As Integer = array.Length
            For index As Integer = 0 To n - 1
                Dim a As T = array(index)
                If predicate(a) Then
                    none = False
                    If all Then
                        Continue For
                    End If

                    Debug.Assert(index > 0)
                    If builder Is Nothing Then
                        builder = New List(Of T)
                    End If

                    builder.Add(a)
                Else
                    If none Then
                        all = False
                        Continue For
                    End If

                    Debug.Assert(index > 0)
                    If all Then
                        Debug.Assert(builder Is Nothing)
                        all = False
                        builder = New List(Of T)
                        For j As Integer = 0 To index - 1
                            builder.Add(array(j))
                        Next j
                    End If
                End If
            Next index

            If builder IsNot Nothing Then
                Debug.Assert(Not all)
                Debug.Assert(Not none)
                Return ImmutableArray.Create(builder.ToArray)
            ElseIf all Then
                Return array
            Else
                Debug.Assert(none)
                Return ImmutableArray(Of T).Empty
            End If
        End Function

        <Extension>
        Friend Function GetAllImplementedMembers(classOrStructType As INamedTypeSymbol,
                                                           interfacesOrAbstractClasses As IEnumerable(Of INamedTypeSymbol),
                                                           cancelToken As CancellationToken) As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))
            Return GetAllImplementedMembers(classOrStructType,
                                            interfacesOrAbstractClasses,
                                            AddressOf IsImplemented,
                                            AddressOf ImplementationExists,
                                            Function(type As INamedTypeSymbol, within As ISymbol)
                                                If type.TypeKind = TypeKind.Interface Then
                                                    Return type.GetMembers().WhereAsArray(
                                                        Function(m As ISymbol) m.DeclaredAccessibility = Accessibility.Public AndAlso
                                                                    m.Kind <> SymbolKind.NamedType AndAlso
                                                                    IsImplementable(m) AndAlso
                                                                    Not IsPropertyWithNonPublicImplementableAccessor(m))
                                                End If
                                                Return type.GetMembers()
                                            End Function,
                                            allowReimplementation:=False,
                                            cancelToken)
        End Function

    End Module
End Namespace
