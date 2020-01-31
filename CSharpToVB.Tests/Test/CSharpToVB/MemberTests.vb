' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports CodeConverter.Tests

Imports Xunit

Namespace CSharpToVB.Tests

    <TestClass()> Public Class MemberTests
        Inherits ConverterTestBase

        <Fact>
        Public Shared Sub CSharpToVBAbstractMethod()
            TestConversionCSharpToVisualBasic("class TestClass
{
    public abstract void TestMethod();
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Public MustOverride Sub TestMethod()
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBConstructor()
            TestConversionCSharpToVisualBasic("class TestClass<T, T2, T3> where T : class, new where T2 : struct
{
    // Comment
    public TestClass(out T argument, ref T2 argument2, T3 argument3)
    {
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass(Of T As {Class, New}, T2 As Structure, T3)

    ' Comment
    Public Sub New(<Runtime.InteropServices.Out> ByRef argument As T, ByRef argument2 As T2, argument3 As T3)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCustomEvent()
            TestConversionCSharpToVisualBasic("using System;
class TestClass
{
    EventHandler backingField;

    public event EventHandler MyEvent {
        add {
            this.backingField += value;
        }
        remove {
            this.backingField -= value;
        }
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Imports System

Class TestClass

    Private backingField As EventHandler

    Public Custom Event MyEvent As EventHandler
        AddHandler(Value As EventHandler)
            AddHandler Me.backingField, value
        End AddHandler

        RemoveHandler(Value As EventHandler)
            RemoveHandler Me.backingField, value
        End RemoveHandler
    End Event
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBDestructor()
            TestConversionCSharpToVisualBasic("class TestClass
{
    ~TestClass()
    {
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Protected Overrides Sub Finalize()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBEvent()
            TestConversionCSharpToVisualBasic("class TestClass
{
    public event EventHandler MyEvent;
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Public Event MyEvent As EventHandler
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBExtensionMethod()
            TestConversionCSharpToVisualBasic("static class TestClass
{
    public static void TestMethod(this String str)
    {
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Imports System.Runtime.CompilerServices

Module TestClass

    <Extension()>
    Public Sub TestMethod(str As String)
    End Sub

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBExtensionMethodWithExistingImport()
            TestConversionCSharpToVisualBasic("using System.Runtime.CompilerServices;

static class TestClass
{
    public static void TestMethod(this String str)
    {
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Imports System.Runtime.CompilerServices

Module TestClass

    <Extension()>
    Public Sub TestMethod(str As String)
    End Sub

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBField()
            TestConversionCSharpToVisualBasic("class TestClass
{
    const int answer = 42;
    int value = 10;
    readonly int v = 15;
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Const answer As Integer = 42

    Private value As Integer = 10

    ReadOnly v As Integer = 15
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBIndexer()
            TestConversionCSharpToVisualBasic("class TestClass
{
    public int this[int index] { get; set; }
    public int this[string index] {
        get { return 0; }
    }
    int m_test3;
    public int this[double index] {
        get { return this.m_test3; }
        set { this.m_test3 = value; }
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Default Public Property Item(index As Integer) As Integer

    Default Public ReadOnly Property Item(index As String) As Integer
        Get
            Return 0
        End Get
    End Property

    Private m_test3 As Integer

    Default Public Property Item(index As Double) As Integer
        Get
            Return Me.m_test3
        End Get

        Set(Value As Integer)
            Me.m_test3 = value
        End Set
    End Property
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMethod()
            TestConversionCSharpToVisualBasic("class TestClass
{
    public void TestMethod<T, T2, T3>(out T argument, ref T2 argument2, T3 argument3) where T : class, new where T2 : struct
    {
        argument = null;
        argument2 = default(T2);
        argument3 = default(T3);
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Public Sub TestMethod(Of T As {Class, New}, T2 As Structure, T3)(<Runtime.InteropServices.Out> ByRef argument As T, ByRef argument2 As T2, argument3 As T3)
        argument = Nothing
        argument2 = CType(Nothing, T2)
        argument3 = CType(Nothing, T3)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMethodOutVar()
            TestConversionCSharpToVisualBasic("using Microsoft.CodeAnalysis

class TestClass
{
    private bool GetConvertMethodForKeywordOrNull(SyntaxNode type)
    {
        var convertedType = semanticModel.GetTypeInfo(type).ConvertedType;
        return createConvertMethodsLookupByReturnType.TryGetValue(convertedType, out var convertMethodName)
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Imports Microsoft.CodeAnalysis

Class TestClass

    Private Function GetConvertMethodForKeywordOrNull(type As SyntaxNode) As Boolean
        Dim _convertedType As Microsoft.CodeAnalysis.ITypeSymbol = semanticModel.GetTypeInfo(type).ConvertedType
        Dim convertMethodName As Object = Nothing
        Return createConvertMethodsLookupByReturnType.TryGetValue(_convertedType, convertMethodName)
    End Function
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMethodWithReturnType()
            TestConversionCSharpToVisualBasic("class TestClass
{
    public int TestMethod<T, T2, T3>(out T argument, ref T2 argument2, T3 argument3) where T : class, new where T2 : struct
    {
        return 0;
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Public Function TestMethod(Of T As {Class, New}, T2 As Structure, T3)(<Runtime.InteropServices.Out> ByRef argument As T, ByRef argument2 As T2, argument3 As T3) As Integer
        Return 0
    End Function
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBProperty()
            TestConversionCSharpToVisualBasic("class TestClass
{
    public int Test { get; set; }
    public int Test2 {
        get { return 0; }
    }
    int m_test3;
    public int Test3 {
        get { return this.m_test3; }
        set { this.m_test3 = value; }
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Public Property Test As Integer

    Public ReadOnly Property Test2 As Integer

        Get
            Return 0
        End Get

    End Property

    Private m_test3 As Integer

    Public Property Test3 As Integer

        Get
            Return Me.m_test3
        End Get

        Set(Value As Integer)
            Me.m_test3 = value
        End Set

    End Property
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBSealedMethod()
            TestConversionCSharpToVisualBasic("class TestClass
{
    public sealed void TestMethod<T, T2, T3>(out T argument, ref T2 argument2, T3 argument3) where T : class, new where T2 : struct
    {
        argument = null;
        argument2 = default(T2);
        argument3 = default(T3);
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Public NotOverridable Sub TestMethod(Of T As {Class, New}, T2 As Structure, T3)(<Runtime.InteropServices.Out> ByRef argument As T, ByRef argument2 As T2, argument3 As T3)
        argument = Nothing
        argument2 = CType(Nothing, T2)
        argument3 = CType(Nothing, T3)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBStaticMethod()
            TestConversionCSharpToVisualBasic("class TestClass
{
    public static void TestMethod<T, T2, T3>(out T argument, ref T2 argument2, T3 argument3) where T : class, new where T2 : struct
    {
        argument = null;
        argument2 = default(T2);
        argument3 = default(T3);
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Public Shared Sub TestMethod(Of T As {Class, New}, T2 As Structure, T3)(<Runtime.InteropServices.Out> ByRef argument As T, ByRef argument2 As T2, argument3 As T3)
        argument = Nothing
        argument2 = CType(Nothing, T2)
        argument3 = CType(Nothing, T3)
    End Sub
End Class")
        End Sub

    End Class

End Namespace
