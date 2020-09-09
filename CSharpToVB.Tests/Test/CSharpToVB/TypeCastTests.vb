' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports CodeConverter.Tests

Imports Xunit

Namespace CSharpToVB.Tests

    <TestClass()> Public Class TypeCastTests
        Inherits ConverterTestBase

        <Fact>
        Public Shared Sub CSharpToVBCastCharacterIncrement()
            TestConversionCSharpToVisualBasic(
"static class TestClass
    {
    void Test() {
        char a = 'A';
        a++;
    }
}",
"Module TestClass

    Private Sub Test()
        Dim a As Char = ""A""c
        a = ChrW(AscW(a)) + 1
    End Sub

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCastCharacterToNumber()
            TestConversionCSharpToVisualBasic(
"static class TestClass
{
    void Test() {
        byte a = (byte)'A';
        decimal b = (byte)'B';
    }
}
",
"Module TestClass

    Private Sub Test()
        Dim a As Byte = AscW(""A""c)
        Dim b As Decimal = AscW(""B""c)
    End Sub

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCastConstantNumberToCharacter()
            TestConversionCSharpToVisualBasic(
"class TestClass
{
    void Test() {
        char CR = (char)0xD;
    }
}",
"Class TestClass

    Private Sub Test()
        Dim CR As Char = ChrW(&HD)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCastConstantNumberToDecimal()
            TestConversionCSharpToVisualBasic(
                "static class TestClass
{
    void Test()
    {
        object o = 5.0m;
    }
}", "Module TestClass

    Private Sub Test()
        Dim o As Object = 5.0D
    End Sub

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCastConstantNumberToFloat()
            TestConversionCSharpToVisualBasic(
                "static class TestClass
{
    void Test()
    {
        object o = 5.0f;
    }
}", "Module TestClass

    Private Sub Test()
        Dim o As Object = 5.0F
    End Sub

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCastConstantNumberToLong()
            TestConversionCSharpToVisualBasic(
                "static class TestClass
{
    void Test()
    {
        object o = 5L;
    }
}", "Module TestClass

    Private Sub Test()
        Dim o As Object = 5L
    End Sub

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCastObjectToGenericList()
            TestConversionCSharpToVisualBasic(
                "class TestClass
{
    void Test()
    {
        object o = new System.Collections.Generic.List<int>();
        System.Collections.Generic.List<int> l = (System.Collections.Generic.List<int>) o;
    }
}", "Class TestClass

    Private Sub Test()
        Dim o As Object = New Collections.Generic.List(Of Integer)
        Dim l As Collections.Generic.List(Of Integer) = CType(o, Collections.Generic.List(Of Integer))
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCastObjectToInteger()
            ' The leading and trailing newlines check that surrounding trivia is selected as part of this (in the comments auto-testing)
            TestConversionCSharpToVisualBasic(
                "static class TestClass
{
    void Test()
    {
        object o = 5;
        int i = (int) o;
    }
}", "Module TestClass

    Private Sub Test()
        Dim o As Object = 5
        Dim i As Integer = CInt(Fix(o))
    End Sub

End Module
")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCastObjectToString()
            TestConversionCSharpToVisualBasic(
                "static class TestClass
{
    void Test()
    {
        object o = ""Test"";
        string s = (string) o;
    }", "Module TestClass

    Private Sub Test()
        Dim o As Object = ""Test""
        Dim s As String = CStr(o)
    End Sub

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMethodInvocation()
            TestConversionCSharpToVisualBasic(
"public class Test {
    public void TestMethod() { }
}
public class Test2 {
    public void TestMethod(object o) {
        ((Test)o).TestMethod();
    }
}", "Public Class Test

    Public Sub TestMethod()
    End Sub
End Class

Public Class Test2

    Public Sub TestMethod(o As Object)
        CType(o, Test).TestMethod()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMethodInvocationTryCast()
            TestConversionCSharpToVisualBasic(
"public class Test {
    public void TestMethod() { }
}
public class Test2 {
    public void TestMethod(object o) {
        (o as Test).TestMethod();
    }
}", "Public Class Test

    Public Sub TestMethod()
    End Sub
End Class

Public Class Test2

    Public Sub TestMethod(o As Object)
        TryCast(o, Test).TestMethod()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTryCastObjectToGenericList()
            TestConversionCSharpToVisualBasic(
                "class TestClass {
    void Test()
    {
        object o = new System.Collections.Generic.List<int>();
        System.Collections.Generic.List<int> l = o as System.Collections.Generic.List<int>;
    }
", "Class TestClass

    Private Sub Test()
        Dim o As Object = New Collections.Generic.List(Of Integer)
        Dim l As Collections.Generic.List(Of Integer) = TryCast(o, Collections.Generic.List(Of Integer))
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTryCastObjectToGenericType()
            TestConversionCSharpToVisualBasic(
"class TestClass
{
    T Test<T>() where T : class {
        return this as T;
    }
}",
"Class TestClass

    Private Function Test(Of T As Class)() As T
        Return TryCast(Me, T)
    End Function
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTryCastObjectToInteger()
            TestConversionCSharpToVisualBasic(
                "class TestClass
{
    void Test()
    {
        object o = 5;
        System.Nullable<int> i = o as int?;
}
}", "Class TestClass

    Private Sub Test()
        Dim o As Object = 5
        Dim i As Nullable(Of Integer) = TryCast(o, Integer?)
    End Sub
End Class")
        End Sub

    End Class

End Namespace
