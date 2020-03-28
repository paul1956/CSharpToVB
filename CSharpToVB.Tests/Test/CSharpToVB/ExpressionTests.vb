' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports CodeConverter.Tests

Imports Xunit

Namespace CSharpToVB.Tests

    <TestClass()> Public Class ExpressionTests
        Inherits ConverterTestBase

        <Fact>
        Public Shared Sub CSharpToVBAwait()
            TestConversionCSharpToVisualBasic("class TestClass
{
    Task<int> SomeAsyncMethod()
    {
        return Task.FromResult(0);
    }

    async void TestMethod()
    {
        int result = await SomeAsyncMethod();
        Console.WriteLine(result);
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Private Function SomeAsyncMethod() As Task(Of Integer)
        Return Task.FromResult(0)
    End Function

    Private Async Sub TestMethod()
        Dim result As Integer = Await SomeAsyncMethod()
        Console.WriteLine(result)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBBaseMemberAccessExpression()
            TestConversionCSharpToVisualBasic("class BaseTestClass
{
    public int member;
}

class TestClass : BaseTestClass
{
    void TestMethod()
    {
        base
            .member = 0;
        base.member = 1;
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class BaseTestClass

    Public member As Integer
End Class

Class TestClass
    Inherits BaseTestClass

    Private Sub TestMethod()
        MyBase.
            member = 0
        MyBase.member = 1
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBComment()
            TestConversionCSharpToVisualBasic("class TestClass
{
    /// <summary>
    ///  Looks up a localized string similar to Analyzer driver threw an exception of type &apos;{0}&apos; with message &apos;{1}&apos;..
    /// </summary>
    void TestMethod()
    {
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    ''' <summary>
    '''  Looks up a localized string similar to Analyzer driver threw an exception of type &apos;{0}&apos; with message &apos;{1}&apos;..
    ''' </summary>
    Private Sub TestMethod()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBComment1()
            TestConversionCSharpToVisualBasic("class TestClass
{
        /// <summary>
        ///   Looks up a localized string similar to Analyzer driver threw the following exception:
        ///&apos;{0}&apos;..
        /// </summary>
     void TestMethod()
    {
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    ''' <summary>
    '''   Looks up a localized string similar to Analyzer driver threw the following exception:
    ''' &apos;{0}&apos;..
    ''' </summary>
    Private Sub TestMethod()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCommentAfterSemicolon()
            TestConversionCSharpToVisualBasic("static void SimpleQuery()
{
    int[] numbers = { 7, 9, 5, 3, 6 };           // Test

}", "Option Explicit Off
Option Infer On
Option Strict Off

Private Shared Sub SimpleQuery()
    Dim numbers As Integer() = {7, 9, 5, 3, 6}           ' Test

End Sub")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBConcatinateComplexString()
            TestConversionCSharpToVisualBasic("using System;
using System.Globalization;
using System.Text.RegularExpressions;

internal static class CanonicalError
{
    // Defines the main pattern for matching messages.
    private static readonly Regex s_originCategoryCodeTextExpression = new Regex
            (
            // Beginning of line and any amount of whitespace.
            @""^\s*""
            // Match a [optional project number prefix 'ddd>'], single letter + colon + remaining filename, or
            // string with no colon followed by a colon.
            + @""(((?<ORIGIN>(((\d+>)?[a-zA-Z]?:[^:]*)|([^:]*))):)""
            // Origin may also be empty. In this case there's no trailing colon.
            + ""|())""
            // Match the empty string or a string without a colon that ends with a space
            + ""(?<SUBCATEGORY>(()|([^:]*? )))""
            // Match 'error' or 'warning'.
            + @""(?<CATEGORY>(error|warning))""
            // Match anything starting with a space that's not a colon/space, followed by a colon.
            // Error code is optional in which case ""error""/""warning"" can be followed immediately by a colon.
            + @""( \s*(?<CODE>[^: ]*))?\s*:""
            // Whatever's left on this line, including colons.
            + ""(?<TEXT>.*)$"",
            RegexOptions.IgnoreCase
            );
}", "Option Explicit Off
Option Infer On
Option Strict Off

Imports System
Imports System.Globalization
Imports System.Text.RegularExpressions

Friend Module CanonicalError

    ' Defines the main pattern for matching messages.
    Private ReadOnly _s_originCategoryCodeTextExpression As New Regex( _
 _ ' Beginning of line and any amount of whitespace.
        ""^\s*"" _
 _ ' Match a [optional project number prefix 'ddd>'], single letter + colon + remaining filename, or
 _ ' string with no colon followed by a colon.
    & ""(((?<ORIGIN>(((\d+>)?[a-zA-Z]?:[^:]*)|([^:]*))):)"" _
 _ ' Origin may also be empty. In this case there's no trailing colon.
    & ""|())"" _
 _ ' Match the empty string or a string without a colon that ends with a space
    & ""(?<SUBCATEGORY>(()|([^:]*? )))"" _
 _ ' Match 'error' or 'warning'.
    & ""(?<CATEGORY>(error|warning))"" _
 _ ' Match anything starting with a space that's not a colon/space, followed by a colon.
 _ ' Error code is optional in which case ""error""/""warning"" can be followed immediately by a colon.
    & ""( \s*(?<CODE>[^: ]*))?\s*:"" _
 _ ' Whatever's left on this line, including colons.
    & ""(?<TEXT>.*)$"",
        RegexOptions.IgnoreCase _
    )

End Module
")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBConcatinateComplexString2()
            TestConversionCSharpToVisualBasic("using System;
using System.Globalization;
using System.Text.RegularExpressions;

internal static class CanonicalError
{
    // Defines the main pattern for matching messages.
    private static readonly Regex s_originCategoryCodeTextExpression = new Regex
            (
            @""^\s*""
            // Match a [optional project number prefix 'ddd>'], single letter + colon + remaining filename, or
            // string with no colon followed by a colon.
            + @""(((?<ORIGIN>(((\d+>)?[a-zA-Z]?:[^:]*)|([^:]*))):)""
            // Origin may also be empty. In this case there's no trailing colon.
            + ""|())""
            // Match the empty string or a string without a colon that ends with a space
            + ""(?<SUBCATEGORY>(()|([^:]*? )))""
            // Match 'error' or 'warning'.
            + @""(?<CATEGORY>(error|warning))""
            // Match anything starting with a space that's not a colon/space, followed by a colon.
            // Error code is optional in which case ""error""/""warning"" can be followed immediately by a colon.
            + @""( \s*(?<CODE>[^: ]*))?\s*:""
            // Whatever's left on this line, including colons.
            + ""(?<TEXT>.*)$"",
            RegexOptions.IgnoreCase
            );
}", "Option Explicit Off
Option Infer On
Option Strict Off

Imports System
Imports System.Globalization
Imports System.Text.RegularExpressions

Friend Module CanonicalError

    ' Defines the main pattern for matching messages.
    Private ReadOnly _s_originCategoryCodeTextExpression As New Regex( _
        ""^\s*"" _
 _ ' Match a [optional project number prefix 'ddd>'], single letter + colon + remaining filename, or
 _ ' string with no colon followed by a colon.
    & ""(((?<ORIGIN>(((\d+>)?[a-zA-Z]?:[^:]*)|([^:]*))):)"" _
 _ ' Origin may also be empty. In this case there's no trailing colon.
    & ""|())"" _
 _ ' Match the empty string or a string without a colon that ends with a space
    & ""(?<SUBCATEGORY>(()|([^:]*? )))"" _
 _ ' Match 'error' or 'warning'.
    & ""(?<CATEGORY>(error|warning))"" _
 _ ' Match anything starting with a space that's not a colon/space, followed by a colon.
 _ ' Error code is optional in which case ""error""/""warning"" can be followed immediately by a colon.
    & ""( \s*(?<CODE>[^: ]*))?\s*:"" _
 _ ' Whatever's left on this line, including colons.
    & ""(?<TEXT>.*)$"",
        RegexOptions.IgnoreCase _
    )

End Module
")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBConcatinateManyString()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        var x = ""Line1"" +
                ""Line2""
                + ""Line3"" + ""Line3A"";
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Private Sub TestMethod()
        Dim x As String = ""Line1"" &
            ""Line2"" _
        & ""Line3"" & ""Line3A""
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBConditionalExpression()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod(string str)
    {
        bool result = (str == """") ? true : false;
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Private Sub TestMethod(str As String)
        Dim result As Boolean = If((str = """"), True, False)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBDelegateExpression()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        var test = delegate(int a) { return a * 2 };

        test(3);
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Private Sub TestMethod()
        Dim test As System.Func(Of Integer, Integer) = Function(a As Integer) a * 2

        test(3)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBElvisOperatorExpression()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod(string str)
    {
        int length = str?.Length ?? -1;
        Console.WriteLine(length);
        Console.ReadKey();
        string redirectUri = context.OwinContext.Authentication?.AuthenticationResponseChallenge?.Properties?.RedirectUri;
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Private Sub TestMethod(str As String)
        Dim _length As Integer = If(str?.Length, -1)
        Console.WriteLine(_length)
        Console.ReadKey()
        Dim _redirectUri As String = context.OwinContext.Authentication?.AuthenticationResponseChallenge?.Properties?.RedirectUri
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBLambdaBodyExpression()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        var test = a => { return a * 2 };
        var test2 = (a, b) => { if (b > 0) return a / b; return 0; }
        var test3 = (a, b) => a % b;

        test(3);
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Private Sub TestMethod()
        Dim test As System.Func(Of Object, Object) = Function(a) a * 2
        Dim test2 As System.Func(Of Object, Object, Integer) = Function(a, b) As Integer
                                                                   If b > 0 Then
                                                                       Return a / b
                                                                   End If

                                                                   Return 0
                                                               End Function
        Dim test3 As System.Func(Of Object, Object, Object) = Function(a, b) a Mod b

        test(3)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBLineCOntinuationComment()
            TestConversionCSharpToVisualBasic("class TestClass
{
    /// <summary>
    ///  Looks up a localized string similar to Analyzer driver threw an exception of type &apos;{0}&apos; with message &apos;{1}&apos;..
    /// </summary>
    void TestMethod()
    {
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    ''' <summary>
    '''  Looks up a localized string similar to Analyzer driver threw an exception of type &apos;{0}&apos; with message &apos;{1}&apos;..
    ''' </summary>
    Private Sub TestMethod()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBLinq1()
            TestConversionCSharpToVisualBasic("static void SimpleQuery()
{
    int[] numbers = { 7, 9, 5, 3, 6 };

    var res = from n in numbers
            where n > 5
            select n;

    foreach (var n in res)
        Console.WriteLine(n);
}", "Option Explicit Off
Option Infer On
Option Strict Off

Private Shared Sub SimpleQuery()
    Dim numbers As Integer() = {7, 9, 5, 3, 6}

    Dim res = From n In numbers
              Where n > 5
              Select n

    For Each n In res
        Console.WriteLine(n)
    Next
End Sub")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBLinq2()
            TestConversionCSharpToVisualBasic("public static void Linq40()
    {
        int[] numbers = { 5, 4, 1, 3, 9, 8, 6, 7, 2, 0 };

        var numberGroups =
            from n in numbers
            group n by n % 5 into g
            select new { Remainder = g.Key, Numbers = g };

        foreach (var g in numberGroups)
        {
            Console.WriteLine($""Numbers with a remainder of {g.Remainder} when divided by 5:"");
            foreach (var n in g.Numbers)
            {
                Console.WriteLine(n);
            }
        }
    }", "Option Explicit Off
Option Infer On
Option Strict Off

Public Shared Sub Linq40()
    Dim numbers As Integer() = {5, 4, 1, 3, 9, 8, 6, 7, 2, 0}

    Dim numberGroups = From n In numbers
                       Group n By __groupByKey1__ = n Mod 5 Into g Select New With {Key .Remainder = g.Key, Key .Numbers = g}

    For Each g In numberGroups
        Console.WriteLine($""Numbers with a remainder of {g.Remainder} when divided by 5:"")
        For Each n In g.Numbers
            Console.WriteLine(n)
        Next
    Next
End Sub")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBLinq3()
            TestConversionCSharpToVisualBasic("class Product {
    public string Category;
    public string ProductName;
}

class Test {
    public void Linq102()
    {
        string[] categories = new string[]{
            ""Beverages"",
            ""Condiments"",
            ""Vegetables"",
            ""Dairy Products"",
            ""Seafood"" };

            Product[] products = GetProductList();

            var q =
                from c in categories
                join p in products on c equals p.Category
                select new { Category = c, p.ProductName };

        foreach (var v in q)
        {
            Console.WriteLine($""{v.ProductName}: {v.Category}"");
        }
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class Product

    Public Category As String

    Public ProductName As String
End Class

Class Test

    Public Sub Linq102()
        Dim categories As String() = New String() { _
            ""Beverages"",
            ""Condiments"",
            ""Vegetables"",
            ""Dairy Products"",
            ""Seafood""}

        Dim products As Product() = GetProductList()

        Dim q = From c In categories
                Join p In products On c Equals p.Category
                Select New With {Key .Category = c, p.ProductName}

        For Each v In q
            Console.WriteLine($""{v.ProductName}: {v.Category}"")
        Next
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBLinq4()
            TestConversionCSharpToVisualBasic("public void Linq103()
{
    string[] categories = new string[]{
        ""Beverages"",
        ""Condiments"",
        ""Vegetables"",
        ""Dairy Products"",
        ""Seafood"" };

        var products = GetProductList();

        var q =
            from c in categories
            join p in products on c equals p.Category into ps
            select new { Category = c, Products = ps };

    foreach (var v in q)
    {
        Console.WriteLine(v.Category + "":"");
        foreach (var p in v.Products)
        {
            Console.WriteLine(""   "" + p.ProductName);
        }
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Public Sub Linq103()
    Dim categories As String() = New String() { _
        ""Beverages"",
        ""Condiments"",
        ""Vegetables"",
        ""Dairy Products"",
        ""Seafood""}

    Dim products = GetProductList()

    Dim q = From c In categories
            Group Join p In products On c Equals p.Category Into ps = Group
            Select New With {Key .Category = c, Key .Products = ps}

    For Each v In q
        Console.WriteLine(v.Category & "":"")
        For Each p In v.Products
            Console.WriteLine(""   "" & p.ProductName)
        Next
    Next
End Sub")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMemberAccessAndInvocationExpression()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod(string str)
    {
        int length;
        length = str.Length;
        Console.WriteLine(""Test""
                            + ""1"");
        Console.ReadKey();
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Private Sub TestMethod(str As String)
        Dim length As Integer
        length = str.Length
        Console.WriteLine(""Test"" _
        & ""1"")
        Console.ReadKey()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMultilineString()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        var x = @""Hello,
World!"";
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Private Sub TestMethod()
        Dim x As String = ""Hello,
World!""
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBNullCoalescingExpression()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod(string str)
    {
        Console.WriteLine(str ?? ""<null>"");
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Private Sub TestMethod(str As String)
        Console.WriteLine(If(str, ""<null>""))
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBObjectInitializerExpression()
            TestConversionCSharpToVisualBasic("class StudentName
{
    public string LastName, FirstName;
}

class TestClass
{
    void TestMethod(string str)
    {
        StudentName student2 = new StudentName
        {
            FirstName = ""Craig"",
            LastName = ""Playmate"",
        };
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class StudentName

    Public LastName, FirstName As String
End Class

Class TestClass

    Private Sub TestMethod(str As String)
        Dim student2 As New StudentName With
            { _
            .FirstName = ""Craig"",
            .LastName = ""Playmate""}
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBObjectInitializerExpression2()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod(string str)
    {
        var student2 = new {
            FirstName = ""Craig"",
            LastName = ""Playmate"",
        };
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Private Sub TestMethod(str As String)
        Dim student2 As New With {Key .FirstName = ""Craig"", Key .LastName = ""Playmate""}
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBThisMemberAccessExpression()
            TestConversionCSharpToVisualBasic("class TestClass
{
    private int member;

    void TestMethod()
    {
        this.member = 0;
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Private _member As Integer

    Private Sub TestMethod()
        Me._member = 0
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTupleAssignment()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        var x = (true, 1, 2.2, ""3"");
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Class TestClass

    Private Sub TestMethod()
        Dim x As (Boolean, Integer, Double, String) = (True, 1, 2.2, ""3"")
    End Sub
End Class")
        End Sub

    End Class

End Namespace
