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
}", "Class TestClass

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
        base.member = 0;
    }
}", "Class BaseTestClass

    Public member As Integer
End Class

Class TestClass
    Inherits BaseTestClass

    Private Sub TestMethod()
        MyBase.member = 0
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBBaseMemberAccessExpressionMultiLine()
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
}", "Class BaseTestClass

    Public member As Integer
End Class

Class TestClass
    Inherits BaseTestClass

    Private Sub TestMethod()
        MyBase.member = 0
        MyBase.member = 1
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCallInvoke()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod(string str)
    {
        Dispatcher.Invoke(new Action(() => Console.WriteLine(1)));
    }
}", "Class TestClass

    Private Sub TestMethod(str As String)
        Dispatcher.Invoke(New Action(Function() Console.WriteLine(1)))
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCoalescingExpressionAssignment()
            TestConversionCSharpToVisualBasic(
"class TestClass {
    string prop;
    string prop2;
    string Property {
        get {
            var z = (() => 3)();
            return this.prop ?? (this.prop2 = CreateProperty());
        }
    }
    string CreateProperty() {
        return """";
    }
}", "Class TestClass

    Private prop As String

    Private prop2 As String

    ReadOnly Property [Property] As String
        Get
            Dim z = (Function() 3)()
            Return If(Me.prop, __InlineAssignHelper(Me.prop2, CreateProperty()))
        End Get
    End Property

    Private Function CreateProperty() As String
        Return """"
    End Function

    <Obsolete(""Please refactor code that uses this function, it is a simple work-around to simulate inline assignment in VB!"")>
    Private Shared Function __InlineAssignHelper(Of T)(ByRef target As T, value As T) As T
        target = value
        Return value
    End Function
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
}", "Class TestClass

    ''' <summary>
    '''  Looks up a localized string similar to Analyzer driver threw an exception of type &apos;{0}&apos; with message &apos;{1}&apos;..
    ''' </summary>
    Private Sub TestMethod()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCommentWithHTML()
            TestConversionCSharpToVisualBasic("class TestClass
{
        /// <summary>
        ///   Looks up a localized string similar to Analyzer driver threw the following exception:
        ///&apos;{0}&apos;..
        /// </summary>
     void TestMethod()
    {
    }
}", "Class TestClass

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
            TestConversionCSharpToVisualBasic("class TestClass
{static void SimpleQuery()
    {
        int[] numbers = { 7, 9, 5, 3, 6 };           // Test

    }
}", "NotInheritable Class TestClass

    Private Shared Sub SimpleQuery()
        Dim numbers As Integer() = {7, 9, 5, 3, 6}           ' Test
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCompoundAssignmentTest()
            TestConversionCSharpToVisualBasic(
"public class TestClass {
    void TestMethod() {
        int x = 10;
        x *= 3;
        x /= 3;
    }
}", "Public Class TestClass

    Private Sub TestMethod()
        Dim x As Integer = 10
        x *= 3
        x /= 3
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBConcatinateComplexString()
            TestConversionCSharpToVisualBasic("using System; //Not required in VB due to global imports
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
}", "Imports System.Globalization
Imports System.Text.RegularExpressions

Friend Module CanonicalError

    ' Defines the main pattern for matching messages.
    Private ReadOnly s_originCategoryCodeTextExpression As New Regex( _
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
            TestConversionCSharpToVisualBasic("using System; //Not required in VB due to global imports
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
}", "Imports System.Globalization
Imports System.Text.RegularExpressions

Friend Module CanonicalError

    ' Defines the main pattern for matching messages.
    Private ReadOnly s_originCategoryCodeTextExpression As New Regex( _
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
}", "Class TestClass

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
}", "Class TestClass

    Private Sub TestMethod(str As String)
        Dim result As Boolean = If((str = """"), True, False)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBDeclarationExpression()
            TestConversionCSharpToVisualBasic("using System.Collections.Generic;

class TestClass
{
    private static bool Do()
    {
        var d = new Dictionary<string, string>();
        return d.TryGetValue("""", out var output);
    }
}", "Imports System.Collections.Generic

Class TestClass

    Private Shared Function [Do]() As Boolean
        Dim d As Collections.Generic.Dictionary(Of String, String) = New Dictionary(Of String, String)
        Dim output As String = Nothing
        Return d.TryGetValue("""", output)
    End Function
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBDefaultLiteralExpression()
            TestConversionCSharpToVisualBasic("public class DefaultLiteralExpression {

    public bool Foo {
        get {
            return (Bar == default);
        }
    }

    public int Bar;

}", "Public Class DefaultLiteralExpression

    Public ReadOnly Property Foo As Boolean
        Get
            Return (Bar = CType(Nothing, Boolean))
        End Get
    End Property

    Public Bar As Integer
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBDelegateExpressionWithPrivate()
            TestConversionCSharpToVisualBasic("class TestClass
{

    private static Action<int> m_Event1 = delegate { };

    void TestMethod()
    {
        var test = delegate(int a) { return a * 2 };

        test(3);
    }
}", "Class TestClass

    Private Shared m_Event1 As Action(Of Integer) = Sub()
                                                    End Sub

    Private Sub TestMethod()
        Dim test As Func(Of Integer, Integer) = Function(a As Integer) As Integer
                                                    Return a * 2
                                                End Function

        test(3)
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
}", "Class TestClass

    Private Sub TestMethod()
        Dim test As Func(Of Integer, Integer) = Function(a As Integer) As Integer
                                                    Return a * 2
                                                End Function

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
}", "Class TestClass

    Private Sub TestMethod(str As String)
        Dim length1 As Integer = If(str?.Length, -1)
        Console.WriteLine(length1)
        Console.ReadKey()
        Dim redirectUri1 As String = context.OwinContext.Authentication?.AuthenticationResponseChallenge?.Properties?.RedirectUri
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBEqualsExpression()
            TestConversionCSharpToVisualBasic(
"public class TestClass {
    public TestClass() {
        int i = 0;
        int j = 0;
        string s1 = ""string1"";
        string s2 = ""string2"";
        object object1 = s1;
        object object2 = s2;
        if(i == j)
            DoSomething();
        if(i == s2)
            DoSomething();
        if(i == object1)
            DoSomething();
        if(s1 == j)
            DoSomething();
        if(s1 == s2)
            DoSomething();
        if(s1 == object2)
            DoSomething();
        if(object1 == j)
            DoSomething();
        if(object1 == s2)
            DoSomething();
        if(object1 == object2)
            DoSomething();
    }
    public void DoSomething() { }
}",
"Public Class TestClass

    Public Sub New()
        Dim i As Integer = 0
        Dim j As Integer = 0
        Dim s1 As String = ""string1""
        Dim s2 As String = ""string2""
        Dim object1 As Object = s1
        Dim object2 As Object = s2
        If i = j Then
            DoSomething()
        End If

        If i = s2 Then
            DoSomething()
        End If

        If i = object1 Then
            DoSomething()
        End If

        If s1 = j Then
            DoSomething()
        End If

        If s1 = s2 Then
            DoSomething()
        End If

        If s1 Is object2 Then
            DoSomething()
        End If

        If object1 = j Then
            DoSomething()
        End If

        If object1 Is s2 Then
            DoSomething()
        End If

        If object1 Is object2 Then
            DoSomething()
        End If
    End Sub

    Public Sub DoSomething()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBExpressionSub()
            TestConversionCSharpToVisualBasic("using System; //Not required in VB due to global imports

static class Program
{
    private static void Main(string[] args)
    {
        Action<string> x = (Action<string>)(_ => Environment.Exit(0));
    }
}", "Module Program

    Private Sub Main(args As String())
        Dim x As Action(Of String) = CType(AddressOf (Sub(__) Environment.[Exit](0)), Action(Of String))
    End Sub

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBIfIsPatternExpression()
            TestConversionCSharpToVisualBasic("class TestClass
{
    private static int GetLength(object node)
    {
        if (node is string s)
        {
            return s.Length;
        }

        return -1;
    }
}", "Class TestClass

    Private Shared Function GetLength(node As Object) As Integer
        Dim TempVar As Boolean = TypeOf node Is String
        Dim s As String
        String.TryParse(node.ToString, s)
        If TempVar Then
            Return s.Length
        End If

        Return -1
    End Function
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBIsNullExpression()
            TestConversionCSharpToVisualBasic("public class Test {

    public bool Foo {
        get {
        return (Bar is null); //Crashes conversion to VB
        }
    }

    public string Bar;

}", "Public Class Test

    Public ReadOnly Property Foo As Boolean
        Get
            Return (Bar Is Nothing) 'Crashes conversion to VB
        End Get
    End Property

    Public Bar As String
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBIssue486MustCastNothing()
            TestConversionCSharpToVisualBasic(
"public class WhyWeNeedToCastNothing
{
    public void Example(int? vbInitValue)
    {
        var withDefault = vbInitValue != null ? 7 : default(int?);
        var withNull = vbInitValue != null ? (int?)8 : null;
    }
}",
"Public Class WhyWeNeedToCastNothing

    Public Sub Example(vbInitValue As Integer?)
        Dim withDefault As Integer? = If(vbInitValue IsNot Nothing, 7, CType(Nothing, Integer?))
        Dim withNull As Integer? = If(vbInitValue IsNot Nothing, CType(8, Integer?), Nothing)
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
}", "Class TestClass

    Private Sub TestMethod()
        Dim test As Func(Of Object, Object) = Function(a) a * 2
        Dim test2 As Func(Of Object, Object, Integer) = Function(a, b) As Integer
                                                            If b > 0 Then
                                                                Return a / b
                                                            End If

                                                            Return 0
                                                        End Function
        Dim test3 As Func(Of Object, Object, Object) = Function(a, b) a Mod b

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
}", "Class TestClass

    ''' <summary>
    '''  Looks up a localized string similar to Analyzer driver threw an exception of type &apos;{0}&apos; with message &apos;{1}&apos;..
    ''' </summary>
    Private Sub TestMethod()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBLinq()
            TestConversionCSharpToVisualBasic("class TestClass
{
    static void SimpleQuery()
    {
        int[] numbers = { 7, 9, 5, 3, 6 };

        var res = from n in numbers
                    where n > 5
                    select n;

        foreach (var n in res)
            Console.WriteLine(n);
    }
}", "NotInheritable Class TestClass

    Private Shared Sub SimpleQuery()
        Dim numbers As Integer() = {7, 9, 5, 3, 6}

        Dim res = From n In numbers
                  Where n > 5
                  Select n

        For Each n In res
            Console.WriteLine(n)
        Next
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBLinq2()
            TestConversionCSharpToVisualBasic("class TestClass
{
    public static void Linq40()
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
    }
}", "NotInheritable Class TestClass

    Public Shared Sub Linq40()
        Dim numbers As Integer() = {5, 4, 1, 3, 9, 8, 6, 7, 2, 0}

        Dim numberGroups = From n In numbers
                           Group n By __groupByKey0__ = n Mod 5 Into g Select New With {Key .Remainder = g.Key, Key .Numbers = g}

        For Each g In numberGroups
            Console.WriteLine($""Numbers with a remainder of {g.Remainder} when divided by 5:"")
            For Each n In g.Numbers
                Console.WriteLine(n)
            Next
        Next
    End Sub
End Class")
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
}", "Class Product

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
            TestConversionCSharpToVisualBasic("class TestClass
{
    public void Linq103()
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
    }
}", "Class TestClass

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
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBLinq2MultiLine()
            TestConversionCSharpToVisualBasic("class TestClass
{
    public static void Linq40()
    {
        int[] numbers = { 5, 4, 1, 3, 9, 8, 6, 7, 2, 0 };

        var numberGroups =
            from n in numbers
            group n by n % 5 into g
            select new {
                Remainder = g.Key,
                Numbers = g
            };

        foreach (var g in numberGroups)
        {
            Console.WriteLine($""Numbers with a remainder of {g.Remainder} when divided by 5:"");
            foreach (var n in g.Numbers)
            {
                Console.WriteLine(n);
            }
        }
    }
}", "NotInheritable Class TestClass

    Public Shared Sub Linq40()
        Dim numbers As Integer() = {5, 4, 1, 3, 9, 8, 6, 7, 2, 0}

        Dim numberGroups = From n In numbers
                           Group n By __groupByKey0__ = n Mod 5 Into g Select New With {Key .Remainder = g.Key, Key .Numbers = g}

        For Each g In numberGroups
            Console.WriteLine($""Numbers with a remainder of {g.Remainder} when divided by 5:"")
            For Each n In g.Numbers
                Console.WriteLine(n)
            Next
        Next
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBLinq3MultiLine()
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
                select new {
                    Category = c, p.ProductName
                };

        foreach (var v in q)
        {
            Console.WriteLine($""{v.ProductName}: {v.Category}"");
        }
    }
}", "Class Product

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
        Public Shared Sub CSharpToVBLinq4MultiLine()
            TestConversionCSharpToVisualBasic("class TestClass
{
    public void Linq103()
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
                select new {
                    Category = c,
                    Products = ps
                };

        foreach (var v in q)
        {
            Console.WriteLine(v.Category + "":"");
            foreach (var p in v.Products)
            {
                Console.WriteLine(""   "" + p.ProductName);
            }
        }
    }
}", "Class TestClass

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
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMemberAccessAndInvocationExpression()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod(string str)
    {
        int length;
        length = str.Length;
        Console.WriteLine(""Test"" + length);
        Console.ReadKey();
    }
}", "Class TestClass

    Private Sub TestMethod(str As String)
        Dim length As Integer
        length = str.Length
        Console.WriteLine(""Test"" & length)
        Console.ReadKey()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMultilineFunctionExpressionWithSingleStatement()
            TestConversionCSharpToVisualBasic(
"using System; //Not required in VB due to global imports
public class TestClass {
    Func<object, string> create = o => {
        if(o is TestClass)
            return ""first"";
        else
            return ""second"";
    };
    public TestClass() {
        string str = create(this);
    }
}", "Public Class TestClass

    Private create As Func(Of Object, String) = Function(o) As String
                                                    If TypeOf o Is TestClass Then
                                                        Return ""first""
                                                    Else
                                                        Return ""second""
                                                    End If
                                                End Function

    Public Sub New()
        Dim str As String = create(Me)()
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
}", "Class TestClass

    Private Sub TestMethod()
        Dim x As String = ""Hello,
World!""
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMultilineSubExpressionWithSingleStatement()
            TestConversionCSharpToVisualBasic(
"public class TestClass : System.Collections.ObjectModel.ObservableCollection<string> {
    public TestClass() {
        PropertyChanged += (o, e) => {
            if (e.PropertyName == ""AnyProperty"") {
                Add(""changed"");
            } else
                RemoveAt(0);
        };
    }
}", "Public Class TestClass
    Inherits Collections.ObjectModel.ObservableCollection(Of String)

    Public Sub New()
        AddHandler PropertyChanged, Sub(o, e)
                                        If e.PropertyName = ""AnyProperty"" Then
                                            Add(""changed"")
                                        Else
                                            RemoveAt(0)
                                        End If
                                    End Sub
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBNameOf()
            TestConversionCSharpToVisualBasic("class TestClass
{
    private string n = nameof(TestMethod);

    private void TestMethod()
    {
    }
}", "Class TestClass

    Private n As String = NameOf(TestMethod)

    Private Sub TestMethod()
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
}", "Class TestClass

    Private Sub TestMethod(str As String)
        Console.WriteLine(If(str, ""<null>""))
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBObjectInitializerExpression()
            TestConversionCSharpToVisualBasic("
class StudentName
{
    public string LastName, FirstName;
}

class TestClass
{
    void TestMethod(string str)
    {
        StudentName student2 = new StudentName {
            FirstName = ""Craig"",
            LastName = ""Playstead"",
        };
    }
}", "Class StudentName

    Public LastName, FirstName As String
End Class

Class TestClass

    Private Sub TestMethod(str As String)
        Dim student2 As New StudentName With
            { _
            .FirstName = ""Craig"",
            .LastName = ""Playstead""
        }
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
            LastName = ""Playstead"",
        };
    }
}", "Class TestClass

    Private Sub TestMethod(str As String)
        Dim student2 As New With {Key .FirstName = ""Craig"", Key .LastName = ""Playstead""}
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBObjectInitializerExpression3()
            TestConversionCSharpToVisualBasic("using System.Collections.Generic;

internal class SomeSettings
{
    public IList<object> Converters { get; set; }
}

internal class Converter
{
    public static readonly SomeSettings Settings = new SomeSettings
    {
        Converters = {},
    };
}", "Imports System.Collections.Generic

Friend Class SomeSettings

    Public Property Converters As IList(Of Object)
End Class

Friend Class Converter

    Public Shared ReadOnly Settings As New SomeSettings With
        { _
        .Converters = New Collections.Generic.IList(Of Object) From {}
    }
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBPrefixUnaryExpressionSingleLineFunction()
            TestConversionCSharpToVisualBasic(
"public class TestClass {
    public TestClass() {
        System.Func<string, bool> func = o => !string.IsNullOrEmpty(""test"");
    }
}",
"Public Class TestClass

    Public Sub New()
        Dim func1 As Func(Of String, Boolean) = Function(o) Not String.IsNullOrEmpty(""test"")
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBReferenceTypeComparison()
            TestConversionCSharpToVisualBasic("class TestClass
{
    public static bool AreTwoObjectsReferenceEqual()
    {
        return new object() == new object();
    }
}", "Class TestClass

    Public Shared Function AreTwoObjectsReferenceEqual() As Boolean
        Return New Object Is New Object
    End Function
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBShiftOperators()
            TestConversionCSharpToVisualBasic("public class Test
{
    public static void Main()
    {
        int y = 1;
        y <<= 1;
        y >>= 1;
        y = y << 1;
        y = y >> 1;
    }
}", "Public NotInheritable Class Test

    Public Shared Sub Main()
        Dim y As Integer = 1
        y <<= 1
        y >>= 1
        y = y << 1
        y = y >> 1
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBStringInterpolationWithDoubleQuotes()
            TestConversionCSharpToVisualBasic(
                "using System; //Not required in VB due to global imports

namespace global::InnerNamespace
{
    public class Test
    {
        public string StringInter(string t, DateTime dt)
        {
            var a = $""pre{t} t"";
            var b = $""pre{t} \"" t"";
            var c = $@""pre{t} """"\ t"";
            var d = $""pre{t + ""\""""} \"" t"";
            var e = $@""pre{t + ""\""""} """"\ t"";
            var f = $""pre{{escapedBraces}}{dt,4:hh}"";
            return a + b + c + d + e + f;
        }
    }
}", "Namespace Global.InnerNamespace

    Public Class Test

        Public Function StringInter(t As String, dt As DateTime) As String
            Dim a As String = $""pre{t} t""
            Dim b As String = $""pre{t} """" t""
            Dim c As String = $""pre{t} """"\ t""
            Dim d As String = $""pre{t & """"""""} """" t""
            Dim e As String = $""pre{t & """"""""} """"\ t""
            Dim f As String = $""pre{{escapedBraces}}{dt,4:hh}""
            Return a & b & c & d & e & f
        End Function
    End Class
End Namespace")
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
}", "Class TestClass

    Private member As Integer

    Private Sub TestMethod()
        Me.member = 0
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBThrowExpression()
            TestConversionCSharpToVisualBasic("using System; //Not required in VB due to global imports

class TestClass
{
    void TestMethod(string str)
    {
        bool result = (str == """") ? throw new Exception(""empty"") : false;
    }
}", "Class TestClass

    Private Sub TestMethod(str As String)
        If (str = """") Then Throw New Exception(""empty"")
        Dim result As Boolean = False
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
}", "Class TestClass

    Private Sub TestMethod()
        Dim x As (Boolean, Integer, Double, String) = (True, 1, 2.2, ""3"")
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTupleType()
            TestConversionCSharpToVisualBasic("public interface ILanguageConversion
{
    IReadOnlyCollection<(string, string)> GetProjectTypeGuidMappings();
    IEnumerable<(string, string)> GetProjectFileReplacementRegexes();
}", "Public Interface ILanguageConversion

    Function GetProjectTypeGuidMappings() As IReadOnlyCollection(Of (String, String))
    Function GetProjectFileReplacementRegexes() As IEnumerable(Of (String, String))

End Interface")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBValueTupleType()
            TestConversionCSharpToVisualBasic("using System; //Not required in VB due to global imports
using System.Collections.Generic;

namespace PreHOPL
{
    static class Program
    {
        private static readonly Dictionary<string, ValueTuple<int, Delegate>> dict =
            new Dictionary<string, ValueTuple<int, Delegate>>()
        {
            [""SAY""] =  (1, (Action<string>)System.Console.WriteLine)
        };
        private static void Main(string[] args)
        {
            dict[""SAY""].Item2.DynamicInvoke(""Hello World!"");
        }
    }
}", "Imports System.Collections.Generic

Namespace PreHOPL

    Module Program

        Private ReadOnly dict As New Dictionary(Of String, ValueTuple(Of Integer, [Delegate])) From { _
            {""SAY"", (1, CType(AddressOf System.Console.WriteLine, Action(Of String)))}
        }
        Private Sub Main(args As String())
            dict(""SAY"").Item2.DynamicInvoke(""Hello World!"")
        End Sub

    End Module
End Namespace")
        End Sub


        <Fact>
        Public Shared Sub CSharpToVBValueWithExpression()
            TestConversionCSharpToVisualBasic("using System;

public class InheritanceExample
{
    public record Point(int X, int Y);
    public record NamedPoint(string Name, int X, int Y) : Point(X, Y);

    public static void Main()
    {
        Point p1 = new NamedPoint(""A"", 0, 0);
        Point p2 = p1 with { X = 5, Y = 3 };
        Console.WriteLine(p2 is NamedPoint);  // output: True
        Console.WriteLine(p2);  // output: NamedPoint { X = 5, Y = 3, Name = A }
    }
}", "Public NotInheritable Class InheritanceExample

    ' TODO TASK: VB has no direct equivalent to C# Records
    Public Class Point

        Dim X As Integer

        Dim Y As Integer

        Sub New(X As Integer, Y As Integer)
            Me.X = X
            Me.Y = Y
        End Sub

        Public Overrides Function Equals(anotherObject As Object) As Boolean
            Dim anotherRecord As Object = TryCast(anotherObject, Point)
            If anotherRecord Is Nothing Then Return False
            Return Equals(anotherRecord)
        End Function

        Public Overloads Function Equals(anotherRecord As Point) As Boolean
            If Not X.Equals(anotherRecord.X) Then Return False
            If Not Y.Equals(anotherRecord.Y) Then Return False
            Return True
        End Function
    End Class

    ' TODO TASK: VB has no direct equivalent to C# Records
    Public Class NamedPoint

        Dim Name As String

        Dim X As Integer

        Dim Y As Integer

        Sub New(Name As String, X As Integer, Y As Integer)
            Me.Name = Name
            Me.X = X
            Me.Y = Y
        End Sub

        Public Overrides Function Equals(anotherObject As Object) As Boolean
            Dim anotherRecord As Object = TryCast(anotherObject, NamedPoint)
            If anotherRecord Is Nothing Then Return False
            Return Equals(anotherRecord)
        End Function

        Public Overloads Function Equals(anotherRecord As NamedPoint) As Boolean
            If Not Name.Equals(anotherRecord.Name) Then Return False
            If Not X.Equals(anotherRecord.X) Then Return False
            If Not Y.Equals(anotherRecord.Y) Then Return False
            Return True
        End Function
    End Class

    Public Shared Sub Main()
        Dim p1 As Point = New NamedPoint(""A"", 0, 0)
        Dim p2 As Point = CType(Function(_p1 As p1) As p1
                                    Dim _p2 As p1 = _p1.Clone
                                    With _p2
                                        .X = 5
                                        .Y = 3
                                    End With
                                End Function, Func(Of _p1, _p1))(p1)
        Console.WriteLine(TypeOf p2 Is NamedPoint)  ' output: True
        Console.WriteLine(p2)  ' output: NamedPoint { X = 5, Y = 3, Name = A }
    End Sub
End Class")
        End Sub
    End Class

End Namespace
