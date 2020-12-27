' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports CodeConverter.Tests

Imports Xunit

Namespace CSharpToVB.Tests

    <TestClass()> Public Class StatementTests
        Inherits ConverterTestBase

        '    Public Function LoadAnalyzer(analyzerPath As String) As Exception
        '        _analyzerLoadException = Nothing
        '        Dim analyzerRef = New AnalyzerFileReference(analyzerPath, FromFileLoader.Instance)
        '        AddHandler analyzerRef.AnalyzerLoadFailed, Function(s, e) _analyzerLoadException = e.Exception
        '        Dim builder = ImmutableArray.CreateBuilder(Of DiagnosticAnalyzer)()
        '        analyzerRef.AddAnalyzers(builder, LanguageNames.CSharp)
        '        Return _analyzerLoadException
        '    End Function
        'End Class
        <Fact>
        Public Shared Sub CSharpToVBAddHandler()
            TestConversionCSharpToVisualBasic("using System; //Not required in VB due to global imports

    class RemoteAnalyzerFileReferenceTest : MarshalByRefObject
    {
        private Exception _analyzerLoadException;

        public override object InitializeLifetimeService()
        {
            return null;
        }

        public Exception LoadAnalyzer(string analyzerPath)
        {
            _analyzerLoadException = null;
            var analyzerRef = new AnalyzerFileReference(analyzerPath, FromFileLoader.Instance);
            analyzerRef.AnalyzerLoadFailed += (s, e) => _analyzerLoadException = e.Exception;
            var builder = ImmutableArray.CreateBuilder<DiagnosticAnalyzer>();
            analyzerRef.AddAnalyzers(builder, LanguageNames.CSharp);
            return _analyzerLoadException;
        }
    }
", "Class RemoteAnalyzerFileReferenceTest
    Inherits MarshalByRefObject

    Private _analyzerLoadException As Exception

    Public Overrides Function InitializeLifetimeService() As Object
        Return Nothing
    End Function

    Public Function LoadAnalyzer(analyzerPath As String) As Exception
        _analyzerLoadException = Nothing
        Dim analyzerRef As New AnalyzerFileReference(analyzerPath, FromFileLoader.Instance)
        analyzerRef.AnalyzerLoadFailed += Sub(s, e) _analyzerLoadException = e.Exception
        Dim builder = ImmutableArray.CreateBuilder(Of DiagnosticAnalyzer)()
        analyzerRef.AddAnalyzers(builder, LanguageNames.CSharp)
        Return _analyzerLoadException
    End Function
End Class
")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBAddHandlerInSimpleLambda()
            TestConversionCSharpToVisualBasic(
"using System; //Not required in VB due to global imports
using System.ComponentModel;
using System.Collections.Generic;

class TestClass {
    List<INotifyPropertyChanged> items;

    void TestMethod(EventHandler e) {
        items.ForEach(x => x.PropertyChanged += OnItemPropertyChanged);
    }

    void OnItemPropertyChanged(object sender, PropertyChangedEventArgs e) { }
}",
"Imports System.ComponentModel
Imports System.Collections.Generic

Class TestClass

    Private items As List(Of INotifyPropertyChanged)

    Private Sub TestMethod(e As EventHandler)
        items.ForEach(Sub(x) AddHandler x.PropertyChanged, AddressOf OnItemPropertyChanged)
    End Sub

    Private Sub OnItemPropertyChanged(sender As Object, e As PropertyChangedEventArgs)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBAddRemoveHandler()
            TestConversionCSharpToVisualBasic("using System; //Not required in VB due to global imports

class TestClass
{
    public event EventHandler MyEvent;

    void TestMethod(EventHandler e)
    {
        this.MyEvent += e;
        this.MyEvent += MyHandler;
    }

    void TestMethod2(EventHandler e)
    {
        this.MyEvent -= e;
        this.MyEvent -= MyHandler;
    }

    void MyHandler(object sender, EventArgs e)
    {

    }
}", "Class TestClass

    Public Event MyEvent As EventHandler

    Private Sub TestMethod(e As EventHandler)
        AddHandler Me.MyEvent, e
        AddHandler Me.MyEvent, AddressOf MyHandler
    End Sub

    Private Sub TestMethod2(e As EventHandler)
        RemoveHandler Me.MyEvent, e
        RemoveHandler Me.MyEvent, AddressOf MyHandler
    End Sub

    Private Sub MyHandler(sender As Object, e As EventArgs)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBArrayDeclarationStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int[] b;
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBArrayInitializationStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int[] b = { 1, 2, 3 };
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer() = {1, 2, 3}
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBArrayInitializationStatementInVarDeclaration()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        var b = { 1, 2, 3 };
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b = {1, 2, 3}
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBArrayInitializationStatementWithLength()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int[] b = new int[3] { 1, 2, 3 };
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer() = New Integer(2) {1, 2, 3}
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBArrayInitializationStatementWithType()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int[] b = new int[] { 1, 2, 3 };
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer() = New Integer() {1, 2, 3}
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBAssignmentStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int b; // This is a Comment
        b = 0;
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer ' This is a Comment
        b = 0
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBAssignmentStatementInDeclaration()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int b = 0;
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer = 0
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBAssignmentStatementInVarDeclaration()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        var b = 0;
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer = 0
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBBlockStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    public static void TestMethod()
    {
        {
            var x = 1;
            Console.WriteLine(x);
        }

        {
            var x = 2;
            Console.WriteLine(x);
        }
    }
}", "NotInheritable Class TestClass

    Public Shared Sub TestMethod()
        If True Then
            Dim x As Integer = 1
            Console.WriteLine(x)
        End If

        If True Then
            Dim x As Integer = 2
            Console.WriteLine(x)
        End If
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBConflictDeclarationsAfterConvertionForToWhile()
            TestConversionCSharpToVisualBasic(
"class TestClass {
    double height;
    void TestMethod() {
        for(double y = 0d; y < height; y += 10d)
            Draw(y);
        for(double y = 0d; y < height; y += 20d)
            Draw(y);
    }
    void Draw(double height) {
    }
}", "Class TestClass

    Private height As Double

    Private Sub TestMethod()
        Dim y As Double = 0R
        While y < height
            Draw(y)
            y += 10R
        End While

        Dim y As Double = 0R
        While y < height
            Draw(y)
            y += 20R
        End While
    End Sub

    Private Sub Draw(height As Double)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBDeclarationStatements()
            TestConversionCSharpToVisualBasic(
                "class Test {
    void TestMethod()
    {
the_beginning:
        int value = 1;
        const double myPIe = Math.PI;
        var text = ""This is my text!"";
        goto the_beginning;
    }
}", "Class Test

    Private Sub TestMethod()
the_beginning:
        Dim value As Integer = 1
        Const myPIe As Double = Math.PI
        Dim text As String = ""This is my text!""
        GoTo the_beginning
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBDoWhileStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int b;
        b = 0;
        do
        {
            if (b == 2)
                continue;
            if (b == 3)
                break;
            b = 1;
        }
        while (b == 0);
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer
        b = 0
        Do
            If b = 2 Then
                Continue Do
            End If

            If b = 3 Then
                Exit Do
            End If

            b = 1
        Loop While b = 0
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBEmptyStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        if (true) ; // This is a Comment
        while (true) ;
        for (;;) ;
        do ; while (true);
    }
}", "Class TestClass

    Private Sub TestMethod()
        If True Then ' This is a Comment
        End If

        While True
        End While

        While True
        End While

        Do
        Loop While True
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBForConvertedToWhileBreak()
            TestConversionCSharpToVisualBasic(
"class TestClass {
    void TestMethod() {
        for (;;)
            break;
    }
}", "Class TestClass

    Private Sub TestMethod()
        While True
            Exit While
        End While
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBForConvertedToWhileBreakContinue()
            TestConversionCSharpToVisualBasic(
"class TestClass {
    void TestMethod(int arg) {
        for (;;) //Becomes while loop
        {
            if (arg == 3) break;
            switch (arg)
            {
                case 1:
                    break; //From switch
                case 2:
                    break; //From switch
                default:
                    continue; // Outer while loop
            }
            for (var i = 0; i < arg; i++) // Becomes For Next loop
            {
                if (arg != 1) break; // From inner for loop
                continue; // Inner for loop
            }
            continue; // Outer while loop
        }
    }
}", "Class TestClass

    Private Sub TestMethod(arg As Integer)
        While True 'Becomes while loop
            If arg = 3 Then
                Exit While
            End If

            Select Case arg
                Case 1 'From switch
                Case 2 'From switch
                Case Else
                    Continue While ' Outer while loop
            End Select

            For i = 0 To arg - 1 ' Becomes For Next loop
                If arg <> 1 Then
                    Exit For ' From inner for loop
                End If

                Continue For ' Inner for loop
            Next

            Continue While ' Outer while loop
        End While
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBForEachStatementWithExplicitType()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod(int[] values)
    {
        foreach (int val in values)
        {
            if (val == 2)
                continue;
            if (val == 3)
                break;
        }
    }
}", "Class TestClass

    Private Sub TestMethod(values As Integer())
        For Each val As Integer In values
            If val = 2 Then
                Continue For
            End If

            If val = 3 Then
                Exit For
            End If
        Next
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBForEachStatementWithVar()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod(int[] values)
    {
        foreach (var val in values)
        {
            if (val == 2)
                continue;
            if (val == 3)
                break;
        }
    }
}", "Class TestClass

    Private Sub TestMethod(values As Integer())
        For Each val As Integer In values
            If val = 2 Then
                Continue For
            End If

            If val = 3 Then
                Exit For
            End If
        Next
    End Sub
End Class

1 target compilation errors:
BC30516: Overload resolution failed because no accessible 'Val' accepts this number of arguments.")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBForWithBlock()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        for (i = 0; i < end; i++) {
            b[i] = s[i];
        }
    }
}", "Class TestClass

    Private Sub TestMethod()
        For i = 0 To [end] - 1
            b(i) = s(i)
        Next
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBForWithBlockDecrement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        for (i = end; i > 0; i--) {
            b[i] = s[i];
        }
    }
}", "Class TestClass

    Private Sub TestMethod()
        For i = [end] To 0 + 1 Step -1
            b(i) = s(i)
        Next
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBForWithDateTimeVariables()
            TestConversionCSharpToVisualBasic(
"class TestClass {
    void TestMethod() {
        int summary = 0;
        for (int month = 1; month <= 12; month++) {
            summary += month;
        }
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim summary As Integer = 0
        For month As Integer = 1 To 12
            summary += month
        Next
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBForWithSingleStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        for (i = 0; i < end; i++) b[i] = s[i];
    }
}", "Class TestClass

    Private Sub TestMethod()
        For i = 0 To [end] - 1
            b(i) = s(i)
        Next
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBForWithUnknownConditionAndBlock()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        for (int i = 0; unknownCondition; i++) { // Increment moves to bottom of loop
            b[i] = s[i];
        }
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim i As Integer = 0
        While unknownCondition
            b(i) = s(i)
            i += 1
        End While
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBForWithUnknownConditionAndSingleStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        for (i = 0; unknownCondition; i++)
            b[i] = s[i];
    }
}", "Class TestClass

    Private Sub TestMethod()
        i = 0
        While unknownCondition
            b(i) = s(i)
            i += 1
        End While
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBIfStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod (int a)
    {
        int b;
        if (a == 0) {
            b = 0;
        } else if (a == 1) {
            b = 1;
        } else if (a == 2 || a == 3) {
            b = 2;
        } else {
            b = 3;
        }
    }
}", "Class TestClass

    Private Sub TestMethod(a As Integer)
        Dim b As Integer
        If a = 0 Then
            b = 0
        ElseIf a = 1 Then
            b = 1
        ElseIf a = 2 OrElse a = 3 Then
            b = 2
        Else
            b = 3
        End If
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBIfStatementWithoutBlock()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod (int a)
    {
        int b;
        if (a == 0)
            b = 0;
        else
            b = 3;
    }
}", "Class TestClass

    Private Sub TestMethod(a As Integer)
        Dim b As Integer
        If a = 0 Then
            b = 0
        Else
            b = 3
        End If
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBJaggedArrayDeclarationStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int[][] b;
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer()()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBJaggedArrayInitializationStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int[][] b = { new int[] { 1, 2 }, new int[] { 3, 4 } };
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer()() = {New Integer() {1, 2}, New Integer() {3, 4}}
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBJaggedArrayInitializationStatementWithLength()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int[][] b = new int[2][] { new int[] { 1, 2 }, new int[] { 3, 4 } };
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer()() = New Integer(1)() {New Integer() {1, 2}, New Integer() {3, 4}}
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBJaggedArrayInitializationStatementWithType()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int[][] b = new int[][] { new int[] { 1, 2 }, new int[] { 3, 4 } };
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer()() = New Integer()() {New Integer() {1, 2}, New Integer() {3, 4}}
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBLabeledAndForStatement()
            TestConversionCSharpToVisualBasic("class GotoTest1
{
    static void Main()
    {
        int x = 200, y = 4;
        int count = 0;
        string[,] array = new string[x, y];

        for (int i = 0; i < x; i++)

            for (int j = 0; j < y; j++)
                array[i, j] = (++count).ToString();

        Console.Write(""Enter the number to search for: "");

        string myNumber = Console.ReadLine();

                for (int i = 0; i < x; i++)
                {
                    for (int j = 0; j < y; j++)
                    {
                        if (array[i, j].Equals(myNumber))
                        {
                            goto Found;
                        }
                    }
                }

            Console.WriteLine(""The number {0} was not found."", myNumber);
            goto Finish;

        Found:
            Console.WriteLine(""The number {0} is found."", myNumber);

        Finish:
            Console.WriteLine(""End of search."");

            Console.WriteLine(""Press any key to exit."");
            Console.ReadKey();
        }
    }", "NotInheritable Class GotoTest1

    Public Shared Sub Main()
        Dim x As Integer = 200, y As Integer = 4
        Dim count As Integer = 0
        Dim array As String(,) = New String(x - 1, y - 1) {}

        For i As Integer = 0 To x - 1

            For j As Integer = 0 To y - 1
                array(i, j) = Threading.Interlocked.Increment(count).ToString()
            Next
        Next

        Console.Write(""Enter the number to search for: "")

        Dim myNumber As String = Console.ReadLine()

        For i As Integer = 0 To x - 1
            For j As Integer = 0 To y - 1
                If array(i, j).Equals(myNumber) Then
                    GoTo Found
                End If
            Next
        Next

        Console.WriteLine(""The number {0} was not found."", myNumber)
        GoTo Finish

Found:
        Console.WriteLine(""The number {0} is found."", myNumber)

Finish:
        Console.WriteLine(""End of search."")

        Console.WriteLine(""Press any key to exit."")
        Console.ReadKey()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMultidimensionalArrayDeclarationStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int[,] b;
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer(,)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMultidimensionalArrayInitializationStatementMultiLine()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int[,] b = {
            {1, 2},
            {3, 4}
        };
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer(,) = { _
            {1, 2},
            {3, 4}
        }
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMultidimensionalArrayInitializationStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int[,] b = { { 1, 2 }, { 3, 4 } };
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer(,) = {{1, 2}, {3, 4}}
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMultidimensionalArrayInitializationStatementWithLengthsMultiLine()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int[,] b = new int[2, 2] {
            {1, 2},
            {3, 4}
        }
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer(,) = New Integer(1, 1) { _
            {1, 2},
            {3, 4}
        }
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMultidimensionalArrayInitializationStatementWithLengths()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int[,] b = new int[2, 2] { { 1, 2 }, { 3, 4 } };
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer(,) = New Integer(1, 1) {{1, 2}, {3, 4}}
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMultidimensionalArrayInitializationStatementWithTypeMultiLine()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int[,] b = new int[,] {
            {1, 2},
            {3, 4}
        };
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer(,) = New Integer(,) { _
            {1, 2},
            {3, 4}
        }
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMultidimensionalArrayInitializationStatementWithType()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int[,] b = new int[,] { { 1, 2 }, { 3, 4 } };
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer(,) = New Integer(,) {{1, 2}, {3, 4}}
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBObjectCreationExpressionInInvocationExpression()
            TestConversionCSharpToVisualBasic(
"class TestClass {
    int field;
    TestClass(int param) {
        this.field = param;
    }

    static void TestMethod() {
        new TestClass(10).Initialize();
    }
    void Initialize() { }
}",
"Class TestClass

    Private field As Integer

    Sub New(param As Integer)
        Me.field = param
    End Sub

    Private Shared Sub TestMethod()
        Call New TestClass(10).Initialize()
    End Sub

    Private Sub Initialize()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBObjectInitializationStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        string b;
        b = new string(""test"");
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As String
        b = New String(""test"")
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBObjectInitializationStatementInDeclaration()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        string b = new string(""test"");
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As New String(""test"")
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBObjectInitializationStatementInVarDeclaration()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        var b = new string(""test"");
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As New String(""test"")
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBSelectCase()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod(int number)
    {
        switch (number) {
            case 0:
            case 1:
            case 2:
                Console.Write(""number is 0, 1, 2"");
                break;

            case 3:
                Console.Write(""section 3"");
                goto case 5;
            case 4:
                Console.Write(""section 4"");
                goto default;
            default:
                Console.Write(""default section""); // Block moves to end - 1
                break; // Block moves to end - 2
            case 5:
                Console.Write(""section 5"");
                break;
        }
    }
}", "Class TestClass

    Private Sub TestMethod(number As Integer)
        Select Case number
            Case 0, 1, 2
                Console.Write(""number is 0, 1, 2"")

            Case 3
                Console.Write(""section 3"")
                GoTo _5
            Case 4
                Console.Write(""section 4"")
                GoTo _Select0_CaseDefault
            Case 5
_5:
                Console.Write(""section 5"")
            Case Else
_Select0_CaseDefault:
                Console.Write(""default section"") ' Block moves to end - 1
        End Select
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBSelectCaseWithWhen()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod(int number)
    {
    Object o = ""demo"";

    switch (o)
    {
        case IEnumerable<string> s :
            Console.WriteLine(""1""); break;
        case string s when s.Length > 5 :
            Console.WriteLine(""2""); break;
        case string s :
            Console.WriteLine(""3""); break;
        default:
            Console.WriteLine(""4""); break;
    }
}", "Class TestClass

    Private Sub TestMethod(number As Integer)
        Dim o As Object = ""demo""

        Select Case True
            Case TypeOf o Is IEnumerable(Of String)
                Dim s As IEnumerable(Of String) = CType(o, IEnumerable(Of String))
                Console.WriteLine(""1"")
            Case TypeOf o Is String
                Dim s As String = CType(o, String)
                If s.Length > 5 Then Exit Select
                Console.WriteLine(""2"")
            Case TypeOf o Is String
                Dim s As String = CType(o, String)
                Console.WriteLine(""3"")
            Case Else
                Console.WriteLine(""4"")
        End Select
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBSelectCaseWithDotInCaseLabel()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod(double number)
    {
        switch (number) {
            case 3:
                Console.Write(""section 3"");
                goto case 5.5;
            case 5.5:
                Console.Write(""section 5"");
                break;
        }
    }
}", "Class TestClass

    Private Sub TestMethod(number As Double)
        Select Case number
            Case 3
                Console.Write(""section 3"")
                GoTo _5Dot_5
            Case 5.5
_5Dot_5:
                Console.Write(""section 5"")
        End Select
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBSubWithForEachMultiline()
            TestConversionCSharpToVisualBasic(
"class TestClass {
    void TestMethod(IEnumerable<int> counts) {
        int summary = 0;
        Action action = () => {
            foreach(var c in counts) {
                var current = c;
                summary += current;
            }
        };
    }
}", "Class TestClass

    Private Sub TestMethod(counts As IEnumerable(Of Integer))
        Dim summary As Integer = 0
        Dim action1 As Action = Sub()
                                    For Each c In counts
                                        Dim current = c
                                        summary += current
                                    Next
                                End Sub
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBSyncLockStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod(object nullObject)
    {
        if (nullObject == null)
            throw new ArgumentNullException(nameof(nullObject));
        lock (nullObject) {
            Console.WriteLine(nullObject);
        }
    }
}", "Class TestClass

    Private Sub TestMethod(nullObject As Object)
        If nullObject Is Nothing Then
            Throw New ArgumentNullException(NameOf(nullObject))
        End If

        SyncLock nullObject
            Console.WriteLine(nullObject)
        End SyncLock
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBThrowStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod(object nullObject)
    {
        if (nullObject == null)
            throw new ArgumentNullException(nameof(nullObject));
    }
}", "Class TestClass

    Private Sub TestMethod(nullObject As Object)
        If nullObject Is Nothing Then
            Throw New ArgumentNullException(NameOf(nullObject))
        End If
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTryCatch()
            TestConversionCSharpToVisualBasic("class TestClass
{
    static bool Log(string message)
    {
        Console.WriteLine(message);
        return false;
    }

    void TestMethod(int number)
    {
        try {
            Console.WriteLine(""try"");
        } catch (Exception e) {
            Console.WriteLine(""catch1"");
        } catch {
            Console.WriteLine(""catch all"");
        } finally {
            Console.WriteLine(""finally"");
        }
        try {
            Console.WriteLine(""try"");
        } catch (System.IO.IOException) {
            Console.WriteLine(""catch1"");
        } catch (Exception e) when (Log(e.Message)) {
            Console.WriteLine(""catch2"");
        }
        try {
            Console.WriteLine(""try"");
        } finally {
            Console.WriteLine(""finally"");
        }
    }
}", "Class TestClass

    Private Shared Function Log(message As String) As Boolean
        Console.WriteLine(message)
        Return False
    End Function

    Private Sub TestMethod(number As Integer)
        Try
            Console.WriteLine(""try"")
        Catch e As Exception
            Console.WriteLine(""catch1"")
        Catch
            Console.WriteLine(""catch all"")
        Finally
            Console.WriteLine(""finally"")
        End Try

        Try
            Console.WriteLine(""try"")
        Catch __unusedIOException1__ As IO.IOException
            Console.WriteLine(""catch1"")
        Catch e As Exception When Log(e.Message)
            Console.WriteLine(""catch2"")
        End Try

        Try
            Console.WriteLine(""try"")
        Finally
            Console.WriteLine(""finally"")
        End Try
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBWhileStatement()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int b;
        b = 0;
        while (b == 0)
        {
            if (b == 2)
                continue;
            if (b == 3)
                break;
            b = 1;
        }
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer
        b = 0
        While b = 0
            If b = 2 Then
                Continue While
            End If

            If b = 3 Then
                Exit While
            End If

            b = 1
        End While
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBYield()
            TestConversionCSharpToVisualBasic("using System.Collections.Generic;

class TestClass
{
    IEnumerable<int> TestMethod(int number)
    {
        if (number < 0)
            yield break;
        for (int i = 0; i < number; i++)
            yield return i;
    }
}", "Imports System.Collections.Generic

Class TestClass

    Private Iterator Function TestMethod(number As Integer) As IEnumerable(Of Integer)
        If number < 0 Then
            Return
        End If

        For i As Integer = 0 To number - 1
            Yield i
        Next
    End Function
End Class")
        End Sub

    End Class

End Namespace
