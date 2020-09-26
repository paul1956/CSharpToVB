' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports CodeConverter.Tests

Imports Xunit

Namespace CSharpToVB.Tests

    <TestClass()> Public Class SpecialConversionTests
        Inherits ConverterTestBase

        ''' <summary>
        ''' VB's overload resolution is much poorer than C#'s in relation to Func/Action (in C# it was improved to support Linq method chaining, but VB has a more extensive query syntax instead)
        ''' If there are any overloads (including the extension method version of a method vs its non extension method version), VB needs an exact match (with no narrowing conversions).
        ''' This means Funcs/Actions need to be wrapped in a typed constructor such as New Action(Of String)
        ''' </summary>
        <Fact> ' (Skip:="Should use Func")>
        Public Shared Sub CSharpToVBAddressOfWhereVbTypeInferenceIsWeaker()
            TestConversionCSharpToVisualBasic("using System; //Not required in VB due to global imports

static class TestClass
{
    private static object TypeSwitch(this object obj, Func<string, object> matchFunc1, Func<int, object> matchFunc2, Func<object, object> defaultFunc)
    {
        return null;
    }

    private static object ConvertInt(int node)
    {
        return node;
    }

    private static object ConvertString(string node)
    {
        return node;
    }

    public static object Convert(object node)
    {
        return node.TypeSwitch(ConvertString, ConvertInt, _ => throw new NotImplementedException($""Conversion for '{node.GetType()}' not implemented""));
    }
}", "Imports System.Runtime.CompilerServices

Module TestClass

    <Extension()>
    Private Function TypeSwitch(obj As Object, matchFunc1 As Func(Of String, Object), matchFunc2 As Func(Of Integer, Object), defaultFunc As Func(Of Object, Object)) As Object
        Return Nothing
    End Function
    Private Function ConvertInt(node As Integer) As Object
        Return node
    End Function
    Private Function ConvertString(node As String) As Object
        Return node
    End Function
    Public Function Convert(node As Object) As Object
        Return node.TypeSwitch(New Func(Of String, Object)(AddressOf ConvertString), New Func(Of Integer, Object)(AddressOf ConvertInt), Function(__) As Object
                                                                                                                                             Throw New NotImplementedException($""Conversion for '{node.[GetType]()}' not implemented"")
                                                                                                                                         End Function)
    End Function

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCaseConflictFieldAndInterfaceProperty()
            TestConversionCSharpToVisualBasic(
"public interface IInterface {
    int Prop { get; set; }
}
public class TestClass : IInterface {
    int prop;
    int IInterface.Prop {
        get { return prop; }
        set { prop = value;}
    }
}", "Public Interface IInterface

    Property Prop As Integer

End Interface

Public Class TestClass
    Implements IInterface

    Private prop As Integer

    Property IInterface_Prop As Integer Implements IInterface.Prop
        Get
            Return prop
        End Get

        Set(Value As Integer)
            prop = value
        End Set
    End Property
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCaseConflictForeignNamespace()
            TestConversionCSharpToVisualBasic(
"namespace System {
    public class TestClass {
        int test;
        public int Test { get { return test; } }
    }
}", "Namespace System

    Public Class TestClass

        Private test As Integer

        Public ReadOnly Property Test1 As Integer
            Get
                Return test
            End Get
        End Property
    End Class
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCaseConflictLocalWithArgumentMethod()
            TestConversionCSharpToVisualBasic(
"static class TestModule
{
    int Method(object test) {
        int tesT = (int)test;
        return tesT;
    }
}", "Module TestModule

    Private Function Method(test As Object) As Integer
        Dim tesT1 As Integer = CInt(Fix(test))
        Return tesT1
    End Function

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCaseConflictLocalWithLocal()
            TestConversionCSharpToVisualBasic(
"static class TestClass
{
    void Test() {
        object aB = 5;
        int Ab = (int) o;
    }
}", "Module TestClass

    Private Sub Test()
        Dim aB As Object = 5
        Dim Ab1 As Integer = CInt(Fix(o))
    End Sub

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCaseConflictLocalWithLocalInEvent()
            TestConversionCSharpToVisualBasic(
"class TestClass {
    System.EventHandler test;

    public event System.EventHandler Test {
        add {
            object teSt = 5;
            int tesT = (int)o;
            test += value;
        }
        remove {
            object teSt = 5;
            int tesT = (int)o;
            test -= value;
        }
    }
}", "Class TestClass

    Private test As EventHandler

    Public Custom Event Test1 As EventHandler
        AddHandler(Value As EventHandler)
            Dim teSt2 As Object = 5
            Dim tesT3 As Integer = CInt(Fix(o))
            test = [Delegate].Combine(test, value)
        End AddHandler

        RemoveHandler(Value As EventHandler)
            Dim teSt2 As Object = 5
            Dim tesT3 As Integer = CInt(Fix(o))
            test = [Delegate].Remove(test, value)
        End RemoveHandler
    End Event
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCaseConflictLocalWithLocalInMethod()
            TestConversionCSharpToVisualBasic(
"static class TestClass
{
    void Test() {
        object test = 5;
        int tesT = (int) o;
    }
}", "Module TestClass

    Private Sub Test()
        Dim test1 As Object = 5
        Dim tesT2 As Integer = CInt(Fix(o))
    End Sub

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCaseConflictLocalWithLocalInProperty()
            TestConversionCSharpToVisualBasic(
"public int Test {
    get {
        object test = 5;
        int tesT = (int) o;
        return test;
    }
}", "Public ReadOnly Property Test As Integer
    Get
        Dim test1 As Object = 5
        Dim tesT2 As Integer = CInt(Fix(o))
        Return test1
    End Get
End Property")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBConstantsShouldBeQualified()
            TestConversionCSharpToVisualBasic(
"public class TestClass {
    public void Method() {
        string vbLf = ""\n"";
        string vbCrLf = ""\r\n"";
    }
}",
"Public Class TestClass

    Public Sub Method()
        Dim vbLf As String = Microsoft.VisualBasic.vbLf
        Dim vbCrLf As String = Microsoft.VisualBasic.vbNewLine
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBExplicitImplementationsMustNotDifferOnlyByReturnType()
            TestConversionCSharpToVisualBasic(
"using System.Collections;
using System.Collections.Generic;

public class AdditionalLocals : IEnumerable<KeyValuePair<string, int>>
{
    private readonly Stack<Dictionary<string, int>> _additionalLocals = new Stack<Dictionary<string, int>>();

    public IEnumerator<KeyValuePair<string, int>> GetEnumerator()
    {
        return _additionalLocals.Peek().GetEnumerator();
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return _additionalLocals.Peek().GetEnumerator();
    }
}",
"Imports System.Collections
Imports System.Collections.Generic

Public Class AdditionalLocals
    Implements IEnumerable(Of KeyValuePair(Of String, Integer))

    Private ReadOnly _additionalLocals As New Stack(Of Dictionary(Of String, Integer))

    Public Function GetEnumerator() As IEnumerator(Of KeyValuePair(Of String, Integer))
        Implements Collections.Generic.IEnumerable(Of Collections.Generic.KeyValuePair(Of String, Integer)).GetEnumerator
        Return _additionalLocals.Peek().GetEnumerator()
    End Function

    Private Function GetEnumerator1() As IEnumerator
        Implements Collections.IEnumerable.GetEnumerator
        Return _additionalLocals.Peek().GetEnumerator()
    End Function
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBHexAndBinaryLiterals()
            TestConversionCSharpToVisualBasic(
                "class Test
{
    public int CR = 0x0D * 0b1;
}", "Class Test

    Public CR As Integer = &H0D * &B00000000000000000000000000000001
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBIfStatementSimilarToRaiseEvent()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        if (FullImage != null) DrawImage();
    }
}", "Class TestClass

    Private Sub TestMethod()
        If FullImage IsNot Nothing Then
            DrawImage()
        End If
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBIfStatementSimilarToRaiseEvent2()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        if (FullImage != null) e.DrawImage();
    }
}", "Class TestClass

    Private Sub TestMethod()
        If FullImage IsNot Nothing Then
            e.DrawImage()
        End If
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBIfStatementSimilarToRaiseEvent3()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        if (FullImage != null) { DrawImage(); }
    }
}", "Class TestClass

    Private Sub TestMethod()
        If FullImage IsNot Nothing Then
            DrawImage()
        End If
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBIfStatementSimilarToRaiseEvent4()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        if (FullImage != null) { e.DrawImage(); }
    }
}", "Class TestClass

    Private Sub TestMethod()
        If FullImage IsNot Nothing Then
            e.DrawImage()
        End If
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBIfStatementSimilarToRaiseEvent5()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        if (Tiles != null) foreach (Tile t in Tiles) this.TileTray.Controls.Remove(t);
    }
}", "Class TestClass

    Private Sub TestMethod()
        If Tiles IsNot Nothing Then
            For Each t As Tile In Tiles
                Me.TileTray.Controls.Remove(t)
            Next
        End If
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBNonConflictingArgumentEvent()
            TestConversionCSharpToVisualBasic(
"using System; //Not required in VB due to global imports

public class TestClass {
    EventHandler value;
    public event EventHandler Value {
        add { this.value += value; }
        remove { this.value -= value; }
    }
}",
"Public Class TestClass

    Private value As EventHandler

    Public Custom Event Value1 As EventHandler
        AddHandler(Value As EventHandler)
            Me.value = [Delegate].Combine(Me.value, value)
        End AddHandler

        RemoveHandler(Value As EventHandler)
            Me.value = [Delegate].Remove(Me.value, value)
        End RemoveHandler
    End Event
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBRaiseEvent()
            TestConversionCSharpToVisualBasic("class TestClass
{
    event EventHandler MyEvent;

    void TestMethod()
    {
        if (MyEvent != null) MyEvent(this, EventArgs.Empty);
    }
}", "Class TestClass

    Private Event MyEvent As EventHandler

    Private Sub TestMethod()
        RaiseEvent MyEvent(Me, EventArgs.Empty)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBRaiseEvent4()

            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        if (this.MyEvent != null) MyEvent(this, EventArgs.Empty);
    }
}", "Class TestClass

    Private Sub TestMethod()
        If Me.MyEvent IsNot Nothing Then
            MyEvent(Me, EventArgs.Empty)
        End If
    End Sub
End Class
")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBRaiseEvent5()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        if (MyEvent != null) this.MyEvent(this, EventArgs.Empty);
    }
}", "Class TestClass

    Private Sub TestMethod()
        If MyEvent IsNot Nothing Then
            Me.MyEvent(Me, EventArgs.Empty)
        End If
    End Sub
End Class
")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBRaiseEvent6()

            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        if ((this.MyEvent != null)) { this.MyEvent(this, EventArgs.Empty); }
    }
}", "Class TestClass

    Private Sub TestMethod()
        If (Me.MyEvent IsNot Nothing) Then
            Me.MyEvent(Me, EventArgs.Empty)
        End If
    End Sub
End Class
")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBSimpleInlineAssign()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int a, b;
        b = a = 5;
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim a, b As Integer
        b = __InlineAssignHelper(a, 5)
    End Sub

    <Obsolete(""Please refactor code that uses this function, it is a simple work-around to simulate inline assignment in VB!"")>
    Private Shared Function __InlineAssignHelper(Of T)(ByRef target As T, value As T) As T
        target = value
        Return value
    End Function
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBSimplePostIncrementAssign()
            TestConversionCSharpToVisualBasic("class TestClass
{
    void TestMethod()
    {
        int a = 5, b;
        b = a++;
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim b As Integer, a As Integer = 5
        b = Math.Min(Threading.Interlocked.Increment(a), a - 1)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestRaiseEventInElse()
            TestConversionCSharpToVisualBasic(
                "using System; //Not required in VB due to global imports

public class Foo
{
    public event EventHandler<EventArgs> Bar;

    protected void OnBar(EventArgs e)
    {
        if (Bar == null)
            System.Diagnostics.Debug.WriteLine(""No subscriber"");
        else
            Bar.Invoke(this, e);
    }
}", "Public Class Foo

    Public Event Bar As EventHandler(Of EventArgs)

    Protected Sub OnBar(e As EventArgs)
        If Bar Is Nothing Then
            Diagnostics.Debug.WriteLine(""No subscriber"")
        Else
            RaiseEvent Bar(Me, e)
        End If
    End Sub
End Class
")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestRaiseEventInNestedBrackets()
            TestConversionCSharpToVisualBasic(
                "using System; //Not required in VB due to global imports

class TestClass
{
    event EventHandler MyEvent;

    void TestMethod()
    {
        if ((MyEvent != null)) this.MyEvent.Invoke(this, EventArgs.Empty);
    }
}", "Class TestClass

    Private Event MyEvent As EventHandler

    Private Sub TestMethod()
        RaiseEvent MyEvent(Me, EventArgs.Empty)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestRaiseEventOneLiners()
            TestConversionCSharpToVisualBasic(
                "using System; //Not required in VB due to global imports

class TestClass
{
    event EventHandler MyEvent;

    void TestMethod()
    {
        MyEvent(this, EventArgs.Empty);
        if (MyEvent != null) MyEvent(this, EventArgs.Empty);
        MyEvent.Invoke(this, EventArgs.Empty);
        MyEvent?.Invoke(this, EventArgs.Empty);
    }
}", "Class TestClass

    Private Event MyEvent As EventHandler

    Private Sub TestMethod()
        RaiseEvent MyEvent(Me, EventArgs.Empty)
        RaiseEvent MyEvent(Me, EventArgs.Empty)
        RaiseEvent MyEvent(Me, EventArgs.Empty)
        MyEvent?.Invoke(Me, EventArgs.Empty)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestRaiseEventQualified()
            TestConversionCSharpToVisualBasic(
                "using System; //Not required in VB due to global imports

class TestClass
{
    event EventHandler MyEvent;

    void TestMethod()
    {
        if (this.MyEvent != null) this.MyEvent(this, EventArgs.Empty);
    }
}", "Class TestClass

    Private Event MyEvent As EventHandler

    Private Sub TestMethod()
        RaiseEvent MyEvent(Me, EventArgs.Empty)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestRaiseEventQualifiedWithNestedBrackets()
            TestConversionCSharpToVisualBasic(
                "using System; //Not required in VB due to global imports

class TestClass
{
    event EventHandler MyEvent;

    void TestMethod()
    {
        if ((this.MyEvent != null)) { this.MyEvent(this, EventArgs.Empty); }
    }
}", "Class TestClass

    Private Event MyEvent As EventHandler

    Private Sub TestMethod()
        RaiseEvent MyEvent(Me, EventArgs.Empty)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestRaiseEventReversedConditional()
            TestConversionCSharpToVisualBasic(
                "using System; //Not required in VB due to global imports

class TestClass
{
    event EventHandler MyEvent;

    void TestMethod()
    {
        if (null != MyEvent) { MyEvent(this, EventArgs.Empty); }
    }
}", "Class TestClass

    Private Event MyEvent As EventHandler

    Private Sub TestMethod()
        RaiseEvent MyEvent(Me, EventArgs.Empty)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestSimpleInlineAssign()
            TestConversionCSharpToVisualBasic(
                "class TestClass
{
    void TestMethod()
    {
        int a, b;
        b = a = 5;
    }
}", "Class TestClass

    Private Sub TestMethod()
        Dim a, b As Integer
        b = __InlineAssignHelper(a, 5)
    End Sub

    <Obsolete(""Please refactor code that uses this function, it is a simple work-around to simulate inline assignment in VB!"")>
    Private Shared Function __InlineAssignHelper(Of T)(ByRef target As T, value As T) As T
        target = value
        Return value
    End Function
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestSimplePostIncrementAssign()
            TestConversionCSharpToVisualBasic(
"class TestClass{
    void TestMethod()
    {
        int a = 5, b;
        b = a++;
    }
}",
"Class TestClass

    Private Sub TestMethod()
        Dim b As Integer, a As Integer = 5
        b = Math.Min(Threading.Interlocked.Increment(a), a - 1)
    End Sub
End Class")
        End Sub

    End Class

End Namespace
