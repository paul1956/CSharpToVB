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
}", "Class TestClass

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
}", "Imports System.Runtime.InteropServices

Class TestClass(Of T As {Class, New}, T2 As Structure, T3)

    ' Comment
    Public Sub New(<Out> ByRef argument As T, ByRef argument2 As T2, argument3 As T3)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCustomEvent()
            TestConversionCSharpToVisualBasic("using System; //Not required in VB due to global imports
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
}", "Class TestClass

    Private backingField As EventHandler

    Public Custom Event MyEvent As EventHandler
        AddHandler(Value As EventHandler)
            Me.backingField = [Delegate].Combine(Me.backingField, value)
        End AddHandler

        RemoveHandler(Value As EventHandler)
            Me.backingField = [Delegate].Remove(Me.backingField, value)
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
}", "Class TestClass

    Protected Overrides Sub Finalize()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBEvent()
            TestConversionCSharpToVisualBasic("class TestClass
{
    public event EventHandler MyEvent;
}", "Class TestClass

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
}", "Imports System.Runtime.CompilerServices

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
}", "Imports System.Runtime.CompilerServices

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
}", "Class TestClass

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
}", "Class TestClass

    Default Public Property item(index As Integer) As Integer

    Default Public ReadOnly Property item(index As String) As Integer
        Get
            Return 0
        End Get
    End Property

    Private m_test3 As Integer

    Default Public Property item(index As Double) As Integer
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
}", "Imports System.Runtime.InteropServices

Class TestClass

    Public Sub TestMethod(Of T As {Class, New}, T2 As Structure, T3)(<Out> ByRef argument As T, ByRef argument2 As T2, argument3 As T3)
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
}", "Imports Microsoft.CodeAnalysis

Class TestClass

    Private Function GetConvertMethodForKeywordOrNull(type As SyntaxNode) As Boolean
        Dim convertedType1 As Microsoft.CodeAnalysis.ITypeSymbol = semanticModel.GetTypeInfo(type).ConvertedType
        Dim convertMethodName As Object = Nothing
        Return createConvertMethodsLookupByReturnType.TryGetValue(convertedType1, convertMethodName)
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
}", "Imports System.Runtime.InteropServices

Class TestClass

    Public Function TestMethod(Of T As {Class, New}, T2 As Structure, T3)(<Out> ByRef argument As T, ByRef argument2 As T2, argument3 As T3) As Integer
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
}", "Class TestClass

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
}", "Imports System.Runtime.InteropServices

Class TestClass

    Public NotOverridable Sub TestMethod(Of T As {Class, New}, T2 As Structure, T3)(<Out> ByRef argument As T, ByRef argument2 As T2, argument3 As T3)
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
}", "Imports System.Runtime.InteropServices

NotInheritable Class TestClass

    Public Shared Sub TestMethod(Of T As {Class, New}, T2 As Structure, T3)(<Out> ByRef argument As T, ByRef argument2 As T2, argument3 As T3)
        argument = Nothing
        argument2 = CType(Nothing, T2)
        argument3 = CType(Nothing, T3)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCaseConflictArgumentFieldAndMethodWithOverloads()
            TestConversionCSharpToVisualBasic(
                "public class HasConflictingMethodAndField {

    public int HasConflictingParam(int test) {
        this.teSt = test;
        return test;
    }

    int teSt;

    private int test() {
        return 1;
    }

    public int Test() {
        return test();
    }

    private int test(int arg) {
        return arg;
    }

    public int Test(int arg) {
        return test(arg);
    }
}", "Public Class HasConflictingMethodAndField

    Public Function HasConflictingParam(test As Integer) As Integer
        Me.teSt = test
        Return test
    End Function

    Private teSt As Integer

    Private Function test() As Integer
        Return 1
    End Function

    Public Function Test1() As Integer
        Return test()
    End Function

    Private Function test(arg As Integer) As Integer
        Return arg
    End Function

    Public Function Test1(arg As Integer) As Integer
        Return test(arg)
    End Function
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCaseConflictArgumentFieldAndProperty()
            TestConversionCSharpToVisualBasic(
"public class HasConflictingPropertyAndField {

    public int HasConflictingParam(int test) {
        this.test = test;
        return test;
    }

    int test;

    public int Test {
        get { return test; }
        set { test = value; }
    }
}", "Public Class HasConflictingPropertyAndField

    Public Function HasConflictingParam(test As Integer) As Integer
        Me.test = test
        Return test
    End Function

    Private test As Integer

    Public Property Test1 As Integer
        Get
            Return test
        End Get

        Set(Value As Integer)
            test = value
        End Set
    End Property
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCaseConflictArgumentPropertyAndField()
            TestConversionCSharpToVisualBasic(
"public class HasConflictingPropertyAndField {
    int test;
    public int Test {
        get { return test; }
        set { test = value; }
    }
    public int HasConflictingParam(int test) {
        Test = test;
        return test;
    }
}", "Public Class HasConflictingPropertyAndField

    Private test As Integer

    Public Property Test1 As Integer
        Get
            Return test
        End Get

        Set(Value As Integer)
            test = value
        End Set
    End Property

    Public Function HasConflictingParam(test As Integer) As Integer
        Test1 = test
        Return test
    End Function
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCaseConflictPartialClassArgumentFieldPropertyAndLocalInBothParts()
            TestConversionCSharpToVisualBasic(
"public partial class HasConflictingPropertyAndField {
    public int HasConflictingParam(int test) {
        int TEST = 0;
        this.test = test + TEST;
        return test;
    }
}

public partial class HasConflictingPropertyAndField {
    int test;
    public int Test {
        get
        {
            int TEST = 0;
            return test + TEST;
        }
        set { test = value; }
    }
}", "Public Partial Class HasConflictingPropertyAndField

    Public Function HasConflictingParam(test As Integer) As Integer
        Dim TEST1 As Integer = 0
        Me.test = test + TEST1
        Return test
    End Function
End Class

Public Partial Class HasConflictingPropertyAndField

    Private test As Integer

    Public Property Test1 As Integer
        Get
            Dim TEST2 As Integer = 0
            Return test + TEST2
        End Get

        Set(Value As Integer)
            test = value
        End Set
    End Property
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBCaseConflictPropertyAndFieldEnsureOtherClassNotAffected()
            TestConversionCSharpToVisualBasic(
"public class HasConflictingPropertyAndField {
    int test;
    public int Test {
        get { return test; }
        set { test = value; }
    }
}
public class ShouldNotChange {
    int test;
    public int Test1 {
        get { return test; }
        set { test = value; }
    }
}
", "Public Class HasConflictingPropertyAndField

    Private test As Integer

    Public Property Test1 As Integer
        Get
            Return test
        End Get

        Set(Value As Integer)
            test = value
        End Set
    End Property
End Class

Public Class ShouldNotChange

    Private test As Integer

    Public Property Test1 As Integer
        Get
            Return test
        End Get

        Set(Value As Integer)
            test = value
        End Set
    End Property
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBDoNotSimplifyArrayTypeInFieldDeclarations()
            TestConversionCSharpToVisualBasic(
"class TestClass {
    int[] answer = { 1, 2 };
}",
"Class TestClass

    Private answer As Integer() = {1, 2}
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBIndexerBadCase()
            TestConversionCSharpToVisualBasic(
"class TestClass {
    public object this[int index] {
        get { }
        set { }
    }
}", "Class TestClass

    Default Public Property item(index As Integer) As Object
        Get
        End Get

        Set(Value As Object)
        End Set
    End Property
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBIndexerEmptySetter()
            TestConversionCSharpToVisualBasic(
"using System.Collections

class TestClass : IList {

    public object this[int index] {
        get { return index; }
        set { }
    }
}", "Imports System.Collections

Class TestClass
    Implements IList

    Default Public Property item(index As Integer) As Object
    Implements Collections.IList.item
    Get
    Return index
    End Get

    Set(Value As Object)
    End Set
    End Property
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBInterfaceGet()
            TestConversionCSharpToVisualBasic(
"public interface IParametersProvider {
    IEnumerable<object> Parameters { get; }
}", "Public Interface IParametersProvider

    ReadOnly Property Parameters As IEnumerable(Of Object)

End Interface")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBInterfaceIndexer()
            TestConversionCSharpToVisualBasic(
"public interface iDisplay {
    object this[int i] { get; set; }
}", "Public Interface iDisplay

    Default Private Property item(i As Integer) As Object

End Interface")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBInterfaceSet()
            TestConversionCSharpToVisualBasic(
"public interface IParametersProvider {
    IEnumerable<object> Parameters { set; }
}", "Public Interface IParametersProvider

    WriteOnly Property Parameters As IEnumerable(Of Object)

End Interface")
        End Sub

        <Fact(Skip:="Overloads Missing")>
        Public Shared Sub CSharpToVBMethodOverloads()
            TestConversionCSharpToVisualBasic(
"public class MailEmployee {
    public string Email { get; set; }
    protected bool Equals(MailEmployee other) {
        return Email == other.Email;
    }
    public override bool Equals(object obj) {
        return Equals((MailEmployee)obj);
    }
}", "Public Class MailEmployee

    Public Property Email As String

    Protected Overloads Function Equals(other As MailEmployee) As Boolean
        Return Equals(Email, other.Email)
    End Function

    Public Overrides Function Equals(obj As Object) As Boolean
        Return Equals(CType(obj, MailEmployee))
    End Function
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBNameMatchesWithTypeDate()
            TestConversionCSharpToVisualBasic(
"using System; //Not required in VB due to global imports

class TestClass {
    private DateTime date;
}", "Class TestClass

    Private [date] As DateTime
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBOperatorOverloads()
            ' Note a couple map to the same thing in C# so occasionally the result won't compile. The user can manually decide what to do in such scenarios.
            TestConversionCSharpToVisualBasic("public class AcmeClass
{
    public static AcmeClass operator +(int i, AcmeClass ac)
    {
        return ac;
    }
    public static AcmeClass operator +(string s, AcmeClass ac)
    {
        return ac;
    }
    public static AcmeClass operator -(int i, AcmeClass ac)
    {
        return ac;
    }
    public static AcmeClass operator !(AcmeClass ac)
    {
        return ac;
    }
    public static AcmeClass operator *(int i, AcmeClass ac)
    {
        return ac;
    }
    public static AcmeClass operator /(int i, AcmeClass ac)
    {
        return ac;
    }
    public static AcmeClass operator %(string s, AcmeClass ac)
    {
        return ac;
    }
    public static AcmeClass operator <<(AcmeClass ac, int i)
    {
        return ac;
    }
    public static AcmeClass operator >>(AcmeClass ac, int i)
    {
        return ac;
    }
    public static AcmeClass operator ==(string s, AcmeClass ac)
    {
        return ac;
    }
    public static AcmeClass operator !=(string s, AcmeClass ac)
    {
        return ac;
    }
    public static AcmeClass operator <(string s, AcmeClass ac)
    {
        return ac;
    }
    public static AcmeClass operator >(string s, AcmeClass ac)
    {
        return ac;
    }
    public static AcmeClass operator <=(string s, AcmeClass ac)
    {
        return ac;
    }
    public static AcmeClass operator >=(string s, AcmeClass ac)
    {
        return ac;
    }
    public static AcmeClass operator &(string s, AcmeClass ac)
    {
        return ac;
    }
    public static AcmeClass operator |(string s, AcmeClass ac)
    {
        return ac;
    }
}", "Public Class AcmeClass

    Public Shared Operator +(i As Integer, ac As AcmeClass) As AcmeClass
        Return ac
    End Operator

    Public Shared Operator &(s As String, ac As AcmeClass) As AcmeClass
        Return ac
    End Operator

    Public Shared Operator -(i As Integer, ac As AcmeClass) As AcmeClass
        Return ac
    End Operator

    Public Shared Operator Not(ac As AcmeClass) As AcmeClass
        Return ac
    End Operator

    Public Shared Operator *(i As Integer, ac As AcmeClass) As AcmeClass
        Return ac
    End Operator

    Public Shared Operator /(i As Integer, ac As AcmeClass) As AcmeClass
        Return ac
    End Operator

    Public Shared Operator Mod(s As String, ac As AcmeClass) As AcmeClass
        Return ac
    End Operator

    Public Shared Operator <<(ac As AcmeClass, i As Integer) As AcmeClass
        Return ac
    End Operator

    Public Shared Operator >>(ac As AcmeClass, i As Integer) As AcmeClass
        Return ac
    End Operator

    Public Shared Operator =(s As String, ac As AcmeClass) As AcmeClass
        Return ac
    End Operator

    Public Shared Operator <>(s As String, ac As AcmeClass) As AcmeClass
        Return ac
    End Operator

    Public Shared Operator <(s As String, ac As AcmeClass) As AcmeClass
        Return ac
    End Operator

    Public Shared Operator >(s As String, ac As AcmeClass) As AcmeClass
        Return ac
    End Operator

    Public Shared Operator <=(s As String, ac As AcmeClass) As AcmeClass
        Return ac
    End Operator

    Public Shared Operator >=(s As String, ac As AcmeClass) As AcmeClass
        Return ac
    End Operator

    Public Shared Operator And(s As String, ac As AcmeClass) As AcmeClass
        Return ac
    End Operator

    Public Shared Operator Or(s As String, ac As AcmeClass) As AcmeClass
        Return ac
    End Operator
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBParameterWithNamespaceTest()
            TestConversionCSharpToVisualBasic(
"public class TestClass {
    public object TestMethod(System.Type param1, System.Globalization.CultureInfo param2) {
        return null;
    }
}", "Public Class TestClass

    Public Function TestMethod(param1 As Type, param2 As Globalization.CultureInfo) As Object
        Return Nothing
    End Function
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBPartialMethod()
            TestConversionCSharpToVisualBasic(
"public partial class Entities {
    partial void OnContextCreated();
}",
"Public Partial Class Entities

    Private Partial Sub OnContextCreated()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBSubscribeEventInPropertySetter()
            TestConversionCSharpToVisualBasic(
"using System.ComponentModel;

class TestClass {
    OwnerClass owner;

    public OwnerClass Owner {
        get { return owner; }
        set {
            owner = value;
            ((INotifyPropertyChanged)owner).PropertyChanged += OnOwnerChanged;
        }
    }
    void OnOwnerChanged(object sender, PropertyChangedEventArgs args) { }
}
class OwnerClass : INotifyPropertyChanged {
}",
"Imports System.ComponentModel

Class TestClass

    Private owner As OwnerClass

    Public Property Owner1 As OwnerClass
        Get
            Return owner
        End Get

        Set(Value As OwnerClass)
            owner = value
            AddHandler CType(owner, INotifyPropertyChanged).PropertyChanged, AddressOf OnOwnerChanged
        End Set
    End Property

    Private Sub OnOwnerChanged(sender As Object, args As PropertyChangedEventArgs)
    End Sub
End Class

Class OwnerClass
    Implements INotifyPropertyChanged

End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestAbstractMethod()
            TestConversionCSharpToVisualBasic(
                "abstract class TestClass
{
    public abstract void TestMethod();
}", "MustInherit Class TestClass

    Public MustOverride Sub TestMethod()
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestConstructor()
            TestConversionCSharpToVisualBasic(
                "class TestClass<T, T2, T3> where T : class, new where T2 : struct
{
    public TestClass(out T argument, ref T2 argument2, T3 argument3)
    {
    }
}", "Imports System.Runtime.InteropServices

Class TestClass(Of T As {Class, New}, T2 As Structure, T3)

    Public Sub New(<Out> ByRef argument As T, ByRef argument2 As T2, argument3 As T3)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestConstructorCallingBase()
            TestConversionCSharpToVisualBasic(
                "public class MyBaseClass
{
    public MyBaseClass(object o)
    {
    }
}

public sealed class MyClass
 : MyBaseClass
{
     public MyClass(object o)
      : base(o)
    {
    }
}", "Public Class MyBaseClass

    Public Sub New(o As Object)
    End Sub
End Class

Public NotInheritable Class [MyClass]
    Inherits MyBaseClass

    Public Sub New(o As Object)
        MyBase.New(o)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestCustomEvent()
            TestConversionCSharpToVisualBasic(
"using System; //Not required in VB due to global imports

class TestClass {
    EventHandler backingField;

    public event EventHandler MyEvent {
        add {
            this.backingField += value;
        }
        remove {
            this.backingField -= value;
        }
    }
    public void Reset() {
        backingField = null;
    }
}", "Class TestClass

    Private backingField As EventHandler

    Public Custom Event MyEvent As EventHandler
        AddHandler(Value As EventHandler)
            Me.backingField = [Delegate].Combine(Me.backingField, value)
        End AddHandler

        RemoveHandler(Value As EventHandler)
            Me.backingField = [Delegate].Remove(Me.backingField, value)
        End RemoveHandler
    End Event

    Public Sub Reset()
        backingField = Nothing
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestCustomEventTrivialExpression()
            TestConversionCSharpToVisualBasic(
"using System; //Not required in VB due to global imports
class TestClass {
    EventHandler _backingField;

    public event EventHandler MyEvent {
        add { _backingField += value; }
        remove { _backingField -= value; }
    }
}", "Class TestClass

    Private _backingField As EventHandler

    Public Custom Event MyEvent As EventHandler
        AddHandler(Value As EventHandler)
            _backingField = [Delegate].Combine(_backingField, value)
        End AddHandler

        RemoveHandler(Value As EventHandler)
            _backingField = [Delegate].Remove(_backingField, value)
        End RemoveHandler
    End Event
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestCustomEventUsingFieldEvent()
            TestConversionCSharpToVisualBasic(
"using System; //Not required in VB due to global imports

class TestClass {
    event EventHandler backingField;

    public event EventHandler MyEvent {
        add { backingField += value; }
        remove { backingField -= value; }
    }
}",
"Class TestClass

    Private Event backingField As EventHandler

    Public Custom Event MyEvent As EventHandler
        AddHandler(Value As EventHandler)
            AddHandler backingField, value
        End AddHandler

        RemoveHandler(Value As EventHandler)
            RemoveHandler backingField, value
        End RemoveHandler
    End Event
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestDestructor()
            TestConversionCSharpToVisualBasic(
                "class TestClass
{
    ~TestClass()
    {
    }
}", "Class TestClass

    Protected Overrides Sub Finalize()
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestEvent()
            TestConversionCSharpToVisualBasic(
                "class TestClass
{
    public event EventHandler MyEvent;
}", "Class TestClass

    Public Event MyEvent As EventHandler
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestExtensionMethod()
            TestConversionCSharpToVisualBasic(
"using System; //Not required in VB due to global imports
static class TestClass
{
    public static void TestMethod(this String str)
    {
    }

    public static void TestMethod2Parameters(this String str, Action<string> _)
    {
    }
}", "Imports System.Runtime.CompilerServices

Module TestClass

    <Extension()>
    Public Sub TestMethod(str As String)
    End Sub
    <Extension()>
    Public Sub TestMethod2Parameters(str As String, __ As Action(Of String))
    End Sub

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestExtensionMethodWithExistingImport()
            TestConversionCSharpToVisualBasic(
"using System; //Not required in VB due to global imports
using System.Runtime.CompilerServices;

static class TestClass
{
    public static void TestMethod(this String str)
    {
    }
}", "Imports System.Runtime.CompilerServices

Module TestClass

    <Extension()>
    Public Sub TestMethod(str As String)
    End Sub

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestExternDllImport()
            TestConversionCSharpToVisualBasic(
                "static class TestClass
{
[DllImport(""kernel32.dll"", SetLastError = true)]
static extern IntPtr OpenProcess(AccessMask dwDesiredAccess, bool bInheritHandle, uint dwProcessId);
                }",
                "Module TestClass

    <DllImport(""kernel32.dll"", SetLastError:=True)>
    Private Function OpenProcess(dwDesiredAccess As AccessMask, bInheritHandle As Boolean, dwProcessId As UInteger) As IntPtr
    End Function

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestField()
            TestConversionCSharpToVisualBasic(
                "class TestClass
{
    const int answer = 42;
    int value = 10;
    readonly int v = 15;
}", "Class TestClass

    Const answer As Integer = 42

    Private value As Integer = 10

    ReadOnly v As Integer = 15
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestIndexer()
            TestConversionCSharpToVisualBasic(
                "class TestClass
{
    private int[] _Items;

    public int this[int index]
    {
        get
        {
            return _Items[index];
        }
        set
        {
            _Items[index] = value;
        }
    }

    public int this[string index]
    {
        get
        {
            return 0;
        }
    }

    private int m_test3;

    public int this[double index]
    {
        get
        {
            return this.m_test3;
        }
        set
        {
            this.m_test3 = value;
        }
    }
}", "Class TestClass

    Private _Items As Integer()

    Default Public Property item(index As Integer) As Integer
        Get
            Return _Items(index)
        End Get

        Set(Value As Integer)
            _Items(index) = value
        End Set
    End Property

    Default Public ReadOnly Property item(index As String) As Integer
        Get
            Return 0
        End Get
    End Property

    Private m_test3 As Integer

    Default Public Property item(index As Double) As Integer
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
        Public Shared Sub CSharpToVBTestInferredPropertyInnerClass()
            TestConversionCSharpToVisualBasic(
"class TestClass {
    class InnerClass {
        public string Text { get; private set; }
    }
}",
"Class TestClass

    Class InnerClass

        Private _text As String

        Public Property Text As String
            Get
                Return _text
            End Get

            Private Set(Value As String)
                _text = Value
            End Set
        End Property
    End Class
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestMethodWithComments()
            TestConversionCSharpToVisualBasic(
                "class TestClass
{
    public void TestMethod<T, T2, T3>(out T argument, ref T2 argument2, T3 argument3) where T : class, new where T2 : struct
    {
        argument = null; //1
        argument2 = default(T2); //2
        argument3 = default(T3); //3
    }
}", "Imports System.Runtime.InteropServices

Class TestClass

    Public Sub TestMethod(Of T As {Class, New}, T2 As Structure, T3)(<Out> ByRef argument As T, ByRef argument2 As T2, argument3 As T3)
        argument = Nothing '1
        argument2 = CType(Nothing, T2) '2
        argument3 = CType(Nothing, T3) '3
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestMethodWithReturnType()
            TestConversionCSharpToVisualBasic(
                "class TestClass
{
    public int TestMethod<T, T2, T3>(out T argument, ref T2 argument2, T3 argument3) where T : class, new where T2 : struct
    {
        return 0;
    }
}", "Imports System.Runtime.InteropServices

Class TestClass

    Public Function TestMethod(Of T As {Class, New}, T2 As Structure, T3)(<Out> ByRef argument As T, ByRef argument2 As T2, argument3 As T3) As Integer
        Return 0
    End Function
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestNewMethodIsOverloadsNotShadows()
            TestConversionCSharpToVisualBasic(
                "class TestClass
{
    public void TestMethod()
    {
    }

    public void TestMethod(int i)
    {
    }
}

class TestSubclass : TestClass
{
    public new void TestMethod()
    {
        TestMethod(3);
        System.Console.WriteLine(""Shadowed implementation"");
    }
}", "Class TestClass

    Public Sub TestMethod()
    End Sub

    Public Sub TestMethod(i As Integer)
    End Sub
End Class

Class TestSubclass
    Inherits TestClass

    Public Shadows Sub TestMethod()
        TestMethod(3)
        Console.WriteLine(""Shadowed implementation"")
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestOmmittedAccessorsReplacedWithExpressionBody()
            TestConversionCSharpToVisualBasic(
                "class MyFavColor
{
    private string[] favColor => new string[] {""Red"", ""Green""};
    public string this[int index] => favColor[index];
}
", "Class MyFavColor

    Private ReadOnly Property favColor As String()
        Get
            Return New String() {""Red"", ""Green""}
        End Get
    End Property

    Default Public ReadOnly Property item(index As Integer) As String
        Get
            Return favColor(index)
        End Get
    End Property
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestProperty()
            TestConversionCSharpToVisualBasic(
                "class TestClass
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
}", "Class TestClass

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
        Public Shared Sub CSharpToVBTestPropertyStaticInferred()
            TestConversionCSharpToVisualBasic(
"class TestClass {
    public static string Text { get; private set; };
    public int Count { get; private set; };
}", "Class TestClass

    Private _text As String

    Public Shared Property Text As String
        Get
            Return _text
        End Get

        Private Set(Value As String)
            _text = Value
        End Set
    End Property

    Private _count As Integer

    Public Property Count As Integer
        Get
            Return _count
        End Get

        Private Set(Value As Integer)
            _count = Value
        End Set
    End Property
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestPropertyStaticInferredInModule()
            TestConversionCSharpToVisualBasic(
"static class TestClass {
    public static string Text { get; private set; }
}", "Module TestClass

    Private _text As String
    Public Property Text As String
        Get
            Return _text
        End Get

        Private Set(Value As String)
            _text = Value
        End Set
    End Property

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestPropertyWithAttribute()
            TestConversionCSharpToVisualBasic(
                "class TestClass
{
    [DatabaseGenerated(DatabaseGeneratedOption.None)]
    int value { get; set; }
}", "Class TestClass

    <DatabaseGenerated(DatabaseGeneratedOption.None)>
    Property value As Integer
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestPropertyWithExpressionBody()
            TestConversionCSharpToVisualBasic(
                "public class ConversionResult
{
    private string _sourcePathOrNull;

    public string SourcePathOrNull {
        get => _sourcePathOrNull;
        set => _sourcePathOrNull = string.IsNullOrWhiteSpace(value) ? null : value;
    }
}", "Public Class ConversionResult

    Private _sourcePathOrNull As String

    Public Property SourcePathOrNull As String
        Get
            Return _sourcePathOrNull
        End Get

        Set(Value As String)
            _sourcePathOrNull = If(String.IsNullOrWhiteSpace(value), Nothing, value)
        End Set
    End Property
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestPropertyWithExpressionBodyThatCanBeStatement()
            TestConversionCSharpToVisualBasic(
                "using System; //Not required in VB due to global imports

public class ConversionResult
{
    private int _num;

    public string Num {
        set => _num++;
    }

    public string Blanket {
        set => throw new Exception();
    }
}", "Public Class ConversionResult

    Private _num As Integer

    Public WriteOnly Property Num As String
        Set(Value As String)
            Call Math.Min(Threading.Interlocked.Increment(_num), _num - 1)
        End Set
    End Property

    Public WriteOnly Property Blanket As String
        Set(Value As String)
            Throw New Exception
        End Set
    End Property
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestPropertyWithModifier()
            TestConversionCSharpToVisualBasic(
"class TestClass {
    public string Text { get; private set; }
}", "Class TestClass

    Private _text As String

    Public Property Text As String
        Get
            Return _text
        End Get

        Private Set(Value As String)
            _text = Value
        End Set
    End Property
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestSealedMethod()
            TestConversionCSharpToVisualBasic(
                "class TestClass
{
    public sealed void TestMethod<T, T2, T3>(out T argument, ref T2 argument2, T3 argument3) where T : class, new where T2 : struct
    {
        argument = null;
        argument2 = default(T2);
        argument3 = default(T3);
    }
}", "Imports System.Runtime.InteropServices

Class TestClass

    Public NotOverridable Sub TestMethod(Of T As {Class, New}, T2 As Structure, T3)(<Out> ByRef argument As T, ByRef argument2 As T2, argument3 As T3)
        argument = Nothing
        argument2 = CType(Nothing, T2)
        argument3 = CType(Nothing, T3)
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestStaticConstructor()
            TestConversionCSharpToVisualBasic(
                "static SurroundingClass()
{
}", "Shared Sub New()
End Sub")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTestStaticMethod()
            TestConversionCSharpToVisualBasic(
                "class TestClass
{
    public static void TestMethod<T, T2, T3>(out T argument, ref T2 argument2, T3 argument3) where T : class, new where T2 : struct
    {
        argument = null;
        argument2 = default(T2);
        argument3 = default(T3);
    }
}", "Imports System.Runtime.InteropServices

NotInheritable Class TestClass

    Public Shared Sub TestMethod(Of T As {Class, New}, T2 As Structure, T3)(<Out> ByRef argument As T, ByRef argument2 As T2, argument3 As T3)
        argument = Nothing
        argument2 = CType(Nothing, T2)
        argument3 = CType(Nothing, T3)
    End Sub
End Class")
        End Sub

    End Class

End Namespace
