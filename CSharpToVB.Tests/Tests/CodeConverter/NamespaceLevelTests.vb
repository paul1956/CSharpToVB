' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Helpers
Imports Xunit

Namespace Tests.CodeConverter

    <TestClass()> Public Class NamespaceLevelTests
        Inherits ConverterTestBase

        <Fact>
        Public Shared Sub CSharpToVbAbstractClass()
            TestConversionCSharpToVisualBasic("namespace Test.@class
{
    abstract class TestClass
    {
    }
}", "Namespace Test.[class]

    MustInherit Class TestClass
    End Class
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbClass()
            TestConversionCSharpToVisualBasic("namespace Test.@class
{
    class TestClass<T>
    {
    }
}", "Namespace Test.[class]

    Class TestClass(Of T)
    End Class
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbClassExplicitlyImplementsInterface()
            TestConversionCSharpToVisualBasic(
"public class ToBeDisplayed : iDisplay
{
    string iDisplay.Name { get; set; }

    private void iDisplay.DisplayName()
    {
    }
}
public interface iDisplay
{
    string Name { get; set; }
    void DisplayName();
}", "Public Class ToBeDisplayed
    Implements iDisplay

    Property iDisplay_Name As String Implements iDisplay.Name

    Private Sub DisplayName()
        Implements iDisplay.DisplayName
    End Sub
End Class

Public Interface iDisplay

    Property Name As String
    Sub DisplayName()

End Interface")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbClassExplicitlyImplementsInterfaceIndexer()
            TestConversionCSharpToVisualBasic(
"public class ToBeDisplayed : iDisplay {
    object iDisplay.this[int i] {
        get { throw new System.NotImplementedException(); }
        set { throw new System.NotImplementedException(); }
    }
}
public interface iDisplay {
    object this[int i] { get; set; }
}", "Public Class ToBeDisplayed
    Implements iDisplay

    Default Private Property item(i As Integer) As Object
    Implements iDisplay.item
    Get
    Throw New NotImplementedException
    End Get

    Set(Value As Object)
    Throw New NotImplementedException
    End Set
    End Property
End Class

Public Interface iDisplay

    Default Private Property item(i As Integer) As Object

End Interface")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbClassImplementsInterface()
            TestConversionCSharpToVisualBasic("using System; //Not required in VB due to global imports
class test : IComparable { }", "Class test
    Implements IComparable

End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbClassImplementsInterface2()
            TestConversionCSharpToVisualBasic("class test : System.IComparable { }",
                                              "Class test
    Implements IComparable

End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbClassInheritanceList()
            TestConversionCSharpToVisualBasic("abstract class ClassA : System.IDisposable
{
    protected abstract void Test();
}", "MustInherit Class ClassA
    Implements IDisposable

    Protected MustOverride Sub Test()
End Class")
            TestConversionCSharpToVisualBasic("abstract class ClassA : System.EventArgs, System.IDisposable
{
    protected abstract void Test();
}", "MustInherit Class ClassA
    Inherits EventArgs
    Implements IDisposable

    Protected MustOverride Sub Test()
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbClassInheritsClass()
            TestConversionCSharpToVisualBasic("using System.IO; class ClassInheritsClass : InvalidDataException { }",
                "Imports System.IO

Class ClassInheritsClass
    Inherits InvalidDataException

End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbClassInheritsClass2()
            TestConversionCSharpToVisualBasic("class ClassInheritsClass2 : System.IO.InvalidDataException { }",
                                              "Class ClassInheritsClass2
    Inherits IO.InvalidDataException

End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbDelegate()
            TestConversionCSharpToVisualBasic("public delegate void Test();",
                                              "Public Delegate Sub Test()")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbDelegate2()
            TestConversionCSharpToVisualBasic("public delegate int Test();",
                                              "Public Delegate Function Test() As Integer")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbDelegate3()
            TestConversionCSharpToVisualBasic("public delegate void Test(int x);",
                                              "Public Delegate Sub Test(x As Integer)")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbDelegate4()
            TestConversionCSharpToVisualBasic("public delegate void Test(ref int x);",
                                              "Public Delegate Sub Test(ByRef x As Integer)")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbEnum()
            TestConversionCSharpToVisualBasic("internal enum ExceptionResource
{
    Argument_ImplementIComparable,
    ArgumentOutOfRange_NeedNonNegNum,
    ArgumentOutOfRange_NeedNonNegNumRequired,
    Arg_ArrayPlusOffTooSmall
}", "Friend Enum ExceptionResource
    Argument_ImplementIComparable
    ArgumentOutOfRange_NeedNonNegNum
    ArgumentOutOfRange_NeedNonNegNumRequired
    Arg_ArrayPlusOffTooSmall
End Enum")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbFullQualificationInImplements()
            TestConversionCSharpToVisualBasic(
"public class TestClass : System.ComponentModel.INotifyPropertyChanged {
    public event System.ComponentModel.PropertyChangedEventHandler PropertyChanged;
}",
"Public Class TestClass
    Implements ComponentModel.INotifyPropertyChanged

    Public Event PropertyChanged As ComponentModel.PropertyChangedEventHandler
    Implements ComponentModel.INotifyPropertyChanged.PropertyChanged
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbGlobalImportsStatement()
            TestConversionCSharpToVisualBasic("using MyAlias = global::System.Data.SqlClient;
using SO = global::System.Data.SqlClient.SqlCommandBuilder;

class ThisUri
{
    private MyAlias.SqlCommand s;
    private SO so;
}",
"Imports MyAlias = Global.System.Data.SqlClient
Imports SO = Global.System.Data.SqlClient.SqlCommandBuilder

Class ThisUri

    Private s As MyAlias.SqlCommand

    Private so1 As SO
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbImplementsEvent()
            TestConversionCSharpToVisualBasic(
"using System.ComponentModel;
public class TestClass : INotifyPropertyChanged {
    public event PropertyChangedEventHandler PropertyChanged;
}", "Imports System.ComponentModel

Public Class TestClass
    Implements INotifyPropertyChanged

    Public Event PropertyChanged As PropertyChangedEventHandler
    Implements ComponentModel.INotifyPropertyChanged.PropertyChanged
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbImplementsGenericInterface()
            TestConversionCSharpToVisualBasic(
"public interface ITestInterface<T> {
    void Method(List<T> list);
}
public class TestClass : ITestInterface<string> {
    public void Method(List<string> list) {
    }
}", "Public Interface ITestInterface(Of T)

    Sub Method(list1 As List(Of T))

End Interface

Public Class TestClass
    Implements ITestInterface(Of String)

    Public Sub Method(list1 As List(Of String)) Implements ITestInterface(Of String).Method
    End Sub
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbImports()
            TestConversionCSharpToVisualBasic("using SomeNamespace;
using VB = Microsoft.VisualBasic;", "Imports SomeNamespace
Imports VB = Microsoft.VisualBasic")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbInnerNamespaceMoveImportsStatement()
            TestConversionCSharpToVisualBasic(
"namespace System {
    using Collections; // Moves outside namespace
    public class TestClass {
        public Hashtable Property { get; set; }
    }
}", "Imports Collections ' Moves outside namespace

Namespace System

    Public Class TestClass

        Public Property [Property] As Hashtable
    End Class
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbInterface()
            TestConversionCSharpToVisualBasic("interface ITest : System.IDisposable
{
    void Test ();
}", "Interface ITest
    Inherits IDisposable

    Sub Test()

End Interface")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbInternalStaticClass()
            TestConversionCSharpToVisualBasic("namespace Test.@class
{
    internal static class TestClass
    {
        public static void Test() {}
        static void Test2() {}
    }
}", "Namespace Test.[class]

    Friend Module TestClass

        Public Sub Test()
        End Sub
        Private Sub Test2()
        End Sub

    End Module
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbMoveImportsStatement()
            TestConversionCSharpToVisualBasic("namespace test { using SomeNamespace; }", "Imports SomeNamespace

Namespace test
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbNamedImport()
            TestConversionCSharpToVisualBasic(
                "using s = System.String;

public class X
{
    s GetStr()
    {
        return s.Empty;
    }
}", "Imports s = System.String

Public Class X

    Private Function GetStr() As s
        Return s.Empty
    End Function
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbNamespace()
            TestConversionCSharpToVisualBasic("namespace Test
{

}", "Namespace Test
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbNamespaceDuplicates()
            TestConversionCSharpToVisualBasic(
"using System.Linq;
namespace System {
    using Linq;
}",
"Imports System.Linq

Namespace System
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbNestedStaticClass()
            TestConversionCSharpToVisualBasic(
"public static class Factory {
    static class Generator {
        public static void Initialize() { }
    }
}", "Public Module Factory

    NotInheritable Class Generator

        Public Sub Initialize()
        End Sub
    End Class

End Module")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbPartialClassOmittedModifier()
            TestConversionCSharpToVisualBasic(
"public partial class Entities {
}
partial class Entities {
}",
"Public Partial Class Entities
End Class

Partial Class Entities
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbSealedClass()
            TestConversionCSharpToVisualBasic("namespace Test.@class
{
    sealed class TestClass
    {
    }
}", "Namespace Test.[class]

    NotInheritable Class TestClass
    End Class
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbStaticGenericClass()
            TestConversionCSharpToVisualBasic(
"using System.Threading.Tasks;

public abstract class Class1 {
}

public static class TestClass<T> where T : Class1, new() {
        static Task task;
        static TestClass() {
        }
        public static Task Method() {
            return task;
        }
    }", "Imports System.Threading.Tasks

Public MustInherit Class Class1
End Class

Public NotInheritable Class TestClass(Of T As {Class1, New})

    Shared task1 As Task

    Shared Sub New()
    End Sub

    Public Shared Function Method() As Task
        Return task1
    End Function
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbStruct()
            TestConversionCSharpToVisualBasic("struct MyType : System.IComparable<MyType>
{
    void Test() {}
}", "Structure MyType
    Implements IComparable(Of MyType)

    Private Sub Test()
    End Sub

End Structure")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbTestAbstractClass()
            TestConversionCSharpToVisualBasic(
"namespace Test.@class
{
    public abstract class TestClass
    {
    }
}
namespace Test
{
    public class Test1 : @class.TestClass
    {
    }
}
", "Namespace Test.[class]

    Public MustInherit Class TestClass
    End Class
End Namespace

Namespace Test

    Public Class Test1
        Inherits [class].TestClass

    End Class
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbTestClass()
            TestConversionCSharpToVisualBasic("namespace Test.@class
{
    class TestClass<T>
    {
    }
}", "Namespace Test.[class]

    Class TestClass(Of T)
    End Class
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbTestClassInheritanceList()
            TestConversionCSharpToVisualBasic(
    "abstract class ClassA : System.IDisposable
{
    protected abstract void Test();
}", "MustInherit Class ClassA
    Implements IDisposable

    Protected MustOverride Sub Test()
End Class")

            TestConversionCSharpToVisualBasic(
                "abstract class ClassA : System.EventArgs, System.IDisposable
{
    protected abstract void Test();
}", "MustInherit Class ClassA
    Inherits EventArgs
    Implements IDisposable

    Protected MustOverride Sub Test()
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbTestDelegate()
            TestConversionCSharpToVisualBasic(
                "public delegate void Test();",
                "Public Delegate Sub Test()")
            TestConversionCSharpToVisualBasic(
                "public delegate int Test();",
                "Public Delegate Function Test() As Integer")
            TestConversionCSharpToVisualBasic(
                "public delegate void Test(int x);",
                "Public Delegate Sub Test(x As Integer)")
            TestConversionCSharpToVisualBasic(
                "public delegate void Test(ref int x);",
                "Public Delegate Sub Test(ByRef x As Integer)")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbTestEnum()
            TestConversionCSharpToVisualBasic(
    "internal enum ExceptionResource
{
    Argument_ImplementIComparable,
    ArgumentOutOfRange_NeedNonNegNum,
    ArgumentOutOfRange_NeedNonNegNumRequired,
    Arg_ArrayPlusOffTooSmall
}", "Friend Enum ExceptionResource
    Argument_ImplementIComparable
    ArgumentOutOfRange_NeedNonNegNum
    ArgumentOutOfRange_NeedNonNegNumRequired
    Arg_ArrayPlusOffTooSmall
End Enum")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbTestEnumWithExplicitBaseType()
            TestConversionCSharpToVisualBasic(
    "public enum ExceptionResource : byte
{
    Argument_ImplementIComparable
}", "Public Enum ExceptionResource As Byte
    Argument_ImplementIComparable
End Enum")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbTestInterface()
            TestConversionCSharpToVisualBasic(
                "interface ITest : System.IDisposable
{
    void Test ();
}", "Interface ITest
    Inherits IDisposable

    Sub Test()

End Interface")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbTestInterfaceWithTwoMembers()
            TestConversionCSharpToVisualBasic(
                "interface ITest : System.IDisposable
{
    void Test ();
    void Test2 ();
}", "Interface ITest
    Inherits IDisposable

    Sub Test()
    Sub Test2()

End Interface")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbTestInternalStaticClass()
            TestConversionCSharpToVisualBasic("namespace Test.@class
{
    internal static class TestClass
    {
        public static void Test() {}
        static void Test2() {}
    }
}", "Namespace Test.[class]

    Friend Module TestClass

        Public Sub Test()
        End Sub
        Private Sub Test2()
        End Sub

    End Module
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbTestNamespace()
            TestConversionCSharpToVisualBasic("namespace Test
{

}", "Namespace Test
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbTestSealedClass()
            TestConversionCSharpToVisualBasic("namespace Test.@class
{
    sealed class TestClass
    {
    }
}", "Namespace Test.[class]

    NotInheritable Class TestClass
    End Class
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbTestStruct()
            TestConversionCSharpToVisualBasic(
    "struct MyType : System.IComparable<MyType>
{
    void Test() {}
}", "Structure MyType
    Implements IComparable(Of MyType)

    Private Sub Test()
    End Sub

End Structure")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbTestTopLevelAttribute()
            TestConversionCSharpToVisualBasic(
                "[assembly: CLSCompliant(true)]", "<Assembly: CLSCompliant(True)>")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbTopLevelAttribute()
            TestConversionCSharpToVisualBasic("[assembly: CLSCompliant(true)]",
                                              "<Assembly: CLSCompliant(True)>")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVbVisibilityStaticClass()
            TestConversionCSharpToVisualBasic(
"public static class Factory {
    private const string Name = ""a"";
    internal const string Name1 = ""b"";
    public const string Name2 = ""c"";
    public static void Initialize() { }
    internal static void Initialize1() { }
    private static void Initialize2() { }
}", "Public Module Factory

    Private Const Name As String = ""a""
    Friend Const Name1 As String = ""b""
    Public Const Name2 As String = ""c""
    Public Sub Initialize()
    End Sub
    Friend Sub Initialize1()
    End Sub
    Private Sub Initialize2()
    End Sub

End Module")
        End Sub

    End Class

End Namespace
