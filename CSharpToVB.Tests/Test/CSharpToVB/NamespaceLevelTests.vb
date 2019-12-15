' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports CodeConverter.Tests

Imports Xunit

Namespace CSharpToVB.Tests

    <TestClass()> Public Class NamespaceLevelTests
        Inherits ConverterTestBase

        <Fact>
        Public Shared Sub CSharpToVBAbstractClass()
            TestConversionCSharpToVisualBasic("namespace Test.@class
{
    abstract class TestClass
    {
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Namespace Test.[class]

    MustInherit Class TestClass
    End Class
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBClass()
            TestConversionCSharpToVisualBasic("namespace Test.@class
{
    class TestClass<T>
    {
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Namespace Test.[class]

    Class TestClass(Of T)
    End Class
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBClassImplementsInterface()
            TestConversionCSharpToVisualBasic("using System; class test : IComparable { }",
                                              "Option Explicit Off
Option Infer On
Option Strict Off

Imports System

Class test
    Implements IComparable

End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBClassImplementsInterface2()
            TestConversionCSharpToVisualBasic("class test : System.IComparable { }",
                                              "Option Explicit Off
Option Infer On
Option Strict Off

Class test
    Implements System.IComparable

End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBClassInheritanceList()
            TestConversionCSharpToVisualBasic("abstract class ClassA : System.IDisposable
{
    protected abstract void Test();
}", "Option Explicit Off
Option Infer On
Option Strict Off

MustInherit Class ClassA
    Implements System.IDisposable

    Protected MustOverride Sub Test()
End Class")
            TestConversionCSharpToVisualBasic("abstract class ClassA : System.EventArgs, System.IDisposable
{
    protected abstract void Test();
}", "Option Explicit Off
Option Infer On
Option Strict Off

MustInherit Class ClassA
    Inherits System.EventArgs
    Implements System.IDisposable

    Protected MustOverride Sub Test()
End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBClassInheritsClass()
            TestConversionCSharpToVisualBasic("using System.IO; class test : InvalidDataException { }", "Option Explicit Off
Option Infer On
Option Strict Off

Imports System.IO

Class test
    Inherits InvalidDataException

End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBClassInheritsClass2()
            TestConversionCSharpToVisualBasic("class test : System.IO.InvalidDataException { }", "Option Explicit Off
Option Infer On
Option Strict Off

Class test
    Inherits System.IO.InvalidDataException

End Class")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBDelegate1()
            TestConversionCSharpToVisualBasic("public delegate void Test();",
                                              "Option Explicit Off
Option Infer On
Option Strict Off

Public Delegate Sub Test()")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBDelegate2()
            TestConversionCSharpToVisualBasic("public delegate int Test();",
                                              "Option Explicit Off
Option Infer On
Option Strict Off

Public Delegate Function Test() As Integer")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBDelegate3()
            TestConversionCSharpToVisualBasic("public delegate void Test(int x);",
                                              "Option Explicit Off
Option Infer On
Option Strict Off

Public Delegate Sub Test(x As Integer)")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBDelegate4()
            TestConversionCSharpToVisualBasic("public delegate void Test(ref int x);",
                                              "Option Explicit Off
Option Infer On
Option Strict Off

Public Delegate Sub Test(ByRef x As Integer)")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBEnum()
            TestConversionCSharpToVisualBasic("internal enum ExceptionResource
{
    Argument_ImplementIComparable,
    ArgumentOutOfRange_NeedNonNegNum,
    ArgumentOutOfRange_NeedNonNegNumRequired,
    Arg_ArrayPlusOffTooSmall
}", "Option Explicit Off
Option Infer On
Option Strict Off

Friend Enum ExceptionResource
    Argument_ImplementIComparable
    ArgumentOutOfRange_NeedNonNegNum
    ArgumentOutOfRange_NeedNonNegNumRequired
    Arg_ArrayPlusOffTooSmall
End Enum")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBImports()
            TestConversionCSharpToVisualBasic("using SomeNamespace;
using VB = Microsoft.VisualBasic;", "Option Explicit Off
Option Infer On
Option Strict Off

Imports SomeNamespace
Imports VB = Microsoft.VisualBasic")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBInterface()
            TestConversionCSharpToVisualBasic("interface ITest : System.IDisposable
{
    void Test ();
}", "Option Explicit Off
Option Infer On
Option Strict Off

Interface ITest
    Inherits System.IDisposable

    Sub Test()

End Interface")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBInternalStaticClass()
            TestConversionCSharpToVisualBasic("namespace Test.@class
{
    internal static class TestClass
    {
        public static void Test() {}
        static void Test2() {}
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Namespace Test.[class]

    Friend Module TestClass

        Public Sub Test()
        End Sub
        Private Sub Test2()
        End Sub

    End Module
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBMoveImportsStatement()
            TestConversionCSharpToVisualBasic("namespace test { using SomeNamespace; }",
                                              "Option Explicit Off
Option Infer On
Option Strict Off

Imports SomeNamespace

Namespace test
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBNamespace()
            TestConversionCSharpToVisualBasic("namespace Test
{

}", "Option Explicit Off
Option Infer On
Option Strict Off

Namespace Test
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBSealedClass()
            TestConversionCSharpToVisualBasic("namespace Test.@class
{
    sealed class TestClass
    {
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Namespace Test.[class]

    NotInheritable Class TestClass
    End Class
End Namespace")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBStruct()
            TestConversionCSharpToVisualBasic("struct MyType : System.IComparable<MyType>
{
    void Test() {}
}", "Option Explicit Off
Option Infer On
Option Strict Off

Structure MyType
    Implements System.IComparable(Of MyType)

    Private Sub Test()
    End Sub

End Structure")
        End Sub

        <Fact>
        Public Shared Sub CSharpToVBTopLevelAttribute()
            TestConversionCSharpToVisualBasic("[assembly: CLSCompliant(true)]",
                                              "Option Explicit Off
Option Infer On
Option Strict Off

<Assembly: CLSCompliant(True)>")
        End Sub

    End Class

End Namespace
