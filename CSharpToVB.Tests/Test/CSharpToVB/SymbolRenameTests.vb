' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports CodeConverter.Tests
Imports Xunit

Namespace CSharpToVB.Tests
    <TestClass()>
    Public Class SymbolRenameTests
        Inherits ConverterTestBase
        <Fact>
        Public Shared Sub CSharpToVBSymbolRename()
            TestConversionCSharpToVisualBasic("public class ClashingNames
{
    private string ab;

    void Test()
    {
        object aB = 5;
        int Ab = 7;
    }

    void Test2(int ab)
    {
        object aB = 5;
        int AB =  6;
        string s = nameof(AB);
        string s2 = nameof(ab);
    }
}", "Option Explicit Off
Option Infer On
Option Strict Off

Public Class ClashingNames

    Private ab As String

    Private Sub Test()
        Dim _aB As Object = 5
        Dim Ab_Renamed As Integer = 7
    End Sub

    Private Sub Test2(ab As Integer)
        Dim _aB As Object = 5
        Dim AB_Renamed As Integer = 6
        Dim s As String = NameOf(AB_Renamed)
        Dim s2 As String = NameOf(ab)
    End Sub
End Class")
        End Sub

    End Class
End Namespace
