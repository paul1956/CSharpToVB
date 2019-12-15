﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.IO

Public Class ColorSelector
    Private Shared ReadOnly s_fullPath As String = Path.Combine(FileIO.SpecialDirectories.MyDocuments, "ColorDictionary.csv")

    Private Shared ReadOnly s_colorMappingDictionary As New Dictionary(Of String, Color) From {
         {"class name", Color.FromArgb(0, 128, 128)},
         {"comment", Color.FromArgb(0, 100, 0)},
         {"constant name", Color.Black},
         {"default", Color.Black},
         {"delegate name", Color.FromArgb(0, 128, 128)},
         {"enum name", Color.FromArgb(0, 128, 128)},
         {"enum member name", Color.FromArgb(0, 128, 128)},
         {"error", Color.Red},
         {"excluded code", Color.FromArgb(128, 128, 128)},
         {"event name", Color.Black},
         {"extension method name", Color.Black},
         {"field name", Color.Black},
         {"identifier", Color.Black},
         {"interface name", Color.FromArgb(0, 128, 128)},
         {"keyword", Color.FromArgb(0, 0, 255)},
         {"keyword - control", Color.FromArgb(143, 8, 196)},
         {"label name", Color.Black},
         {"local name", Color.Black},
         {"method name", Color.Black},
         {"module name", Color.FromArgb(0, 128, 128)},
         {"namespace name", Color.Black},
         {"number", Color.Black},
         {"operator", Color.Black},
         {"operator - overloaded", Color.Black},
         {"parameter name", Color.Black},
         {"preprocessor keyword", Color.Gray},
         {"preprocessor text", Color.Black},
         {"property name", Color.Black},
         {"punctuation", Color.Black},
         {"static symbol", Color.Black},
         {"string - escape character", Color.Yellow},
         {"string - verbatim", Color.FromArgb(128, 0, 0)},
         {"string", Color.FromArgb(163, 21, 21)},
         {"struct name", Color.FromArgb(43, 145, 175)},
         {"text", Color.Black},
         {"type parameter name", Color.DarkGray},
         {"xml doc comment - attribute name", Color.FromArgb(128, 128, 128)},
         {"xml doc comment - attribute quotes", Color.FromArgb(128, 128, 128)},
         {"xml doc comment - attribute value", Color.FromArgb(128, 128, 128)},
         {"xml doc comment - cdata section", Color.FromArgb(128, 128, 128)},
         {"xml doc comment - comment", Color.FromArgb(128, 128, 128)},
         {"xml doc comment - delimiter", Color.FromArgb(128, 128, 128)},
         {"xml doc comment - entity reference", Color.FromArgb(0, 128, 0)},
         {"xml doc comment - name", Color.FromArgb(128, 128, 128)},
         {"xml doc comment - processing instruction", Color.FromArgb(128, 128, 128)},
         {"xml doc comment - text", Color.FromArgb(0, 128, 0)},
         {"xml literal - attribute name", Color.FromArgb(128, 128, 128)},
         {"xml literal - attribute quotes", Color.FromArgb(128, 128, 128)},
         {"xml literal - attribute value", Color.FromArgb(128, 128, 128)},
         {"xml literal - cdata section", Color.FromArgb(128, 128, 128)},
         {"xml literal - comment", Color.FromArgb(128, 128, 128)},
         {"xml literal - delimiter", Color.FromArgb(100, 100, 185)},
         {"xml literal - embedded expression", Color.FromArgb(128, 128, 128)},
         {"xml literal - entity reference", Color.FromArgb(185, 100, 100)},
         {"xml literal - name", Color.FromArgb(132, 70, 70)},
         {"xml literal - processing instruction", Color.FromArgb(128, 128, 128)},
         {"xml literal - text", Color.FromArgb(85, 85, 85)}
     }

    Sub New()
        UpdateColorDictionaryFromFile(s_fullPath)
    End Sub

    Sub New(filePath As String)
        UpdateColorDictionaryFromFile(filePath)
    End Sub

    Public Shared Sub SetColor(name As String, value As Color)
        s_colorMappingDictionary(name) = value
        WriteColorDictionaryToFile(s_fullPath)
    End Sub

    Friend Shared Function GetColorFromName(Name As String) As Color
        Try
            If String.IsNullOrWhiteSpace(Name) Then
                Return s_colorMappingDictionary("default")
            End If
            Return s_colorMappingDictionary(Name)
        Catch ex As Exception
            Debug.Print($"GetColorFromName missing({Name})")
            Stop
            Return s_colorMappingDictionary("error")
        End Try
    End Function

    Public Shared Function GetColorNameList() As Dictionary(Of String, Color).KeyCollection
        Return s_colorMappingDictionary.Keys
    End Function

    Public Shared Sub WriteColorDictionaryToFile()
        WriteColorDictionaryToFile(s_fullPath)
    End Sub

    Public Shared Sub WriteColorDictionaryToFile(FPath As String)
        Dim FileStream As FileStream = File.OpenWrite(FPath)
        Dim sw As New StreamWriter(FileStream)
        sw.WriteLine($"Key,R,G,B")
        For Each kvp As KeyValuePair(Of String, Color) In s_colorMappingDictionary
            sw.WriteLine($"{kvp.Key},{kvp.Value.R},{kvp.Value.G},{kvp.Value.B}")
        Next
        sw.Flush()
        sw.Close()
        FileStream.Close()
    End Sub

    Public Shared Sub UpdateColorDictionaryFromFile(FPath As String)
        If Not File.Exists(FPath) Then
            WriteColorDictionaryToFile(FPath)
            Exit Sub
        End If
        Dim FileStream As FileStream = File.OpenRead(FPath)
        Dim sr As New StreamReader(FileStream)
        sr.ReadLine()
        While (sr.Peek() <> -1)
            Dim line As String = sr.ReadLine()
            Dim Split() As String = line.Split(","c)
            Dim key As String = Split(0)
            Dim R As Integer = Convert.ToInt32(Split(1), Globalization.CultureInfo.InvariantCulture)
            Dim G As Integer = Convert.ToInt32(Split(2), Globalization.CultureInfo.InvariantCulture)
            Dim B As Integer = Convert.ToInt32(Split(3), Globalization.CultureInfo.InvariantCulture)
            s_colorMappingDictionary(key) = Color.FromArgb(R, G, B)
        End While
        sr.Close()
        FileStream.Close()
    End Sub

End Class
