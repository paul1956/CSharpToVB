' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Runtime.CompilerServices

Partial Public Module ColorSelector

    Public Const DarkModeStr As String = "Dark Mode"

    Public Const LightModeStr As String = "Light Mode"
    Public ReadOnly Property DarkModeDictionaryFileName As String = "DarkModeColorDictionary.csv"
    Public ReadOnly Property LightModeDictionaryFileName As String = "LightModeColorDictionary.csv"
    Friend Property DefaultColor As New ColorDescriptor(Color.Black, Color.White)

    <Extension>
    Private Function AreNotSame(filename As String, filename2 As String) As Boolean
        Dim epsilon As Double = 2.0
        Dim totalSeconds As Double = (File.GetLastWriteTime(filename) - File.GetLastWriteTime(filename2)).TotalSeconds

        If Math.Abs(Math.Round(totalSeconds)) > epsilon Then
            Return True
        End If
        Return False
    End Function

    Private Function MergeColorDictionary(filePath As String, lastWriteTime As Date, colorDictionary As Dictionary(Of String, ColorDescriptor)) As Dictionary(Of String, ColorDescriptor)
        Dim tmpDictionary As New Dictionary(Of String, ColorDescriptor)(StringComparer.OrdinalIgnoreCase)
        LoadColorDictionaryFromFile(filePath, tmpDictionary)
        For Each name As String In colorDictionary.Keys
            If Not tmpDictionary.ContainsKey(name) Then
                tmpDictionary.Add(name, colorDictionary(name))
            End If
        Next
        WriteColorDictionaryToFile(filePath, tmpDictionary)

        File.SetLastWriteTime(filePath, lastWriteTime)
        Return tmpDictionary
    End Function

    Friend Function GetColorFromName(name As String) As ColorDescriptor
        If String.IsNullOrWhiteSpace(name) Then
            Return DefaultColor
        End If
        Dim returnValue As ColorDescriptor = Nothing
        If My.Forms.Form1.CurrentThemeDictionary.TryGetValue(name, returnValue) Then
            Return returnValue
        End If
        Debug.Print($"GetColorFromName missing({name})")
        Return My.Forms.Form1.CurrentThemeDictionary("error")
    End Function

    <Extension>
    Friend Function IsLightMode(text As String) As Boolean
        Return text = LightModeStr
    End Function

    Public Function GetColorNameList() As Dictionary(Of String, ColorDescriptor).KeyCollection
        Return My.Forms.Form1.CurrentThemeDictionary.Keys
    End Function

    Public Sub LoadColorDictionaryFromFile(fPath As String, themeDictionary As Dictionary(Of String, ColorDescriptor))
        If Not File.Exists(fPath) Then
            Exit Sub
        End If
        Dim fileStream As FileStream = File.OpenRead(fPath)
        Dim sr As New StreamReader(fileStream)
        sr.ReadLine()
        While sr.Peek() <> -1
            Dim line As String = sr.ReadLine()
            If Not line.Any Then
                Continue While
            End If
            Dim splitLine() As String = line.Split(","c)
            Dim key As String = splitLine(0)
            If Not themeDictionary.ContainsKey(key) Then
                themeDictionary.Add(key, DefaultColor)
            End If
            Dim foreground As Color = Color.FromArgb(red:=Convert.ToInt32(splitLine(1), Globalization.CultureInfo.InvariantCulture),
                                                                  green:=Convert.ToInt32(splitLine(2), Globalization.CultureInfo.InvariantCulture),
                                                                  blue:=Convert.ToInt32(splitLine(3), Globalization.CultureInfo.InvariantCulture))
            Dim background As Color = Color.FromArgb(red:=Convert.ToInt32(splitLine(4), Globalization.CultureInfo.InvariantCulture),
                                                                  green:=Convert.ToInt32(splitLine(5), Globalization.CultureInfo.InvariantCulture),
                                                                  blue:=Convert.ToInt32(splitLine(6), Globalization.CultureInfo.InvariantCulture))
            themeDictionary(key) = New ColorDescriptor(foreground, background)
        End While
        sr.Close()
        fileStream.Close()
    End Sub

    Public Sub UpdateColorDictionariesFromFile()
        Dim executableDirectoryPath As String = Path.Combine(Path.GetDirectoryName(Application.ExecutablePath), "Assets")
        Dim assetColorFile As String = Path.Combine(executableDirectoryPath, LightModeDictionaryFileName)
        Dim userColorFile As String = Path.Combine(FileIO.SpecialDirectories.MyDocuments, LightModeDictionaryFileName)

        If File.Exists(userColorFile) AndAlso assetColorFile.AreNotSame(userColorFile) Then
            s_LightModeColorDictionary = MergeColorDictionary(userColorFile, File.GetLastAccessTime(assetColorFile), s_LightModeColorDictionary)
        End If
        assetColorFile = Path.Combine(executableDirectoryPath, DarkModeDictionaryFileName)
        userColorFile = Path.Combine(FileIO.SpecialDirectories.MyDocuments, DarkModeDictionaryFileName)
        If File.Exists(userColorFile) AndAlso assetColorFile.AreNotSame(userColorFile) Then
            s_DarkModeColorDictionary = MergeColorDictionary(userColorFile, File.GetLastWriteTime(assetColorFile), s_DarkModeColorDictionary)
        End If
    End Sub

    Public Sub WriteColorDictionaryToFile()
        Dim saveFilePath As String = Path.Combine(FileIO.SpecialDirectories.MyDocuments,
                                            $"{If(IsLightMode(My.Forms.Form1.TSThemeButton.Text), "LightMode", "DarkMode")}ColorDictionary.csv")
        If File.Exists(saveFilePath) Then
            WriteColorDictionaryToFile(saveFilePath, My.Forms.Form1.CurrentThemeDictionary)
        End If
    End Sub

    Public Sub WriteColorDictionaryToFile(filePath As String, themeDictionary As Dictionary(Of String, ColorDescriptor))
        Using fileStream As FileStream = File.OpenWrite(filePath)
            Using sw As New StreamWriter(fileStream)
                sw.WriteLine($"Key,ForegroundR,ForegroundG,ForegroundB,BackgroundR,BackgroundG,BackgroundB")
                For Each kvp As KeyValuePair(Of String, ColorDescriptor) In themeDictionary
                    sw.WriteLine($"{kvp.Key},{kvp.Value.Foreground.R},{kvp.Value.Foreground.G},{kvp.Value.Foreground.B},{kvp.Value.Background.R},{kvp.Value.Background.G},{kvp.Value.Background.B}")
                Next
                sw.Flush()
                sw.Close()
            End Using
        End Using
    End Sub

End Module
