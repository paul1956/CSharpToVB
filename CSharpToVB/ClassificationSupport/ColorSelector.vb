' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Runtime.CompilerServices

Public Module ColorSelector

    Friend s_DarkModeColorDictionary As New Dictionary(Of String, (ForeGround As Color, Background As Color))(StringComparer.OrdinalIgnoreCase)
    Friend s_LightModeColorDictionary As New Dictionary(Of String, (ForeGround As Color, Background As Color))(StringComparer.OrdinalIgnoreCase)
    Public ReadOnly _darkModeDictionaryFileName As String = "DarkModeColorDictionary.csv"
    Public ReadOnly _lightModeDictionaryFileName As String = "LightModeColorDictionary.csv"
    Friend Property DefaultColor As (ForeGround As Color, Background As Color) = (Color.Black, Color.White)

    <Extension>
    Private Function IsNotIdentical(filename As String, filename2 As String) As Boolean
        Dim epsillon As Double = 2.0
        Dim assetLastWriteTime As Date = File.GetLastWriteTime(filename)
        Dim userDictionaryLastWriteTime As Date = File.GetLastWriteTime(filename2)
        Dim totalSeconds As Double = (assetLastWriteTime - userDictionaryLastWriteTime).TotalSeconds

        If Math.Abs(Math.Round(totalSeconds)) > epsillon Then
            Return True
        End If
        Return False
    End Function

    Private Function MergeColorDictionary(FilePath As String, LastWriteTime As Date, ColorDictionary As Dictionary(Of String, (ForeGround As Color, Background As Color))) As Dictionary(Of String, (ForeGround As Color, Background As Color))
        Dim tmpDictionary As New Dictionary(Of String, (ForeGround As Color, Background As Color))(StringComparer.OrdinalIgnoreCase)
        LoadColorDictionaryFromFile(FilePath, tmpDictionary)
        For Each name As String In ColorDictionary.Keys
            If Not tmpDictionary.ContainsKey(name) Then
                tmpDictionary.Add(name, ColorDictionary(name))
            End If
        Next
        WriteColorDictionaryToFile(FilePath, tmpDictionary)

        File.SetLastWriteTime(FilePath, LastWriteTime)
        Return tmpDictionary
    End Function

    Friend Function GetColorFromName(Name As String) As (ForeGround As Color, Background As Color)
        If String.IsNullOrWhiteSpace(Name) Then
            Return DefaultColor
        End If
        Dim returnValue As (ForeGround As Color, Background As Color) = Nothing
        If My.Forms.Form1.CurrentThemeDictionary.TryGetValue(Name, returnValue) Then
            Return returnValue
        End If
        Debug.Print($"GetColorFromName missing({Name})")
        Return My.Forms.Form1.CurrentThemeDictionary("error")
    End Function

    Public Function GetColorNameList() As Dictionary(Of String, (ForeGround As Color, Background As Color)).KeyCollection
        Return My.Forms.Form1.CurrentThemeDictionary.Keys
    End Function

    Public Sub LoadColorDictionaryFromFile(FPath As String, ThemeDictionary As Dictionary(Of String, (ForeGround As Color, Background As Color)))
        If Not File.Exists(FPath) Then
            Exit Sub
        End If
        Dim fileStream As FileStream = File.OpenRead(FPath)
        Dim sr As New StreamReader(fileStream)
        sr.ReadLine()
        While sr.Peek() <> -1
            Dim line As String = sr.ReadLine()
            Dim splitLine() As String = line.Split(","c)
            Dim key As String = splitLine(0)
            If Not ThemeDictionary.ContainsKey(key) Then
                ThemeDictionary.Add(key, DefaultColor)
            End If
            ThemeDictionary(key) = (Color.FromArgb(red:=Convert.ToInt32(splitLine(1), Globalization.CultureInfo.InvariantCulture),
                                                   green:=Convert.ToInt32(splitLine(2), Globalization.CultureInfo.InvariantCulture),
                                                   blue:=Convert.ToInt32(splitLine(3), Globalization.CultureInfo.InvariantCulture)),
                                    Color.FromArgb(red:=Convert.ToInt32(splitLine(4), Globalization.CultureInfo.InvariantCulture),
                                                   green:=Convert.ToInt32(splitLine(5), Globalization.CultureInfo.InvariantCulture),
                                                   blue:=Convert.ToInt32(splitLine(6), Globalization.CultureInfo.InvariantCulture)))
        End While
        sr.Close()
        fileStream.Close()
    End Sub

    Public Sub UpdateColorDictionariesFromFile()
        Dim executableDirectoryPath As String = Path.Combine(Path.GetDirectoryName(Application.ExecutablePath), "Assets")
        Dim assetColorFile As String = Path.Combine(executableDirectoryPath, _lightModeDictionaryFileName)
        Dim userColorFile As String = Path.Combine(FileIO.SpecialDirectories.MyDocuments, _lightModeDictionaryFileName)

        If File.Exists(userColorFile) AndAlso assetColorFile.IsNotIdentical(userColorFile) Then
            s_LightModeColorDictionary = MergeColorDictionary(userColorFile, File.GetLastAccessTime(assetColorFile), s_LightModeColorDictionary)
        Else
            LoadColorDictionaryFromFile(assetColorFile, s_LightModeColorDictionary)
        End If
        assetColorFile = Path.Combine(executableDirectoryPath, _darkModeDictionaryFileName)
        userColorFile = Path.Combine(FileIO.SpecialDirectories.MyDocuments, _darkModeDictionaryFileName)
        If File.Exists(userColorFile) AndAlso assetColorFile.IsNotIdentical(userColorFile) Then
            s_DarkModeColorDictionary = MergeColorDictionary(userColorFile, File.GetLastWriteTime(assetColorFile), s_DarkModeColorDictionary)
        Else
            LoadColorDictionaryFromFile(assetColorFile, s_DarkModeColorDictionary)
        End If
    End Sub

    Public Sub WriteColorDictionaryToFile()
        Dim saveFilePath As String = Path.Combine(FileIO.SpecialDirectories.MyDocuments,
                                                $"{If(My.Forms.Form1.TSThemeButton.Text = "Light Mode", "LightMode", "DarkMode")}ColorDictionary.csv")
        If File.Exists(saveFilePath) Then
            WriteColorDictionaryToFile(saveFilePath, My.Forms.Form1.CurrentThemeDictionary)
        End If
    End Sub

    Public Sub WriteColorDictionaryToFile(FPath As String, ThemeDictionary As Dictionary(Of String, (ForeGround As Color, Background As Color)))
        Using fileStream As FileStream = File.OpenWrite(FPath)
            Using sw As New StreamWriter(fileStream)
                sw.WriteLine($"Key,ForeGroundR,ForeGroundG,ForeGroundB,BackgroundR,BackgroundG,BackgroundB")
                For Each kvp As KeyValuePair(Of String, (ForeGround As Color, Background As Color)) In ThemeDictionary
                    sw.WriteLine($"{kvp.Key},{kvp.Value.ForeGround.R},{kvp.Value.ForeGround.G},{kvp.Value.ForeGround.B},{kvp.Value.Background.R},{kvp.Value.Background.G},{kvp.Value.Background.B}")
                Next
                sw.Flush()
                sw.Close()
            End Using
        End Using
    End Sub

End Module
