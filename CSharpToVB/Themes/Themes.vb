' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Xml.Serialization

'''<remarks/>
<Serializable(),
 ComponentModel.DesignerCategory("code"),
 XmlType(AnonymousType:=True),
 XmlRoot([Namespace]:="", IsNullable:=False)>
Partial Public Class Themes
    Sub New()
    End Sub

    '''<remarks/>
    Public Property Theme() As ThemesTheme

    Public Shared Function LoadDictionaryFromTheme(CurrentTheme As Themes, ThemeDictionary As Dictionary(Of String, (ForeGround As Color, Background As Color))) As Dictionary(Of String, (ForeGround As Color, Background As Color))
        If CurrentTheme Is Nothing Then
            Throw New ArgumentNullException(NameOf(CurrentTheme))
        End If
        If ThemeDictionary Is Nothing Then
            Throw New ArgumentNullException(NameOf(ThemeDictionary))
        End If
        Dim categoryColorList As New List(Of ThemesThemeCategoryColor)
        For Each cat As ThemesThemeCategory In CurrentTheme.Theme.Category
            If cat.Name = "Text Editor Language Service Items" Then
                categoryColorList.AddRange(cat.Color)
            End If
        Next

        For Each cat As ThemesThemeCategory In CurrentTheme.Theme.Category
            If cat.Name = "Text Editor Language Service Items" Then
                Continue For
            End If
            For Each categoryColor As ThemesThemeCategoryColor In cat.Color
                Dim found As Boolean = False
                If ThemeDictionary.ContainsKey(categoryColor.Name) Then
                    For Each c As ThemesThemeCategoryColor In categoryColorList
                        If c.Name = categoryColor.Name Then
                            found = True
                            Exit For
                        End If
                    Next
                    If Not found Then
                        categoryColorList.Add(categoryColor)
                    End If
                End If
            Next
        Next
        DefaultColor = (categoryColorList(0).Foreground.Source.ToColor, categoryColorList(0).Background.Source.ToColor)
        For Each categoryColor As ThemesThemeCategoryColor In categoryColorList
            Dim background As Color = DefaultColor.Background
            Dim foreground As Color = DefaultColor.ForeGround
            If ThemeDictionary.ContainsKey(categoryColor.Name) Then
                If categoryColor.Background IsNot Nothing Then
                    background = categoryColor.Background.Source.ToColor
                End If
                If categoryColor.Foreground IsNot Nothing Then
                    foreground = categoryColor.Foreground.Source.ToColor
                End If
                ThemeDictionary(categoryColor.Name) = (foreground, background)
            Else
                ThemeDictionary.Add(categoryColor.Name, (foreground, background))
            End If
        Next
        Return ThemeDictionary
    End Function
End Class
