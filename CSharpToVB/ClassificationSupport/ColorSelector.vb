' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Xml
Imports System.Xml.Serialization

Public Module ColorSelector

    Private ReadOnly s_fullPath As String = Path.Combine(FileIO.SpecialDirectories.MyDocuments, $"{If(My.Forms.Form1.TSThemeButton.Text = "Light Mode", "LightMode", "DarkMode")}ColorDictionary.csv")

#Region "Default Themes"

    Public ReadOnly s_DarkThemeMappingDictionary As New Dictionary(Of String, (ForeGround As Color, Background As Color))(StringComparer.OrdinalIgnoreCase) From {
                        {DefaultValue, (Color.White, Color.Black)},
                        {ClassName, (Color.FromArgb(0, 128, 128), Color.Black)},
                        {Comment, (Color.FromArgb(0, 100, 0), Color.Black)},
                        {ConstantName, (Color.White, Color.Black)},
                        {ControlKeyword, (Color.FromArgb(143, 8, 196), Color.Black)},
                        {DelegateName, (Color.FromArgb(0, 128, 128), Color.Black)},
                        {EnumMemberName, (Color.FromArgb(0, 128, 128), Color.Black)},
                        {EnumName, (Color.FromArgb(0, 128, 128), Color.Black)},
                        {ErrorValue, (Color.Red, Color.White)},
                        {EventName, (Color.White, Color.Black)},
                        {ExcludedCode, (Color.FromArgb(128, 128, 128), Color.Black)},
                        {ExtensionMethodName, (Color.White, Color.Black)},
                        {FunctionKeyword, (Color.FromArgb(0, 0, 255), Color.Black)},
                        {FieldName, (Color.White, Color.Black)},
                        {Identifier, (Color.White, Color.Black)},
                        {InterfaceName, (Color.FromArgb(0, 128, 128), Color.Black)},
                        {Keyword, (Color.FromArgb(0, 0, 255), Color.Black)},
                        {LabelName, (Color.White, Color.Black)},
                        {LocalName, (Color.White, Color.Black)},
                        {MethodName, (Color.White, Color.Black)},
                        {ModuleName, (Color.FromArgb(0, 128, 128), Color.Black)},
                        {NamespaceName, (Color.White, Color.Black)},
                        {NumericLiteral, (Color.White, Color.Black)},
                        {OperatorOverloaded, (Color.White, Color.Black)},
                        {[Operator], (Color.White, Color.Black)},
                        {ParameterName, (Color.White, Color.Black)},
                        {PreprocessorKeyword, (Color.Gray, Color.White)},
                        {PreprocessorText, (Color.White, Color.Black)},
                        {PropertyName, (Color.White, Color.Black)},
                        {Punctuation, (Color.White, Color.Black)},
                        {RegexAlternation, (Color.Teal, Color.White)},
                        {RegexAnchor, (Color.Pink, Color.White)},
                        {RegexCharacterClass, (Color.Blue, Color.White)},
                        {RegexComment, (Color.DarkGreen, Color.White)},
                        {RegexGrouping, (Color.Teal, Color.White)},
                        {RegexOtherEscape, (Color.Brown, Color.White)},
                        {RegexQuantifier, (Color.Pink, Color.White)},
                        {RegexSelfEscapedCharacter, (Color.DarkRed, Color.White)},
                        {RegexText, (Color.DarkRed, Color.White)},
                        {StaticSymbol, (Color.White, Color.Black)},
                        {StringEscapeCharacter, (Color.DarkBlue, Color.White)},
                        {VerbatimStringLiteral, (Color.FromArgb(128, 0, 0), Color.Black)},
                        {StringLiteral, (Color.FromArgb(163, 21, 21), Color.Black)},
                        {StructName, (Color.FromArgb(43, 145, 175), Color.Black)},
                        {Text, (Color.White, Color.Black)},
                        {TypeParameterName, (Color.DarkGray, Color.White)},
                        {XmlDocCommentAttributeName, (Color.FromArgb(128, 128, 128), Color.Black)},
                        {XmlDocCommentAttributeQuotes, (Color.FromArgb(128, 128, 128), Color.Black)},
                        {XmlDocCommentAttributeValue, (Color.FromArgb(128, 128, 128), Color.Black)},
                        {XmlDocCommentCDataSection, (Color.FromArgb(128, 128, 128), Color.Black)},
                        {XmlDocCommentComment, (Color.FromArgb(128, 128, 128), Color.Black)},
                        {XmlDocCommentDelimiter, (Color.FromArgb(128, 128, 128), Color.Black)},
                        {XmlDocCommentEntityReference, (Color.FromArgb(0, 128, 0), Color.Black)},
                        {XmlDocCommentName, (Color.FromArgb(128, 128, 128), Color.Black)},
                        {XmlDocCommentProcessingInstruction, (Color.FromArgb(128, 128, 128), Color.Black)},
                        {XmlDocCommentText, (Color.FromArgb(0, 128, 0), Color.Black)},
                        {XmlLiteralAttributeName, (Color.FromArgb(128, 128, 128), Color.Black)},
                        {XmlLiteralAttributeQuotes, (Color.FromArgb(128, 128, 128), Color.Black)},
                        {XmlLiteralAttributeValue, (Color.FromArgb(128, 128, 128), Color.Black)},
                        {XmlLiteralCDataSection, (Color.FromArgb(128, 128, 128), Color.Black)},
                        {XmlLiteralComment, (Color.FromArgb(128, 128, 128), Color.Black)},
                        {XmlLiteralDelimiter, (Color.FromArgb(100, 100, 185), Color.Black)},
                        {XmlLiteralEmbeddedExpression, (Color.FromArgb(128, 128, 128), Color.Black)},
                        {XmlLiteralEntityReference, (Color.FromArgb(185, 100, 100), Color.Black)},
                        {XmlLiteralName, (Color.FromArgb(132, 70, 70), Color.Black)},
                        {XmlLiteralProcessingInstruction, (Color.FromArgb(128, 128, 128), Color.Black)},
                        {XmlLiteralText, (Color.FromArgb(85, 85, 85), Color.Black)},
                        {"Button", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"CheckBox", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"ComboBox", (Color.White, SystemColors.Window)},
                        {"ContextMenuStrip", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"FloatingMenu", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"FolderBrowserDialog", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"Label", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"LineNumbersForRichTextBox", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"ListBox", (Color.White, SystemColors.Window)},
                        {"Menu", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"MenuStrip", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"MenuText", (Color.Black, SystemColors.ControlDark)},
                        {"Panel", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"PanelBoarderStyle", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"ProgressBar", (SystemColors.Highlight, SystemColors.ControlDark)},
                        {"RichTextBox", (Color.White, SystemColors.Window)},
                        {"SplitBar", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"SplitContainer", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"StatusBar", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"TextBox", (Color.White, SystemColors.ControlDark)},
                        {"ToolBar", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"ToolStrip", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"ToolStripButton", (SystemColors.ControlText, SystemColors.MenuBar)},
                        {"ToolStripCheckBox", (SystemColors.GrayText, SystemColors.ControlDark)},
                        {"ToolStripComboBox", (SystemColors.GrayText, SystemColors.Window)},
                        {"ToolStripLabel", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"ToolStripMenuItem", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"ToolStripProgressBar", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"ToolStripSeparator", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"ToolStripStatusLabel", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"ToolStripTextProgressBar", (SystemColors.ControlText, SystemColors.ControlDark)},
                        {"ToolTip", (SystemColors.ControlText, SystemColors.ControlDark)}
                        }

    Public ReadOnly s_LightThemeMappingDictionary As New Dictionary(Of String, (ForeGround As Color, Background As Color))(StringComparer.OrdinalIgnoreCase) From {
                        {DefaultValue, (Color.Black, Color.White)},
                        {ClassName, (Color.FromArgb(0, 128, 128), Color.White)},
                        {Comment, (Color.FromArgb(0, 100, 0), Color.White)},
                        {ConstantName, (Color.Black, Color.White)},
                        {ControlKeyword, (Color.FromArgb(143, 8, 196), Color.White)},
                        {DelegateName, (Color.FromArgb(0, 128, 128), Color.White)},
                        {EnumMemberName, (Color.FromArgb(0, 128, 128), Color.White)},
                        {EnumName, (Color.FromArgb(0, 128, 128), Color.White)},
                        {ErrorValue, (Color.Red, Color.White)},
                        {EventName, (Color.Black, Color.White)},
                        {ExcludedCode, (Color.FromArgb(128, 128, 128), Color.White)},
                        {ExtensionMethodName, (Color.Black, Color.White)},
                        {FieldName, (Color.Black, Color.White)},
                        {FunctionKeyword, (Color.FromArgb(0, 0, 255), Color.White)},
                        {Identifier, (Color.Black, Color.White)},
                        {InterfaceName, (Color.FromArgb(0, 128, 128), Color.White)},
                        {Keyword, (Color.FromArgb(0, 0, 255), Color.White)},
                        {LabelName, (Color.Black, Color.White)},
                        {LocalName, (Color.Black, Color.White)},
                        {MethodName, (Color.Black, Color.White)},
                        {ModuleName, (Color.FromArgb(0, 128, 128), Color.White)},
                        {NamespaceName, (Color.Black, Color.White)},
                        {NumericLiteral, (Color.Black, Color.White)},
                        {OperatorOverloaded, (Color.Black, Color.White)},
                        {[Operator], (Color.Black, Color.White)},
                        {ParameterName, (Color.Black, Color.White)},
                        {PreprocessorKeyword, (Color.Gray, Color.White)},
                        {PreprocessorText, (Color.Black, Color.White)},
                        {PropertyName, (Color.Black, Color.White)},
                        {Punctuation, (Color.Black, Color.White)},
                        {RegexAlternation, (Color.Teal, Color.White)},
                        {RegexAnchor, (Color.Pink, Color.White)},
                        {RegexCharacterClass, (Color.Blue, Color.White)},
                        {RegexComment, (Color.DarkGreen, Color.White)},
                        {RegexGrouping, (Color.Teal, Color.White)},
                        {RegexOtherEscape, (Color.Brown, Color.White)},
                        {RegexQuantifier, (Color.Pink, Color.White)},
                        {RegexSelfEscapedCharacter, (Color.DarkRed, Color.White)},
                        {RegexText, (Color.DarkRed, Color.White)},
                        {StaticSymbol, (Color.Black, Color.White)},
                        {StringEscapeCharacter, (Color.DarkBlue, Color.White)},
                        {VerbatimStringLiteral, (Color.FromArgb(128, 0, 0), Color.White)},
                        {StringLiteral, (Color.FromArgb(163, 21, 21), Color.White)},
                        {StructName, (Color.FromArgb(43, 145, 175), Color.White)},
                        {Text, (Color.Black, Color.White)},
                        {TypeParameterName, (Color.DarkGray, Color.White)},
                        {XmlDocCommentAttributeName, (Color.FromArgb(128, 128, 128), Color.White)},
                        {XmlDocCommentAttributeQuotes, (Color.FromArgb(128, 128, 128), Color.White)},
                        {XmlDocCommentAttributeValue, (Color.FromArgb(128, 128, 128), Color.White)},
                        {XmlDocCommentCDataSection, (Color.FromArgb(128, 128, 128), Color.White)},
                        {XmlDocCommentComment, (Color.FromArgb(128, 128, 128), Color.White)},
                        {XmlDocCommentDelimiter, (Color.FromArgb(128, 128, 128), Color.White)},
                        {XmlDocCommentEntityReference, (Color.FromArgb(0, 128, 0), Color.White)},
                        {XmlDocCommentName, (Color.FromArgb(128, 128, 128), Color.White)},
                        {XmlDocCommentProcessingInstruction, (Color.FromArgb(128, 128, 128), Color.White)},
                        {XmlDocCommentText, (Color.FromArgb(0, 128, 0), Color.White)},
                        {XmlLiteralAttributeName, (Color.FromArgb(128, 128, 128), Color.White)},
                        {XmlLiteralAttributeQuotes, (Color.FromArgb(128, 128, 128), Color.White)},
                        {XmlLiteralAttributeValue, (Color.FromArgb(128, 128, 128), Color.White)},
                        {XmlLiteralCDataSection, (Color.FromArgb(128, 128, 128), Color.White)},
                        {XmlLiteralComment, (Color.FromArgb(128, 128, 128), Color.White)},
                        {XmlLiteralDelimiter, (Color.FromArgb(100, 100, 185), Color.White)},
                        {XmlLiteralEmbeddedExpression, (Color.FromArgb(128, 128, 128), Color.White)},
                        {XmlLiteralEntityReference, (Color.FromArgb(185, 100, 100), Color.White)},
                        {XmlLiteralName, (Color.FromArgb(132, 70, 70), Color.White)},
                        {XmlLiteralProcessingInstruction, (Color.FromArgb(128, 128, 128), Color.White)},
                        {XmlLiteralText, (Color.FromArgb(85, 85, 85), Color.Black)},
                        {"Button", (SystemColors.ControlText, SystemColors.Control)},
                        {"CheckBox", (SystemColors.ControlText, SystemColors.Control)},
                        {"ComboBox", (SystemColors.WindowText, SystemColors.Window)},
                        {"ContextMenuStrip", (SystemColors.ControlText, SystemColors.Control)},
                        {"FloatingMenu", (SystemColors.ControlText, SystemColors.Control)},
                        {"FolderBrowserDialog", (SystemColors.ControlText, SystemColors.Control)},
                        {"Label", (SystemColors.ControlText, SystemColors.Control)},
                        {"LineNumbersForRichTextBox", (SystemColors.ControlText, SystemColors.Control)},
                        {"ListBox", (SystemColors.WindowText, SystemColors.Window)},
                        {"Menu", (SystemColors.ControlText, SystemColors.Control)},
                        {"MenuStrip", (SystemColors.ControlText, SystemColors.Control)},
                        {"MenuText", (Color.Black, SystemColors.Control)},
                        {"Panel", (SystemColors.ControlText, SystemColors.Control)},
                        {"PanelBoarderStyle", (SystemColors.ControlText, SystemColors.Control)},
                        {"ProgressBar", (SystemColors.Highlight, SystemColors.Control)},
                        {"RichTextBox", (SystemColors.WindowText, SystemColors.Window)},
                        {"SplitBar", (SystemColors.ControlText, SystemColors.Control)},
                        {"SplitContainer", (SystemColors.ControlText, SystemColors.Control)},
                        {"StatusBar", (SystemColors.ControlText, SystemColors.Control)},
                        {"TextBox", (SystemColors.WindowText, SystemColors.Control)},
                        {"ToolBar", (SystemColors.ControlText, SystemColors.Control)},
                        {"ToolStrip", (SystemColors.ControlText, SystemColors.Control)},
                        {"ToolStripButton", (SystemColors.ControlText, SystemColors.MenuBar)},
                        {"ToolStripCheckBox", (SystemColors.ControlText, SystemColors.Control)},
                        {"ToolStripComboBox", (SystemColors.ControlText, SystemColors.Window)},
                        {"ToolStripLabel", (SystemColors.ControlText, SystemColors.Control)},
                        {"ToolStripMenuItem", (SystemColors.ControlText, SystemColors.Control)},
                        {"ToolStripProgressBar", (SystemColors.ControlText, SystemColors.Control)},
                        {"ToolStripSeparator", (SystemColors.ControlText, SystemColors.Control)},
                        {"ToolStripStatusLabel", (SystemColors.ControlText, SystemColors.Control)},
                        {"ToolStripTextProgressBar", (SystemColors.ControlText, SystemColors.Control)},
                        {"ToolTip", (SystemColors.ControlText, SystemColors.Control)}
                        }

    ' Do not rename the next two fields need to be last in region
    Public ReadOnly s_xDarkModeDefaultCount As Integer = s_DarkThemeMappingDictionary.Count
    Public ReadOnly s_xlightModeDefaultCount As Integer = s_LightThemeMappingDictionary.Count

#End Region

    Public Property DefaultColor As (ForeGround As Color, Background As Color) = (Color.Black, Color.White)

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

    Public Function GetXMLThemeFromFile(Filename As String) As Themes
        Using sr As New StreamReader(Filename)
            Using xr As XmlReader = XmlReader.Create(sr)
                Return CType(New XmlSerializer(GetType(Themes)).Deserialize(xr), Themes)
                ' "Text Editor Language Service Items"
            End Using
        End Using
    End Function

    Public Sub SetColor(name As String, value As (ForeGround As Color, Background As Color))
        My.Forms.Form1.CurrentThemeDictionary(name) = value
        WriteColorDictionaryToFile(s_fullPath)
    End Sub

    Public Sub UpdateColorDictionaryFromFile(FPath As String)
        If Not File.Exists(FPath) Then
            WriteColorDictionaryToFile(FPath)
            Exit Sub
        End If
        Dim fileStream As FileStream = File.OpenRead(FPath)
        Dim sr As New StreamReader(fileStream)
        sr.ReadLine()
        While sr.Peek() <> -1
            Dim line As String = sr.ReadLine()
            Dim splitLine() As String = line.Split(","c)
            Dim key As String = splitLine(0)
            My.Forms.Form1.CurrentThemeDictionary(key) = (Color.FromArgb(red:=Convert.ToInt32(splitLine(1), Globalization.CultureInfo.InvariantCulture),
                                                                green:=Convert.ToInt32(splitLine(2), Globalization.CultureInfo.InvariantCulture),
                                                                blue:=Convert.ToInt32(splitLine(3), Globalization.CultureInfo.InvariantCulture)),
                                                          Color.FromArgb(red:=Convert.ToInt32(splitLine(4), Globalization.CultureInfo.InvariantCulture),
                                                                green:=Convert.ToInt32(splitLine(5), Globalization.CultureInfo.InvariantCulture),
                                                                blue:=Convert.ToInt32(splitLine(6), Globalization.CultureInfo.InvariantCulture)))
        End While
        sr.Close()
        fileStream.Close()
    End Sub

    Public Sub WriteColorDictionaryToFile()
        WriteColorDictionaryToFile(s_fullPath)
    End Sub

    Public Sub WriteColorDictionaryToFile(FPath As String)
        Using fileStream As FileStream = File.OpenWrite(FPath)
            Using sw As New StreamWriter(fileStream)
                sw.WriteLine($"Key,ForeGroundR,ForeGroundG,ForeGroundB,BackgroundR,BackgroundG,BackgroundB")
                For Each kvp As KeyValuePair(Of String, (ForeGround As Color, Background As Color)) In My.Forms.Form1.CurrentThemeDictionary
                    sw.WriteLine($"{kvp.Key},{kvp.Value.ForeGround.R},{kvp.Value.ForeGround.G},{kvp.Value.ForeGround.B},{kvp.Value.Background.R},{kvp.Value.Background.G},{kvp.Value.Background.B}")
                Next
                sw.Flush()
                sw.Close()
            End Using
        End Using
    End Sub

End Module
