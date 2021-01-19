' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO

Public Module ColorSelector

#Region "Default Themes"

    Public ReadOnly s_DarkThemeMappingDictionary As New Dictionary(Of String, (ForeGround As Color, Background As Color))(StringComparer.OrdinalIgnoreCase) From {
                        {DefaultValue, (Color.White, Color.FromArgb(30, 30, 30))},
                        {ClassName, (Color.FromArgb(0, 128, 128), Color.FromArgb(18, 32, 42))},
                        {Comment, (Color.FromArgb(0, 100, 0), Color.FromArgb(18, 32, 42))},
                        {ConstantName, (Color.FromArgb(255, 255, 255), Color.FromArgb(18, 32, 42))},
                        {ControlKeyword, (Color.FromArgb(143, 8, 196), Color.FromArgb(18, 32, 42))},
                        {DelegateName, (Color.FromArgb(0, 128, 128), Color.FromArgb(18, 32, 42))},
                        {EnumMemberName, (Color.FromArgb(0, 128, 128), Color.FromArgb(18, 32, 42))},
                        {EnumName, (Color.FromArgb(0, 128, 128), Color.FromArgb(18, 32, 42))},
                        {ErrorValue, (Color.FromArgb(216, 80, 80), Color.FromArgb(18, 32, 42))},
                        {EventName, (Color.FromArgb(220, 220, 220), Color.FromArgb(18, 32, 42))},
                        {ExcludedCode, (Color.FromArgb(128, 128, 128), Color.FromArgb(18, 32, 42))},
                        {ExtensionMethodName, (Color.FromArgb(220, 220, 170), Color.FromArgb(18, 32, 42))},
                        {FieldName, (Color.FromArgb(220, 220, 220), Color.FromArgb(18, 32, 42))},
                        {FunctionKeyword, (Color.FromArgb(141, 141, 207), Color.FromArgb(18, 32, 42))},
                        {Identifier, (Color.FromArgb(220, 220, 220), Color.FromArgb(18, 32, 42))},
                        {InterfaceName, (Color.FromArgb(0, 128, 128), Color.FromArgb(18, 32, 42))},
                        {Keyword, (Color.FromArgb(86, 156, 214), Color.FromArgb(18, 32, 42))},
                        {LabelName, (Color.FromArgb(RGB(220, 220, 220)), Color.FromArgb(18, 32, 42))},
                        {LocalName, (Color.FromArgb(RGB(156, 220, 254)), Color.FromArgb(18, 32, 42))},
                        {MethodName, (Color.FromArgb(220, 220, 170), Color.FromArgb(18, 32, 42))},
                        {ModuleName, (Color.FromArgb(78, 201, 176), Color.FromArgb(18, 32, 42))},
                        {NamespaceName, (Color.FromArgb(220, 220, 220), Color.FromArgb(18, 32, 42))},
                        {NumericLiteral, (Color.FromArgb(181, 206, 168), Color.FromArgb(18, 32, 42))},
                        {[Operator], (Color.FromArgb(180, 180, 180), Color.FromArgb(18, 32, 42))},
                        {OperatorOverloaded, (Color.FromArgb(220, 220, 170), Color.FromArgb(18, 32, 42))},
                        {ParameterName, (Color.FromArgb(156, 220, 254), Color.FromArgb(18, 32, 42))},
                        {PreprocessorKeyword, (Color.Gray, Color.FromArgb(18, 32, 42))},
                        {PreprocessorText, (Color.FromArgb(220, 220, 220), Color.FromArgb(18, 32, 42))},
                        {PropertyName, (Color.FromArgb(156, 220, 254), Color.FromArgb(18, 32, 42))},
                        {Punctuation, (Color.FromArgb(220, 220, 220), Color.FromArgb(18, 32, 42))},
                        {RegexAlternation, (Color.Teal, Color.FromArgb(18, 32, 42))},
                        {RegexAnchor, (Color.Pink, Color.FromArgb(18, 32, 42))},
                        {RegexCharacterClass, (Color.Blue, Color.FromArgb(18, 32, 42))},
                        {RegexComment, (Color.DarkGreen, Color.FromArgb(18, 32, 42))},
                        {RegexGrouping, (Color.Teal, Color.FromArgb(18, 32, 42))},
                        {RegexOtherEscape, (Color.Brown, Color.FromArgb(18, 32, 42))},
                        {RegexQuantifier, (Color.Pink, Color.FromArgb(18, 32, 42))},
                        {RegexSelfEscapedCharacter, (Color.DarkRed, Color.FromArgb(18, 32, 42))},
                        {RegexText, (Color.DarkRed, Color.FromArgb(18, 32, 42))},
                        {StaticSymbol, (Color.FromArgb(95, 149, 250), Color.FromArgb(18, 32, 42))},
                        {StringEscapeCharacter, (Color.FromArgb(255, 214, 143), Color.FromArgb(18, 32, 42))},
                        {StringLiteral, (Color.FromArgb(163, 21, 21), Color.FromArgb(18, 32, 42))},
                        {StructName, (Color.FromArgb(43, 145, 175), Color.FromArgb(18, 32, 42))},
                        {Text, (Color.FromArgb(220, 220, 220), Color.FromArgb(18, 32, 42))},
                        {TypeParameterName, (Color.DarkGray, Color.FromArgb(18, 32, 42))},
                        {StringVerbatimLiteral, (Color.FromArgb(128, 0, 0), Color.FromArgb(18, 32, 42))},
                        {XmlDocCommentAttributeName, (Color.FromArgb(128, 128, 128), Color.FromArgb(18, 32, 42))},
                        {XmlDocCommentAttributeQuotes, (Color.FromArgb(128, 128, 128), Color.FromArgb(18, 32, 42))},
                        {XmlDocCommentAttributeValue, (Color.FromArgb(128, 128, 128), Color.FromArgb(18, 32, 42))},
                        {XmlDocCommentCDataSection, (Color.FromArgb(128, 128, 128), Color.FromArgb(18, 32, 42))},
                        {XmlDocCommentComment, (Color.FromArgb(128, 128, 128), Color.FromArgb(18, 32, 42))},
                        {XmlDocCommentDelimiter, (Color.FromArgb(128, 128, 128), Color.FromArgb(18, 32, 42))},
                        {XmlDocCommentEntityReference, (Color.FromArgb(0, 128, 0), Color.FromArgb(18, 32, 42))},
                        {XmlDocCommentName, (Color.FromArgb(128, 128, 128), Color.FromArgb(18, 32, 42))},
                        {XmlDocCommentProcessingInstruction, (Color.FromArgb(128, 128, 128), Color.FromArgb(18, 32, 42))},
                        {XmlDocCommentText, (Color.FromArgb(0, 128, 0), Color.FromArgb(18, 32, 42))},
                        {XmlLiteralAttributeName, (Color.FromArgb(128, 128, 128), Color.FromArgb(18, 32, 42))},
                        {XmlLiteralAttributeQuotes, (Color.FromArgb(128, 128, 128), Color.FromArgb(18, 32, 42))},
                        {XmlLiteralAttributeValue, (Color.FromArgb(128, 128, 128), Color.FromArgb(18, 32, 42))},
                        {XmlLiteralCDataSection, (Color.FromArgb(128, 128, 128), Color.FromArgb(18, 32, 42))},
                        {XmlLiteralComment, (Color.FromArgb(128, 128, 128), Color.FromArgb(18, 32, 42))},
                        {XmlLiteralDelimiter, (Color.FromArgb(100, 100, 185), Color.FromArgb(18, 32, 42))},
                        {XmlLiteralEmbeddedExpression, (Color.FromArgb(128, 128, 128), Color.FromArgb(18, 32, 42))},
                        {XmlLiteralEntityReference, (Color.FromArgb(185, 100, 100), Color.FromArgb(18, 32, 42))},
                        {XmlLiteralName, (Color.FromArgb(132, 70, 70), Color.FromArgb(18, 32, 42))},
                        {XmlLiteralProcessingInstruction, (Color.FromArgb(128, 128, 128), Color.FromArgb(18, 32, 42))},
                        {XmlLiteralText, (Color.FromArgb(85, 85, 85), Color.FromArgb(18, 32, 42))},
                        {"Button", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"CheckBox", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"ComboBox", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"ContextMenuStrip", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"FloatingMenu", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"FolderBrowserDialog", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"Label", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"LineNumbersForRichTextBox", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"ListBox", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"MenuStrip", (SystemColors.ControlLight, Color.FromArgb(45, 45, 48))},
                        {"Panel", (SystemColors.ControlLight, Color.FromArgb(30, 30, 30))},
                        {"PanelBoarderStyle", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"ProgressBar", (SystemColors.Highlight, SystemColors.ControlDark)},
                        {"RichTextBox", (SystemColors.ControlLight, Color.FromArgb(30, 30, 30))},
                        {"SplitBar", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"SplitContainer", (SystemColors.ControlLight, Color.FromArgb(30, 30, 30))},
                        {"StatusBar", (SystemColors.ControlLight, Color.FromArgb(45, 45, 48))},
                        {"TextBox", (SystemColors.ControlLight, Color.FromArgb(45, 45, 48))},
                        {"TitleBar", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"ToolBar", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"ToolStrip", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"ToolStripButton", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"ToolStripCheckBox", (SystemColors.GrayText, SystemColors.ControlDark)},
                        {"ToolStripComboBox", (SystemColors.GrayText, SystemColors.ControlDark)},
                        {"ToolStripLabel", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"ToolStripMenuItem", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"ToolStripProgressBar", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"ToolStripSeparator", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"ToolStripStatusLabel", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"ToolStripTextProgressBar", (SystemColors.ControlLight, SystemColors.ControlDark)},
                        {"ToolTip", (SystemColors.ControlLight, SystemColors.ControlDark)}
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
                        {[Operator], (Color.Black, Color.White)},
                        {OperatorOverloaded, (Color.Black, Color.White)},
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
                        {StringLiteral, (Color.FromArgb(163, 21, 21), Color.White)},
                        {StructName, (Color.FromArgb(43, 145, 175), Color.White)},
                        {Text, (Color.Black, Color.White)},
                        {TypeParameterName, (Color.DarkGray, Color.White)},
                        {StringVerbatimLiteral, (Color.FromArgb(128, 0, 0), Color.White)},
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
                        {XmlLiteralText, (Color.FromArgb(85, 85, 85), Color.White)},
                        {"Button", (SystemColors.ControlText, SystemColors.Control)},
                        {"CheckBox", (SystemColors.ControlText, SystemColors.Control)},
                        {"ComboBox", (SystemColors.WindowText, SystemColors.Window)},
                        {"ContextMenuStrip", (SystemColors.ControlText, SystemColors.Control)},
                        {"FloatingMenu", (SystemColors.ControlText, SystemColors.Control)},
                        {"FolderBrowserDialog", (SystemColors.ControlText, SystemColors.Control)},
                        {"Label", (SystemColors.ControlText, SystemColors.Control)},
                        {"LineNumbersForRichTextBox", (SystemColors.ControlText, SystemColors.Control)},
                        {"ListBox", (SystemColors.WindowText, SystemColors.Window)},
                        {"MenuStrip", (SystemColors.ControlText, SystemColors.Control)},
                        {"Panel", (SystemColors.ControlText, SystemColors.Control)},
                        {"PanelBoarderStyle", (SystemColors.ControlText, SystemColors.Control)},
                        {"ProgressBar", (SystemColors.Highlight, SystemColors.Control)},
                        {"RichTextBox", (SystemColors.WindowText, SystemColors.Window)},
                        {"SplitBar", (SystemColors.ControlText, SystemColors.Control)},
                        {"SplitContainer", (SystemColors.ControlText, SystemColors.Control)},
                        {"StatusBar", (SystemColors.ControlText, SystemColors.Control)},
                        {"TextBox", (SystemColors.WindowText, SystemColors.Control)},
                        {"TitleBar", (SystemColors.ControlText, SystemColors.Control)},
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

    Public Sub UpdateColorDictionaryFromFile(FPath As String, ThemeDictionary As Dictionary(Of String, (ForeGround As Color, Background As Color)))
        If Not File.Exists(FPath) Then
            WriteColorDictionaryToFile(FPath, My.Forms.Form1.CurrentThemeDictionary)
            Exit Sub
        End If
        Dim fileStream As FileStream = File.OpenRead(FPath)
        Dim sr As New StreamReader(fileStream)
        sr.ReadLine()
        While sr.Peek() <> -1
            Dim line As String = sr.ReadLine()
            Dim splitLine() As String = line.Split(","c)
            Dim key As String = splitLine(0)
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

    Public Sub WriteColorDictionaryToFile()
        WriteColorDictionaryToFile(Path.Combine(FileIO.SpecialDirectories.MyDocuments,
                                                $"{If(My.Forms.Form1.TSThemeButton.Text = "Light Mode", "LightMode", "DarkMode")}ColorDictionary.csv"), My.Forms.Form1.CurrentThemeDictionary)
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
