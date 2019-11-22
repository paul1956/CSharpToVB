' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Diagnostics.CodeAnalysis
Imports System.IO
Imports System.Reflection
Imports System.Text
Imports System.Threading
Imports System.Xml

Imports CSharpToVBApp

Imports CSharpToVBCodeConverter
Imports CSharpToVBCodeConverter.ConversionResult
Imports CSharpToVBCodeConverter.Util

Imports ManageProgressBar

Imports Microsoft.Build.Locator
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Emit
Imports Microsoft.CodeAnalysis.MSBuild
Imports Microsoft.VisualBasic.FileIO

#If NETCOREAPP Then

Imports VBMsgBox

#End If

Public Class Form1

#If NETCOREAPP Then

    <STAThread>
    Public Shared Sub Main()
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault(False)
        Dim f As New Form1()
        Application.Run(f)
        f.Dispose()
    End Sub

#End If

    Private Shared ReadOnly SnippetFileWithPath As String = Path.Combine(SpecialDirectories.MyDocuments, "CSharpToVBLastSnippet.RTF")
    Private _CurrentBuffer As RichTextBox

    Private RequestToConvert As ConvertRequest
    Private ResultOfConversion As ConversionResult

    Private RTFLineStart As Integer

    Private Property CurrentBuffer As RichTextBox
        Get
            Return _CurrentBuffer
        End Get
        Set(value As RichTextBox)
            _CurrentBuffer = value
            If value IsNot Nothing Then
                _CurrentBuffer.Focus()
            End If
        End Set
    End Property

    Property MSBuildInstance As VisualStudioInstance = Nothing
    Private CTS As CancellationTokenSource

    Private Shared Function ConvertSourceFileToDestinationFile(ProjectDirectory As String, ProjectSavePath As String, DocumentName As Document) As String
        If String.IsNullOrWhiteSpace(ProjectSavePath) Then
            Return String.Empty
        End If
        Dim SubPathFromProject As String = Path.GetDirectoryName(DocumentName.FilePath).Replace(ProjectDirectory, "", StringComparison.InvariantCultureIgnoreCase).Trim("\"c)
        Dim PathToSaveDirectory As String = Path.Combine(ProjectSavePath, SubPathFromProject)
        If Not Directory.Exists(PathToSaveDirectory) Then
            Directory.CreateDirectory(PathToSaveDirectory)
        End If
        Return PathToSaveDirectory
    End Function

    Private Shared Function CreateDirectoryIfNonexistent(SolutionRoot As String) As String
        If Not Directory.Exists(SolutionRoot) Then
            Directory.CreateDirectory(SolutionRoot)
        End If
        Return SolutionRoot
    End Function

    Private Sub ButtonStop_Click(sender As Object, e As EventArgs) Handles ButtonStop.Click
        ButtonStop.Visible = False
        CTS.Cancel()
        Application.DoEvents()
    End Sub

    Private Sub ButtonStop_MouseEnter(sender As Object, e As EventArgs) Handles ButtonStop.MouseEnter
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=False)
    End Sub

    Private Sub ButtonStop_MouseLeave(sender As Object, e As EventArgs) Handles ButtonStop.MouseLeave
        ButtonStop.BackColor = System.Drawing.SystemColors.Control
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=ButtonStop.Visible)
    End Sub

    Private Sub ButtonStop_VisibleChanged(sender As Object, e As EventArgs) Handles ButtonStop.VisibleChanged
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=ButtonStop.Visible)
    End Sub

    Private Sub Colorize(FragmentRange As IEnumerable(Of Range), ConversionBuffer As RichTextBox, Lines As Integer, Optional failures As IEnumerable(Of Diagnostic) = Nothing)
        Try ' Prevent crash when exiting
            If failures IsNot Nothing Then
                For Each dia As Diagnostic In failures
                    RichTextBoxErrorList.AppendText($"{dia.Id} Line = {dia.Location.GetLineSpan.StartLinePosition.Line + 1} {dia.GetMessage}")
                    RichTextBoxErrorList.AppendText(vbCrLf)
                Next
            End If
            Dim Progress As New ReportProgress(ConversionProgressBar)
            Progress.SetTotalItems(Lines)

            With ConversionBuffer
                .Clear()
                .Select(.TextLength, 0)
                For Each range As Range In FragmentRange
                    .Select(.TextLength, 0)
                    .SelectionColor = ColorSelector.GetColorFromName(range.ClassificationType)
                    .AppendText(range.Text)
                    If range.Text.Contains(vbLf, StringComparison.InvariantCultureIgnoreCase) Then
                        Progress.UpdateProgress(range.Text.Count(CType(vbLf, Char)))
                        Application.DoEvents()
                    End If
                    If RequestToConvert.CancelToken.IsCancellationRequested Then
                        Exit Sub
                    End If
                Next range
                Application.DoEvents()
                If failures?.Count > 0 Then
                    For Each dia As Diagnostic In failures
                        Dim ErrorLine As Integer = dia.Location.GetLineSpan.StartLinePosition.Line
                        Dim ErrorCharactorPosition As Integer = dia.Location.GetLineSpan.StartLinePosition.Character
                        Dim Length As Integer = dia.Location.GetLineSpan.EndLinePosition.Character - ErrorCharactorPosition
                        .Select(.GetFirstCharIndexFromLine(ErrorLine) + ErrorCharactorPosition, Length)
                        .SelectionColor = Color.Red
                        .Select(.TextLength, 0)
                    Next
                    .Select(.GetFirstCharIndexFromLine(failures(0).Location.GetLineSpan.StartLinePosition.Line), 0)
                    .ScrollToCaret()
                End If
            End With
            If failures?.Count > 0 Then
                LineNumbers_For_RichTextBoxInput.Visible = True
                LineNumbers_For_RichTextBoxOutput.Visible = True
            End If
        Catch ex As Exception
            Stop
        End Try
        ConversionProgressBar.Value = 0
    End Sub

    Private Sub Compile_Colorize(TextToCompile As String)
        Dim CompileResult As EmitResult = CompileVisualBasicString(StringToBeCompiled:=TextToCompile, ErrorsToBeIgnored, DiagnosticSeverity.Error, ResultOfConversion)

        LabelErrorCount.Text = $"Number of Errors: {ResultOfConversion.GetFilteredListOfFailures().Count}"
        Dim FragmentRange As IEnumerable(Of Range) = GetClassifiedRanges(TextToCompile, LanguageNames.VisualBasic)

        If Not CompileResult?.Success Then
            If Not ResultOfConversion.GetFilteredListOfFailures().Any Then
                ResultOfConversion.ResultStatus = ResultTriState.Success
                If My.Settings.ColorizeOutput Then
                    Colorize(FragmentRange, RichTextBoxConversionOutput, TextToCompile.SplitLines.Length, ResultOfConversion.GetFilteredListOfFailures())
                Else
                    RichTextBoxConversionOutput.Text = ResultOfConversion.ConvertedCode
                End If
            Else
                Colorize(FragmentRange, RichTextBoxConversionOutput, TextToCompile.SplitLines.Length, ResultOfConversion.GetFilteredListOfFailures())
            End If
        Else
            If My.Settings.ColorizeOutput Then
                Colorize(FragmentRange, RichTextBoxConversionOutput, TextToCompile.SplitLines.Length)
            Else
                RichTextBoxConversionOutput.Text = ResultOfConversion.ConvertedCode
            End If
        End If
        Application.DoEvents()
    End Sub

    Private Sub ContextMenuCopy_Click(sender As Object, e As EventArgs) Handles ContextMenuCopy.Click
        CType(ContextMenuStrip1.SourceControl, RichTextBox).Copy()
    End Sub

    Private Sub ContextMenuCut_Click(sender As Object, e As EventArgs) Handles ContextMenuCut.Click
        CType(ContextMenuStrip1.SourceControl, RichTextBox).Cut()
    End Sub

    Private Sub ContextMenuPaste_Click(sender As Object, e As EventArgs) Handles ContextMenuPaste.Click
        CType(ContextMenuStrip1.SourceControl, RichTextBox).Paste()
    End Sub

    Private Function Convert_Compile_Colorize(RequestToConvert As ConvertRequest, OptionalReferences() As MetadataReference, CancelToken As CancellationToken) As Boolean
        ResultOfConversion = ConvertInputRequest(RequestToConvert, OptionalReferences, New ReportProgress(ConversionProgressBar), CancelToken)
        mnuEditSaveAs.Enabled = Me.ResultOfConversion.ResultStatus = ResultTriState.Success
        Select Case ResultOfConversion.ResultStatus
            Case ResultTriState.Success
                Compile_Colorize(ResultOfConversion.ConvertedCode)
                Dim FilteredErrorCount As Integer = ResultOfConversion.GetFilteredListOfFailures().Count
                LabelErrorCount.Text = $"Number of Errors: {FilteredErrorCount}"
                Return FilteredErrorCount = 0
            Case ResultTriState.Failure
                If TypeOf ResultOfConversion.Exceptions(0) IsNot OperationCanceledException Then
                    RichTextBoxConversionOutput.SelectionColor = Color.Red
                    RichTextBoxConversionOutput.Text = GetExceptionsAsString(ResultOfConversion.Exceptions)
                End If
            Case ResultTriState.Ignore
                RichTextBoxConversionOutput.Text = ""
                LabelErrorCount.Text = "File Skipped"
        End Select
        Return ResultOfConversion.ResultStatus <> ResultTriState.Failure
    End Function

    ''' <summary>
    ''' Look in SearchBuffer for text and highlight it
    '''  No error is displayed if not found
    ''' </summary>
    ''' <param name="SearchBuffer"></param>
    ''' <param name="StartLocation"></param>
    ''' <param name="SelectionTextLength"></param>
    ''' <returns>True if found, False is not found</returns>
    Private Function FindTextInBuffer(SearchBuffer As RichTextBox, ByRef StartLocation As Integer, ByRef SelectionTextLength As Integer) As Boolean
        If SelectionTextLength > 0 Then
            SearchBuffer.SelectionBackColor = Color.White
            ' Find the end index. End Index = number of characters in textbox
            ' remove highlight from the search string
            SearchBuffer.Select(StartLocation, SelectionTextLength)
            Application.DoEvents()
        End If
        Dim SearchForward As Boolean = SearchDirection.SelectedIndex = 0
        If StartLocation >= SearchBuffer.Text.Length Then
            StartLocation = If(SearchForward, 0, SearchBuffer.Text.Length - 1)
        End If
#Disable Warning CC0014 ' Use Ternary operator.
        If SearchForward Then
#Enable Warning CC0014 ' Use Ternary operator.
            ' Forward Search
            ' If string was found in the RichTextBox, highlight it
            StartLocation = SearchBuffer.Find(SearchInput.Text, StartLocation, RichTextBoxFinds.None)
        Else
            ' Back Search
            StartLocation = SearchBuffer.Find(SearchInput.Text, StartLocation, RichTextBoxFinds.Reverse)
        End If

        If StartLocation >= 0 Then
            SearchBuffer.ScrollToCaret()
            ' Set the highlight background color as Orange
            SearchBuffer.SelectionBackColor = Color.Orange
            ' Find the end index. End Index = number of characters in textbox
            SelectionTextLength = SearchInput.Text.Length
            ' Highlight the search string
            SearchBuffer.Select(StartLocation, SelectionTextLength)
            ' mark the start position after the position of
            ' last search string
            StartLocation = If(SearchForward, StartLocation + SelectionTextLength, StartLocation - 1)
            Return True
        End If
        StartLocation = If(SearchForward, 0, SearchInput.Text.Length - 1)
        SelectionTextLength = 0
        Return False
    End Function

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles Me.Load
        Dim items(ImageList1.Images.Count - 1) As String
        For i As Integer = 0 To ImageList1.Images.Count - 1
            items(i) = "Item " & i.ToString(Globalization.CultureInfo.InvariantCulture)
        Next
        SearchDirection.Items.AddRange(items)
        SearchDirection.DropDownStyle = ComboBoxStyle.DropDownList
        SearchDirection.DrawMode = DrawMode.OwnerDrawVariable
        SearchDirection.ItemHeight = ImageList1.ImageSize.Height
        SearchDirection.Width = ImageList1.ImageSize.Width + 30
        SearchDirection.MaxDropDownItems = ImageList1.Images.Count
        SearchDirection.SelectedIndex = 0

        PictureBox1.Height = ImageList1.ImageSize.Height + 4
        PictureBox1.Width = ImageList1.ImageSize.Width + 4
        PictureBox1.Top = SearchDirection.Top + 2
        PictureBox1.Left = SearchDirection.Left + 2
        SearchWhere.SelectedIndex = 0

        SplitContainer1.SplitterDistance = SplitContainer1.Height - RichTextBoxErrorList.Height

        ' Load all settings
        If My.Settings.UpgradeRequired Then
            My.Settings.Upgrade()
            My.Settings.UpgradeRequired = False
            My.Settings.Save()
        End If

        mnuOptionsColorizeSource.Checked = My.Settings.ColorizeInput
        mnuOptionsColorizeResult.Checked = My.Settings.ColorizeOutput

        Select Case My.Settings.ConversionDelay
            Case 0
                mnuOptionsDelayBetweenConversions.SelectedIndex = 0
            Case 5
                mnuOptionsDelayBetweenConversions.SelectedIndex = 1
            Case 10
                mnuOptionsDelayBetweenConversions.SelectedIndex = 2
            Case Else
                mnuOptionsDelayBetweenConversions.SelectedIndex = 0
        End Select

        mnuFileSnippetLoadLast.Enabled = File.Exists(SnippetFileWithPath)
        mnuOptionsPauseConvertOnSuccess.Checked = My.Settings.PauseConvertOnSuccess
        mnuOptionsSkipSkipAutoGenerated.Checked = My.Settings.SkipAutoGenerated
        mnuOptionsSkipSkipBinAndObjFolders.Checked = My.Settings.SkipBinAndObjFolders
        mnuOptionsSkipSkipTestResourceFiles.Checked = My.Settings.SkipTestResourceFiles

        mnuOptionsStartFolderConvertFromLastFile.Checked = My.Settings.StartFolderConvertFromLastFile
        mnuViewShowDestinationLineNumbers.Checked = My.Settings.ShowDestinationLineNumbers
        LineNumbers_For_RichTextBoxOutput.Visible = My.Settings.ShowDestinationLineNumbers
        mnuViewShowSourceLineNumbers.Checked = My.Settings.ShowSourceLineNumbers
        LineNumbers_For_RichTextBoxInput.Visible = My.Settings.ShowSourceLineNumbers

        If String.IsNullOrWhiteSpace(My.Settings.DefaultProjectDirectory) Then
            My.Settings.DefaultProjectDirectory = GetLatestVisualStudioProjectPath()
            My.Settings.Save()
            Application.DoEvents()
        End If

        Width = Screen.PrimaryScreen.Bounds.Width
        Height = CInt(Screen.PrimaryScreen.Bounds.Height * 0.95)
        CenterToScreen()
    End Sub

    Private Sub Form1_Resize(sender As Object, e As EventArgs) Handles Me.Resize
        ResizeRichTextBuffers()
    End Sub

    <ExcludeFromCodeCoverage>
    Private Shared Function GetExceptionsAsString(Exceptions As IReadOnlyList(Of Exception)) As String
        If Exceptions Is Nothing OrElse Not Exceptions.Any Then
            Return String.Empty
        End If

        Dim builder As New StringBuilder()
        For i As Integer = 0 To Exceptions.Count - 1
            builder.AppendFormat(Globalization.CultureInfo.InvariantCulture, "----- Exception {0} of {1} -----" & Environment.NewLine, i + 1, Exceptions.Count)
            builder.AppendLine(Exceptions(i).ToString())
        Next i
        Return builder.ToString()
    End Function

    ''' <summary>
    ''' To work with Git we need to create a new folder tree from the parent of this project
    ''' </summary>
    ''' <param name="DirectoryToBeTranslatedWithPath"></param>
    ''' <param name="SourceLanguageExtension"></param>
    ''' <returns>Path to new solution that mirrors the DirectoryToBeTranslated, new solution folder is rename with _SourceLanguageExtension</returns>
    Private Function GetFoldertSavePath(DirectoryToBeTranslatedWithPath As String, SourceLanguageExtension As String, ConvertingProject As Boolean) As String
        Dim TargetLanguageExtension As String = If(SourceLanguageExtension.ToUpperInvariant = "VB", "_cs", "_vb")
        Debug.Assert(Directory.Exists(DirectoryToBeTranslatedWithPath), $"{DirectoryToBeTranslatedWithPath} does Not exist")
        Debug.Assert(Directory.GetDirectoryRoot(DirectoryToBeTranslatedWithPath) <> DirectoryToBeTranslatedWithPath, $"{DirectoryToBeTranslatedWithPath} does Not exist")

        Dim SolutionRoot As String = DirectoryToBeTranslatedWithPath
        Dim CurrentDirectory As String = SolutionRoot
        Dim SystemtRootDirectory As String = Directory.GetDirectoryRoot(CurrentDirectory)

        While SystemtRootDirectory <> CurrentDirectory
            If Directory.GetFiles(CurrentDirectory, "*.sln").Any OrElse Directory.GetFiles(CurrentDirectory, "*.gitignore").Any Then
                If ConvertingProject Then
                    SolutionRoot = CurrentDirectory
                    Exit While
                Else
                    SolutionRoot = Directory.GetParent(CurrentDirectory).FullName
                End If
            End If
            CurrentDirectory = Directory.GetParent(CurrentDirectory).FullName
        End While
        ' At this point Solution Directory is the remainder of the path from SolutionRoot
        Dim PathFromSolutionRoot As List(Of String) = DirectoryToBeTranslatedWithPath.Replace(SolutionRoot, "", StringComparison.InvariantCultureIgnoreCase) _
                                                                                     .Trim(Path.DirectorySeparatorChar) _
                                                                                     .Split(Path.DirectorySeparatorChar).ToList
        SolutionRoot = $"{SolutionRoot}{Path.DirectorySeparatorChar}{PathFromSolutionRoot(0)}{TargetLanguageExtension}"
        PathFromSolutionRoot.RemoveAt(0)
        If File.Exists(SolutionRoot) Then
            MsgBox($"A file exists at {SolutionRoot} this Is a fatal error the program will exit", MsgBoxStyle.OkOnly And MsgBoxStyle.Critical, "Fatal Error")
            Close()
            End
        End If
        If Directory.Exists(SolutionRoot) Then
            Select Case MsgBox($"The converted project will be save to {SolutionRoot} a directory which already exists. To use it And overwrite existing files select Yes. Selecting No will delete existing content, Selecting Cancel will stop conversion. , ", MsgBoxStyle.YesNoCancel, "Target Directory Save Options")
                Case MsgBoxResult.Cancel
                    Return String.Empty
                Case MsgBoxResult.No
                    If MsgBoxResult.Yes = MsgBox($"Are you sure you want to delete {SolutionRoot}?", MsgBoxStyle.YesNo Or MsgBoxStyle.Critical, "Warning Deleting Directory") Then
                        Directory.Delete(SolutionRoot, recursive:=True)
                        Directory.CreateDirectory(SolutionRoot)
                    End If
                Case MsgBoxResult.Yes
                Case Else
                    Stop
            End Select
        End If
        Return CreateDirectoryIfNonexistent(Path.Combine(SolutionRoot, PathFromSolutionRoot.Join(Path.DirectorySeparatorChar)))
    End Function

    Private Sub LineNumbers_For_RichTextBoxInput_Resize(sender As Object, e As EventArgs) Handles LineNumbers_For_RichTextBoxInput.Resize
        ResizeRichTextBuffers()
    End Sub

    ' Don't save state here only if user changes
    Private Sub LineNumbers_For_RichTextBoxInput_VisibleChanged(sender As Object, e As EventArgs) Handles LineNumbers_For_RichTextBoxInput.VisibleChanged
        mnuViewShowSourceLineNumbers.Checked = LineNumbers_For_RichTextBoxInput.Visible
        ResizeRichTextBuffers()
    End Sub

    Private Sub LineNumbers_For_RichTextBoxOutput_Resize(sender As Object, e As EventArgs) Handles LineNumbers_For_RichTextBoxOutput.Resize
        ResizeRichTextBuffers()
    End Sub

    ' Don't save state here only if user changes
    Private Sub LineNumbers_For_RichTextBoxOutput_VisibleChanged(sender As Object, e As EventArgs) Handles LineNumbers_For_RichTextBoxOutput.VisibleChanged
        mnuViewShowDestinationLineNumbers.Checked = LineNumbers_For_RichTextBoxOutput.Visible
        ResizeRichTextBuffers()
    End Sub

    Private Function LoadInputBufferFromStream(LanguageExtension As String, fileStream As Stream) As Integer
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=True)
        Dim SourceText As String = GetFileTextFromStream(fileStream)
        Dim InputLines As Integer
        Dim ConversionInputLinesArray() As String = SourceText.SplitLines
        InputLines = ConversionInputLinesArray.Length
        If mnuOptionsColorizeSource.Checked Then
            Colorize(GetClassifiedRanges(SourceCode:=ConversionInputLinesArray.Join(vbCrLf), Language:=If(LanguageExtension = "vb", LanguageNames.VisualBasic, LanguageNames.CSharp)), ConversionBuffer:=RichTextBoxConversionInput, Lines:=InputLines)
        Else
            RichTextBoxConversionInput.Text = ConversionInputLinesArray.Join(vbCrLf)
        End If
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=False)
        Return InputLines
    End Function

    Private Sub mnuCompile_Click(sender As Object, e As EventArgs) Handles mnuCompile.Click
        LineNumbers_For_RichTextBoxInput.Visible = False
        LineNumbers_For_RichTextBoxOutput.Visible = False

        If String.IsNullOrWhiteSpace(RichTextBoxConversionOutput.Text) Then
            Exit Sub
        End If
        RichTextBoxErrorList.Text = ""
        Compile_Colorize(RichTextBoxConversionOutput.Text)
    End Sub

    Private Sub mnuConvertConvertSnippet_Click(sender As Object, e As EventArgs) Handles mnuConvertConvertSnippet.Click
        SetButtonStopAndCursor(MeForm:=Me, StopButton:=ButtonStop, StopButtonVisible:=True)
        RichTextBoxErrorList.Text = ""
        RichTextBoxFileList.Text = ""
        LineNumbers_For_RichTextBoxOutput.Visible = False
        ResizeRichTextBuffers()
        If CTS IsNot Nothing Then
            CTS.Dispose()
        End If
        CTS = New CancellationTokenSource
        RequestToConvert = New ConvertRequest(My.Settings.SkipAutoGenerated, New ReportProgress(ConversionProgressBar), CTS.Token) With {
            .SourceCode = RichTextBoxConversionInput.Text
        }

        Convert_Compile_Colorize(RequestToConvert, CSharpReferences(Assembly.Load("System.Windows.Forms").Location, Nothing).ToArray, CTS.Token)
        If RequestToConvert.CancelToken.IsCancellationRequested Then
            MsgBox($"Conversion canceled.", MsgBoxStyle.Information, Title:="C# to VB")
            ConversionProgressBar.Value = 0
        End If
        mnuConvertConvertFolder.Enabled = True
        SetButtonStopAndCursor(MeForm:=Me, StopButton:=ButtonStop, StopButtonVisible:=False)
    End Sub

    Private Sub mnuConvertFolder_Click(sender As Object, e As EventArgs) Handles mnuConvertConvertFolder.Click
        LineNumbers_For_RichTextBoxInput.Visible = False
        LineNumbers_For_RichTextBoxOutput.Visible = False
        Dim SourceFolderName As String
        Dim ProjectSavePath As String
        Using OFD As New OpenFolderDialog
            With OFD
                .Description = "Select folder to convert..."
                .FolderMustExist = True
                .InitialFolder = My.Settings.DefaultProjectDirectory
                .ShowNewFolderButton = False
                If .ShowDialog(Me) <> DialogResult.OK Then
                    Return
                End If
                SourceFolderName = .SelectedPath
                ProjectSavePath = GetFoldertSavePath((.SelectedPath), "cs", ConvertingProject:=False)
            End With
            If Not Directory.Exists(SourceFolderName) Then
                MsgBox($"{SourceFolderName} is not a directory.", Title:="C# to VB")
                Exit Sub
            End If
            If String.IsNullOrWhiteSpace(ProjectSavePath) Then
                MsgBox($"Conversion aborted.", Title:="C# to VB")
                Exit Sub
            End If
            Dim LastFileNameWithPath As String = If(My.Settings.StartFolderConvertFromLastFile, My.Settings.MRU_Data.Last, "")
            Dim FilesProcessed As Long = 0L
            CTS = New CancellationTokenSource
            If ProcessAllFiles(SourceFolderName,
                                ProjectSavePath,
                                LastFileNameWithPath,
                                "cs",
                                FilesProcessed,
                                CTS.Token
                                ) Then
                If CTS.Token.IsCancellationRequested Then
                    MsgBox($"Conversion canceled, {FilesProcessed} files completed successfully.", Title:="C# to VB")
                Else
                    MsgBox($"Conversion completed, {FilesProcessed} files completed successfully.", Title:="C# to VB")
                End If
            Else
                MsgBox($"Conversion stopped.", Title:="C# to VB")
            End If
        End Using

    End Sub

    Private Sub mnuEditCopy_Click(sender As Object, e As EventArgs) Handles mnuEditCopy.Click
        CurrentBuffer.Copy()
    End Sub

    Private Sub mnuEditCut_Click(sender As Object, e As EventArgs) Handles mnuEditCut.Click
        CurrentBuffer.Cut()
    End Sub

    Private Sub mnuEditFind_Click(sender As Object, e As EventArgs) Handles mnuEditFind.Click
        SearchBoxVisibility(Visible:=True)
    End Sub

    Private Sub mnuEditPaste_Click(sender As Object, e As EventArgs) Handles mnuEditPaste.Click
        RichTextBoxConversionInput.SelectedText = Clipboard.GetText(TextDataFormat.Text)
    End Sub

    Private Sub mnuEditSaveAs_Click(sender As Object, e As EventArgs) Handles mnuEditSaveAs.Click

        SaveFileDialog1.AddExtension = True
        SaveFileDialog1.CreatePrompt = False
        SaveFileDialog1.DefaultExt = "vb"
        SaveFileDialog1.FileName = Path.ChangeExtension(OpenFileDialog1.SafeFileName, "vb")
        SaveFileDialog1.Filter = "VB Code Files (*.vb)|*.vb"
        SaveFileDialog1.FilterIndex = 0
        SaveFileDialog1.OverwritePrompt = True
        SaveFileDialog1.SupportMultiDottedExtensions = False
        SaveFileDialog1.Title = $"Save vb Output..."
        SaveFileDialog1.ValidateNames = True
        Dim FileSaveResult As DialogResult = SaveFileDialog1.ShowDialog
        If FileSaveResult = DialogResult.OK Then
            RichTextBoxConversionOutput.SaveFile(SaveFileDialog1.FileName, RichTextBoxStreamType.PlainText)
        End If
    End Sub

    Private Sub mnuEditUndo_Click(sender As Object, e As EventArgs) Handles mnuEditUndo.Click
        CurrentBuffer.Undo()
    End Sub

    Private Sub mnuFileExit_Click(sender As Object, e As EventArgs) Handles mnuFileExit.Click
        Close()
        End
    End Sub

    Private Sub mnu_MRUList_Click(sender As Object, e As EventArgs)
        ' open the file...
        OpenFile(DirectCast(sender, ToolStripItem).Tag.ToString().Substring(4), "cs")
    End Sub

    Private Sub mnu_MRUList_MouseDown(sender As Object, e As MouseEventArgs)
        If e.Button = Windows.Forms.MouseButtons.Right Then
            Clipboard.SetText(text:=CType(sender, ToolStripMenuItem).Text)
        End If
    End Sub

    Private Sub mnuFileLastFolder_Click(sender As Object, e As EventArgs) Handles mnuFileLastFolder.Click
        Dim FolderName As String = CType(sender, ToolStripMenuItem).Text
        If Directory.Exists(FolderName) Then
            Dim SourceLanguageExtension As String = "cs"
            Dim ProjectSavePath As String = GetFoldertSavePath(FolderName, SourceLanguageExtension, ConvertingProject:=False)
            ' This path is a directory.
            Dim LastFileNameWithPath As String = If(My.Settings.StartFolderConvertFromLastFile, My.Settings.MRU_Data.Last, "")
            Dim FilesProcessed As Long = 0
            If CTS IsNot Nothing Then
                CTS.Dispose()
            End If
            CTS = New CancellationTokenSource
            If ProcessAllFiles(FolderName, ProjectSavePath, LastFileNameWithPath, SourceLanguageExtension, FilesProcessed, CTS.Token) Then
                MsgBox($"Conversion completed.")
            End If
        Else
            MsgBox($"{FolderName} Is Not a directory.")
        End If
    End Sub

    Private Sub mnuFileOpen_Click(sender As Object, e As EventArgs) Handles mnuFileOpen.Click
        Dim LanguageExtension As String = "cs"
        With OpenFileDialog1
            .AddExtension = True
            .DefaultExt = LanguageExtension
            .FileName = ""
            .Filter = If(LanguageExtension = "vb", "VB Code Files (*.vb)|*.vb", "C# Code Files (*.cs)|*.cs")
            SaveFileDialog1.FilterIndex = 0
            .Multiselect = False
            .ReadOnlyChecked = True
            .Title = $"Open {LanguageExtension.ToUpperInvariant} Source file"
            .ValidateNames = True
            If .ShowDialog = DialogResult.OK Then
                mnuConvertConvertFolder.Enabled = False
                OpenFile(OpenFileDialog1.FileName, LanguageExtension)
            Else
                mnuConvertConvertFolder.Enabled = True
            End If
        End With
    End Sub

    Private Sub mnuFileOpenProject_Click(sender As Object, e As EventArgs) Handles mnuFileOpenProject.Click
        With OpenFileDialog1
            .AddExtension = True
            .DefaultExt = "cs"
            .FileName = ""
            .Filter = "C# Project File (*.csproj)|*.csproj"
            .FilterIndex = 0
            .Multiselect = False
            .ReadOnlyChecked = True
            .Title = "Open cs Project file"
            .ValidateNames = True
            ' InputLines is used for future progress bar
            If .ShowDialog = DialogResult.OK Then
                Dim ProjectSavePath As String = GetFoldertSavePath(Path.GetDirectoryName(.FileName), "cs", ConvertingProject:=True)
                mnuConvertConvertFolder.Enabled = True
                If MSBuildInstance Is Nothing Then
                    Dim VS_Selector As New VSSelectorDialog
                    If VS_Selector.ShowDialog(Me) <> DialogResult.OK Then
                        Stop
                    End If
                    Console.WriteLine($"Using MSBuild at '{VS_Selector.MSBuildInstance.MSBuildPath}' to load projects.")
                    ' NOTE: Be sure to register an instance with the MSBuildLocator
                    '       before calling MSBuildWorkspace.Create()
                    '       otherwise, MSBuildWorkspace won't MEF compose.
                    MSBuildInstance = VS_Selector.MSBuildInstance
                    MSBuildLocator.RegisterInstance(MSBuildInstance)
                    VS_Selector.Dispose()
                End If

                Using Workspace As MSBuildWorkspace = MSBuildWorkspace.Create()
                    AddHandler Workspace.WorkspaceFailed, AddressOf MSBuildWorkspaceFailed
                    Dim currentProject As Project = Workspace.OpenProjectAsync(.FileName).Result
                    Workspace.LoadMetadataForReferencedProjects = True
                    If currentProject.HasDocuments Then
                        If CTS IsNot Nothing Then
                            CTS.Dispose()
                        End If
                        CTS = New CancellationTokenSource
                        Dim References() As MetadataReference = SharedReferences.CSharpReferences(Assembly.Load("System.Windows.Forms").Location, currentProject.MetadataReferences).ToArray
                        Dim xmlDoc As New XmlDocument With {
                            .PreserveWhitespace = True
                        }
                        xmlDoc.Load(currentProject.FilePath)
                        Dim root As XmlNode = xmlDoc.FirstChild
                        If root.Attributes.Count > 0 AndAlso root.Attributes(0).Name.Equals("SDK", StringComparison.InvariantCultureIgnoreCase) Then
                            ConvertProjectFile(ProjectSavePath, currentProject, xmlDoc, root)
                        End If

                        RichTextBoxErrorList.Text = ""
                        RichTextBoxFileList.Text = ""
                        SetButtonStopAndCursor(Me, ButtonStop, StopButtonVisible:=True)
                        Dim FilesProcessed As Integer = 0
                        Dim TotalFilesToProcess As Integer = currentProject.Documents.Count
                        For Each document As Document In currentProject.Documents
                            If ParseCSharpSource(document.GetTextAsync(Nothing).Result.ToString).GetRoot.SyntaxTree.IsGeneratedCode(Function(t As SyntaxTrivia) As Boolean
                                                                                                                                        Return t.IsComment OrElse t.IsRegularOrDocComment
                                                                                                                                    End Function, CTS.Token) Then
                                TotalFilesToProcess -= 1
                                FilesConversionProgress.Text = $"Processed {FilesProcessed: N0} of {TotalFilesToProcess:N0} Files"
                                Application.DoEvents()
                                Continue For
                            Else
                                FilesProcessed += 1
                                RichTextBoxFileList.AppendText($"{FilesProcessed.ToString(Globalization.CultureInfo.InvariantCulture).PadLeft(5)} {document.FilePath}{vbCrLf}")
                                RichTextBoxFileList.Select(RichTextBoxFileList.TextLength, 0)
                                RichTextBoxFileList.ScrollToCaret()
                                FilesConversionProgress.Text = $"Processed {FilesProcessed:N0} of {TotalFilesToProcess:N0} Files"
                                Application.DoEvents()
                            End If
                            Dim TargetLanguageExtension As String = If("cs" = "cs", "vb", "cs")
                            If Not ProcessFile(document.FilePath, ConvertSourceFileToDestinationFile(Path.GetDirectoryName(.FileName), ProjectSavePath, document), "cs", References, CTS.Token) _
                                OrElse RequestToConvert.CancelToken.IsCancellationRequested Then
                                Exit For
                            End If
                        Next document
                    End If
                End Using
                SetButtonStopAndCursor(MeForm:=Me, StopButton:=ButtonStop, StopButtonVisible:=False)
            Else
                mnuConvertConvertFolder.Enabled = True
            End If
        End With
    End Sub

    Private Shared Sub ConvertProjectFile(ProjectSavePath As String, currentProject As Project, xmlDoc As XmlDocument, root As XmlNode)
        Dim IsDesktopProject As Boolean = root.Attributes(0).Value = "Microsoft.NET.Sdk.WindowsDesktop"
        If root.Attributes(0).Value = "Microsoft.NET.Sdk" OrElse IsDesktopProject Then
            Dim FoundUseWindowsFormsWpf As Boolean = False
            Dim FrameworkReferenceNodeIndex As Integer = -1
            Dim PropertyGroupIndex As Integer = 0
            Dim LeadingXMLSpace As XmlNode = xmlDoc.CreateDocumentFragment()
            LeadingXMLSpace.InnerXml = "    "
            If root.HasChildNodes Then
                Dim RemoveNode As XmlNode = Nothing
                For i As Integer = 0 To root.ChildNodes.Count - 1
                    Dim RootChildNode As XmlNode = root.ChildNodes(i)
                    Select Case RootChildNode.Name
                        Case "PropertyGroup"
                            PropertyGroupIndex = i
                            For J As Integer = 0 To RootChildNode.ChildNodes.Count - 1
                                Dim PropertyGroupChildNode As XmlNode = RootChildNode.ChildNodes(J)
                                Select Case PropertyGroupChildNode.Name
                                    Case "OutputType"
                                        ' Ignore
                                    Case "TargetFramework"
                                        ' Ignore
                                    Case "RootNamespace"
                                        ' Ignore
                                    Case "AssemblyName"
                                        ' Ignore
                                    Case "GenerateAssemblyInfo"
                                        ' Ignore
                                    Case "UseWindowsForms"
                                        FoundUseWindowsFormsWpf = True
                                    Case "UseWpf"
                                        FoundUseWindowsFormsWpf = True
                                    Case "#whitespace"
                                        ' Capture leading space
                                        If LeadingXMLSpace.InnerXml.Length = 0 Then
                                            LeadingXMLSpace.InnerXml = PropertyGroupChildNode.InnerXml
                                        End If
                                    Case "#comment"
                                        root.ChildNodes(i).ChildNodes(J).Value = PropertyGroupChildNode.Value.Replace(".cs", ".vb", StringComparison.InvariantCultureIgnoreCase)
                                    Case Else
                                        Stop
                                End Select
                            Next J
                        Case "ItemGroup"
                            For J As Integer = 0 To RootChildNode.ChildNodes.Count - 1
                                Dim xmlNode As XmlNode = root.ChildNodes(i).ChildNodes(J)
                                Select Case RootChildNode.ChildNodes(J).Name
                                    Case "FrameworkReference"
                                        If RootChildNode.ChildNodes.Count = 3 AndAlso
                                            RootChildNode.ChildNodes(0).Name = "#whitespace" AndAlso
                                            RootChildNode.ChildNodes(0).Name = "#whitespace" Then
                                            FrameworkReferenceNodeIndex = i
                                        Else
                                            RemoveNode = xmlNode
                                        End If
                                    Case "#whitespace"
                                                            ' Ignore
                                    Case "#comment"
                                        root.ChildNodes(i).ChildNodes(J).Value = xmlNode.Value.Replace(".cs", ".vb", StringComparison.InvariantCultureIgnoreCase)
                                    Case "Compile"
                                        Dim CompileValue As String = ""
                                        For k As Integer = 0 To xmlNode.Attributes.Count - 1
                                            CompileValue = xmlNode.Attributes(k).Value
                                            root.ChildNodes(i).ChildNodes(J).Attributes(k).Value = ChangeExtension(CompileValue, "cs", "vb")
                                        Next k
                                        For k As Integer = 0 To xmlNode.ChildNodes.Count - 1
                                            Select Case root.ChildNodes(i).ChildNodes(J).ChildNodes(k).Name
                                                Case "DependentUpon"
                                                    Dim DependentUponNodeValue As String = xmlNode.ChildNodes(k).ChildNodes(0).Value
                                                    If DependentUponNodeValue.EndsWith(".cs", StringComparison.InvariantCultureIgnoreCase) Then
                                                        root.ChildNodes(i).ChildNodes(J).ChildNodes(k).ChildNodes(0).Value = ChangeExtension(DependentUponNodeValue, "cs", "vb")
                                                    Else
                                                        CopyFile(ProjectSavePath, currentProject, Path.Combine(Path.GetDirectoryName(CompileValue), DependentUponNodeValue))
                                                    End If
                                                Case "#whitespace"
                                                    ' Ignore
                                                Case Else
                                                    Stop
                                            End Select
                                        Next k
                                    Case "EmbeddedResource"
                                        If xmlNode.Attributes(0).Value.EndsWith(".resx", StringComparison.InvariantCultureIgnoreCase) Then
                                            CopyFile(ProjectSavePath, currentProject, xmlNode.Attributes(0).Value)
                                        End If
                                        For k As Integer = 0 To xmlNode.ChildNodes.Count - 1
                                            Select Case xmlNode.ChildNodes(k).Name
                                                Case "DependentUpon"
                                                    For l As Integer = 0 To xmlNode.ChildNodes(k).ChildNodes.Count - 1
                                                        root.ChildNodes(i).ChildNodes(J).ChildNodes(k).ChildNodes(l).Value = ChangeExtension(xmlNode.ChildNodes(k).ChildNodes(l).Value, "cs", "vb")
                                                    Next l
                                                Case "#whitespace"
                                                    ' Ignore
                                                Case Else
                                                    Stop
                                            End Select
                                        Next k
                                    Case "PackageReference"
                                        ' Ignore
                                    Case "Content"
                                        If xmlNode.Attributes(0).Name.ToUpperInvariant = "INCLUDE" Then
                                            Dim SourceFileName As String = Path.Combine((New FileInfo(currentProject.FilePath)).Directory.FullName, xmlNode.Attributes(0).Value)
                                            If File.Exists(SourceFileName) Then
                                                File.Copy(SourceFileName, Path.Combine(ProjectSavePath, xmlNode.Attributes(0).Value), overwrite:=True)
                                            Else
                                                If (Path.GetExtension(SourceFileName).ToUpperInvariant = ".TXT") Then
                                                    Dim NewValue As String = ChangeExtension(xmlNode.Attributes(0).Value, "TXT", "md")
                                                    SourceFileName = Path.ChangeExtension(SourceFileName, "md")
                                                    If File.Exists(SourceFileName) Then
                                                        File.Copy(SourceFileName, Path.Combine(ProjectSavePath, NewValue), overwrite:=True)
                                                        root.ChildNodes(i).ChildNodes(J).Attributes(0).Value = NewValue
                                                        xmlNode.Attributes(0).Value = NewValue
                                                    End If
                                                End If
                                            End If
                                        End If
                                    Case Else
                                        Stop
                                End Select
                            Next J
                        Case "#whitespace"
                        Case "#comment"
                            root.ChildNodes(i).Value = RootChildNode.Value.Replace(".cs", ".vb", StringComparison.InvariantCultureIgnoreCase)
                        Case Else
                            Stop
                    End Select
                Next i
                If RemoveNode IsNot Nothing Then
                    RemoveNode.RemoveAll()
                End If
            End If
            If FrameworkReferenceNodeIndex >= 0 Then
                root.Attributes(0).Value = "Microsoft.NET.Sdk.WindowsDesktop"
                root.ChildNodes(FrameworkReferenceNodeIndex).RemoveAll()
            End If
            If Not FoundUseWindowsFormsWpf Then
                Dim xmlDocFragment As XmlDocumentFragment = xmlDoc.CreateDocumentFragment()
                xmlDocFragment.InnerXml = "<UseWindowsForms>true</UseWindowsForms>"                                                                '<UseWindowsForms>true</UseWindowsForms>
                root.ChildNodes(PropertyGroupIndex).AppendChild(LeadingXMLSpace)
                root.ChildNodes(PropertyGroupIndex).AppendChild(xmlDocFragment)
            End If

            xmlDoc.Save(Path.Combine(ProjectSavePath, New FileInfo(currentProject.FilePath).Name.Replace(".csproj", "_VB.vbproj", StringComparison.InvariantCultureIgnoreCase)))
        End If
    End Sub

    Private Shared Sub CopyFile(ProjectSavePath As String, currentProject As Project, PartialPathWithFileName As String)
        Dim DestFileNameWithPath As String = Path.Combine(ProjectSavePath, PartialPathWithFileName)
        Directory.CreateDirectory(Path.GetDirectoryName(DestFileNameWithPath))
        File.Copy(Path.Combine((New FileInfo(currentProject.FilePath)).Directory.FullName, PartialPathWithFileName), DestFileNameWithPath, overwrite:=True)
    End Sub

    Private Shared Function ChangeExtension(AttributeValue As String, OldExtension As String, NewExtension As String) As String
        If AttributeValue.EndsWith($".{OldExtension}", StringComparison.InvariantCultureIgnoreCase) Then
            Return Path.ChangeExtension(AttributeValue, NewExtension)
        End If
        Return AttributeValue
    End Function

    Private Sub mnuFileSnippetLoadLast_Click(sender As Object, e As EventArgs) Handles mnuFileSnippetLoadLast.Click
        If My.Settings.ColorizeInput Then
            mnuConvertConvertSnippet.Enabled = 0 <> LoadInputBufferFromStream("CS", File.OpenRead(path:=SnippetFileWithPath))
        Else
            RichTextBoxConversionInput.LoadFile(SnippetFileWithPath, RichTextBoxStreamType.PlainText)
        End If
        mnuCompile.Enabled = True
    End Sub

    Private Sub mnuFileSnippetSave_Click(sender As Object, e As EventArgs) Handles mnuFileSnippetSave.Click
        If RichTextBoxConversionInput.TextLength = 0 Then
            Exit Sub
        End If
        RichTextBoxConversionInput.SaveFile(SnippetFileWithPath, RichTextBoxStreamType.PlainText)
    End Sub

    Private Sub mnuFileSnippett_Click(sender As Object, e As EventArgs) Handles mnuFileSnippet.Click
        If Not File.Exists(SnippetFileWithPath) Then
            Exit Sub
        End If
    End Sub

    Private Sub mnuOptionsAddFilesToIgnoreFilesEithErrorsList_Click(sender As Object, e As EventArgs) Handles mnuOptionsAddFilesToIgnoreFilesEithErrorsList.Click
        Dim SourceFileNameWithPath As String = My.Settings.MRU_Data.Last
        If Not My.Settings.IgnoreFileList.Contains(SourceFileNameWithPath) Then
            My.Settings.IgnoreFileList.Add(SourceFileNameWithPath)
            My.Settings.Save()
        End If
    End Sub

    Private Sub mnuOptionsAdvanced_Click(sender As Object, e As EventArgs) Handles mnuOptionsAdvanced.Click
        Using o As New OptionsDialog
            Dim r As DialogResult = o.ShowDialog(Me)
        End Using
    End Sub

    Private Sub mnuOptionsColorizeResult_Click(sender As Object, e As EventArgs) Handles mnuOptionsColorizeResult.Click
        My.Settings.ColorizeOutput = mnuOptionsColorizeResult.Checked
        My.Settings.Save()
        Application.DoEvents()
    End Sub

    Private Sub mnuOptionsColorizeSource_Click(sender As Object, e As EventArgs) Handles mnuOptionsColorizeSource.Click
        My.Settings.ColorizeInput = mnuOptionsColorizeSource.Checked
        My.Settings.Save()
        Application.DoEvents()
    End Sub

    Private Sub mnuOptionsDelayBetweenConversions_SelectedIndexChanged(sender As Object, e As EventArgs) Handles mnuOptionsDelayBetweenConversions.SelectedIndexChanged
        Select Case mnuOptionsDelayBetweenConversions.Text.Substring("Delay Between Conversions = ".Length)
            Case "None"
                If My.Settings.ConversionDelay <> 0 Then
                    My.Settings.ConversionDelay = 0
                End If
            Case "5 Seconds"
                If My.Settings.ConversionDelay <> 5 Then
                    My.Settings.ConversionDelay = 5
                End If
            Case "10 Seconds"
                If My.Settings.ConversionDelay <> 10 Then
                    My.Settings.ConversionDelay = 10
                End If
            Case Else
                Exit Sub
        End Select
        My.Settings.Save()
        Application.DoEvents()
    End Sub

    Private Sub mnuOptionsEditIgnoreFilesWithErrorsList_Click(sender As Object, e As EventArgs) Handles mnuOptionsEditIgnoreFilesWithErrorsList.Click
        Dim IgnoreFilesWithErrorsDialog As New IgnoreFilesWithErrorsList
        IgnoreFilesWithErrorsDialog.ShowDialog(Me)
        If Not String.IsNullOrWhiteSpace(IgnoreFilesWithErrorsDialog.FileToLoad) Then
            mnuConvertConvertFolder.Enabled = False
            OpenFile(IgnoreFilesWithErrorsDialog.FileToLoad, "cs")
        End If
        IgnoreFilesWithErrorsDialog.Dispose()
    End Sub

    Private Sub mnuOptionsPauseConvertOnSuccess_Click(sender As Object, e As EventArgs) Handles mnuOptionsPauseConvertOnSuccess.Click
        My.Settings.PauseConvertOnSuccess = mnuOptionsPauseConvertOnSuccess.Checked
        My.Settings.Save()
        Application.DoEvents()
    End Sub

    Private Sub mnuOptionsSkipSkipAutoGenerated_Click(sender As Object, e As EventArgs) Handles mnuOptionsSkipSkipAutoGenerated.Click
        My.Settings.SkipAutoGenerated = CType(sender, ToolStripMenuItem).Checked
        My.Settings.Save()
        Application.DoEvents()
    End Sub

    Private Sub mnuOptionsSkipSkipBinAndObjFolders_Click(sender As Object, e As EventArgs) Handles mnuOptionsSkipSkipBinAndObjFolders.Click
        My.Settings.SkipBinAndObjFolders = CType(sender, ToolStripMenuItem).Checked
        My.Settings.Save()
        Application.DoEvents()
    End Sub

    Private Sub mnuOptionsSkipSkipTestResourceFiles_Click(sender As Object, e As EventArgs) Handles mnuOptionsSkipSkipTestResourceFiles.Click
        My.Settings.SkipTestResourceFiles = CType(sender, ToolStripMenuItem).Checked
        My.Settings.Save()
        Application.DoEvents()
    End Sub

    Private Sub mnuOptionsStartFolderConvertFromLastFile_Click(sender As Object, e As EventArgs) Handles mnuOptionsStartFolderConvertFromLastFile.Click
        My.Settings.StartFolderConvertFromLastFile = CType(sender, ToolStripMenuItem).Checked
        My.Settings.Save()
        Application.DoEvents()
    End Sub

    Private Sub mnuViewShowDestinationLineNumbers_Click(sender As Object, e As EventArgs) Handles mnuViewShowDestinationLineNumbers.Click
        LineNumbers_For_RichTextBoxOutput.Visible = CType(sender, ToolStripMenuItem).Checked
        My.Settings.ShowDestinationLineNumbers = mnuViewShowDestinationLineNumbers.Checked
        My.Settings.Save()
    End Sub

    Private Sub mnuViewShowSourceLineNumbers_Click(sender As Object, e As EventArgs) Handles mnuViewShowSourceLineNumbers.Click
        LineNumbers_For_RichTextBoxInput.Visible = mnuViewShowSourceLineNumbers.Checked
        My.Settings.ShowSourceLineNumbers = mnuViewShowSourceLineNumbers.Checked
        My.Settings.Save()
        ResizeRichTextBuffers()
    End Sub

    Private Sub MRU_AddTo(Path As String)
        StatusStripCurrentFileName.Text = Path
        ' remove the item from the collection if exists so that we can
        ' re-add it to the beginning...
        If My.Settings.MRU_Data.Contains(Path) Then
            My.Settings.MRU_Data.Remove(Path)
        End If
        ' add to MRU list..
        My.Settings.MRU_Data.Add(Path)
        ' make sure there are only ever 5 items...
        While My.Settings.MRU_Data.Count > 5
            My.Settings.MRU_Data.RemoveAt(0)
        End While
        ' update UI..
        MRU_Update()
    End Sub

    Private Sub MRU_Update()
        ' clear MRU menu items...
        Dim clsItems As New List(Of ToolStripItem)
        ' create a temporary collection containing every MRU menu item
        ' (identified by the tag text when added to the list)...
        For Each clsMenu As ToolStripItem In mnuFile.DropDownItems
            If Not clsMenu.Tag Is Nothing Then
                If (clsMenu.Tag.ToString().StartsWith("MRU:", StringComparison.InvariantCulture)) Then
                    clsItems.Add(clsMenu)
                End If
            End If
        Next
        ' iterate through list and remove each from menu...
        For Each clsMenu As ToolStripItem In clsItems
            RemoveHandler clsMenu.Click, AddressOf mnu_MRUList_Click
            RemoveHandler clsMenu.MouseDown, AddressOf mnu_MRUList_MouseDown
            mnuFile.DropDownItems.Remove(clsMenu)
        Next
        ' display items (in reverse order so the most recent is on top)...
        For iCounter As Integer = My.Settings.MRU_Data.Count - 1 To 0 Step -1
            Dim sPath As String = My.Settings.MRU_Data(iCounter)
            ' create new ToolStripItem, displaying the name of the file...
            ' set the tag - identifies the ToolStripItem as an MRU item and
            ' contains the full path so it can be opened later...
            Dim clsItem As New ToolStripMenuItem(sPath) With {
                .Tag = "MRU:" & sPath
            }
            ' hook into the click event handler so we can open the file later...
            AddHandler clsItem.Click, AddressOf mnu_MRUList_Click
            AddHandler clsItem.MouseDown, AddressOf mnu_MRUList_MouseDown
            ' insert into DropDownItems list...
            mnuFile.DropDownItems.Insert(mnuFile.DropDownItems.Count - 11, clsItem)
        Next
        ' show separator...
        My.Settings.Save()
        If My.Settings.MRU_Data.Count > 0 Then
            mnuFileLastFolder.Text = IO.Path.GetDirectoryName(My.Settings.MRU_Data.Last)
            mnuFileLastFolder.Visible = True
            mnuFileSep1.Visible = True
            mnuFolderSep1.Visible = True
        Else
            mnuFileLastFolder.Visible = False
            mnuFileSep1.Visible = False
            mnuFolderSep1.Visible = False
        End If

    End Sub

    ' Print message for WorkspaceFailed event to help diagnosing project load failures.
    Private Sub MSBuildWorkspaceFailed(_1 As Object, e1 As WorkspaceDiagnosticEventArgs)
        If MsgBox(e1.Diagnostic.Message, MsgBoxStyle.AbortRetryIgnore, "MSBuild Failed") = MsgBoxResult.Abort Then
            End
        End If
    End Sub

    Private Sub OpenFile(FileNameWithPath As String, LanguageExtension As String)
        Using myFileStream As FileStream = File.OpenRead(path:=FileNameWithPath)
            mnuConvertConvertSnippet.Enabled = LoadInputBufferFromStream(LanguageExtension, myFileStream) <> 0
        End Using
        MRU_AddTo(FileNameWithPath)
    End Sub

    Private Sub PictureBox1_Click(sender As Object, e As EventArgs) Handles PictureBox1.Click
        If String.IsNullOrWhiteSpace(SearchInput.Text) Then
            Exit Sub
        End If
        Select Case SearchWhere.SelectedIndex
            Case 0
                If Not FindTextInBuffer(RichTextBoxConversionInput, RichTextBoxConversionInput.SelectionStart, RichTextBoxConversionInput.SelectionLength) Then
                    MsgBox($"'{SearchInput.Text}' not found in Source Buffer!", MsgBoxStyle.OkOnly, "Not Found!")
                End If
            Case 1
                If Not FindTextInBuffer(RichTextBoxConversionOutput, RichTextBoxConversionOutput.SelectionStart, RichTextBoxConversionOutput.SelectionLength) Then
                    MsgBox($"'{SearchInput.Text}' not found Conversion Buffer!", MsgBoxStyle.OkOnly, "Not Found!")
                End If
            Case 2
                If Not FindTextInBuffer(RichTextBoxConversionInput, RichTextBoxConversionInput.SelectionStart, RichTextBoxConversionInput.SelectionLength) Then
                    MsgBox($"'{SearchInput.Text}' not found in Source Buffer!", MsgBoxStyle.OkOnly, "Not Found!")
                End If
                If Not FindTextInBuffer(RichTextBoxConversionOutput, RichTextBoxConversionOutput.SelectionStart, RichTextBoxConversionOutput.SelectionLength) Then
                    MsgBox($"'{SearchInput.Text}' not found Conversion Buffer!", MsgBoxStyle.OkOnly, "Not Found!")
                End If
        End Select
    End Sub

    Private Sub PictureBox1_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseDown
        PictureBox1.BorderStyle = BorderStyle.Fixed3D
    End Sub

    Private Sub PictureBox1_MouseUp(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseUp
        PictureBox1.BorderStyle = BorderStyle.FixedSingle
    End Sub

    ''' <summary>
    ''' Process all files in the directory passed in, recurse on any directories
    ''' that are found, and process the files they contain.
    ''' </summary>
    ''' <param name="SourceDirectory">Start location of where to process directories</param>
    ''' <param name="TargetDirectory">Start location of where to process directories</param>
    ''' <param name="LastFileNameWithPath">Pass Last File Name to Start Conversion where you left off</param>
    ''' <param name="SourceLanguageExtension">vb or cs</param>
    ''' <param name="FilesProcessed">Count of the number of tiles processed</param>
    ''' <returns>
    ''' False if error and user wants to stop, True if success or user wants to ignore error
    ''' </returns>
    Private Function ProcessAllFiles(SourceDirectory As String, TargetDirectory As String, LastFileNameWithPath As String, SourceLanguageExtension As String, ByRef FilesProcessed As Long, CancelToken As CancellationToken) As Boolean
        Try
            RichTextBoxErrorList.Text = ""
            RichTextBoxFileList.Text = ""
            SetButtonStopAndCursor(Me, ButtonStop, StopButtonVisible:=True)
            Dim TotalFilesToProcess As Long = GetFileCount(SourceDirectory, SourceLanguageExtension, My.Settings.SkipBinAndObjFolders, My.Settings.SkipTestResourceFiles)
            ' Process the list of files found in the directory.
            Return ProcessDirectory(SourceDirectory, TargetDirectory, MeForm:=Me, ButtonStop, RichTextBoxFileList, LastFileNameWithPath, SourceLanguageExtension, FilesProcessed, TotalFilesToProcess, AddressOf ProcessFile, CancelToken)
        Catch ex As OperationCanceledException
            ConversionProgressBar.Value = 0
        Catch ex As Exception
            ' don't crash on exit
            Stop
        Finally
            SetButtonStopAndCursor(Me, ButtonStop, StopButtonVisible:=False)
        End Try
        Return False
    End Function

    ''' <summary>
    ''' Convert one file
    ''' </summary>
    ''' <param name="SourceFileNameWithPath">Complete path including file name to file to be converted</param>
    ''' <param name="TargetDirectory">Complete path up to File to be converted</param>
    ''' <param name="SourceLanguageExtension">vb or cs</param>
    ''' <returns>False if error and user wants to stop, True if success or user wants to ignore error.</returns>
    Private Function ProcessFile(SourceFileNameWithPath As String, TargetDirectory As String, SourceLanguageExtension As String, OptionalReferences() As MetadataReference, CancelToken As CancellationToken) As Boolean
        If My.Settings.IgnoreFileList.Contains(SourceFileNameWithPath) Then
            Return True
        End If
        ButtonStop.Visible = True
        RichTextBoxConversionOutput.Text = ""
        MRU_AddTo(SourceFileNameWithPath)
        Dim fsRead As FileStream = File.OpenRead(SourceFileNameWithPath)
        Dim lines As Integer = LoadInputBufferFromStream(SourceLanguageExtension, fsRead)
        If lines > 0 Then
            RequestToConvert = New ConvertRequest(My.Settings.SkipAutoGenerated, New ReportProgress(ConversionProgressBar), CTS.Token) With {
                .SourceCode = RichTextBoxConversionInput.Text
            }
            If Not Convert_Compile_Colorize(RequestToConvert, OptionalReferences, CancelToken) Then
                If RequestToConvert.CancelToken.IsCancellationRequested Then
                    ConversionProgressBar.Value = 0
                    Return False
                End If
                Select Case MsgBox($"Conversion failed, do you want to stop processing this file automatically in the future? Yes and No will continue processing files, Cancel will stop conversions!", MsgBoxStyle.YesNoCancel)
                    Case MsgBoxResult.Cancel
                        Return False
                    Case MsgBoxResult.No
                        Return True
                    Case MsgBoxResult.Yes
                        If Not My.Settings.IgnoreFileList.Contains(SourceFileNameWithPath) Then
                            My.Settings.IgnoreFileList.Add(SourceFileNameWithPath)
                            My.Settings.Save()
                            RichTextBoxErrorList.Text = ""
                            LineNumbers_For_RichTextBoxInput.Visible = My.Settings.ShowSourceLineNumbers
                            LineNumbers_For_RichTextBoxOutput.Visible = My.Settings.ShowDestinationLineNumbers
                        End If
                        Return True
                End Select
            Else
                If Not String.IsNullOrWhiteSpace(TargetDirectory) Then
                    If Not RequestToConvert.CancelToken.IsCancellationRequested Then
                        Dim NewFileName As String = Path.ChangeExtension(New FileInfo(SourceFileNameWithPath).Name, If(SourceLanguageExtension = "vb", "cs", "vb"))
                        WriteTextToStream(TargetDirectory, NewFileName, RichTextBoxConversionOutput.Text)
                    Else
                        Return False
                    End If
                End If
                If My.Settings.PauseConvertOnSuccess Then
                    If MsgBox($"{SourceFileNameWithPath} successfully converted, Continue?", MsgBoxStyle.MsgBoxSetForeground Or MsgBoxStyle.YesNo) = MsgBoxResult.No Then
                        Return False
                    End If
                End If
            End If
            ' 5 second delay
            Const LoopSleep As Integer = 25
            Dim Delay As Integer = CInt((1000 * My.Settings.ConversionDelay) / LoopSleep)
            For i As Integer = 0 To Delay
                Application.DoEvents()
                Threading.Thread.Sleep(LoopSleep)
                If CancelToken.IsCancellationRequested Then
                    Return False

                End If
            Next
            Application.DoEvents()
        Else
            RichTextBoxConversionOutput.Clear()
        End If
        Return True
    End Function

    Private Sub ResizeRichTextBuffers()
        Dim LineNumberInputWidth As Integer = If(LineNumbers_For_RichTextBoxInput.Visible, LineNumbers_For_RichTextBoxInput.Width, 0)
        Dim LineNumberOutputWidth As Integer = If(LineNumbers_For_RichTextBoxOutput.Visible, LineNumbers_For_RichTextBoxOutput.Width, 0)

        RichTextBoxConversionInput.Width = CInt((ClientSize.Width / 2 + 0.5)) - LineNumberInputWidth
        RichTextBoxFileList.Width = CInt(ClientSize.Width / 2 + 0.5)

        RichTextBoxConversionOutput.Width = ClientSize.Width - (RichTextBoxConversionInput.Width + LineNumberInputWidth + LineNumberOutputWidth)
        RichTextBoxConversionOutput.Left = RichTextBoxConversionInput.Width + LineNumberInputWidth + LineNumberOutputWidth

        Dim HalfClientWidth As Integer = CInt(ClientSize.Width / 2)
        RichTextBoxErrorList.Left = HalfClientWidth
        RichTextBoxErrorList.Width = HalfClientWidth
        StatusStripCurrentFileName.Width = HalfClientWidth

    End Sub

    Private Sub RichTexBoxErrorList_DoubleClick(sender As Object, e As EventArgs) Handles RichTextBoxErrorList.DoubleClick
        If RTFLineStart > 0 AndAlso RichTextBoxConversionOutput.SelectionStart <> RTFLineStart Then
            RichTextBoxConversionOutput.Select(RTFLineStart, 0)
            RichTextBoxConversionOutput.ScrollToCaret()
        End If
    End Sub

    Private Sub RichTexBoxErrorList_MouseDown(sender As Object, e As MouseEventArgs) Handles RichTextBoxErrorList.MouseDown
        Dim box As RichTextBox = DirectCast(sender, RichTextBox)
        If box.TextLength = 0 Then
            Exit Sub
        End If
        Dim index As Integer = box.GetCharIndexFromPosition(e.Location)
        Dim line As Integer = box.GetLineFromCharIndex(index)
        Dim lineStart As Integer = box.GetFirstCharIndexFromLine(line)
        Dim AfterEquals As Integer = box.GetFirstCharIndexFromLine(line) + 15
        Dim LineText As String = box.Text.Substring(box.GetFirstCharIndexFromLine(line))
        If Not LineText.StartsWith("BC", StringComparison.InvariantCulture) Then
            Exit Sub
        End If
        If Not LineText.Contains(" Line = ", StringComparison.InvariantCulture) Then
            Exit Sub
        End If
        Dim NumberCount As Integer = LineText.Substring(15).IndexOf(" ", StringComparison.InvariantCulture)
        Dim ErrorLine As Integer = CInt(Val(box.Text.Substring(AfterEquals, NumberCount)))
        If ErrorLine <= 0 Then
            Exit Sub
        End If
        RTFLineStart = RichTextBoxConversionOutput.GetFirstCharIndexFromLine(ErrorLine - 1)
    End Sub

    Private Sub RichTextBoxConversionInput_Enter(sender As Object, e As EventArgs) Handles RichTextBoxConversionInput.Enter
        CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub RichTextBoxConversionInput_MouseEnter(sender As Object, e As EventArgs) Handles RichTextBoxConversionInput.MouseEnter
        CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub RichTextBoxConversionInput_TextChanged(sender As Object, e As EventArgs) Handles RichTextBoxConversionInput.TextChanged
        Dim InputBufferInUse As Boolean = CType(sender, RichTextBox).TextLength > 0
        mnuFileSnippetSave.Enabled = InputBufferInUse
        mnuConvertConvertSnippet.Enabled = InputBufferInUse
        mnuConvertConvertFolder.Enabled = InputBufferInUse
    End Sub

    Private Sub RichTextBoxConversionOutput_HorizScrollBarRightClicked(sender As Object, loc As Point) Handles RichTextBoxConversionOutput.HorizScrollBarRightClicked
        Dim p As Integer = CType(sender, AdvancedRTB).HScrollPos
        MsgBox($"Horizontal Right Clicked At: {loc.ToString}, HScrollPos = {p}")
    End Sub

    Private Sub RichTextBoxConversionOutput_MouseEnter(sender As Object, e As EventArgs) Handles RichTextBoxConversionOutput.MouseEnter
        CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub RichTextBoxConversionOutput_TextChanged(sender As Object, e As EventArgs) Handles RichTextBoxConversionOutput.TextChanged
        Dim OutputBufferInUse As Boolean = CType(sender, RichTextBox).TextLength > 0
        mnuCompile.Enabled = OutputBufferInUse
    End Sub

    Private Sub RichTextBoxConversionOutput_VertScrollBarRightClicked(sender As Object, loc As Point) Handles RichTextBoxConversionOutput.VertScrollBarRightClicked
        Dim p As Integer = CType(sender, AdvancedRTB).VScrollPos
        MsgBox($"Vertical Right Clicked At: {loc.ToString}, VScrollPos = {p} ")
    End Sub

    Private Sub RichTextBoxErrorList_Enter(sender As Object, e As EventArgs) Handles RichTextBoxErrorList.Enter
        CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub RichTextBoxErrorList_MouseEnter(sender As Object, e As EventArgs) Handles RichTextBoxErrorList.MouseEnter
        CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub RichTextBoxFileList_Enter(sender As Object, e As EventArgs) Handles RichTextBoxFileList.Enter
        CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub RichTextBoxFileList_MouseEnter(sender As Object, e As EventArgs) Handles RichTextBoxFileList.MouseEnter
        CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub SearchBoxVisibility(Visible As Boolean)
        SearchDirection.Visible = Visible
        PictureBox1.Visible = Visible
        SearchInput.Visible = Visible
    End Sub

    Private Sub SearchDirection_DrawItem(sender As Object, e As DrawItemEventArgs) Handles SearchDirection.DrawItem
        If e.Index <> -1 Then
            e.Graphics.DrawImage(ImageList1.Images(e.Index), e.Bounds.Left, e.Bounds.Top)
        End If
    End Sub

    Private Sub SearchDirection_MeasureItem(sender As Object, e As MeasureItemEventArgs) Handles SearchDirection.MeasureItem
        e.ItemHeight = ImageList1.ImageSize.Height
        e.ItemWidth = ImageList1.ImageSize.Width
    End Sub

    Private Sub SearchDirection_SelectedIndexChanged(sender As Object, e As EventArgs) Handles SearchDirection.SelectedIndexChanged
        PictureBox1.Image = ImageList1.Images(SearchDirection.SelectedIndex)
    End Sub

    Private Sub SearchInput_TextChanged(sender As Object, e As EventArgs) Handles SearchInput.TextChanged
        RichTextBoxConversionInput.SelectionStart = 0
        RichTextBoxConversionInput.SelectionLength = 0
        RichTextBoxConversionOutput.SelectionStart = 0
        RichTextBoxConversionOutput.SelectionLength = 0
    End Sub

    Private Sub SplitContainer1_SplitterMoved(sender As Object, e As SplitterEventArgs) Handles SplitContainer1.SplitterMoved
        RichTextBoxFileList.Height = SplitContainer1.Panel2.Height - StatusStrip1.Height
        RichTextBoxErrorList.Height = SplitContainer1.Panel2.Height - StatusStrip1.Height
    End Sub

    Private Sub StatusStripCurrentFileName_MouseDown(sender As Object, e As MouseEventArgs) Handles StatusStripCurrentFileName.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Right Then
            Clipboard.SetText(text:=CType(sender, ToolStripStatusLabel).Text)
        End If
    End Sub

    Protected Overrides Sub OnLoad(e As EventArgs)
        SetStyle(ControlStyles.AllPaintingInWmPaint Or ControlStyles.UserPaint Or ControlStyles.DoubleBuffer, True)
        ' enable events...
        MyBase.OnLoad(e)
        If My.Settings.IgnoreFileList Is Nothing Then
            My.Settings.IgnoreFileList = New Specialized.StringCollection
        End If

        ' load MRU...
        If My.Settings.MRU_Data Is Nothing Then
            My.Settings.MRU_Data = New Specialized.StringCollection
        End If
        ' display MRU if there are any items to display...
        MRU_Update()
    End Sub

    Private Sub mnuConvert_Click(sender As Object, e As EventArgs) Handles mnuConvert.Click
        mnuConvertConvertSnippet.Enabled = RichTextBoxConversionInput.TextLength > 0
    End Sub

End Class
