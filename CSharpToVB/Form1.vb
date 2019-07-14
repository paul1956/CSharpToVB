Option Explicit On
Option Infer Off
Option Strict On

Imports System.Diagnostics.CodeAnalysis
Imports System.IO
Imports System.Text

Imports CSharpToVBApp

Imports IVisualBasicCode.CodeConverter
Imports IVisualBasicCode.CodeConverter.ConversionResult
Imports IVisualBasicCode.CodeConverter.Util

Imports ManageProgressBar

Imports Microsoft.Build.Locator
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Emit
Imports Microsoft.CodeAnalysis.MSBuild
Imports Microsoft.VisualBasic.FileIO

#If NETCOREAPP3_0 Then
Imports VBMsgBox
#End If

Public Class Form1

#If NETCOREAPP3_0 Then
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
            Return Me._CurrentBuffer
        End Get
        Set(value As RichTextBox)
            Me._CurrentBuffer = value
            If value IsNot Nothing Then
                Me._CurrentBuffer.Focus()
            End If
        End Set
    End Property

    Property MSBuildInstance As VisualStudioInstance = Nothing
    Property StopRequested As Boolean = False

    Private Shared Function ConvertSourceFileToDestinationFile(ProjectDirectory As String, ProjectSavePath As String, DocumentName As Document) As String
        If ProjectSavePath.IsEmptyNullOrWhitespace Then
            Return String.Empty
        End If
        Dim SubPathFromProject As String = Path.GetDirectoryName(DocumentName.FilePath).Replace(ProjectDirectory, "").Trim("\"c)
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

    Private Sub ButtonSearch_Click(sender As Object, e As EventArgs) Handles ButtonSearch.Click
        Me.SearchBoxVisibility(Visible:=False)
    End Sub

    Private Sub ButtonStop_Click(sender As Object, e As EventArgs) Handles ButtonStop.Click
        Me.StopRequested = True
        Me.ButtonStop.Visible = False
        Me.mnuConvertConvertSnippet.Enabled = False
        Application.DoEvents()
    End Sub

    Private Sub ButtonStop_MouseEnter(sender As Object, e As EventArgs) Handles ButtonStop.MouseEnter
        LocalUseWaitCutsor(MeForm:=Me, Enable:=False)
    End Sub

    Private Sub ButtonStop_MouseLeave(sender As Object, e As EventArgs) Handles ButtonStop.MouseLeave
        LocalUseWaitCutsor(MeForm:=Me, Enable:=Not Me.StopRequested)
    End Sub

    Private Sub Colorize(FragmentRange As IEnumerable(Of Range), ConversionBuffer As RichTextBox, Lines As Integer, Optional failures As IEnumerable(Of Diagnostic) = Nothing)
        Try ' Prevent crash when exiting
            If failures IsNot Nothing Then
                For Each dia As Diagnostic In failures
                    Me.RichTextBoxErrorList.AppendText($"{dia.Id} Line = {dia.Location.GetLineSpan.StartLinePosition.Line + 1} {dia.GetMessage}")
                    Me.RichTextBoxErrorList.AppendText(vbCrLf)
                Next
            End If
            Dim Progress As New ReportProgress(Me.ConversionProgressBar)
            Progress.SetTotalItems(Lines)

            With ConversionBuffer
                .Clear()
                .Select(.TextLength, 0)
                For Each range As Range In FragmentRange
                    .Select(.TextLength, 0)
                    .SelectionColor = ColorSelector.GetColorFromName(range.ClassificationType)
                    .AppendText(range.Text)
                    If range.Text.Contains(vbLf) Then
                        Progress.UpdateProgress(range.Text.Count(CType(vbLf, Char)))
                        Application.DoEvents()
                    End If
                    If Me.StopRequested Then
                        Me.mnuConvertConvertSnippet.Enabled = True
                        Application.DoEvents()
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
                Me.LineNumbers_For_RichTextBoxInput.Visible = True
                Me.LineNumbers_For_RichTextBoxOutput.Visible = True
            End If
        Catch ex As Exception
            Stop
        End Try
        Me.ConversionProgressBar.Value = 0
    End Sub

    Private Sub Compile_Colorize(TextToCompile As String)
        Dim CompileResult As EmitResult = CompileVisualBasicString(StringToBeCompiled:=TextToCompile, ErrorsToBeIgnored, DiagnosticSeverity.Error, Me.ResultOfConversion)

        Me.LabelErrorCount.Text = $"Number of Errors: {Me.ResultOfConversion.FilteredListOfFailures.Count}"
        Dim FragmentRange As IEnumerable(Of Range) = GetClassifiedRanges(TextToCompile, LanguageNames.VisualBasic)

        If Not CompileResult?.Success Then
            If Me.ResultOfConversion.FilteredListOfFailures.Count = 0 Then
                Me.ResultOfConversion.ResultStatus = ResultTriState.Success
                If My.Settings.ColorizeOutput Then
                    Me.Colorize(FragmentRange, Me.RichTextBoxConversionOutput, TextToCompile.SplitLines.Length, Me.ResultOfConversion.FilteredListOfFailures)
                Else
                    Me.RichTextBoxConversionOutput.Text = Me.ResultOfConversion.ConvertedCode
                End If
            Else
                Me.Colorize(FragmentRange, Me.RichTextBoxConversionOutput, TextToCompile.SplitLines.Length, Me.ResultOfConversion.FilteredListOfFailures)
            End If
        Else
            If My.Settings.ColorizeOutput Then
                Me.Colorize(FragmentRange, Me.RichTextBoxConversionOutput, TextToCompile.SplitLines.Length)
            Else
                Me.RichTextBoxConversionOutput.Text = Me.ResultOfConversion.ConvertedCode
            End If
        End If
        Application.DoEvents()
    End Sub

    Private Sub ContextMenuCopy_Click(sender As Object, e As EventArgs) Handles ContextMenuCopy.Click
        CType(Me.ContextMenuStrip1.SourceControl, RichTextBox).Copy()
    End Sub

    Private Sub ContextMenuCut_Click(sender As Object, e As EventArgs) Handles ContextMenuCut.Click
        CType(Me.ContextMenuStrip1.SourceControl, RichTextBox).Cut()
    End Sub

    Private Sub ContextMenuPaste_Click(sender As Object, e As EventArgs) Handles ContextMenuPaste.Click
        CType(Me.ContextMenuStrip1.SourceControl, RichTextBox).Paste()
    End Sub

    Private Function Convert_Compile_Colorize(RequestToConvert As ConvertRequest, OptionalReferences() As MetadataReference) As Boolean
        Me.ResultOfConversion = ConvertInputRequest(RequestToConvert, OptionalReferences)
        Me.mnuEditSaveAs.Enabled = Me.ResultOfConversion.ResultStatus = ResultTriState.Success
        Select Case Me.ResultOfConversion.ResultStatus
            Case ResultTriState.Success
                Me.Compile_Colorize(Me.ResultOfConversion.ConvertedCode)
                Dim FilteredErrorCount As Integer = Me.ResultOfConversion.FilteredListOfFailures.Count
                Me.LabelErrorCount.Text = $"Number of Errors: {FilteredErrorCount}"
                Return FilteredErrorCount = 0
            Case ResultTriState.Failure
                Me.RichTextBoxConversionOutput.SelectionColor = Color.Red
                Me.RichTextBoxConversionOutput.Text = Me.GetExceptionsAsString(Me.ResultOfConversion.Exceptions)
            Case ResultTriState.Ignore
                Me.RichTextBoxConversionOutput.Text = ""
                Me.LabelErrorCount.Text = "File Skipped"
        End Select
        Return Me.ResultOfConversion.ResultStatus <> ResultTriState.Failure
    End Function

    ' Do not remove, this is part of initialization and could be used for future support of VB to C#
    Private Sub CSharp2VB_CheckedChanged(sender As Object, e As EventArgs) Handles CSharp2VB.CheckedChanged
        Me.RichTextBoxConversionInput.Text = ""
        Me.RichTextBoxConversionOutput.Text = ""
        Dim Progress As New ReportProgress(Me.ConversionProgressBar)
        If Me.CSharp2VB.Checked Then
            Me.VB2CSharp.Checked = False
            Me.RequestToConvert = New ConvertRequest(ConvertRequest.CS_To_VB, My.Settings.SkipAutoGenerated, AddressOf Application.DoEvents, Progress)
            Me.Text = "Convert C# To Visual Basic"
        Else
            Me.VB2CSharp.Checked = True
            Me.RequestToConvert = New ConvertRequest(ConvertRequest.VB_To_CS, My.Settings.SkipAutoGenerated, AddressOf Application.DoEvents, Progress)
            Me.Text = "Convert Visual Basic to C#"
        End If
    End Sub

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
        Dim SearchForward As Boolean = Me.SearchDirection.SelectedIndex = 0
        If StartLocation >= SearchBuffer.Text.Length Then
            StartLocation = If(SearchForward, 0, SearchBuffer.Text.Length - 1)
        End If
#Disable Warning CC0014 ' Use Ternary operator.
        If SearchForward Then
#Enable Warning CC0014 ' Use Ternary operator.
            ' Forward Search
            ' If string was found in the RichTextBox, highlight it
            StartLocation = SearchBuffer.Find(Me.SearchInput.Text, StartLocation, RichTextBoxFinds.None)
        Else
            ' Back Search
            StartLocation = SearchBuffer.Find(Me.SearchInput.Text, StartLocation, RichTextBoxFinds.Reverse)
        End If

        If StartLocation >= 0 Then
            SearchBuffer.ScrollToCaret()
            ' Set the highlight background color as Orange
            SearchBuffer.SelectionBackColor = Color.Orange
            ' Find the end index. End Index = number of characters in textbox
            SelectionTextLength = Me.SearchInput.Text.Length
            ' Highlight the search string
            SearchBuffer.Select(StartLocation, SelectionTextLength)
            ' mark the start position after the position of
            ' last search string
            StartLocation = If(SearchForward, StartLocation + SelectionTextLength, StartLocation - 1)
            Return True
        End If
        StartLocation = If(SearchForward, 0, Me.SearchInput.Text.Length - 1)
        SelectionTextLength = 0
        Return False
    End Function

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles Me.Load
        Dim items(Me.ImageList1.Images.Count - 1) As String
        For i As Integer = 0 To Me.ImageList1.Images.Count - 1
            items(i) = "Item " & i.ToString
        Next
        Me.SearchDirection.Items.AddRange(items)
        Me.SearchDirection.DropDownStyle = ComboBoxStyle.DropDownList
        Me.SearchDirection.DrawMode = DrawMode.OwnerDrawVariable
        Me.SearchDirection.ItemHeight = Me.ImageList1.ImageSize.Height
        Me.SearchDirection.Width = Me.ImageList1.ImageSize.Width + 30
        Me.SearchDirection.MaxDropDownItems = Me.ImageList1.Images.Count
        Me.SearchDirection.SelectedIndex = 0

        Me.PictureBox1.Height = Me.ImageList1.ImageSize.Height + 4
        Me.PictureBox1.Width = Me.ImageList1.ImageSize.Width + 4
        Me.PictureBox1.Top = Me.SearchDirection.Top + 2
        Me.PictureBox1.Left = Me.SearchDirection.Left + 2
        Me.SearchWhere.SelectedIndex = 0

        Me.SplitContainer1.SplitterDistance = Me.SplitContainer1.Height - Me.RichTextBoxErrorList.Height

        ' Load all settings
        If My.Settings.UpgradeRequired Then
            My.Settings.Upgrade()
            My.Settings.UpgradeRequired = False
            My.Settings.Save()
        End If

        Me.mnuOptionsColorizeSource.Checked = My.Settings.ColorizeInput
        Me.mnuOptionsColorizeResult.Checked = My.Settings.ColorizeOutput

        Select Case My.Settings.ConversionDelay
            Case 0
                Me.mnuOptionsDelayBetweenConversions.SelectedIndex = 0
            Case 5
                Me.mnuOptionsDelayBetweenConversions.SelectedIndex = 1
            Case 10
                Me.mnuOptionsDelayBetweenConversions.SelectedIndex = 2
            Case Else
                Me.mnuOptionsDelayBetweenConversions.SelectedIndex = 0
        End Select

        Me.mnuFileSnippetLoadLast.Enabled = File.Exists(SnippetFileWithPath)
        Me.mnuOptionsPauseConvertOnSuccess.Checked = My.Settings.PauseConvertOnSuccess
        Me.mnuOptionsSkipSkipAutoGenerated.Checked = My.Settings.SkipAutoGenerated
        Me.mnuOptionsSkipSkipBinAndObjFolders.Checked = My.Settings.SkipBinAndObjFolders
        Me.mnuOptionsSkipSkipTestResourceFiles.Checked = My.Settings.SkipTestResourceFiles

        Me.mnuOptionsStartFolderConvertFromLastFile.Checked = My.Settings.StartFolderConvertFromLastFile
        Me.mnuViewShowDestinationLineNumbers.Checked = My.Settings.ShowDestinationLineNumbers
        Me.LineNumbers_For_RichTextBoxOutput.Visible = My.Settings.ShowDestinationLineNumbers
        Me.mnuViewShowSourceLineNumbers.Checked = My.Settings.ShowSourceLineNumbers
        Me.LineNumbers_For_RichTextBoxInput.Visible = My.Settings.ShowSourceLineNumbers

        If My.Settings.DefaultProjectDirectory.IsEmptyNullOrWhitespace Then
            My.Settings.DefaultProjectDirectory = GetLatestVisualStudioProjectPath()
            My.Settings.Save()
            Application.DoEvents()
        End If

        Me.Width = Screen.PrimaryScreen.Bounds.Width
        Me.Height = CInt(Screen.PrimaryScreen.Bounds.Height * 0.95)
        Me.CenterToScreen()
    End Sub

    Private Sub Form1_Resize(sender As Object, e As EventArgs) Handles Me.Resize
        Me.ResizeRichTextBuffers()
    End Sub

    <ExcludeFromCodeCoverage>
    Private Function GetExceptionsAsString(Exceptions As IReadOnlyList(Of Exception)) As String
        If Exceptions Is Nothing OrElse Exceptions.Count = 0 Then
            Return String.Empty
        End If

        Dim builder As New StringBuilder()
        For i As Integer = 0 To Exceptions.Count - 1
            builder.AppendFormat("----- Exception {0} of {1} -----" & Environment.NewLine, i + 1, Exceptions.Count)
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
    Private Function GetFoldertSavePath(DirectoryToBeTranslatedWithPath As String, SourceLanguageExtension As String) As String
        Dim TargetLanguageExtension As String = If(SourceLanguageExtension.ToLower = "vb", "_cs", "_vb")
        Debug.Assert(Directory.Exists(DirectoryToBeTranslatedWithPath), $"{DirectoryToBeTranslatedWithPath} does Not exist")
        Debug.Assert(Directory.GetDirectoryRoot(DirectoryToBeTranslatedWithPath) <> DirectoryToBeTranslatedWithPath, $"{DirectoryToBeTranslatedWithPath} does Not exist")

        Dim SolutionRoot As String = DirectoryToBeTranslatedWithPath
        Dim CurrentDirectory As String = SolutionRoot
        Dim SystemtRootDirectory As String = Directory.GetDirectoryRoot(CurrentDirectory)

        While SystemtRootDirectory <> CurrentDirectory
            If Directory.GetFiles(CurrentDirectory, "*.sln").Count > 0 OrElse Directory.GetFiles(CurrentDirectory, "*.gitignore").Count > 0 Then
                SolutionRoot = Directory.GetParent(CurrentDirectory).FullName
            End If
            CurrentDirectory = Directory.GetParent(CurrentDirectory).FullName
        End While
        ' At this point Solution Directory is the remainder of the path from SolutionRoot
        Dim PathFromSolutionRoot As List(Of String) = DirectoryToBeTranslatedWithPath.Replace(SolutionRoot, "").Trim(Path.DirectorySeparatorChar).Split(Path.DirectorySeparatorChar).ToList
        SolutionRoot = $"{SolutionRoot}{Path.DirectorySeparatorChar}{PathFromSolutionRoot(0)}{TargetLanguageExtension}"
        PathFromSolutionRoot.RemoveAt(0)
        If File.Exists(SolutionRoot) Then
            MsgBox($"A file exists at {SolutionRoot} this Is a fatal error the program will exit", MsgBoxStyle.OkOnly And MsgBoxStyle.Critical, "Fatal Error")
            Me.Close()
#If NETCOREAPP3_0 Then
            Environment.Exit(0)
#Else
            End
#End If
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
        If PathFromSolutionRoot.Count = 0 Then
            CreateDirectoryIfNonexistent(SolutionRoot)
        End If
        Return CreateDirectoryIfNonexistent(Path.Combine(SolutionRoot, PathFromSolutionRoot.Join(Path.DirectorySeparatorChar)))
    End Function

    Private Sub LineNumbers_For_RichTextBoxInput_Resize(sender As Object, e As EventArgs) Handles LineNumbers_For_RichTextBoxInput.Resize
        Me.ResizeRichTextBuffers()
    End Sub

    ' Don't save state here only if user changes
    Private Sub LineNumbers_For_RichTextBoxInput_VisibleChanged(sender As Object, e As EventArgs) Handles LineNumbers_For_RichTextBoxInput.VisibleChanged
        Me.mnuViewShowSourceLineNumbers.Checked = Me.LineNumbers_For_RichTextBoxInput.Visible
        Me.ResizeRichTextBuffers()
    End Sub

    Private Sub LineNumbers_For_RichTextBoxOutput_Resize(sender As Object, e As EventArgs) Handles LineNumbers_For_RichTextBoxOutput.Resize
        Me.ResizeRichTextBuffers()
    End Sub

    ' Don't save state here only if user changes
    Private Sub LineNumbers_For_RichTextBoxOutput_VisibleChanged(sender As Object, e As EventArgs) Handles LineNumbers_For_RichTextBoxOutput.VisibleChanged
        Me.mnuViewShowDestinationLineNumbers.Checked = Me.LineNumbers_For_RichTextBoxOutput.Visible
        Me.ResizeRichTextBuffers()
    End Sub

    Private Function LoadInputBufferFromStream(LanguageExtension As String, fileStream As Stream) As Integer
        Me.StopRequested = False
        LocalUseWaitCutsor(MeForm:=Me, Enable:=True)
        Dim SourceText As String = GetFileTextFromStream(fileStream)
        Dim InputLines As Integer
        Dim ConversionInputLinesArray() As String = SourceText.SplitLines
        InputLines = ConversionInputLinesArray.Length
        If Me.mnuOptionsColorizeSource.Checked Then
            Me.Colorize(GetClassifiedRanges(SourceCode:=ConversionInputLinesArray.Join(vbCrLf), Language:=If(LanguageExtension = "vb", LanguageNames.VisualBasic, LanguageNames.CSharp)), ConversionBuffer:=Me.RichTextBoxConversionInput, Lines:=InputLines)
        Else
            Me.RichTextBoxConversionInput.Text = ConversionInputLinesArray.Join(vbCrLf)
        End If
        LocalUseWaitCutsor(MeForm:=Me, Enable:=False)
        Return InputLines
    End Function

    Private Sub mnuCompile_Click(sender As Object, e As EventArgs) Handles mnuCompile.Click
        Me.LineNumbers_For_RichTextBoxInput.Visible = False
        Me.LineNumbers_For_RichTextBoxOutput.Visible = False

        If Me.RichTextBoxConversionOutput.Text.IsEmptyNullOrWhitespace Then
            Exit Sub
        End If
        Me.RichTextBoxErrorList.Text = ""
        Me.Compile_Colorize(Me.RichTextBoxConversionOutput.Text)
    End Sub

    Private Sub mnuConvertConvertSnippet_Click(sender As Object, e As EventArgs) Handles mnuConvertConvertSnippet.Click
        SetButtonStopAndCursor(MeForm:=Me, StopButton:=Me.ButtonStop, StopButtonVisible:=True)
        Me.RichTextBoxErrorList.Text = ""
        Me.RichTextBoxFileList.Text = ""
        Me.LineNumbers_For_RichTextBoxOutput.Visible = False
        Me.ResizeRichTextBuffers()
        Me.RequestToConvert.SourceCode = Me.RichTextBoxConversionInput.Text
        Me.Convert_Compile_Colorize(Me.RequestToConvert, VisualBasicReferences.ToArray)
        Me.mnuConvertConvertFolder.Enabled = True
        SetButtonStopAndCursor(MeForm:=Me, StopButton:=Me.ButtonStop, StopButtonVisible:=False)
    End Sub

    Private Sub mnuConvertFolder_Click(sender As Object, e As EventArgs) Handles mnuConvertConvertFolder.Click
        Me.LineNumbers_For_RichTextBoxInput.Visible = False
        Me.LineNumbers_For_RichTextBoxOutput.Visible = False
        Using OFD As New OpenFolderDialog
            With OFD
                .Description = "Select folder to convert..."
                .FolderMustExist = True
                .InitialFolder = My.Settings.DefaultProjectDirectory
                .ShowNewFolderButton = False
                If .ShowDialog(Me) = DialogResult.OK Then
                    Dim SourceFolderName As String = .SelectedPath
                    If Directory.Exists(SourceFolderName) Then
                        Dim SourceLanguageExtension As String = Me.RequestToConvert.GetSourceExtension
                        Dim ProjectSavePath As String = Me.GetFoldertSavePath((.SelectedPath), SourceLanguageExtension)
                        If ProjectSavePath = "" Then
                            MsgBox($"Conversion aborted.", Title:="C# to VB")
                            Exit Sub
                        End If
                        Dim LastFileNameWithPath As String = If(My.Settings.StartFolderConvertFromLastFile, My.Settings.MRU_Data.Last, "")
                        Dim FilesProcessed As Long = 0L
                        If Me.ProcessAllFiles(SourceFolderName,
                                            ProjectSavePath,
                                            LastFileNameWithPath,
                                            SourceLanguageExtension,
                                            FilesProcessed
                                            ) Then
                            MsgBox($"Conversion completed, {FilesProcessed} files completed successfully.", Title:="C# to VB")
                        Else
                            MsgBox($"Conversion stopped.", Title:="C# to VB")
                        End If
                    Else
                        MsgBox($"{SourceFolderName} is not a directory.", Title:="C# to VB")
                    End If
                End If
            End With
        End Using

    End Sub

    Private Sub mnuEditCopy_Click(sender As Object, e As EventArgs) Handles mnuEditCopy.Click
        Me.CurrentBuffer.Copy()
    End Sub

    Private Sub mnuEditCut_Click(sender As Object, e As EventArgs) Handles mnuEditCut.Click
        Me.CurrentBuffer.Cut()
    End Sub

    Private Sub mnuEditFind_Click(sender As Object, e As EventArgs) Handles mnuEditFind.Click
        Me.SearchBoxVisibility(Visible:=True)
    End Sub

    Private Sub mnuEditPaste_Click(sender As Object, e As EventArgs) Handles mnuEditPaste.Click
        Me.RichTextBoxConversionInput.SelectedText = Clipboard.GetText(TextDataFormat.Text)
    End Sub

    Private Sub mnuEditSaveAs_Click(sender As Object, e As EventArgs) Handles mnuEditSaveAs.Click
        Dim Extension As String = Me.RequestToConvert.GetTargetExtension

        Me.SaveFileDialog1.AddExtension = True
        Me.SaveFileDialog1.CreatePrompt = False
        Me.SaveFileDialog1.DefaultExt = Extension
        Me.SaveFileDialog1.FileName = Path.ChangeExtension(Me.OpenFileDialog1.SafeFileName, Extension)
        Me.SaveFileDialog1.Filter = If(Extension = "vb", "VB Code Files (*.vb)|*.vb", "C# Code Files (*.cs)|*.cs")
        Me.SaveFileDialog1.FilterIndex = 0
        Me.SaveFileDialog1.OverwritePrompt = True
        Me.SaveFileDialog1.SupportMultiDottedExtensions = False
        Me.SaveFileDialog1.Title = $"Save {Extension.ToUpper} Output..."
        Me.SaveFileDialog1.ValidateNames = True
        Dim FileSaveResult As DialogResult = Me.SaveFileDialog1.ShowDialog
        If FileSaveResult = DialogResult.OK Then
            Me.RichTextBoxConversionOutput.SaveFile(Me.SaveFileDialog1.FileName, RichTextBoxStreamType.PlainText)
        End If
    End Sub

    Private Sub mnuEditUndo_Click(sender As Object, e As EventArgs) Handles mnuEditUndo.Click
        Me.CurrentBuffer.Undo()
    End Sub

    Private Sub mnuFileExit_Click(sender As Object, e As EventArgs) Handles mnuFileExit.Click
        Me.Close()
#If NETCOREAPP3_0 Then
        Environment.Exit(0)
#Else
        End
#End If
    End Sub

    Private Sub mnuFileLastFileList_Click(ByVal sender As Object, ByVal e As EventArgs)
        ' open the file...
        Dim LanguageExtension As String = Me.RequestToConvert.GetSourceExtension
        Me.OpenFile(DirectCast(sender, ToolStripItem).Tag.ToString().Substring(4), LanguageExtension)
    End Sub

    Private Sub mnuFileLastFileList_MouseDown(sender As Object, e As MouseEventArgs)
        If e.Button = Windows.Forms.MouseButtons.Right Then
            Clipboard.SetText(text:=CType(sender, ToolStripMenuItem).Text)
        End If
    End Sub

    Private Sub mnuFileLastFolder_Click(sender As Object, e As EventArgs) Handles mnuFileLastFolder.Click
        Dim FolderName As String = CType(sender, ToolStripMenuItem).Text
        If Directory.Exists(FolderName) Then
            Dim SourceLanguageExtension As String = Me.RequestToConvert.GetSourceExtension
            Dim ProjectSavePath As String = Me.GetFoldertSavePath(FolderName, SourceLanguageExtension)
            ' This path is a directory.
            Dim LastFileNameWithPath As String = If(My.Settings.StartFolderConvertFromLastFile, My.Settings.MRU_Data.Last, "")
            Dim FilesProcessed As Long = 0
            If Me.ProcessAllFiles(FolderName, ProjectSavePath, LastFileNameWithPath, SourceLanguageExtension, FilesProcessed) Then
                MsgBox($"Conversion completed.")
            End If
        Else
            MsgBox($"{FolderName} Is Not a directory.")
        End If
    End Sub

    Private Sub mnuFileOpen_Click(sender As Object, e As EventArgs) Handles mnuFileOpen.Click
        Dim LanguageExtension As String = Me.RequestToConvert.GetSourceExtension
        With Me.OpenFileDialog1
            .AddExtension = True
            .DefaultExt = LanguageExtension
            .FileName = ""
            .Filter = If(LanguageExtension = "vb", "VB Code Files (*.vb)|*.vb", "C# Code Files (*.cs)|*.cs")
            Me.SaveFileDialog1.FilterIndex = 0
            .Multiselect = False
            .ReadOnlyChecked = True
            .Title = $"Open {LanguageExtension.ToUpper} Source file"
            .ValidateNames = True
            If .ShowDialog = DialogResult.OK Then
                Me.mnuConvertConvertFolder.Enabled = False
                Me.OpenFile(Me.OpenFileDialog1.FileName, LanguageExtension)
            Else
                Me.mnuConvertConvertFolder.Enabled = True
            End If
        End With
    End Sub

    Private Sub mnuFileOpenProject_Click(sender As Object, e As EventArgs) Handles mnuFileOpenProject.Click
        Dim SourceLanguageExtension As String = Me.RequestToConvert.GetSourceExtension
        With Me.OpenFileDialog1
            .AddExtension = True
            .DefaultExt = SourceLanguageExtension
            .FileName = ""
            .Filter = If(SourceLanguageExtension = "vb", "VB Project File (*.vbproj)|*.vbproj", "C# Project File (*.csproj)|*.csproj")
            .FilterIndex = 0
            .Multiselect = False
            .ReadOnlyChecked = True
            .Title = $"Open {SourceLanguageExtension.ToUpper} Project file"
            .ValidateNames = True
            ' InputLines is used for future progress bar
            If .ShowDialog = DialogResult.OK Then
                Dim ProjectSavePath As String = Me.GetFoldertSavePath(Path.GetDirectoryName(.FileName), SourceLanguageExtension)
                Me.mnuConvertConvertFolder.Enabled = True
                If Me.MSBuildInstance Is Nothing Then
                    If VS_Selector_Dialog1.ShowDialog(Me) <> DialogResult.OK Then
                        Stop
                    End If
                    Console.WriteLine($"Using MSBuild at '{VS_Selector_Dialog1.MSBuildInstance.MSBuildPath}' to load projects.")
                    ' NOTE: Be sure to register an instance with the MSBuildLocator
                    '       before calling MSBuildWorkspace.Create()
                    '       otherwise, MSBuildWorkspace won't MEF compose.
                    Me.MSBuildInstance = VS_Selector_Dialog1.MSBuildInstance
                    MSBuildLocator.RegisterInstance(Me.MSBuildInstance)
                End If

                Using Workspace As MSBuildWorkspace = MSBuildWorkspace.Create()
                    AddHandler Workspace.WorkspaceFailed, AddressOf Me.MSBuildWorkspaceFailed
                    Dim currentProject As Project = Workspace.OpenProjectAsync(.FileName).Result
                    Workspace.LoadMetadataForReferencedProjects = True
                    If currentProject.HasDocuments Then
                        Me.RichTextBoxErrorList.Text = ""
                        Me.RichTextBoxFileList.Text = ""
                        SetButtonStopAndCursor(Me, Me.ButtonStop, StopButtonVisible:=True)
                        Dim FilesProcessed As Integer = 0
                        Dim TotalFilesToProcess As Integer = currentProject.Documents.Count
                        For Each document As Document In currentProject.Documents
                            If ParseCSharpSource(document.GetTextAsync(Nothing).Result.ToString).GetRoot.SyntaxTree.IsGeneratedCode(Function(t As SyntaxTrivia) As Boolean
                                                                                                                                        Return t.IsComment OrElse t.IsRegularOrDocComment
                                                                                                                                    End Function, cancellationToken:=Nothing) Then
                                TotalFilesToProcess -= 1
                                Me.FilesConversionProgress.Text = $"Processed {FilesProcessed:N0} of {TotalFilesToProcess:N0} Files"
                                Application.DoEvents()
                                Continue For
                            Else
                                FilesProcessed += 1
                                Me.RichTextBoxFileList.AppendText($"{FilesProcessed.ToString.PadLeft(5)} {document.FilePath}{vbCrLf}")
                                Me.RichTextBoxFileList.Select(Me.RichTextBoxFileList.TextLength, 0)
                                Me.RichTextBoxFileList.ScrollToCaret()
                                Me.FilesConversionProgress.Text = $"Processed {FilesProcessed:N0} of {TotalFilesToProcess:N0} Files"
                                Application.DoEvents()
                            End If
                            Dim TargetLanguageExtension As String = If(SourceLanguageExtension = "cs", "vb", "cs")

                            If Not Me.ProcessFile(document.FilePath, ConvertSourceFileToDestinationFile(Path.GetDirectoryName(.FileName), ProjectSavePath, document), SourceLanguageExtension, currentProject.MetadataReferences.ToArray) Then
                                Exit For
                            End If
                        Next document
                        SetButtonStopAndCursor(Me, Me.ButtonStop, StopButtonVisible:=False)
                    End If
                End Using
                SetButtonStopAndCursor(MeForm:=Me, StopButton:=Me.ButtonStop, StopButtonVisible:=False)
            Else
                Me.mnuConvertConvertFolder.Enabled = True
            End If
        End With
    End Sub

    Private Sub mnuFileSnippetLoadLast_Click(sender As Object, e As EventArgs) Handles mnuFileSnippetLoadLast.Click
        If My.Settings.ColorizeInput Then
            Me.mnuConvertConvertSnippet.Enabled = 0 <> Me.LoadInputBufferFromStream("CS", File.OpenRead(path:=SnippetFileWithPath))
        Else
            Me.RichTextBoxConversionInput.LoadFile(SnippetFileWithPath, RichTextBoxStreamType.PlainText)
        End If
    End Sub

    Private Sub mnuFileSnippetSave_Click(sender As Object, e As EventArgs) Handles mnuFileSnippetSave.Click
        If Me.RichTextBoxConversionInput.TextLength = 0 Then
            Exit Sub
        End If
        Me.RichTextBoxConversionInput.SaveFile(SnippetFileWithPath, RichTextBoxStreamType.PlainText)
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
        OptionsDialog.Show(Me)
    End Sub

    Private Sub mnuOptionsColorizeResult_Click(sender As Object, e As EventArgs) Handles mnuOptionsColorizeResult.Click
        My.Settings.ColorizeOutput = Me.mnuOptionsColorizeResult.Checked
        My.Settings.Save()
        Application.DoEvents()
    End Sub

    Private Sub mnuOptionsColorizeSource_Click(sender As Object, e As EventArgs) Handles mnuOptionsColorizeSource.Click
        My.Settings.ColorizeInput = Me.mnuOptionsColorizeSource.Checked
        My.Settings.Save()
        Application.DoEvents()
    End Sub

    Private Sub mnuOptionsDelayBetweenConversions_SelectedIndexChanged(sender As Object, e As EventArgs) Handles mnuOptionsDelayBetweenConversions.SelectedIndexChanged
        Select Case Me.mnuOptionsDelayBetweenConversions.Text.Substring("Delay Between Conversions = ".Length)
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
        IgnoreFilesWithErrorsList.ShowDialog(Me)
        If IgnoreFilesWithErrorsList.FileToLoad <> "" Then
            Dim LanguageExtension As String = Me.RequestToConvert.GetSourceExtension
            Me.mnuConvertConvertFolder.Enabled = False
            Me.OpenFile(IgnoreFilesWithErrorsList.FileToLoad, LanguageExtension)
        End If
    End Sub

    Private Sub mnuOptionsPauseConvertOnSuccess_Click(sender As Object, e As EventArgs) Handles mnuOptionsPauseConvertOnSuccess.Click
        My.Settings.PauseConvertOnSuccess = Me.mnuOptionsPauseConvertOnSuccess.Checked
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
        Me.LineNumbers_For_RichTextBoxOutput.Visible = CType(sender, ToolStripMenuItem).Checked
        My.Settings.ShowDestinationLineNumbers = Me.mnuViewShowDestinationLineNumbers.Checked
        My.Settings.Save()
    End Sub

    Private Sub mnuViewShowSourceLineNumbers_Click(sender As Object, e As EventArgs) Handles mnuViewShowSourceLineNumbers.Click
        Me.LineNumbers_For_RichTextBoxInput.Visible = Me.mnuViewShowSourceLineNumbers.Checked
        My.Settings.ShowSourceLineNumbers = Me.mnuViewShowSourceLineNumbers.Checked
        My.Settings.Save()
        Me.ResizeRichTextBuffers()
    End Sub

    Private Sub MRU_AddTo(ByVal Path As String)
        Me.StatusStripCurrentFileName.Text = Path
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
        Me.MRU_Update()
    End Sub

    Private Sub MRU_Update()
        ' clear MRU menu items...
        Dim clsItems As New List(Of ToolStripItem)
        ' create a temporary collection containing every MRU menu item
        ' (identified by the tag text when added to the list)...
        For Each clsMenu As ToolStripItem In Me.mnuFile.DropDownItems
            If Not clsMenu.Tag Is Nothing Then
                If (clsMenu.Tag.ToString().StartsWith("MRU:")) Then
                    clsItems.Add(clsMenu)
                End If
            End If
        Next
        ' iterate through list and remove each from menu...
        For Each clsMenu As ToolStripItem In clsItems
            RemoveHandler clsMenu.Click, AddressOf Me.mnuFileLastFileList_Click
            RemoveHandler clsMenu.MouseDown, AddressOf Me.mnuFileLastFileList_MouseDown
            Me.mnuFile.DropDownItems.Remove(clsMenu)
        Next
        ' display items (in reverse order so the most recent is on top)...
        For iCounter As Integer = My.Settings.MRU_Data.Count - 1 To 0 Step -1
            Dim sPath As String = My.Settings.MRU_Data(iCounter)
            ' create new ToolStripItem, displaying the name of the file...
            ' set the tag - identifies the ToolStripItem as an MRU item and
            ' contains the full path so it can be opened later...
#Disable Warning IDE0067 ' Dispose objects before losing scope
            Dim clsItem As New ToolStripMenuItem(sPath) With {
                .Tag = "MRU:" & sPath
            }
#Enable Warning IDE0067 ' Dispose objects before losing scope
            ' hook into the click event handler so we can open the file later...
            AddHandler clsItem.Click, AddressOf Me.mnuFileLastFileList_Click
            AddHandler clsItem.MouseDown, AddressOf Me.mnuFileLastFileList_MouseDown
            ' insert into DropDownItems list...
            Me.mnuFile.DropDownItems.Insert(Me.mnuFile.DropDownItems.Count - 11, clsItem)
        Next
        ' show separator...
        My.Settings.Save()
        If My.Settings.MRU_Data.Count > 0 Then
            Me.mnuFileLastFolder.Text = IO.Path.GetDirectoryName(My.Settings.MRU_Data.Last)
            Me.mnuFileLastFolder.Visible = True
            Me.mnuFileSep1.Visible = True
            Me.mnuFolderSep1.Visible = True
        Else
            Me.mnuFileLastFolder.Visible = False
            Me.mnuFileSep1.Visible = False
            Me.mnuFolderSep1.Visible = False
        End If

    End Sub

    ' Print message for WorkspaceFailed event to help diagnosing project load failures.
    Private Sub MSBuildWorkspaceFailed(o As Object, e1 As WorkspaceDiagnosticEventArgs)
        MsgBox(e1.Diagnostic.Message, MsgBoxStyle.AbortRetryIgnore)
    End Sub

    Private Sub OpenFile(FileNameWithPath As String, LanguageExtension As String)
        Me.mnuConvertConvertSnippet.Enabled = Me.LoadInputBufferFromStream(LanguageExtension, File.OpenRead(path:=FileNameWithPath)) <> 0
        Me.MRU_AddTo(FileNameWithPath)
    End Sub

    Private Sub PictureBox1_Click(sender As Object, e As EventArgs) Handles PictureBox1.Click
        If Me.SearchInput.Text.IsEmptyNullOrWhitespace Then
            Exit Sub
        End If
        Select Case Me.SearchWhere.SelectedIndex
            Case 0
                If Not Me.FindTextInBuffer(Me.RichTextBoxConversionInput, Me.RichTextBoxConversionInput.SelectionStart, Me.RichTextBoxConversionInput.SelectionLength) Then
                    MsgBox($"'{Me.SearchInput.Text}' not found in Source Buffer!", MsgBoxStyle.OkOnly, "Not Found!")
                End If
            Case 1
                If Not Me.FindTextInBuffer(Me.RichTextBoxConversionOutput, Me.RichTextBoxConversionOutput.SelectionStart, Me.RichTextBoxConversionOutput.SelectionLength) Then
                    MsgBox($"'{Me.SearchInput.Text}' not found Conversion Buffer!", MsgBoxStyle.OkOnly, "Not Found!")
                End If
            Case 2
                If Not Me.FindTextInBuffer(Me.RichTextBoxConversionInput, Me.RichTextBoxConversionInput.SelectionStart, Me.RichTextBoxConversionInput.SelectionLength) Then
                    MsgBox($"'{Me.SearchInput.Text}' not found in Source Buffer!", MsgBoxStyle.OkOnly, "Not Found!")
                End If
                If Not Me.FindTextInBuffer(Me.RichTextBoxConversionOutput, Me.RichTextBoxConversionOutput.SelectionStart, Me.RichTextBoxConversionOutput.SelectionLength) Then
                    MsgBox($"'{Me.SearchInput.Text}' not found Conversion Buffer!", MsgBoxStyle.OkOnly, "Not Found!")
                End If
        End Select
    End Sub

    Private Sub PictureBox1_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseDown
        Me.PictureBox1.BorderStyle = BorderStyle.Fixed3D
    End Sub

    Private Sub PictureBox1_MouseUp(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseUp
        Me.PictureBox1.BorderStyle = BorderStyle.FixedSingle
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
    Private Function ProcessAllFiles(SourceDirectory As String, TargetDirectory As String, LastFileNameWithPath As String, SourceLanguageExtension As String, ByRef FilesProcessed As Long) As Boolean
        Try
            Me.RichTextBoxErrorList.Text = ""
            Me.RichTextBoxFileList.Text = ""
            SetButtonStopAndCursor(Me, Me.ButtonStop, StopButtonVisible:=True)
            Dim TotalFilesToProcess As Long = GetFileCount(SourceDirectory, SourceLanguageExtension, My.Settings.SkipBinAndObjFolders, My.Settings.SkipTestResourceFiles)
            ' Process the list of files found in the directory.
            Return ProcessDirectory(SourceDirectory, TargetDirectory, MeForm:=Me, Me.ButtonStop, Me.RichTextBoxFileList, LastFileNameWithPath, SourceLanguageExtension, FilesProcessed, TotalFilesToProcess, AddressOf Me.ProcessFile)
        Catch ex As Exception
            ' don't crash on exit
            Stop
        Finally
            SetButtonStopAndCursor(Me, Me.ButtonStop, StopButtonVisible:=False)
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
    Private Function ProcessFile(SourceFileNameWithPath As String, TargetDirectory As String, SourceLanguageExtension As String, OptionalReferences() As MetadataReference) As Boolean
        If My.Settings.IgnoreFileList.Contains(SourceFileNameWithPath) Then
            Return True
        End If
        Me.ButtonStop.Visible = True
        Me.RichTextBoxConversionOutput.Text = ""
        Me.StopRequested = False
        Me.MRU_AddTo(SourceFileNameWithPath)
        Dim fsRead As FileStream = File.OpenRead(SourceFileNameWithPath)
        Dim lines As Integer = Me.LoadInputBufferFromStream(SourceLanguageExtension, fsRead)
        If lines > 0 Then
            Me.RequestToConvert.SourceCode = Me.RichTextBoxConversionInput.Text
            If Not Me.Convert_Compile_Colorize(Me.RequestToConvert, OptionalReferences) Then
                Select Case MsgBox($"Conversion failed, do you want to stop processing this file automatically in the future? Yes and No will continue processing files, Cancel will stop conversions!", MsgBoxStyle.YesNoCancel)
                    Case MsgBoxResult.Cancel
                        Return False
                    Case MsgBoxResult.No
                        Return True
                    Case MsgBoxResult.Yes
                        If Not My.Settings.IgnoreFileList.Contains(SourceFileNameWithPath) Then
                            My.Settings.IgnoreFileList.Add(SourceFileNameWithPath)
                            My.Settings.Save()
                            Me.RichTextBoxErrorList.Text = ""
                            Me.LineNumbers_For_RichTextBoxInput.Visible = My.Settings.ShowSourceLineNumbers
                            Me.LineNumbers_For_RichTextBoxOutput.Visible = My.Settings.ShowDestinationLineNumbers
                        End If
                        Return True
                End Select
            Else
                If TargetDirectory.IsNotEmptyNullOrWhitespace Then
                    Dim NewFileName As String = Path.ChangeExtension(New FileInfo(SourceFileNameWithPath).Name, If(SourceLanguageExtension = "vb", "cs", "vb"))
                    WriteTextToStream(TargetDirectory, NewFileName, Me.RichTextBoxConversionOutput.Text)
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
                If Me.StopRequested Then
                    Me.StopRequested = False
                    Return False

                End If
            Next
            Application.DoEvents()
        Else
            Me.RichTextBoxConversionOutput.Clear()
        End If
        Return True
    End Function

    Private Sub ResizeRichTextBuffers()
        Dim LineNumberInputWidth As Integer = If(Me.LineNumbers_For_RichTextBoxInput.Visible, Me.LineNumbers_For_RichTextBoxInput.Width, 0)
        Dim LineNumberOutputWidth As Integer = If(Me.LineNumbers_For_RichTextBoxOutput.Visible, Me.LineNumbers_For_RichTextBoxOutput.Width, 0)

        Me.RichTextBoxConversionInput.Width = CInt((Me.ClientSize.Width / 2 + 0.5)) - LineNumberInputWidth
        Me.RichTextBoxFileList.Width = CInt(Me.ClientSize.Width / 2 + 0.5)

        Me.RichTextBoxConversionOutput.Width = Me.ClientSize.Width - (Me.RichTextBoxConversionInput.Width + LineNumberInputWidth + LineNumberOutputWidth)
        Me.RichTextBoxConversionOutput.Left = Me.RichTextBoxConversionInput.Width + LineNumberInputWidth + LineNumberOutputWidth

        Dim HalfClientWidth As Integer = CInt(Me.ClientSize.Width / 2)
        Me.RichTextBoxErrorList.Left = HalfClientWidth
        Me.RichTextBoxErrorList.Width = HalfClientWidth
        Me.StatusStripCurrentFileName.Width = HalfClientWidth

    End Sub

    Private Sub RichTexBoxErrorList_DoubleClick(sender As Object, e As EventArgs) Handles RichTextBoxErrorList.DoubleClick
        If Me.RTFLineStart > 0 AndAlso Me.RichTextBoxConversionOutput.SelectionStart <> Me.RTFLineStart Then
            Me.RichTextBoxConversionOutput.Select(Me.RTFLineStart, 0)
            Me.RichTextBoxConversionOutput.ScrollToCaret()
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
        If Not LineText.StartsWith("BC") Then
            Exit Sub
        End If
        If Not LineText.Contains(" Line = ") Then
            Exit Sub
        End If
        Dim NumberCount As Integer = LineText.Substring(15).IndexOf(" ")
        Dim ErrorLine As Integer = CInt(Val(box.Text.Substring(AfterEquals, NumberCount)))
        If ErrorLine <= 0 Then
            Exit Sub
        End If
        Me.RTFLineStart = Me.RichTextBoxConversionOutput.GetFirstCharIndexFromLine(ErrorLine - 1)
    End Sub

    Private Sub RichTextBoxConversionInput_Enter(sender As Object, e As EventArgs) Handles RichTextBoxConversionInput.Enter
        Me.CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub RichTextBoxConversionInput_MouseEnter(sender As Object, e As EventArgs) Handles RichTextBoxConversionInput.MouseEnter
        Me.CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub RichTextBoxConversionInput_TextChanged(sender As Object, e As EventArgs) Handles RichTextBoxConversionInput.TextChanged
        Dim InputBufferInUse As Boolean = CType(sender, RichTextBox).TextLength > 0
        Me.mnuFileSnippetSave.Enabled = InputBufferInUse
        Me.mnuConvertConvertSnippet.Enabled = InputBufferInUse
        Me.mnuConvertConvertFolder.Enabled = InputBufferInUse
    End Sub

    Private Sub RichTextBoxConversionOutput_HorizScrollBarRightClicked(ByVal sender As Object, ByVal loc As Point) Handles RichTextBoxConversionOutput.HorizScrollBarRightClicked
        Dim p As Integer = CType(sender, AdvancedRTB).HScrollPos
        MsgBox($"Horizontal Right Clicked At: {loc.ToString}, HScrollPos = {p}")
    End Sub

    Private Sub RichTextBoxConversionOutput_MouseEnter(sender As Object, e As EventArgs) Handles RichTextBoxConversionOutput.MouseEnter
        Me.CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub RichTextBoxConversionOutput_TextChanged(sender As Object, e As EventArgs) Handles RichTextBoxConversionOutput.TextChanged
        Dim OutputBufferInUse As Boolean = CType(sender, RichTextBox).TextLength > 0
        Me.mnuCompile.Enabled = OutputBufferInUse
    End Sub

    Private Sub RichTextBoxConversionOutput_VertScrollBarRightClicked(ByVal sender As Object, ByVal loc As Point) Handles RichTextBoxConversionOutput.VertScrollBarRightClicked
        Dim p As Integer = CType(sender, AdvancedRTB).VScrollPos
        MsgBox($"Vertical Right Clicked At: {loc.ToString}, VScrollPos = {p} ")
    End Sub

    Private Sub RichTextBoxErrorList_Enter(sender As Object, e As EventArgs) Handles RichTextBoxErrorList.Enter
        Me.CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub RichTextBoxErrorList_MouseEnter(sender As Object, e As EventArgs) Handles RichTextBoxErrorList.MouseEnter
        Me.CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub RichTextBoxFileList_Enter(sender As Object, e As EventArgs) Handles RichTextBoxFileList.Enter
        Me.CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub RichTextBoxFileList_MouseEnter(sender As Object, e As EventArgs) Handles RichTextBoxFileList.MouseEnter
        Me.CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub SearchBoxVisibility(Visible As Boolean)
        Me.ButtonSearch.Visible = Visible
        Me.SearchDirection.Visible = Visible
        Me.PictureBox1.Visible = Visible
        Me.SearchInput.Visible = Visible
    End Sub

    Private Sub SearchDirection_DrawItem(ByVal sender As Object, ByVal e As DrawItemEventArgs) Handles SearchDirection.DrawItem
        If e.Index <> -1 Then
            e.Graphics.DrawImage(Me.ImageList1.Images(e.Index), e.Bounds.Left, e.Bounds.Top)
        End If
    End Sub

    Private Sub SearchDirection_MeasureItem(ByVal sender As Object, ByVal e As MeasureItemEventArgs) Handles SearchDirection.MeasureItem
        e.ItemHeight = Me.ImageList1.ImageSize.Height
        e.ItemWidth = Me.ImageList1.ImageSize.Width
    End Sub

    Private Sub SearchDirection_SelectedIndexChanged(sender As Object, e As EventArgs) Handles SearchDirection.SelectedIndexChanged
        Me.PictureBox1.Image = Me.ImageList1.Images(Me.SearchDirection.SelectedIndex)
    End Sub

    Private Sub SearchInput_TextChanged(sender As Object, e As EventArgs) Handles SearchInput.TextChanged
        Me.RichTextBoxConversionInput.SelectionStart = 0
        Me.RichTextBoxConversionInput.SelectionLength = 0
        Me.RichTextBoxConversionOutput.SelectionStart = 0
        Me.RichTextBoxConversionOutput.SelectionLength = 0
    End Sub

    Private Sub SplitContainer1_SplitterMoved(sender As Object, e As SplitterEventArgs) Handles SplitContainer1.SplitterMoved
        Me.RichTextBoxFileList.Height = Me.SplitContainer1.Panel2.Height - Me.StatusStrip1.Height
        Me.RichTextBoxErrorList.Height = Me.SplitContainer1.Panel2.Height - Me.StatusStrip1.Height
    End Sub

    Private Sub StatusStripCurrentFileName_MouseDown(sender As Object, e As MouseEventArgs) Handles StatusStripCurrentFileName.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Right Then
            Clipboard.SetText(text:=CType(sender, ToolStripStatusLabel).Text)
        End If
    End Sub

    Private Sub VB2CSharp_CheckedChanged(sender As Object, e As EventArgs) Handles VB2CSharp.CheckedChanged
        Me.RichTextBoxConversionInput.Text = ""
        Me.RichTextBoxConversionOutput.Text = ""

        If Me.VB2CSharp.Checked Then
            Me.CSharp2VB.Checked = False
            Dim Progress As New ReportProgress(Me.ConversionProgressBar)
            Me.RequestToConvert = New ConvertRequest(ConvertRequest.VB_To_CS, My.Settings.SkipAutoGenerated, AddressOf Application.DoEvents, Progress)
            Me.Text = "Convert Visual Basic To C#"
        End If
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        Me.SetStyle(ControlStyles.AllPaintingInWmPaint Or ControlStyles.UserPaint Or ControlStyles.DoubleBuffer, True)
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
        Me.MRU_Update()
    End Sub

End Class