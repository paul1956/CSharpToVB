' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.ComponentModel
Imports System.IO
Imports System.Reflection
Imports System.Threading
Imports CSharpToVBApp
Imports CSharpToVBConverter
Imports Microsoft.CodeAnalysis
Imports Microsoft.VisualBasic.FileIO
Imports ProgressReportLibrary

Partial Public Class Form1

    Private Shared ReadOnly s_snippetFileWithPath As String = Path.Combine(SpecialDirectories.MyDocuments, "CSharpToVBLastSnippet.RTF")
    Private ReadOnly _frameworkTypeList As New List(Of ToolStripMenuItem)
    Private ReadOnly _frameworkVersionList As New Dictionary(Of String, (Item As ToolStripMenuItem, Parent As ToolStripMenuItem))
    Private _currentBuffer As Control
    Friend _cancellationTokenSource As CancellationTokenSource
    Friend _doNotFailOnError As Boolean
    Friend _inColorize As Boolean
    Friend _requestToConvert As ConvertRequest
    Friend _resultOfConversion As ConversionResult

    Public Sub New()
        Me.InitializeComponent()
    End Sub

    Friend Property BufferToSearch As SearchBuffers = SearchBuffers.CS

    Private Property CurrentBuffer As Control
        Get
            Return _currentBuffer
        End Get
        Set(value As Control)
            _currentBuffer = value
            If value IsNot Nothing Then
                _currentBuffer.Focus()
            End If
        End Set
    End Property

    Private Sub ButtonStop_Click(sender As Object, e As EventArgs) Handles ButtonStopConversion.Click
        Me.ButtonStopConversion.Visible = False
        _cancellationTokenSource.Cancel()
        Application.DoEvents()
    End Sub

    Private Sub ButtonStop_MouseEnter(sender As Object, e As EventArgs) Handles ButtonStopConversion.MouseEnter
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=False)
    End Sub

    Private Sub ButtonStop_MouseLeave(sender As Object, e As EventArgs) Handles ButtonStopConversion.MouseLeave
        Me.ButtonStopConversion.BackColor = SystemColors.Control
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=Me.ButtonStopConversion.Visible)
    End Sub

    Private Sub ButtonStop_VisibleChanged(sender As Object, e As EventArgs) Handles ButtonStopConversion.VisibleChanged
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=Me.ButtonStopConversion.Visible)
    End Sub

    Private Sub ContextMenuCopy_Click(sender As Object, e As EventArgs) Handles ContextMenuCopy.Click
        If TypeOf Me.ContextMenuStrip1.SourceControl Is RichTextBox Then
            CType(Me.ContextMenuStrip1.SourceControl, RichTextBox).Copy()
        Else
            If Me.ContextMenuStrip1.SourceControl IsNot Nothing Then
                Clipboard.SetText(CType(Me.ContextMenuStrip1.SourceControl, ListBox).SelectedItem.ToString)
            End If
        End If
    End Sub

    Private Sub ContextMenuCut_Click(sender As Object, e As EventArgs) Handles ContextMenuCut.Click
        If TypeOf Me.ContextMenuStrip1.SourceControl Is RichTextBox Then
            CType(Me.ContextMenuStrip1.SourceControl, RichTextBox).Cut()
        End If
    End Sub

    Private Sub ContextMenuPaste_Click(sender As Object, e As EventArgs) Handles ContextMenuPaste.Click
        Dim sourceControl As RichTextBox = CType(Me.ContextMenuStrip1.SourceControl, RichTextBox)
        If sourceControl Is Nothing Then
            Exit Sub
        End If
        If sourceControl.CanPaste(DataFormats.GetFormat(DataFormats.Rtf)) OrElse
            sourceControl.CanPaste(DataFormats.GetFormat(DataFormats.Text)) Then
            sourceControl.Paste(DataFormats.GetFormat("Text"))
        End If
    End Sub

    Private Sub ContextMenuRedo_Click(sender As Object, e As EventArgs) Handles ContextMenuRedo.Click
        Dim sourceControl As RichTextBox = CType(Me.ContextMenuStrip1.SourceControl, RichTextBox)
        If sourceControl IsNot Nothing AndAlso sourceControl.CanRedo Then
            sourceControl.Redo()
        End If
    End Sub

    Private Sub ContextMenuSelectAll_Click(sender As Object, e As EventArgs) Handles ContextMenuSelectAll.Click
        Dim sourceControl As RichTextBox = CType(Me.ContextMenuStrip1.SourceControl, RichTextBox)
        If sourceControl IsNot Nothing Then
            sourceControl.SelectAll()
        End If
    End Sub

    Private Sub ContextMenuStrip1_Opening(sender As Object, e As CancelEventArgs) Handles ContextMenuStrip1.Opening
        Dim ContextMenu As ContextMenuStrip = CType(sender, ContextMenuStrip)

        If TypeOf Me.CurrentBuffer Is RichTextBox Then
            Dim sourceBuffer As RichTextBox = CType(Me.CurrentBuffer, RichTextBox)
            ContextMenu.Items(ContextMenu.IndexOf(NameOf(ContextMenuCopy))).Enabled = sourceBuffer.TextLength > 0 And sourceBuffer.SelectedText.Any
            ContextMenu.Items(ContextMenu.IndexOf(NameOf(ContextMenuCut))).Enabled = sourceBuffer.TextLength > 0 And sourceBuffer.SelectedText.Any
            ContextMenu.Items(ContextMenu.IndexOf(NameOf(ContextMenuPaste))).Enabled = sourceBuffer.CanPaste(DataFormats.GetFormat(DataFormats.Rtf)) OrElse sourceBuffer.CanPaste(DataFormats.GetFormat(DataFormats.Text))
            ContextMenu.Items(ContextMenu.IndexOf(NameOf(ContextMenuRedo))).Enabled = sourceBuffer.CanRedo
            ContextMenu.Items(ContextMenu.IndexOf(NameOf(ContextMenuUndo))).Enabled = sourceBuffer.CanUndo
        Else
            ContextMenu.Items(ContextMenu.IndexOf(NameOf(ContextMenuCut))).Enabled = False
            ContextMenu.Items(ContextMenu.IndexOf(NameOf(ContextMenuPaste))).Enabled = False
            ContextMenu.Items(ContextMenu.IndexOf(NameOf(ContextMenuRedo))).Enabled = False
            ContextMenu.Items(ContextMenu.IndexOf(NameOf(ContextMenuUndo))).Enabled = False
        End If
    End Sub

    Private Sub ContextMenuUndo_Click(sender As Object, e As EventArgs) Handles ContextMenuUndo.Click
        Dim sourceControl As RichTextBox = CType(Me.ContextMenuStrip1.SourceControl, RichTextBox)
        If sourceControl IsNot Nothing AndAlso sourceControl.CanUndo Then
            sourceControl.Undo()
        End If
    End Sub

    Private Sub ConversionInput_Enter(sender As Object, e As EventArgs) Handles ConversionInput.Enter
        Me.CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub ConversionInput_MouseEnter(sender As Object, e As EventArgs) Handles ConversionInput.MouseEnter
        Me.CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub ConversionInput_TextChanged(sender As Object, e As EventArgs) Handles ConversionInput.TextChanged
        Dim inputBufferInUse As Boolean = Me.SetSearchControls(True)
        Me.mnuViewShowSourceLineNumbers.Checked = inputBufferInUse And My.Settings.ShowSourceLineNumbers
        Me.mnuFileSaveSnippet.Enabled = inputBufferInUse
        If Me.mnuOptionsColorizeSource.Checked AndAlso Not _inColorize Then
            Colorize(Me, GetClassifiedRanges(SourceCode:=Me.ConversionInput.Text, LanguageNames.CSharp), ConversionBuffer:=Me.ConversionInput, Lines:=Me.ConversionInput.Lines.Length)
        End If
    End Sub

    Private Sub ConversionOutput_MouseEnter(sender As Object, e As EventArgs) Handles ConversionOutput.MouseEnter
        Me.CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub ConversionOutput_TextChanged(sender As Object, e As EventArgs) Handles ConversionOutput.TextChanged
        Dim OutputBufferInUse As Boolean = CType(sender, RichTextBox).TextLength > 0
        Me.mnuViewShowDestinationLineNumbers.Checked = OutputBufferInUse And My.Settings.ShowDestinationLineNumbers
        Me.mnuCompile.Enabled = OutputBufferInUse
        Me.SetSearchControls()
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles Me.Load
        Me.SplitContainer1.SplitterDistance = Me.SplitContainer1.Height - (Me.ListBoxErrorList.Height + 20)

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

        Me.mnuFileLoadLastSnippet.Enabled = File.Exists(s_snippetFileWithPath)
        Me.mnuOptionsPauseConvertOnSuccess.Checked = My.Settings.PauseConvertOnSuccess
        Me.mnuOptionsFolderConversionsOptionsSkipAutoGenerated.Checked = My.Settings.SkipAutoGenerated
        Me.mnuOptionsFolderConversionsOptionsSkipBinAndObjFolders.Checked = My.Settings.SkipBinAndObjFolders
        Me.mnuOptionsFolderConversionsOptionsSkipTestResourceFiles.Checked = My.Settings.SkipTestResourceFiles

        Me.mnuConvertStartFolderConvertFromLastFile.Checked = My.Settings.StartFolderConvertFromLastFile

        If String.IsNullOrWhiteSpace(My.Settings.DefaultProjectDirectory) Then
            My.Settings.DefaultProjectDirectory = GetLatestVisualStudioProjectPath()
            My.Settings.Save()
            Application.DoEvents()
        End If

        Me.Width = Screen.PrimaryScreen.Bounds.Width
        Me.Height = CInt(Screen.PrimaryScreen.Bounds.Height * 0.95)

        Me.ListBoxFileList.Height = Me.SplitContainer1.Panel2.ClientSize.Height
        Me.ListBoxErrorList.Height = Me.SplitContainer1.Panel2.ClientSize.Height

        For Each FrameworkType As ToolStripMenuItem In Me.mnuOptionsDefaultFramework.DropDownItems
            If FrameworkType.Text = ".Net Full Framework" Then
                For Each f As String In GetAllFrameworkVersions()
                    FrameworkType.DropDownItems.AddDropDownMenuItem(f)
                Next
            Else
                For Each f As String In GetAllCoreVersions()
                    FrameworkType.DropDownItems.AddDropDownMenuItem(f)
                Next
            End If
        Next
        For Each FrameworkType As ToolStripMenuItem In Me.mnuOptionsDefaultFramework.DropDownItems
            _frameworkTypeList.Add(FrameworkType)
            For Each FrameworkVersion As ToolStripMenuItem In FrameworkType.DropDownItems
                If FrameworkVersion.Text = My.Settings.Framework Then
                    FrameworkType.Checked = True
                    FrameworkVersion.Checked = True
                    FrameworkVersion.Enabled = False
                Else
                    FrameworkType.Checked = False
                    FrameworkVersion.Checked = False
                    FrameworkVersion.Enabled = True
                End If
                AddHandler FrameworkVersion.CheckedChanged, AddressOf Me.mnuOptionDefaultFramework_CheckedChanged
                _frameworkVersionList.Add(FrameworkVersion.Text, (FrameworkVersion, FrameworkType))
            Next
        Next
        If My.Settings.LastProject.Length > 0 Then
            Me.mnuFileLastProject.Text = $"Last Project - {My.Settings.LastProject}"
            Me.mnuFileLastProject.Enabled = True
        End If
        If My.Settings.LastSolution.Length > 0 Then
            Me.mnuFileLastSolution.Text = $"Last Solution - {My.Settings.LastSolution}"
            Me.mnuFileLastSolution.Enabled = True
        End If
        Me.ProjectConversionInitProgressBar.Visible = False
        Me.CenterToScreen()
        Me.ProjectConversionInitProgressBar.Location = New Point(Me.ClientSize.Width \ 4, Me.ClientSize.Height \ 2)
        Me.ProjectConversionInitProgressLabel.Left = Me.ProjectConversionInitProgressBar.Left
        Me.ProjectConversionInitProgressLabel.Top = Me.ProjectConversionInitProgressBar.Top - (Me.ProjectConversionInitProgressLabel.Height * 2)
        Me.ToolTipErrorList.SetToolTip(Me.ListBoxErrorList, "Double-Click to scroll to VB error")
        Me.ToolTipFileList.SetToolTip(Me.ListBoxFileList, "Double-Click to open C# and corresponding VB file if available")
        Application.DoEvents()
        Me.TSFindLookInComboBox.DropDownStyle = ComboBoxStyle.Simple
        Me.TSFindLookInComboBox.SelectedIndex = 0
        Me.TSFindMatchCaseCheckBox.Checked = My.Settings.TSFindMatchCase
        Me.TSFindMatchWholeWordCheckBox.Checked = My.Settings.TSFindMatchWholeWord
    End Sub

    Private Sub Form1_Resize(sender As Object, e As EventArgs) Handles Me.Resize
        Me.ResizeRichTextBuffers()
    End Sub

    Private Sub LineNumbers_For_RichTextBoxInput_Resize(sender As Object, e As EventArgs) Handles LineNumbersForConversionInput.Resize
        Me.ResizeRichTextBuffers()
    End Sub

    ' Don't save state here only if user changes
    Private Sub LineNumbers_For_RichTextBoxInput_VisibleChanged(sender As Object, e As EventArgs) Handles LineNumbersForConversionInput.VisibleChanged
        Me.ResizeRichTextBuffers()
    End Sub

    Private Sub LineNumbers_For_RichTextBoxOutput_Resize(sender As Object, e As EventArgs) Handles LineNumbersForConversionOutput.Resize
        Me.ResizeRichTextBuffers()
    End Sub

    ' Don't save state here only if user changes
    Private Sub LineNumbers_For_RichTextBoxOutput_VisibleChanged(sender As Object, e As EventArgs) Handles LineNumbersForConversionOutput.VisibleChanged
        Me.ResizeRichTextBuffers()
    End Sub

    Private Sub ListboxErrorList_DoubleClick(sender As Object, e As EventArgs) Handles ListBoxErrorList.DoubleClick
        Dim box As ListBox = DirectCast(sender, ListBox)
        If box.Text.Length = 0 Then
            Exit Sub
        End If
        Dim LineText As String = box.Text
        If Not LineText.StartsWith("BC", StringComparison.Ordinal) Then
            Exit Sub
        End If
        Dim startIndex As Integer = LineText.IndexOf(" Line = ", StringComparison.OrdinalIgnoreCase) + 8
        If startIndex <= 0 Then
            Exit Sub
        End If
        Dim count As Integer = LineText.Substring(startIndex).IndexOf(" ", StringComparison.OrdinalIgnoreCase)
        Dim lineStartPosition As Integer = Me.ConversionOutput.GetFirstCharIndexFromLine(CInt(LineText.Substring(startIndex, count)) - 1)

        If lineStartPosition > 0 AndAlso Me.ConversionOutput.SelectionStart <> lineStartPosition Then
            Me.ConversionOutput.Select(lineStartPosition, 0)
            Me.ConversionOutput.ScrollToCaret()
        End If
    End Sub

    Private Sub ListBoxErrorList_Enter(sender As Object, e As EventArgs) Handles ListBoxErrorList.Enter
        If Me.ListBoxErrorList.Items.Count = 0 Then
            Me.ListBoxErrorList.Enabled = False
            Exit Sub
        End If
        Me.ListBoxErrorList.Enabled = True
        Me.CurrentBuffer = Me.ListBoxErrorList
    End Sub

    Private Sub ListBoxErrorList_MouseEnter(sender As Object, e As EventArgs) Handles ListBoxErrorList.MouseEnter
        If Me.ListBoxErrorList.Items.Count = 0 Then
            Me.ListBoxErrorList.Enabled = False
            Exit Sub
        End If
        Me.ListBoxErrorList.Enabled = True
        Me.CurrentBuffer = Me.ListBoxErrorList
    End Sub

    Private Sub ListBoxErrorList_SelectedValueChanged(sender As Object, e As EventArgs) Handles ListBoxErrorList.SelectedValueChanged
        Me.ListBoxErrorList.Enabled = Me.ListBoxErrorList.Items.Count > 0
    End Sub

    Private Sub ListBoxFileList_DoubleClick(sender As Object, e As EventArgs) Handles ListBoxFileList.DoubleClick
        Dim FileList As ListBox = CType(sender, ListBox)
        If FileList.Items.Count = 0 Then
            Exit Sub
        End If
        Dim item As NumberedListItem = CType(FileList.SelectedItem, NumberedListItem)

        Dim SourceFileNameWithPath As String = item.SourceFileWithPath
        If String.IsNullOrWhiteSpace(SourceFileNameWithPath) OrElse Not File.Exists(SourceFileNameWithPath) Then
            Exit Sub
        End If
        LoadInputBufferFromStream(Me, SourceFileNameWithPath)
        Dim ConvertedFileNameWithPath As String = item.ValueItem
        If Not File.Exists(ConvertedFileNameWithPath) Then
            Exit Sub
        End If

        LoadOutputBufferFromStream(Me, ConvertedFileNameWithPath)
    End Sub

    Private Sub ListBoxFileList_Enter(sender As Object, e As EventArgs) Handles ListBoxFileList.Enter
        If Me.ListBoxFileList.Items.Count = 0 Then
            Exit Sub
        End If
        Me.CurrentBuffer = Me.ListBoxFileList
    End Sub

    Private Sub ListBoxFileList_MouseEnter(sender As Object, e As EventArgs) Handles ListBoxFileList.MouseEnter
        If Me.ListBoxFileList.Items.Count = 0 Then
            Exit Sub
        End If
        Me.CurrentBuffer = Me.ListBoxFileList
    End Sub

    Private Sub ListBoxFileList_SelectedValueChanged(sender As Object, e As EventArgs) Handles ListBoxFileList.SelectedValueChanged
        Me.ListBoxFileList.Enabled = Me.ListBoxFileList.Items.Count > 0
    End Sub

    Private Sub mnuCompile_Click(sender As Object, e As EventArgs) Handles mnuCompile.Click
        Me.LineNumbersForConversionInput.Visible = False
        Me.LineNumbersForConversionOutput.Visible = False
        Me.ListBoxErrorList.Items.Clear()

        If String.IsNullOrWhiteSpace(Me.ConversionOutput.Text) Then
            Exit Sub
        End If
        Me.ListBoxErrorList.Text = ""
        Dim VBPreprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
            New KeyValuePair(Of String, Object)(My.Settings.Framework, True)
        }
        Compile_Colorize(Me, Me.ConversionOutput.Text, VBPreprocessorSymbols)
    End Sub

    Private Sub mnuConvert_DropDownOpened(sender As Object, e As EventArgs) Handles mnuConvert.DropDownOpened
        Me.mnuConvertConvertSnippet.Enabled = Me.ConversionInput.TextLength > 0
    End Sub

    Private Sub mnuConvert_EnabledChanged(sender As Object, e As EventArgs) Handles mnuConvert.EnabledChanged
        Me.SetSearchControls()
    End Sub

    Private Async Sub mnuConvertConvertSnippet_Click(sender As Object, e As EventArgs) Handles mnuConvertConvertSnippet.Click
        SetButtonStopAndCursor(MeForm:=Me, StopButton:=Me.ButtonStopConversion, StopButtonVisible:=True)
        Me.ListBoxErrorList.Items.Clear()
        Me.ListBoxFileList.Items.Clear()
        Me.LineNumbersForConversionOutput.Visible = False
        Me.StatusStripCurrentFileName.Text = ""
        Me.ResizeRichTextBuffers()
        If _cancellationTokenSource IsNot Nothing Then
            _cancellationTokenSource.Dispose()
        End If
        _cancellationTokenSource = New CancellationTokenSource

        _requestToConvert = New ConvertRequest(My.Settings.SkipAutoGenerated, New Progress(Of ProgressReport)(AddressOf Me.StatusStripConversionProgressBar.Update), _cancellationTokenSource.Token) With
                {
                .SourceCode = Me.ConversionInput.Text
                }
        Dim CSPreprocessorSymbols As New List(Of String) From {
            My.Settings.Framework
        }

        Dim VBPreprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
            KeyValuePair.Create(Of String, Object)(My.Settings.Framework, True)
            }

        Dim DontDisplayLineNumbers As Boolean = Await Convert_Compile_ColorizeAsync(Me, _requestToConvert, CSPreprocessorSymbols, VBPreprocessorSymbols, SharedReferences.CSharpReferences(Assembly.Load("System.Windows.Forms").Location, OptionalReference:=Nothing).ToArray, _cancellationTokenSource.Token).ConfigureAwait(True)
        If _requestToConvert.CancelToken.IsCancellationRequested Then
            MsgBox($"Conversion canceled.",
                       MsgBoxStyle.OkOnly Or MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground,
                       Title:="C# to Visual Basic")
            Me.StatusStripConversionProgressBar.Clear()
        End If
        SetButtonStopAndCursor(MeForm:=Me, StopButton:=Me.ButtonStopConversion, StopButtonVisible:=False)
        Me.LineNumbersForConversionOutput.Visible = (Not DontDisplayLineNumbers) OrElse My.Settings.ShowDestinationLineNumbers
    End Sub

    Private Async Sub mnuConvertFolder_Click(sender As Object, e As EventArgs) Handles mnuConvertConvertFolder.Click
        Me.mnuConvertConvertFolder.Enabled = False
        Me.LineNumbersForConversionInput.Visible = False
        Me.LineNumbersForConversionOutput.Visible = False
        Me.StatusStripCurrentFileName.Text = ""
        Dim SourceFolderName As String
        Dim solutionSavePath As String
        Using OFD As New FolderBrowserDialog
            With OFD
                .Description = "Select folder to convert..."
                .SelectedPath = My.Settings.DefaultProjectDirectory.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar) & Path.DirectorySeparatorChar
                .ShowNewFolderButton = False
                If .ShowDialog(Me) <> DialogResult.OK Then
                    Me.mnuConvertConvertFolder.Enabled = True
                    Exit Sub
                End If
                SourceFolderName = .SelectedPath
                Dim fullFolderPath As (SolutionRoot As String, ProjectRelativePath As String) = Me.GetSavePath(.SelectedPath, PromptIfDirExsits:=True)
                solutionSavePath = Path.Combine(fullFolderPath.SolutionRoot, fullFolderPath.ProjectRelativePath)
            End With
        End Using
        If Not Directory.Exists(SourceFolderName) Then
            MsgBox($"{SourceFolderName} is not a directory.",
                   MsgBoxStyle.OkOnly Or MsgBoxStyle.Exclamation Or MsgBoxStyle.MsgBoxSetForeground,
                   Title:="Convert C# to Visual Basic")
            Me.mnuConvertConvertFolder.Enabled = True
            Exit Sub
        End If
        If String.IsNullOrWhiteSpace(solutionSavePath) Then
            MsgBox($"Conversion aborted.",
                   MsgBoxStyle.OkOnly Or MsgBoxStyle.Exclamation Or MsgBoxStyle.MsgBoxSetForeground,
                   Title:="Convert C# to Visual Basic")
            Me.mnuConvertConvertFolder.Enabled = True
            Exit Sub
        End If
        Dim LastFileNameWithPath As String = If(My.Settings.StartFolderConvertFromLastFile, My.Settings.MRU_Data.Last, "")
        Dim Stats As New ProcessingStats(LastFileNameWithPath)
        _cancellationTokenSource = New CancellationTokenSource
        Me.StatusStripElapasedTimeLabel.Text = ""
        ' Create new stopwatch
        Dim stopwatch As New Stopwatch()
        ' Begin timing
        stopwatch.Start()
        Dim prompt As String
        If Await ProcessFilesAsync(Me, SourceFolderName,
            solutionSavePath,
            SourceLanguageExtension:="cs",
            Stats:=Stats,
            CancelToken:=_cancellationTokenSource.Token
                                     ).ConfigureAwait(True) Then
            stopwatch.Stop()
            If _cancellationTokenSource.Token.IsCancellationRequested Then
                prompt = $"Conversion canceled, {Stats.FilesProcessed} files completed successfully."
            Else
                prompt = $"Conversion completed, {Stats.FilesProcessed} files completed, with {My.Settings.IgnoreFileList.Count} files ignored."
                Me.mnuConvertStartFolderConvertFromLastFile.Checked = False
                My.Settings.StartFolderConvertFromLastFile = False
                My.Settings.Save()
                Application.DoEvents()
            End If
        Else
            stopwatch.Stop()
            prompt = "Conversion stopped."
        End If
        MsgBox(prompt,
               MsgBoxStyle.OkOnly Or MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground,
               Title:="Convert C# To Visual Basic")
        Dim elapsed As TimeSpan = stopwatch.Elapsed
        Me.StatusStripElapasedTimeLabel.Text = $"Elapsed Time - {elapsed.Hours}: {elapsed.Minutes}:{elapsed.Seconds}.{elapsed.Milliseconds}"
        Me.mnuConvertConvertFolder.Enabled = True

    End Sub

    Private Sub mnuConvertStartFolderConvertFromLastFile_Click(sender As Object, e As EventArgs) Handles mnuConvertStartFolderConvertFromLastFile.Click
        My.Settings.StartFolderConvertFromLastFile = CType(sender, ToolStripMenuItem).Checked
        My.Settings.Save()
        Application.DoEvents()
    End Sub

    Private Sub mnuEdit_DropDownOpening(sender As Object, e As EventArgs) Handles mnuEdit.DropDownOpening
        If Me.TSFindToolStrip.Visible Then
            Me.mnuEditFind.Text = "Hide &Find Toolbar"
        Else
            Me.mnuEditFind.Text = "Show &Find Toolbar"
        End If
        If TypeOf Me.CurrentBuffer Is RichTextBox Then
            Dim sourceBuffer As RichTextBox = CType(Me.CurrentBuffer, RichTextBox)
            Me.mnuEditCopy.Enabled = sourceBuffer.TextLength > 0 AndAlso sourceBuffer.SelectedText.Length > 0
            Me.mnuEditCut.Enabled = sourceBuffer.TextLength > 0 AndAlso sourceBuffer.SelectedText.Length > 0
            Me.mnuEditPaste.Enabled = sourceBuffer.CanPaste(DataFormats.GetFormat(DataFormats.Rtf)) OrElse sourceBuffer.CanPaste(DataFormats.GetFormat(DataFormats.Text))
            Me.mnuEditUndo.Enabled = sourceBuffer.CanUndo
            Me.mnuEditRedo.Enabled = sourceBuffer.CanRedo
        Else
            Me.mnuEditCut.Enabled = False
            Me.mnuEditPaste.Enabled = False
            Me.mnuEditUndo.Enabled = False
            Me.mnuEditRedo.Enabled = False
        End If
    End Sub

    Private Sub mnuEditCopy_Click(sender As Object, e As EventArgs) Handles mnuEditCopy.Click
        If TypeOf Me.CurrentBuffer Is RichTextBox Then
            CType(Me.CurrentBuffer, RichTextBox).Copy()
        Else
            Clipboard.SetText(CType(Me.CurrentBuffer, ListBox).Text)
        End If
    End Sub

    Private Sub mnuEditCut_Click(sender As Object, e As EventArgs) Handles mnuEditCut.Click
        If TypeOf Me.CurrentBuffer Is RichTextBox Then
            CType(Me.CurrentBuffer, RichTextBox).Cut()
        End If
    End Sub

    Private Sub mnuEditFind_Click(sender As Object, e As EventArgs) Handles mnuEditFind.Click
        Me.TSFindToolStrip.Visible = Not Me.TSFindToolStrip.Visible
    End Sub

    Private Sub mnuEditPaste_Click(sender As Object, e As EventArgs) Handles mnuEditPaste.Click
        If TypeOf Me.CurrentBuffer Is RichTextBox Then
            CType(Me.CurrentBuffer, RichTextBox).SelectedText = Clipboard.GetText(TextDataFormat.Text)
        End If
    End Sub

    Private Sub mnuEditRedo_Click(sender As Object, e As EventArgs) Handles mnuEditRedo.Click
        Dim sourceControl As RichTextBox = TryCast(Me.CurrentBuffer, RichTextBox)
        If sourceControl IsNot Nothing AndAlso sourceControl.CanRedo Then
            sourceControl.Redo()
        End If

    End Sub

    Private Sub mnuEditSelectAll_Click(sender As Object, e As EventArgs) Handles mnuEditSelectAll.Click
        Dim sourceControl As RichTextBox = TryCast(Me.CurrentBuffer, RichTextBox)
        If sourceControl IsNot Nothing Then
            sourceControl.SelectAll()
        End If

    End Sub

    Private Sub mnuEditUndo_Click(sender As Object, e As EventArgs) Handles mnuEditUndo.Click
        Dim sourceControl As RichTextBox = TryCast(Me.CurrentBuffer, RichTextBox)
        If sourceControl IsNot Nothing AndAlso sourceControl.CanUndo Then
            sourceControl.Undo()
        End If
    End Sub

    Private Sub mnuFile_DropDownOpening(sender As Object, e As EventArgs) Handles mnuFile.DropDownOpening
        Me.mnuFileLoadLastSnippet.Enabled = File.Exists(s_snippetFileWithPath)
    End Sub

    Private Sub mnuFileExit_Click(sender As Object, e As EventArgs) Handles mnuFileExit.Click
        Me.Close()
        End
    End Sub

    Private Async Sub mnuFileLastFolder_Click(sender As Object, e As EventArgs) Handles mnuFileLastFolder.Click
        Dim FolderName As String = CType(sender, ToolStripMenuItem).Text
        If Directory.Exists(FolderName) Then
            Dim SourceLanguageExtension As String = "cs"
            Dim fullFolderPath As (SolutionRoot As String, ProjectRelativePath As String) = Me.GetSavePath(FolderName, PromptIfDirExsits:=True)
            Dim targetSavePath As String = Path.Combine(fullFolderPath.SolutionRoot, fullFolderPath.ProjectRelativePath)
            If String.IsNullOrWhiteSpace(targetSavePath) Then
                Exit Sub
            End If
            ' This path is a directory.
            Dim LastFileNameWithPath As String = If(My.Settings.StartFolderConvertFromLastFile, My.Settings.MRU_Data.Last, "")
            Dim Stats As New ProcessingStats(LastFileNameWithPath)
            Dim FilesProcessed As Long = 0
            If _cancellationTokenSource IsNot Nothing Then
                _cancellationTokenSource.Dispose()
            End If
            _cancellationTokenSource = New CancellationTokenSource
            If Await ProcessFilesAsync(Me, FolderName, targetSavePath, SourceLanguageExtension, Stats, _cancellationTokenSource.Token).ConfigureAwait(True) Then
                MsgBox($"Conversion completed.",
                       MsgBoxStyle.OkOnly Or MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground)
            End If
        Else
            MsgBox($"{FolderName} Is Not a directory.",
                   MsgBoxStyle.OkOnly Or MsgBoxStyle.Exclamation Or MsgBoxStyle.MsgBoxSetForeground)
        End If
    End Sub

    Private Sub mnuFileLastProject_Click(sender As Object, e As EventArgs) Handles mnuFileLastProject.Click
        Dim projectFileName As String = My.Settings.LastProject
        ProcessProjectOrSolutionAsync(Me, projectFileName)
    End Sub

    Private Sub mnuFileLastSolution_Click(sender As Object, e As EventArgs) Handles mnuFileLastSolution.Click
        Dim solutionFileName As String = My.Settings.LastSolution
        ProcessProjectOrSolutionAsync(Me, solutionFileName)
    End Sub

    Private Sub mnuFileOpen_Click(sender As Object, e As EventArgs) Handles mnuFileOpen.Click
        Dim LanguageExtension As String = "cs"
        With Me.OpenFileDialog1
            .AddExtension = True
            .DefaultExt = LanguageExtension
            .InitialDirectory = My.Settings.DefaultProjectDirectory
            .FileName = ""
            .Filter = "C# Code Files (*.cs)|*.cs"
            .FilterIndex = 0
            .Multiselect = False
            .ReadOnlyChecked = True
            .Title = $"Open C# Source file"
            .ValidateNames = True
            If .ShowDialog = DialogResult.OK Then
                OpenSourceFile(Me, Me.OpenFileDialog1.FileName)
            End If
        End With
    End Sub

    Private Sub mnuFileOpenProject_Click(sender As Object, e As EventArgs) Handles mnuFileConvertProject.Click
        With Me.OpenFileDialog1
            .AddExtension = True
            .CheckFileExists = True
            .CheckPathExists = True
            .FileName = ""
            .Filter = "C# Project or Solution (*.csproj, *.sln)|*.csproj; *.sln"
            .FilterIndex = 0
            .Multiselect = False
            .ReadOnlyChecked = True
            .Title = $"Open Project/Solution"
            .ValidateNames = True
            If .ShowDialog <> DialogResult.OK Then
                Exit Sub
            End If
            ProcessProjectOrSolutionAsync(Me, .FileName)
        End With
    End Sub

    Private Sub mnuFileSaveAs_Click(sender As Object, e As EventArgs) Handles mnuFileSaveAs.Click

        Me.SaveFileDialog1.AddExtension = True
        Me.SaveFileDialog1.CreatePrompt = False
        Me.SaveFileDialog1.DefaultExt = "vb"
        Me.SaveFileDialog1.FileName = Path.ChangeExtension(Me.OpenFileDialog1.SafeFileName, "vb")
        Me.SaveFileDialog1.Filter = "VB Code Files (*.vb)|*.vb"
        Me.SaveFileDialog1.FilterIndex = 0
        Me.SaveFileDialog1.OverwritePrompt = True
        Me.SaveFileDialog1.SupportMultiDottedExtensions = False
        Me.SaveFileDialog1.Title = $"Save {Me.SaveFileDialog1.DefaultExt} Output..."
        Me.SaveFileDialog1.ValidateNames = True
        Dim FileSaveResult As DialogResult = Me.SaveFileDialog1.ShowDialog
        If FileSaveResult = DialogResult.OK Then
            Me.ConversionOutput.SaveFile(Me.SaveFileDialog1.FileName, RichTextBoxStreamType.PlainText)
        End If
    End Sub

    Private Sub mnuFileSnippetLoadLast_Click(sender As Object, e As EventArgs) Handles mnuFileLoadLastSnippet.Click
        If My.Settings.ColorizeInput Then
            Me.mnuConvertConvertSnippet.Enabled = 0 <> LoadInputBufferFromStream(Me, s_snippetFileWithPath)
        Else
            Me.ConversionInput.LoadFile(s_snippetFileWithPath, RichTextBoxStreamType.PlainText)
        End If
        Me.mnuCompile.Enabled = True
    End Sub

    Private Sub mnuFileSnippetSave_Click(sender As Object, e As EventArgs) Handles mnuFileSaveSnippet.Click
        If Me.ConversionInput.TextLength = 0 Then
            Exit Sub
        End If
        Me.ConversionInput.SaveFile(s_snippetFileWithPath, RichTextBoxStreamType.PlainText)
    End Sub

    Private Sub mnuHelpAboutMenuItem_Click(sender As Object, e As EventArgs) Handles mnuHelpAboutMenuItem.Click
        Dim About As New AboutBox1
        About.ShowDialog()
        About.Dispose()
    End Sub

    Private Sub mnuHelpReportIssueMenuItem_Click(sender As Object, e As EventArgs) Handles mnuHelpReportIssueMenuItem.Click
        Dim webAddress As String = "http://github.com/paul1956/CSharpToVB/issues"
        Try
            'Devices.Mouse.OverrideCursor = Cursors.AppStarting
            Me.Cursor = Cursors.AppStarting
            launchBrowser(webAddress)
        Catch ex As Exception
            Stop
            Throw
        Finally
            Me.Cursor = Cursors.AppStarting
            'Devices.Mouse.OverrideCursor = Nothing
        End Try
    End Sub

    Private Sub mnuOptionDefaultFramework_CheckedChanged(sender As Object, e As EventArgs) Handles mnuOptionsDefaultFramework.CheckedChanged
        Dim MenuItem As ToolStripMenuItem = CType(sender, ToolStripMenuItem)
        If Not MenuItem.Checked Then
            Exit Sub
        End If
        For Each kvp As KeyValuePair(Of String, (Item As ToolStripMenuItem, Parent As ToolStripMenuItem)) In _frameworkVersionList
            If kvp.Key = MenuItem.Text Then
                MenuItem.Enabled = False
                My.Settings.Framework = MenuItem.Text
                My.Settings.Save()
                kvp.Value.Parent.Checked = True
                For Each ParentItem As ToolStripMenuItem In _frameworkTypeList
                    ParentItem.Checked = kvp.Value.Parent.Text = ParentItem.Text
                Next
            Else
                kvp.Value.Item.Enabled = True
                If kvp.Value.Item.Checked Then
                    kvp.Value.Item.Checked = False
                End If
            End If
        Next
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
        Dim IgnoreFilesWithErrorsDialog As New IgnoreFilesWithErrorsList
        IgnoreFilesWithErrorsDialog.ShowDialog(Me)
        If Not String.IsNullOrWhiteSpace(IgnoreFilesWithErrorsDialog.FileToLoad) Then
            OpenSourceFile(Me, IgnoreFilesWithErrorsDialog.FileToLoad)
        End If
        IgnoreFilesWithErrorsDialog.Dispose()
    End Sub

    Private Sub mnuOptionsFolderConversionsOptionsSkipAutoGenerated_Click(sender As Object, e As EventArgs) Handles mnuOptionsFolderConversionsOptionsSkipAutoGenerated.Click
        My.Settings.SkipAutoGenerated = CType(sender, ToolStripMenuItem).Checked
        My.Settings.Save()
        Application.DoEvents()
    End Sub

    Private Sub mnuOptionsFolderConversionsOptionsSkipBinAndObjFolders_Click(sender As Object, e As EventArgs) Handles mnuOptionsFolderConversionsOptionsSkipBinAndObjFolders.Click
        My.Settings.SkipBinAndObjFolders = CType(sender, ToolStripMenuItem).Checked
        My.Settings.Save()
        Application.DoEvents()
    End Sub

    Private Sub mnuOptionsFolderConversionsOptionsSkipTestResourceFiles_Click(sender As Object, e As EventArgs) Handles mnuOptionsFolderConversionsOptionsSkipTestResourceFiles.Click
        My.Settings.SkipTestResourceFiles = CType(sender, ToolStripMenuItem).Checked
        My.Settings.Save()
        Application.DoEvents()
    End Sub

    Private Sub mnuOptionsPauseConvertOnSuccess_Click(sender As Object, e As EventArgs) Handles mnuOptionsPauseConvertOnSuccess.Click
        My.Settings.PauseConvertOnSuccess = Me.mnuOptionsPauseConvertOnSuccess.Checked
        My.Settings.Save()
        Application.DoEvents()
    End Sub

    Private Sub mnuView_DropDownOpening(sender As Object, e As EventArgs) Handles mnuView.DropDownOpening
        Me.mnuViewShowDestinationLineNumbers.Enabled = Me.ConversionOutput.TextLength > 0
        Me.mnuViewShowSourceLineNumbers.Enabled = Me.ConversionInput.TextLength > 0
    End Sub

    Private Sub mnuViewShowDestinationLineNumbers_Click(sender As Object, e As EventArgs) Handles mnuViewShowDestinationLineNumbers.Click
        Dim checked As Boolean = CType(sender, ToolStripMenuItem).Checked
        Me.LineNumbersForConversionOutput.Visible = checked
        My.Settings.ShowDestinationLineNumbers = checked
        My.Settings.Save()
    End Sub

    Private Sub mnuViewShowSourceLineNumbers_CheckStateChanged(sender As Object, e As EventArgs) Handles mnuViewShowSourceLineNumbers.CheckStateChanged
        Dim checked As Boolean = CType(sender, ToolStripMenuItem).Checked
        Me.LineNumbersForConversionInput.Visible = checked
    End Sub

    Private Sub mnuViewShowSourceLineNumbers_Click(sender As Object, e As EventArgs) Handles mnuViewShowSourceLineNumbers.Click
        Dim checked As Boolean = CType(sender, ToolStripMenuItem).Checked
        My.Settings.ShowSourceLineNumbers = checked
        My.Settings.Save()
    End Sub

    Private Sub SplitContainer1_SplitterMoved(sender As Object, e As SplitterEventArgs) Handles SplitContainer1.SplitterMoved
        Me.ListBoxFileList.Height = Me.SplitContainer1.Panel2.ClientSize.Height
        Me.ListBoxErrorList.Height = Me.SplitContainer1.Panel2.ClientSize.Height
    End Sub

    Private Sub StatusStripCurrentFileName_MouseDown(sender As Object, e As MouseEventArgs) Handles StatusStripCurrentFileName.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Right Then
            Clipboard.SetText(text:=CType(sender, ToolStripStatusLabel).Text)
        End If
    End Sub

    Protected Overrides Sub OnLoad(e As EventArgs)
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
        Me.UpdateLastFileMenu()

        If My.Settings.TSFindMRU_Data Is Nothing Then
            My.Settings.TSFindMRU_Data = New Specialized.StringCollection
            My.Settings.Save()
        End If
        Me.TSFindFindWhatComboBox.TSFindWhatMRUUpdateUI()

        ' display MRU if there are any items to display...
        Me.mnuFile.DropDownItems.FileMenuMRUUpdateUI(AddressOf Me.mnu_MRUList_Click)
    End Sub

    ''' <summary>
    ''' This will be used at runtime to add this Event to all items on mnuFileMRU list
    ''' </summary>
    ''' <param name="sender"></param>
    ''' <param name="e"></param>
    Friend Sub mnu_MRUList_Click(sender As Object, e As EventArgs)
        ' open the file...
        OpenSourceFile(Me, DirectCast(sender, ToolStripItem).Tag.ToString().Substring(startIndex:=4))
    End Sub

#Region "TSFind Events"

    Private Sub TSFindClearHighlightsButton_Click(sender As Object, e As EventArgs) Handles TSFindClearHighlightsButton.Click
        Dim selectionstart As Integer
        If Me.BufferToSearch.IsFlagSet(SearchBuffers.CS) Then
            selectionstart = Me.ConversionInput.SelectionStart
            Me.ConversionInput.SelectAll()
            Me.ConversionInput.SelectionBackColor = Color.White
            Me.ConversionInput.Select(selectionstart, 0)
            Me.ConversionInput.ScrollToCaret()
        End If
        If Me.BufferToSearch.IsFlagSet(SearchBuffers.VB) Then
            selectionstart = Me.ConversionOutput.SelectionStart
            Me.ConversionOutput.SelectAll()
            Me.ConversionOutput.SelectionBackColor = Color.White
            Me.ConversionOutput.Select(selectionstart, 0)
            Me.ConversionOutput.ScrollToCaret()
        End If
        Application.DoEvents()
    End Sub

    Private Sub TSFindFindNextButton_Click(sender As Object, e As EventArgs) Handles TSFindFindNextButton.Click
        Me.DoFind(SearchForward:=True)
    End Sub

    Private Sub TSFindFindPreviousButton_Click(sender As Object, e As EventArgs) Handles TSFindFindPreviousButton.Click
        Me.DoFind(SearchForward:=False)
    End Sub

    Private Sub TSFindFindWhatComboBox_Click(sender As Object, e As EventArgs) Handles TSFindFindWhatComboBox.Click
        If Me.TSFindFindWhatComboBox.Text = "Search..." Then
            Me.TSFindFindWhatComboBox.Text = ""
            Me.TSFindFindWhatComboBox.ForeColor = SystemColors.ControlText
        End If
    End Sub

    Private Sub TSFindFindWhatComboBox_Leave(sender As Object, e As EventArgs) Handles TSFindFindWhatComboBox.Leave
        If Not Me.TSFindFindWhatComboBox.Text.Any Then
            Me.TSFindFindWhatComboBox.ForeColor = SystemColors.GrayText
            Me.TSFindFindWhatComboBox.Text = "Search..."
        End If
    End Sub

    Private Sub TSFindFindWhatComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles TSFindFindWhatComboBox.SelectedIndexChanged
        Me.SetSearchControls()
    End Sub

    Private Sub TSFindFindWhatComboBox_TextChanged(sender As Object, e As EventArgs) Handles TSFindFindWhatComboBox.TextChanged
        Me.SetSearchControls()
    End Sub

    Private Sub TSFindLookInComboBox_Click(sender As Object, e As EventArgs) Handles TSFindLookInComboBox.Click
        Me.SetSearchControls()
    End Sub

    Private Sub TSFindLookInComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles TSFindLookInComboBox.SelectedIndexChanged
        Select Case Me.TSFindLookInComboBox.SelectedIndex
            Case 0
                Me.BufferToSearch = SearchBuffers.CS
            Case 1
                Me.BufferToSearch = SearchBuffers.VB
            Case 2
                Me.BufferToSearch = SearchBuffers.Both
        End Select
        Me.SetSearchControls()
    End Sub

    Private Sub TSFindMatchCaseCheckBox_Click(sender As Object, e As EventArgs) Handles TSFindMatchCaseCheckBox.Click
        My.Settings.TSFindMatchCase = Me.TSFindMatchCaseCheckBox.Checked
        My.Settings.Save()
    End Sub

    Private Sub TSFindMatchWholeWordCheckBox_Click(sender As Object, e As EventArgs) Handles TSFindMatchWholeWordCheckBox.Click
        My.Settings.TSFindMatchWholeWord = Me.TSFindMatchWholeWordCheckBox.Checked
        My.Settings.Save()
    End Sub

#End Region

#Region "Form Support Routines"

    Private Sub DoUpdate(progressStr As String)
        Me.ProjectConversionInitProgressLabel.Text = progressStr
        If progressStr.Any Then
            Me.ConversionInput.Text = ""
            Me.ConversionOutput.Text = ""
            Me.ProjectConversionInitProgressLabel.Visible = True
            Me.ProjectConversionInitProgressLabel.Left = Math.Max((Me.ConversionInput.Width \ 2) - (Me.ProjectConversionInitProgressLabel.Width \ 2), 0)
            Me.ProjectConversionInitProgressLabel.Top = (Me.ConversionInput.Height \ 2) - (Me.ProjectConversionInitProgressLabel.Height \ 2)

            Me.ProjectConversionInitProgressBar.Visible = True
            Me.ProjectConversionInitProgressBar.Left = Math.Max((Me.ConversionInput.Width \ 2) - (Me.ProjectConversionInitProgressBar.Width \ 2), 0)
            Me.ProjectConversionInitProgressBar.Top = Me.ProjectConversionInitProgressLabel.Bottom + 5
        Else
            Me.ProjectConversionInitProgressLabel.Visible = False
            Me.ProjectConversionInitProgressBar.Visible = False
        End If
        Application.DoEvents()
    End Sub

    Private Sub ResizeRichTextBuffers()
        Dim LineNumberInputWidth As Integer = If(Me.LineNumbersForConversionInput.Visible AndAlso Me.ConversionInput.TextLength > 0, Me.LineNumbersForConversionInput.Width, 0)
        Dim LineNumberOutputWidth As Integer = If(Me.LineNumbersForConversionOutput.Visible AndAlso Me.ConversionOutput.TextLength > 0, Me.LineNumbersForConversionOutput.Width, 0)
        Dim HalfClientWidth As Integer = Me.ClientSize.Width \ 2
        Me.ConversionInput.Left = LineNumberInputWidth - 1
        Me.ConversionInput.Width = HalfClientWidth - LineNumberInputWidth
        Me.ListBoxFileList.Width = HalfClientWidth

        Me.ConversionOutput.Width = Me.ClientSize.Width - (Me.ConversionInput.Width + LineNumberInputWidth + LineNumberOutputWidth)
        Me.ConversionOutput.Left = Me.ConversionInput.Width + LineNumberInputWidth + LineNumberOutputWidth

        Me.ListBoxErrorList.Left = HalfClientWidth
        Me.ListBoxErrorList.Width = HalfClientWidth
        Me.StatusStripCurrentFileName.Width = HalfClientWidth
    End Sub

    Friend Sub UpdateProgressLabels(progressStr As String)
        If Me.InvokeRequired Then
            Me.Invoke(Sub()
                          Me.DoUpdate(progressStr)
                      End Sub)
        Else
            Me.DoUpdate(progressStr)
        End If
    End Sub

#End Region

End Class
