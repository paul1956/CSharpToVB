' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.ComponentModel
Imports System.IO
Imports System.Net
Imports System.Reflection
Imports System.Threading
Imports CSharpToVBApp
Imports CSharpToVBConverter
Imports Microsoft.CodeAnalysis
Imports Microsoft.VisualBasic.ApplicationServices
Imports Microsoft.VisualBasic.FileIO
Imports ProgressReportLibrary

Partial Public Class Form1
    Private Const ProjectGitHubURL As String = "https://github.com/paul1956/CSharpToVB/"
    Private Shared ReadOnly s_snippetFileWithPath As String = Path.Combine(SpecialDirectories.MyDocuments, "CSharpToVBLastSnippet.RTF")
    Private ReadOnly _frameworkTypeList As New List(Of ToolStripMenuItem)
    Private ReadOnly _frameworkVersionList As New Dictionary(Of String, (item As ToolStripMenuItem, Parent As ToolStripMenuItem))
    Private _currentBuffer As Control
    Friend _cancellationTokenSource As CancellationTokenSource
    Friend _doNotFailOnError As Boolean
    Friend _inColorize As Boolean
    Friend _requestToConvert As ConvertRequest
    Friend _resultOfConversion As ConversionResult

    Public Sub New()
        Me.InitializeComponent()
    End Sub

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

    Friend Property BufferToSearch As SearchBuffers = SearchBuffers.CS

    Private Shared Function IsNewerVersion(LatestVersionStrings() As String, AppVersion As Version, ConverterVersion As Version) As Boolean
        If Not (String.IsNullOrWhiteSpace(LatestVersionStrings(0)) OrElse LatestVersionStrings(0) = AppVersion.ToString) Then
            Return True
        End If
        If LatestVersionStrings.Length = 1 Then
            Return False
        End If
        Return LatestVersionStrings(1) <> ConverterVersion.ToString
    End Function

    Private Sub ButtonStop_Click(sender As Object, e As EventArgs) Handles ButtonStopConversion.Click
        Me.ButtonStopConversion.Visible = False
        _cancellationTokenSource.Cancel()
        Application.DoEvents()
    End Sub

    Private Sub ButtonStop_MouseEnter(sender As Object, e As EventArgs) Handles ButtonStopConversion.MouseEnter
        LocalUseWaitCursor(Me, WaitCursorEnable:=False)
    End Sub

    Private Sub ButtonStop_MouseLeave(sender As Object, e As EventArgs) Handles ButtonStopConversion.MouseLeave
        Me.ButtonStopConversion.BackColor = SystemColors.Control
        LocalUseWaitCursor(Me, WaitCursorEnable:=Me.ButtonStopConversion.Visible)
    End Sub

    Private Sub ButtonStop_VisibleChanged(sender As Object, e As EventArgs) Handles ButtonStopConversion.VisibleChanged
        LocalUseWaitCursor(Me, WaitCursorEnable:=Me.ButtonStopConversion.Visible)
    End Sub

    Private Sub CheckForUpdates(ReportResults As Boolean)
        Try
            Dim request As WebRequest = WebRequest.Create($"{ProjectGitHubURL}blob/master/ReadMe.MD")
            request.Timeout = 4000
            Dim response As WebResponse = request.GetResponse()
            Using reader As New StreamReader(response.GetResponseStream())
                Dim line As String
                Dim index As Integer = -1
                Do
                    line = reader.ReadLine
                    If line Is Nothing Then
                        Exit Do
                    End If
                    If line.Contains("What's New in this release", StringComparison.Ordinal) Then
                        Exit Do
                    End If
                Loop
                Do
                    line = reader.ReadLine
                    If line Is Nothing Then
                        Exit Do
                    End If
                    index = line.IndexOf("New in ", StringComparison.OrdinalIgnoreCase)
                    Exit Do
                Loop
                If index < 0 Then
                    Exit Sub
                End If
                Dim versionStr As String = line.Substring(index + "New In ".Length)

                index = versionStr.IndexOf("<"c)
                If index > 0 Then
                    versionStr = versionStr.Substring(0, index)
                End If
                Dim gitHubVersion() As String = versionStr.Split("/")
                Dim codeConverterInfo As New AssemblyInfo(GetType(CodeWithOptions).Assembly)
                If IsNewerVersion(gitHubVersion, My.Application.Info.Version, codeConverterInfo.Version) Then
                    Me.StatusStripUpdateAvailable.Visible = True
                    If ReportResults Then
                        If MsgBox("There is a newer version available, do you want to install now?", MsgBoxStyle.YesNo, "Updates Available") = MsgBoxResult.Yes Then
                            Me.OpenURLInBrowser(ProjectGitHubURL)
                        End If
                    End If
                Else
                    Me.StatusStripUpdateAvailable.Visible = False
                    If ReportResults Then
                        MsgBox("You are running latest version", MsgBoxStyle.OkOnly, "No Updates Available")
                    End If
                End If
            End Using
        Catch ex As Exception
            If ReportResults Then
                MsgBox("Failed while checking for new  version: " + ex.Message, MsgBoxStyle.Information, "Version Check Failed")
            End If
        End Try
    End Sub

    Private Sub ContextMenuCopy_Click(sender As Object, e As EventArgs) Handles ContextMenuCopy.Click
        Dim sourceControl As RichTextBox
        Select Case True
            Case TypeOf Me.ContextMenuStrip1.SourceControl Is RichTextBox
                sourceControl = TryCast(Me.ContextMenuStrip1.SourceControl, RichTextBox)
            Case TypeOf Me.ContextMenuStrip1.SourceControl Is ListBox
                Dim selectedItem As NumberedListItem = CType(CType(Me.ContextMenuStrip1.SourceControl, ListBox).SelectedItem, NumberedListItem)
                Clipboard.SetText(selectedItem.SourceFileWithPath)
                Exit Sub
            Case Else
                If TypeOf Me.CurrentBuffer Is RichTextBox Then
                    sourceControl = CType(Me.CurrentBuffer, RichTextBox)
                Else
                    Exit Sub
                End If
        End Select
        sourceControl.Copy()
    End Sub

    Private Sub ContextMenuCut_Click(sender As Object, e As EventArgs) Handles ContextMenuCut.Click
        Dim sourceControl As RichTextBox = TryCast(Me.ContextMenuStrip1.SourceControl, RichTextBox)
        If sourceControl Is Nothing Then
            If TypeOf Me.CurrentBuffer Is RichTextBox Then
                sourceControl = CType(Me.CurrentBuffer, RichTextBox)
            Else
                Exit Sub
            End If
        End If
        sourceControl.Cut()
    End Sub

    Private Sub ContextMenuPaste_Click(sender As Object, e As EventArgs) Handles ContextMenuPaste.Click
        Dim sourceControl As RichTextBox = TryCast(Me.ContextMenuStrip1.SourceControl, RichTextBox)
        If sourceControl Is Nothing Then
            If TypeOf Me.CurrentBuffer Is RichTextBox Then
                sourceControl = CType(Me.CurrentBuffer, RichTextBox)
            Else
                Exit Sub
            End If
        End If
        If sourceControl.CanPaste(DataFormats.GetFormat(DataFormats.Rtf)) OrElse
            sourceControl.CanPaste(DataFormats.GetFormat(DataFormats.Text)) Then
            sourceControl.Paste(DataFormats.GetFormat("Text"))
        End If
    End Sub

    Private Sub ContextMenuRedo_Click(sender As Object, e As EventArgs) Handles ContextMenuRedo.Click
        Dim sourceControl As RichTextBox = TryCast(Me.ContextMenuStrip1.SourceControl, RichTextBox)
        If sourceControl Is Nothing Then
            If TypeOf Me.CurrentBuffer Is RichTextBox Then
                sourceControl = CType(Me.CurrentBuffer, RichTextBox)
            Else
                Exit Sub
            End If
        End If
        If sourceControl.CanRedo Then
            sourceControl.Redo()
        End If
    End Sub

    Private Sub ContextMenuSelectAll_Click(sender As Object, e As EventArgs) Handles ContextMenuSelectAll.Click
        Dim sourceControl As RichTextBox = TryCast(Me.ContextMenuStrip1.SourceControl, RichTextBox)
        If sourceControl Is Nothing Then
            If TypeOf Me.CurrentBuffer Is RichTextBox Then
                sourceControl = CType(Me.CurrentBuffer, RichTextBox)
            Else
                Exit Sub
            End If
        End If
        sourceControl.SelectAll()
    End Sub

    Private Sub ContextMenuStrip1_Opening(sender As Object, e As CancelEventArgs) Handles ContextMenuStrip1.Opening
        Dim contextMenu As ContextMenuStrip = CType(sender, ContextMenuStrip)

        If TypeOf Me.CurrentBuffer Is RichTextBox Then
            Dim sourceBuffer As RichTextBox = CType(Me.CurrentBuffer, RichTextBox)
            contextMenu.Items(contextMenu.IndexOf(NameOf(ContextMenuCopy))).Enabled = sourceBuffer.TextLength > 0 And sourceBuffer.SelectedText.Any
            contextMenu.Items(contextMenu.IndexOf(NameOf(ContextMenuCut))).Enabled = sourceBuffer.TextLength > 0 And sourceBuffer.SelectedText.Any
            contextMenu.Items(contextMenu.IndexOf(NameOf(ContextMenuPaste))).Enabled = sourceBuffer.CanPaste(DataFormats.GetFormat(DataFormats.Rtf)) OrElse sourceBuffer.CanPaste(DataFormats.GetFormat(DataFormats.Text))
            contextMenu.Items(contextMenu.IndexOf(NameOf(ContextMenuRedo))).Enabled = sourceBuffer.CanRedo
            contextMenu.Items(contextMenu.IndexOf(NameOf(ContextMenuUndo))).Enabled = sourceBuffer.CanUndo
        Else
            contextMenu.Items(contextMenu.IndexOf(NameOf(ContextMenuCut))).Enabled = False
            contextMenu.Items(contextMenu.IndexOf(NameOf(ContextMenuPaste))).Enabled = False
            contextMenu.Items(contextMenu.IndexOf(NameOf(ContextMenuRedo))).Enabled = False
            contextMenu.Items(contextMenu.IndexOf(NameOf(ContextMenuUndo))).Enabled = False
        End If
    End Sub

    Private Sub ContextMenuUndo_Click(sender As Object, e As EventArgs) Handles ContextMenuUndo.Click
        Dim sourceControl As RichTextBox = CType(Me.ContextMenuStrip1.SourceControl, RichTextBox)
        If sourceControl Is Nothing Then
            If TypeOf Me.CurrentBuffer Is RichTextBox Then
                sourceControl = CType(Me.CurrentBuffer, RichTextBox)
            Else
                Exit Sub
            End If
        End If
        If sourceControl.CanUndo Then
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
        Dim outputBufferInUse As Boolean = CType(sender, RichTextBox).TextLength > 0
        Me.mnuViewShowDestinationLineNumbers.Checked = outputBufferInUse And My.Settings.ShowDestinationLineNumbers
        Me.mnuCompile.Enabled = outputBufferInUse
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

        Me.UpdateLastFileMenu()
        Me.TSFindFindWhatComboBox.TSFindWhatMRUUpdateUI()

        ' display MRU if there are any items to display...
        Me.mnuFile.DropDownItems.FileMenuMRUUpdateUI(AddressOf Me.mnu_MRUList_Click)

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
        End If

        If My.Settings.EditorFont.Name <> My.Settings.EditorFontName Then
            My.Settings.EditorFont = New Font("Consolas", 11.0!, FontStyle.Regular)
            My.Settings.EditorFontName = "Consolas"
        End If
        Me.ConversionInput.Font = My.Settings.EditorFont
        Me.ConversionOutput.Font = My.Settings.EditorFont
        My.Settings.Save()

        Me.Width = Screen.PrimaryScreen.Bounds.Width
        Me.Height = CInt(Screen.PrimaryScreen.Bounds.Height * 0.95)

        Me.ListBoxFileList.Height = Me.SplitContainer1.Panel2.ClientSize.Height
        Me.ListBoxErrorList.Height = Me.SplitContainer1.Panel2.ClientSize.Height

        For Each frameworkType As ToolStripMenuItem In Me.mnuOptionsDefaultFramework.DropDownItems
            If frameworkType.Text = ".Net Full Framework" Then
                For Each f As String In GetAllFrameworkVersions()
                    frameworkType.DropDownItems.AddDropDownMenuItem(f)
                Next
            Else
                For Each f As String In GetAllCoreVersions()
                    frameworkType.DropDownItems.AddDropDownMenuItem(f)
                Next
            End If
        Next
        For Each frameworkType As ToolStripMenuItem In Me.mnuOptionsDefaultFramework.DropDownItems
            _frameworkTypeList.Add(frameworkType)
            For Each frameworkVersion As ToolStripMenuItem In frameworkType.DropDownItems
                If frameworkVersion.Text = My.Settings.Framework Then
                    frameworkType.Checked = True
                    frameworkVersion.Checked = True
                    frameworkVersion.Enabled = False
                Else
                    frameworkType.Checked = False
                    frameworkVersion.Checked = False
                    frameworkVersion.Enabled = True
                End If
                AddHandler frameworkVersion.CheckedChanged, AddressOf Me.mnuOptionDefaultFramework_CheckedChanged
                _frameworkVersionList.Add(frameworkVersion.Text, (frameworkVersion, frameworkType))
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
        Me.TSFindLookInComboBox.DropDownStyle = ComboBoxStyle.Simple
        Me.TSFindLookInComboBox.SelectedIndex = 0
        Me.TSFindMatchCaseCheckBox.Checked = My.Settings.TSFindMatchCase
        Me.TSFindMatchWholeWordCheckBox.Checked = My.Settings.TSFindMatchWholeWord
        Application.DoEvents()
        Me.CheckForUpdates(ReportResults:=False)
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
        Dim lineText As String = box.Text
        If Not lineText.StartsWith("BC", StringComparison.Ordinal) Then
            Exit Sub
        End If
        Dim startIndex As Integer = lineText.IndexOf(" Line = ", StringComparison.OrdinalIgnoreCase) + 8
        If startIndex <= 0 Then
            Exit Sub
        End If
        Dim count As Integer = lineText.Substring(startIndex).IndexOf(" ", StringComparison.OrdinalIgnoreCase)
        Dim lineStartPosition As Integer = Me.ConversionOutput.GetFirstCharIndexFromLine(CInt(lineText.Substring(startIndex, count)) - 1)

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
        Using fileList As ListBox = CType(sender, ListBox)
            If fileList.Items.Count = 0 Then
                Exit Sub
            End If
            Dim item As NumberedListItem = CType(fileList.SelectedItem, NumberedListItem)

            Dim sourceFileNameWithPath As String = item.SourceFileWithPath
            If String.IsNullOrWhiteSpace(sourceFileNameWithPath) OrElse Not File.Exists(sourceFileNameWithPath) Then
                Exit Sub
            End If
            LoadInputBufferFromStream(Me, sourceFileNameWithPath)
            Dim convertedFileNameWithPath As String = item.ValueItem
            If Not File.Exists(convertedFileNameWithPath) Then
                Exit Sub
            End If

            LoadOutputBufferFromStream(Me, convertedFileNameWithPath)
        End Using
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
        Dim preprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
            New KeyValuePair(Of String, Object)(My.Settings.Framework, True)
        }
        Compile_Colorize(Me, Me.ConversionOutput.Text, preprocessorSymbols)
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

        Dim dontDisplayLineNumbers As Boolean = Await Convert_Compile_ColorizeAsync(Me,
                                                                                    _requestToConvert,
                                                                                    New List(Of String) From {My.Settings.Framework},
                                                                                    New List(Of KeyValuePair(Of String, Object)) From {KeyValuePair.Create(Of String, Object)(My.Settings.Framework, True)},
                                                                                    SharedReferences.CSharpReferences(Assembly.Load("System.Windows.Forms").Location,
                                                                                    OptionalReference:=Nothing).ToArray,
                                                                                    _cancellationTokenSource.Token
                                                                                   ).ConfigureAwait(True)
        If _requestToConvert.CancelToken.IsCancellationRequested Then
            MsgBox($"Conversion canceled.",
                       MsgBoxStyle.OkOnly Or MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground,
                       Title:="C# to Visual Basic")
            Me.StatusStripConversionProgressBar.Clear()
        End If
        SetButtonStopAndCursor(MeForm:=Me, StopButton:=Me.ButtonStopConversion, StopButtonVisible:=False)
        Me.LineNumbersForConversionOutput.Visible = (Not dontDisplayLineNumbers) OrElse My.Settings.ShowDestinationLineNumbers
    End Sub

    Private Async Sub mnuConvertFolder_Click(sender As Object, e As EventArgs) Handles mnuConvertConvertFolder.Click
        Me.mnuConvertConvertFolder.Enabled = False
        Me.LineNumbersForConversionInput.Visible = False
        Me.LineNumbersForConversionOutput.Visible = False
        Me.StatusStripCurrentFileName.Text = ""
        Dim sourceFolderName As String
        Dim solutionSavePath As String
        Using browseFolderDialog As New FolderBrowserDialog
            With browseFolderDialog
                .Description = "Select folder to convert..."
                .SelectedPath = My.Settings.DefaultProjectDirectory.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar) & Path.DirectorySeparatorChar
                .ShowNewFolderButton = False
                If .ShowDialog(Me) <> DialogResult.OK Then
                    Me.mnuConvertConvertFolder.Enabled = True
                    Exit Sub
                End If
                sourceFolderName = .SelectedPath
                Dim fullFolderPath As (SolutionRoot As String, ProjectRelativePath As String) = Me.GetSavePath(.SelectedPath, PromptIfDirExsits:=True)
                solutionSavePath = Path.Combine(fullFolderPath.SolutionRoot, fullFolderPath.ProjectRelativePath)
            End With
        End Using
        If Not Directory.Exists(sourceFolderName) Then
            MsgBox($"{sourceFolderName} is not a directory.",
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
        Dim lastFileNameWithPath As String = If(My.Settings.StartFolderConvertFromLastFile, My.Settings.MRU_Data.Last, "")
        Dim stats As New ProcessingStats(lastFileNameWithPath)
        _cancellationTokenSource = New CancellationTokenSource
        Me.StatusStripElapasedTimeLabel.Text = ""
        ' Create new stopwatch
        Dim prompt As String
        If Await ProcessFilesAsync(Me,
                                   sourceFolderName,
                                   solutionSavePath,
                                   SourceLanguageExtension:="cs",
                                   stats,
                                   CancelToken:=_cancellationTokenSource.Token
                                  ).ConfigureAwait(True) Then
            stats.ElapasedTimer.Stop()
            If _cancellationTokenSource.Token.IsCancellationRequested Then
                prompt = $"Conversion canceled, {stats.FilesProcessed} files completed successfully."
            Else
                prompt = $"Conversion completed, {stats.FilesProcessed} files completed, with {My.Settings.IgnoreFileList.Count} files ignored."
                Me.mnuConvertStartFolderConvertFromLastFile.Checked = False
                My.Settings.StartFolderConvertFromLastFile = False
                My.Settings.Save()
                Application.DoEvents()
            End If
        Else
            stats.ElapasedTimer.Stop()
            prompt = "Conversion stopped."
        End If
        MsgBox(prompt,
               MsgBoxStyle.OkOnly Or MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground,
               Title:="Convert C# To Visual Basic")
        Dim elapsed As TimeSpan = stats.ElapasedTimer.Elapsed
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
        Dim folderName As String = CType(sender, ToolStripMenuItem).Text
        If Directory.Exists(folderName) Then
            Dim srcLanguageExtension As String = "cs"
            Dim fullFolderPath As (SolutionRoot As String, ProjectRelativePath As String) = Me.GetSavePath(folderName, PromptIfDirExsits:=True)
            Dim targetSavePath As String = Path.Combine(fullFolderPath.SolutionRoot, fullFolderPath.ProjectRelativePath)
            If String.IsNullOrWhiteSpace(targetSavePath) Then
                Exit Sub
            End If
            ' This path is a directory.
            Dim lastFileNameWithPath As String = If(My.Settings.StartFolderConvertFromLastFile, My.Settings.MRU_Data.Last, "")
            Dim stats As New ProcessingStats(lastFileNameWithPath)
            Dim filesProcessed As Long = 0
            If _cancellationTokenSource IsNot Nothing Then
                _cancellationTokenSource.Dispose()
            End If
            _cancellationTokenSource = New CancellationTokenSource
            If Await ProcessFilesAsync(Me, folderName, targetSavePath, srcLanguageExtension, stats, _cancellationTokenSource.Token).ConfigureAwait(True) Then
                MsgBox($"Conversion completed.",
                       MsgBoxStyle.OkOnly Or MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground)
            End If
        Else
            MsgBox($"{folderName} Is Not a directory.",
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
        With Me.OpenFileDialog1
            .AddExtension = True
            .DefaultExt = "cs"
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
            Dim colorizeOutput As Boolean = My.Settings.ColorizeOutput
            My.Settings.ColorizeOutput = colorizeOutput = False
            ProcessProjectOrSolutionAsync(Me, .FileName)
            My.Settings.ColorizeOutput = colorizeOutput = colorizeOutput
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
        Dim fileSaveResult As DialogResult = Me.SaveFileDialog1.ShowDialog
        If fileSaveResult = DialogResult.OK Then
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
        Using about As New AboutBox1
            about.ShowDialog()
        End Using
    End Sub

    Private Sub mnuHelpCheckForUpdatesMenuItem_Click(sender As Object, e As EventArgs) Handles mnuHelpCheckForUpdatesMenuItem.Click
        Me.CheckForUpdates(ReportResults:=True)
    End Sub

    Private Sub mnuHelpReportIssueMenuItem_Click(sender As Object, e As EventArgs) Handles mnuHelpReportIssueMenuItem.Click
        Me.OpenURLInBrowser($"{ProjectGitHubURL}issues")
    End Sub

    Private Sub mnuOptionDefaultFramework_CheckedChanged(sender As Object, e As EventArgs) Handles mnuOptionsDefaultFramework.CheckedChanged
        Dim menuItem As ToolStripMenuItem = CType(sender, ToolStripMenuItem)
        If Not menuItem.Checked Then
            Exit Sub
        End If
        For Each kvp As KeyValuePair(Of String, (item As ToolStripMenuItem, Parent As ToolStripMenuItem)) In _frameworkVersionList
            If kvp.Key = menuItem.Text Then
                menuItem.Enabled = False
                My.Settings.Framework = menuItem.Text
                My.Settings.Save()
                kvp.Value.Parent.Checked = True
                For Each parentItem As ToolStripMenuItem In _frameworkTypeList
                    parentItem.Checked = kvp.Value.Parent.Text = parentItem.Text
                Next
            Else
                kvp.Value.item.Enabled = True
                If kvp.Value.item.Checked Then
                    kvp.Value.item.Checked = False
                End If
            End If
        Next
    End Sub

    Private Sub mnuOptionsAddFilesToIgnoreFilesEithErrorsList_Click(sender As Object, e As EventArgs) Handles mnuOptionsAddFilesToIgnoreFilesEithErrorsList.Click
        Dim srcFileNameWithPath As String = My.Settings.MRU_Data.Last
        If Not My.Settings.IgnoreFileList.Contains(srcFileNameWithPath) Then
            My.Settings.IgnoreFileList.Add(srcFileNameWithPath)
            My.Settings.Save()
        End If
    End Sub

    Private Sub mnuOptionsAdvanced_Click(sender As Object, e As EventArgs) Handles mnuOptionsAdvanced.Click
        Using o As New OptionsDialog
            o.MainForm = Me
            o.ShowDialog(Me)
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
        IgnoreFilesWithErrorsListDialog.ShowDialog(Me)
        If Not String.IsNullOrWhiteSpace(IgnoreFilesWithErrorsListDialog.FileToLoad) Then
            OpenSourceFile(Me, IgnoreFilesWithErrorsListDialog.FileToLoad)
        End If
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

    Private Sub OpenURLInBrowser(webAddress As String)
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

    Private Sub SplitContainer1_SplitterMoved(sender As Object, e As SplitterEventArgs) Handles SplitContainer1.SplitterMoved
        Me.ListBoxFileList.Height = Me.SplitContainer1.Panel2.ClientSize.Height
        Me.ListBoxErrorList.Height = Me.SplitContainer1.Panel2.ClientSize.Height
    End Sub

    Private Sub StatusStripCurrentFileName_MouseDown(sender As Object, e As MouseEventArgs) Handles StatusStripCurrentFileName.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Right Then
            Clipboard.SetText(text:=CType(sender, ToolStripStatusLabel).Text)
        End If
    End Sub

    Private Sub StatusStripUpdateAvailable_Click(sender As Object, e As EventArgs) Handles StatusStripUpdateAvailable.Click
        Me.OpenURLInBrowser(ProjectGitHubURL)
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

        If My.Settings.TSFindMRU_Data Is Nothing Then
            My.Settings.TSFindMRU_Data = New Specialized.StringCollection
            My.Settings.Save()
        End If

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

    Private Sub ResizeRichTextBuffers()
        Dim lineNumberInputWidth As Integer = If(Me.LineNumbersForConversionInput.Visible AndAlso Me.ConversionInput.TextLength > 0, Me.LineNumbersForConversionInput.Width, 0)
        Dim lineNumberOutputWidth As Integer = If(Me.LineNumbersForConversionOutput.Visible AndAlso Me.ConversionOutput.TextLength > 0, Me.LineNumbersForConversionOutput.Width, 0)
        Dim halfClientWidth As Integer = Me.ClientSize.Width \ 2
        Me.ConversionInput.Left = lineNumberInputWidth - 1
        Me.ConversionInput.Width = halfClientWidth - lineNumberInputWidth
        Me.ListBoxFileList.Width = halfClientWidth

        Me.ConversionOutput.Width = Me.ClientSize.Width - (Me.ConversionInput.Width + lineNumberInputWidth + lineNumberOutputWidth)
        Me.ConversionOutput.Left = Me.ConversionInput.Width + lineNumberInputWidth + lineNumberOutputWidth

        Me.ListBoxErrorList.Left = halfClientWidth
        Me.ListBoxErrorList.Width = halfClientWidth
        Me.StatusStripCurrentFileName.Width = halfClientWidth
    End Sub

    Friend Sub UpdateProgress(progressStr As String)
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

    Friend Sub UpdateProgressLabels(progressStr As String)
        If Me.InvokeRequired Then
            Me.Invoke(Sub()
                          Me.UpdateProgress(progressStr)
                      End Sub)
        Else
            Me.UpdateProgress(progressStr)
        End If
    End Sub

#End Region

End Class
