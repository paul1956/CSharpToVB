' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.ComponentModel
Imports System.IO
Imports System.Net.Http
Imports System.Text
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.VisualBasic.FileIO

Partial Public Class Form1
    Private Shared ReadOnly s_snippetFileWithPath As String = Path.Combine(SpecialDirectories.MyDocuments, "CSharpToVBLastSnippet.RTF")
    Private Shared _loading As Boolean = True
    Private ReadOnly _frameworkTypeList As New List(Of ToolStripMenuItem)
    Private ReadOnly _frameworkVersionList As New Dictionary(Of String, (item As ToolStripMenuItem, Parent As ToolStripMenuItem))
    Private _mCapturedRenderer As ToolStripRenderer
    Private _topLevelStatementConversionEnable As Boolean
    Friend ReadOnly _client As New HttpClient()
    Friend _cancellationTokenSource As CancellationTokenSource
    Friend _doNotFailOnError As Boolean
    Friend _inColorize As Boolean
    Friend _requestToConvert As ConvertRequest
    Friend _resultOfConversion As ConversionResult
    Public Const ProjectGitHubUrl As String = "https://github.com/paul1956/CSharpToVB/"
    Public Const ProjectGitHubReadMe as String = "https://raw.githubusercontent.com/paul1956/CSharpToVB/master/ReadMe.MD"
    Public _currentThemeDictionary As Dictionary(Of String, ColorDescriptor)

    Public Sub New()
        Me.InitializeComponent()
    End Sub

    Friend Property LanguageBuffersToSearch As LanguageBufferToSearch = LanguageBufferToSearch.Csharp
    Friend Property ResourceManager As ComponentResourceManager = New ComponentResourceManager(GetType(Form1))
    Private Property CurrentBuffer As RichTextBox = Nothing

    Private Sub ButtonStopConversion_Click(sender As Object, e As EventArgs) Handles ButtonStopConversion.Click
        SetButtonStopCursorAndCancelToken(Me, stopButtonVisible:=False)
        _cancellationTokenSource.Cancel()
        Application.DoEvents()
    End Sub

    Private Sub ButtonStopConversion_MouseEnter(sender As Object, e As EventArgs) Handles ButtonStopConversion.MouseEnter
        LocalUseWaitCursor(Me, waitCursorEnable:=False)
    End Sub

    Private Sub ButtonStopConversion_MouseLeave(sender As Object, e As EventArgs) Handles ButtonStopConversion.MouseLeave
        Me.ButtonStopConversion.BackColor = SystemColors.Control
        LocalUseWaitCursor(Me, waitCursorEnable:=Me.ButtonStopConversion.Visible)
    End Sub

    Private Sub ButtonStopConversion_VisibleChanged(sender As Object, e As EventArgs) Handles ButtonStopConversion.VisibleChanged
        If Me.ButtonStopConversion.Visible Then
            If _cancellationTokenSource IsNot Nothing Then
                _cancellationTokenSource.Dispose()
            End If
            _cancellationTokenSource = New CancellationTokenSource
        End If
        LocalUseWaitCursor(Me, waitCursorEnable:=Me.ButtonStopConversion.Visible)
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
                    sourceControl = Me.CurrentBuffer
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
                sourceControl = Me.CurrentBuffer
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
                sourceControl = Me.CurrentBuffer
            Else
                Exit Sub
            End If
        End If
        If sourceControl IsNot Nothing AndAlso (sourceControl.CanPaste(DataFormats.GetFormat(DataFormats.Rtf)) OrElse
                                                sourceControl.CanPaste(DataFormats.GetFormat(DataFormats.Text))
                                               ) Then
            _inColorize = True
            SetButtonStopCursorAndCancelToken(Me, stopButtonVisible:=True)
            sourceControl.SelectedText = Clipboard.GetText(TextDataFormat.Text)
            _inColorize = False
            If Me.mnuOptionsColorizeSource.Checked AndAlso Me.CurrentBuffer.Equals(Me.ConversionInput) Then
                Colorize(Me, GetClassifiedRanges(sourceControl.Text, LanguageNames.CSharp).ToList(), sourceControl, sourceControl.Lines.Length, New List(Of Diagnostic))
            Else
                If Me.mnuOptionsColorizeResult.Checked AndAlso Me.CurrentBuffer.Equals(Me.ConversionOutput) Then
                    Colorize(Me, GetClassifiedRanges(sourceControl.Text, LanguageNames.VisualBasic).ToList(), sourceControl, sourceControl.Lines.Length, New List(Of Diagnostic))
                End If
            End If
            SetButtonStopCursorAndCancelToken(Me, stopButtonVisible:=False)
        End If
    End Sub

    Private Sub ContextMenuRedo_Click(sender As Object, e As EventArgs) Handles ContextMenuRedo.Click
        Dim sourceControl As RichTextBox = TryCast(Me.ContextMenuStrip1.SourceControl, RichTextBox)
        If sourceControl Is Nothing Then
            If TypeOf Me.CurrentBuffer Is RichTextBox Then
                sourceControl = Me.CurrentBuffer
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
                sourceControl = Me.CurrentBuffer
            Else
                Exit Sub
            End If
        End If
        sourceControl.Focus()
        sourceControl.SelectAll()
    End Sub

    Private Sub ContextMenuStrip1_Opening(sender As Object, e As CancelEventArgs) Handles ContextMenuStrip1.Opening
        Dim contextMenu As ContextMenuStrip = CType(sender, ContextMenuStrip)
        Dim sourceRtb As RichTextBox = TryCast(Me.ContextMenuStrip1.SourceControl, RichTextBox)
        If sourceRtb IsNot Nothing Then
            contextMenu.Items(contextMenu.IndexOf(NameOf(ContextMenuCopy))).Enabled = sourceRtb.TextLength > 0 And sourceRtb.SelectedText.Any
            contextMenu.Items(contextMenu.IndexOf(NameOf(ContextMenuCut))).Enabled = sourceRtb.TextLength > 0 And sourceRtb.SelectedText.Any
            contextMenu.Items(contextMenu.IndexOf(NameOf(ContextMenuPaste))).Enabled = sourceRtb.CanPaste(DataFormats.GetFormat(DataFormats.Rtf)) OrElse sourceRtb.CanPaste(DataFormats.GetFormat(DataFormats.Text))
            contextMenu.Items(contextMenu.IndexOf(NameOf(ContextMenuRedo))).Enabled = sourceRtb.CanRedo
            contextMenu.Items(contextMenu.IndexOf(NameOf(ContextMenuUndo))).Enabled = sourceRtb.CanUndo
            Exit Sub
        End If
        If TypeOf Me.ContextMenuStrip1.SourceControl Is ListBox Then
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
                sourceControl = Me.CurrentBuffer
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
            Colorize1Range(Me.ConversionInput, LanguageNames.CSharp)
        End If
        If Me.ConversionInput.Text.Any() Then
            If _topLevelStatementConversionEnable = False AndAlso Me.ConversionInput.Text(0) <> "/" Then
                Dim selectionStart As Integer = Me.ConversionInput.SelectionStart
                Dim selectionLength As Integer = Me.ConversionInput.SelectionLength

                _topLevelStatementConversionEnable = Me.ConversionInput.Text(0) <> "/"
                Dim keywordIndex As Integer = Me.ConversionInput.FindIndexOfAny("namespace", "internal static class")
                If keywordIndex >= 0 Then
                    Dim firstCharIndexOfNamespaceLine As Integer = Me.ConversionInput.GetFirstCharIndexFromLine(Me.ConversionInput.GetLineFromCharIndex(keywordIndex))
                    _topLevelStatementConversionEnable = keywordIndex <> firstCharIndexOfNamespaceLine
                End If
                Me.ConversionInput.Select(selectionStart, selectionLength)
            End If
        Else
            _topLevelStatementConversionEnable = False
        End If
    End Sub

    Private Sub ConversionOutput_Enter(sender As Object, e As EventArgs) Handles ConversionOutput.Enter
        Me.CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub ConversionOutput_MouseEnter(sender As Object, e As EventArgs) Handles ConversionOutput.MouseEnter
        Me.CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub ConversionOutput_TextChanged(sender As Object, e As EventArgs) Handles ConversionOutput.TextChanged
        Dim outputBufferInUse As Boolean = CType(sender, RichTextBox).TextLength > 0
        Me.mnuViewShowDestinationLineNumbers.Checked = outputBufferInUse And My.Settings.ShowDestinationLineNumbers
        Me.mnuCompile.Enabled = outputBufferInUse
        If Me.mnuOptionsColorizeSource.Checked AndAlso Not _inColorize Then
            Colorize1Range(Me.ConversionOutput, LanguageNames.VisualBasic)
        End If
        Me.SetSearchControls()
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles Me.Load
        Me.SplitContainer1.SplitterDistance = Me.SplitContainer1.Height - (Me.SplitContainer1.Panel2.Height + 15)

        ' Load all settings
        If My.Settings.UpgradeRequired Then
            My.Settings.Upgrade()
            My.Settings.UpgradeRequired = False
            My.Settings.Save()
        End If
        If My.Settings.IgnoreFileList Is Nothing Then
            My.Settings.IgnoreFileList = New Specialized.StringCollection
        End If

        Me.UpdateLastFileMenu()
        Me.TSFindFindWhatComboBox.TsFindWhatMruUpdateUi()

        ' display MRU if there are any items to display...
        Me.mnuFile.DropDownItems.FileMenuMruUpdateUi(AddressOf Me.mnu_MRUList_Click)

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

        If My.Settings.EditorFont Is Nothing OrElse My.Settings.EditorFont.Name <> My.Settings.EditorFontName Then
            My.Settings.EditorFont = New Font("Consolas", 11.25!, FontStyle.Regular)
            My.Settings.EditorFontName = "Consolas"
        End If
        Me.ConversionInput.Font = My.Settings.EditorFont
        Me.ConversionOutput.Font = My.Settings.EditorFont
        My.Settings.Save()

        Me.Width = Screen.PrimaryScreen.Bounds.Width
        Me.Height = CInt(Screen.PrimaryScreen.Bounds.Height * 0.95)

        Me.FileListListBox.Height = Me.SplitContainer1.Panel2.ClientSize.Height
        Me.ErrorListListBox.Height = Me.SplitContainer1.Panel2.ClientSize.Height

        For Each frameworkType As ToolStripMenuItem In Me.mnuOptionsDefaultFramework.DropDownItems
            If frameworkType.Text = $".Net Full Framework" Then
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
        Me.ToolTipErrorList.SetToolTip(Me.ErrorListListBox, "Double-Click to scroll to VB error")
        Me.ToolTipFileList.SetToolTip(Me.FileListListBox, "Double-Click to open C# and corresponding VB file if available")
        Me.TSFindLookInComboBox.DropDownStyle = ComboBoxStyle.Simple
        Me.TSFindLookInComboBox.SelectedIndex = 0
        Me.TSFindMatchCaseCheckBox.Checked = My.Settings.TSFindMatchCase
        Me.TSFindMatchWholeWordCheckBox.Checked = My.Settings.TSFindMatchWholeWord
        Application.DoEvents()
        UpdateColorDictionariesFromFile()
        Dim isDarkMode As Boolean = Not My.Settings.ColorMode.IsLightMode
        DarkMode.ToggleImmersiveDarkMode(Me.Handle, isDarkMode)
        Me.ToggleColorMode(Me, isDarkMode)
        _loading = False
        CheckForUpdatesAsync(Me, reportResults:=False)
        Me.SplitContainer1.SplitterIncrement = Me.FileListListBox.ItemHeight + 2
        Me.ResizeRichTextBuffers()
        DefaultColor = _currentThemeDictionary(ThemeDefaultColor)
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

    Private Sub ListboxErrorList_DoubleClick(sender As Object, e As EventArgs) Handles ErrorListListBox.DoubleClick
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

    Private Sub ListBoxErrorList_SelectedValueChanged(sender As Object, e As EventArgs) Handles ErrorListListBox.SelectedValueChanged
        Me.ErrorListListBox.Enabled = Me.ErrorListListBox.Items.Count > 0
    End Sub

    Private Sub ListBoxFileList_DoubleClick(sender As Object, e As EventArgs) Handles FileListListBox.DoubleClick
        Using fileList As ListBox = CType(sender, ListBox)
            If fileList.Items.Count = 0 Then
                Exit Sub
            End If
            Dim item As NumberedListItem = CType(fileList.SelectedItem, NumberedListItem)

            Dim sourceFileNameWithPath As String = item.SourceFileWithPath
            If String.IsNullOrWhiteSpace(sourceFileNameWithPath) OrElse Not File.Exists(sourceFileNameWithPath) Then
                Exit Sub
            End If
            SetButtonStopCursorAndCancelToken(Me, True)
            LoadInputBufferFromStream(Me, sourceFileNameWithPath)
            Dim convertedFileNameWithPath As String = item.ValueItem
            If Not File.Exists(convertedFileNameWithPath) Then
                SetButtonStopCursorAndCancelToken(Me, False)
                Exit Sub
            End If

            LoadOutputBufferFromStream(Me, convertedFileNameWithPath)
            SetButtonStopCursorAndCancelToken(Me, False)
        End Using
    End Sub

    Private Sub ListBoxFileList_SelectedValueChanged(sender As Object, e As EventArgs) Handles FileListListBox.SelectedValueChanged
        Me.FileListListBox.Enabled = Me.FileListListBox.Items.Count > 0
    End Sub

    Private Sub mnuCompile_Click(sender As Object, e As EventArgs) Handles mnuCompile.Click
        Me.LineNumbersForConversionInput.Visible = False
        Me.LineNumbersForConversionOutput.Visible = False
        Me.ErrorListListBox.Items.Clear()

        If String.IsNullOrWhiteSpace(Me.ConversionOutput.Text) Then
            Exit Sub
        End If
        Me.ErrorListListBox.Text = ""
        Dim preprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
            New KeyValuePair(Of String, Object)(My.Settings.Framework, True)
        }
        Compile_Colorize(Me, Me.ConversionOutput.Text, preprocessorSymbols)
    End Sub

    Private Sub mnuConvert_DropDownOpening(sender As Object, e As EventArgs) Handles mnuConvert.DropDownOpening
        If Me.ConversionInput.Text.Any Then
            Me.mnuConvertConvertSnippet.Enabled = True ' For now always enable, Not _topLevelStatementConversionEnable
            Me.mnuConvertConvertTopLevelStmts.Enabled = _topLevelStatementConversionEnable
        Else
            Me.mnuConvertConvertSnippet.Enabled = False
            Me.mnuConvertConvertTopLevelStmts.Enabled = False
        End If
    End Sub

    Private Sub mnuConvert_EnabledChanged(sender As Object, e As EventArgs) Handles mnuConvert.EnabledChanged
        Me.SetSearchControls()
    End Sub

    Private Async Sub mnuConvertConvertSnippet_Click(sender As Object, e As EventArgs) Handles mnuConvertConvertSnippet.Click
        SetButtonStopCursorAndCancelToken(Me, stopButtonVisible:=True)
        Await Me.ConvertSnippetOfTopLevelStmt(Me.ConversionInput.Text)
        SetButtonStopCursorAndCancelToken(Me, stopButtonVisible:=False)
    End Sub

    Private Async Sub mnuConvertConvertTopLevelStmts_Click(sender As Object, e As EventArgs) Handles mnuConvertConvertTopLevelStmts.Click
        SetButtonStopCursorAndCancelToken(Me, stopButtonVisible:=True)
        Dim usingSb As New StringBuilder
        Dim stmts As New StringBuilder
        Dim asyncValue As String = ""
        For Each stmt As String In Me.ConversionInput.Text.SplitLines
            If stmt.StartsWith("using") Then
                usingSb.AppendLine(stmt)
            Else
                If stmt.Trim.StartsWith("await", StringComparison.Ordinal) Then
                    asyncValue = " async"
                End If
                stmts.AppendLine(stmt)
            End If
        Next

        Await Me.ConvertSnippetOfTopLevelStmt($"/* Top Level Code boilerplate is included, to remove deselect 'Top Level Statements' under Options/Advance Options */
{usingSb}
namespace Application
{{
    class Program
    {{
        static{asyncValue} void Main(string[] args)
        {{
            {stmts}
        }}
    }}
}}
")
        SetButtonStopCursorAndCancelToken(Me, stopButtonVisible:=False)
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
                .Description = $"Select folder to convert..."
                .SelectedPath = My.Settings.DefaultProjectDirectory.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar) & Path.DirectorySeparatorChar
                .ShowNewFolderButton = False
                If .ShowDialog(Me) <> DialogResult.OK Then
                    Me.mnuConvertConvertFolder.Enabled = True
                    Exit Sub
                End If
                sourceFolderName = .SelectedPath
                Dim fullFolderPath As (SolutionRoot As String, ProjectRelativePath As String) = Me.GetSavePath(.SelectedPath, promptIfDirExists:=True)
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
        Me.StatusStripElapasedTimeLabel.Text = ""
        ' Create new stopwatch
        Dim prompt As String
        SetButtonStopCursorAndCancelToken(Me, stopButtonVisible:=True)

        If Await ProcessFilesAsync(Me,
                                   sourceFolderName,
                                   solutionSavePath,
                                   sourceLanguageExtension:="cs",
                                   stats,
                                   cancelToken:=_cancellationTokenSource.Token
                                  ).ConfigureAwait(True) Then
            stats._elapsedTimer.Stop()
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
            stats._elapsedTimer.Stop()
            prompt = "Conversion stopped."
        End If
        MsgBox(prompt,
               MsgBoxStyle.OkOnly Or MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground,
               Title:="Convert C# To Visual Basic")
        Dim elapsed As TimeSpan = stats._elapsedTimer.Elapsed
        Me.StatusStripElapasedTimeLabel.Text = $"Elapsed Time - {elapsed.Hours}: {elapsed.Minutes}:{elapsed.Seconds}.{elapsed.Milliseconds}"
        Me.mnuConvertConvertFolder.Enabled = True
        SetButtonStopCursorAndCancelToken(Me, stopButtonVisible:=False)
    End Sub

    Private Sub mnuConvertStartFolderConvertFromLastFile_Click(sender As Object, e As EventArgs) Handles mnuConvertStartFolderConvertFromLastFile.Click
        My.Settings.StartFolderConvertFromLastFile = CType(sender, ToolStripMenuItem).Checked
        My.Settings.Save()
        Application.DoEvents()
    End Sub

    Private Sub mnuEdit_DropDownOpening(sender As Object, e As EventArgs) Handles mnuEdit.DropDownOpening
        If Me.TSFindToolStrip.Visible Then
            Me.mnuEditFind.Text = $"Hide &Find Toolbar"
        Else
            Me.mnuEditFind.Text = $"Show &Find Toolbar"
        End If
        If TypeOf Me.CurrentBuffer Is RichTextBox Then
            Dim sourceBuffer As RichTextBox = Me.CurrentBuffer
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
        If Me.CurrentBuffer IsNot Nothing Then
            Me.CurrentBuffer.Copy()
        End If
    End Sub

    Private Sub mnuEditCut_Click(sender As Object, e As EventArgs) Handles mnuEditCut.Click
        If Me.CurrentBuffer IsNot Nothing Then
            Me.CurrentBuffer.Cut()
        End If
    End Sub

    Private Sub mnuEditFind_Click(sender As Object, e As EventArgs) Handles mnuEditFind.Click
        Me.TSFindToolStrip.Visible = Not Me.TSFindToolStrip.Visible
    End Sub

    Private Sub mnuEditPaste_Click(sender As Object, e As EventArgs) Handles mnuEditPaste.Click
        If Me.CurrentBuffer IsNot Nothing Then
            _inColorize = True
            Me.CurrentBuffer.SelectedText = Clipboard.GetText(TextDataFormat.Text)
            _inColorize = False
            SetButtonStopCursorAndCancelToken(Me, stopButtonVisible:=True)
            If Me.mnuOptionsColorizeSource.Checked AndAlso Me.CurrentBuffer.Equals(Me.ConversionInput) Then
                Colorize(Me, GetClassifiedRanges(Me.CurrentBuffer.Text, LanguageNames.CSharp).ToList(), Me.CurrentBuffer, Me.CurrentBuffer.Lines.Length, New List(Of Diagnostic))
            Else
                If Me.mnuOptionsColorizeResult.Checked AndAlso Me.CurrentBuffer.Equals(Me.ConversionOutput) Then
                    Colorize(Me, GetClassifiedRanges(Me.CurrentBuffer.Text, LanguageNames.VisualBasic).ToList(), Me.CurrentBuffer, Me.CurrentBuffer.Lines.Length, New List(Of Diagnostic))
                End If
            End If
            Me.CurrentBuffer.Focus()
            SetButtonStopCursorAndCancelToken(Me, stopButtonVisible:=False)
        End If
    End Sub

    Private Sub mnuEditRedo_Click(sender As Object, e As EventArgs) Handles mnuEditRedo.Click
        Dim sourceControl As RichTextBox = Me.CurrentBuffer
        If sourceControl IsNot Nothing AndAlso sourceControl.CanRedo Then
            sourceControl.Focus()
            sourceControl.Redo()
        End If

    End Sub

    Private Sub mnuEditSelectAll_Click(sender As Object, e As EventArgs) Handles mnuEditSelectAll.Click
        Dim sourceControl As RichTextBox = Me.CurrentBuffer
        If sourceControl IsNot Nothing Then
            sourceControl.Focus()
            sourceControl.SelectAll()
        End If
    End Sub

    Private Sub mnuEditUndo_Click(sender As Object, e As EventArgs) Handles mnuEditUndo.Click
        Dim sourceControl As RichTextBox = Me.CurrentBuffer
        If sourceControl IsNot Nothing AndAlso sourceControl.CanUndo Then
            sourceControl.Undo()
        End If
    End Sub

    Private Sub mnuFile_DropDownOpening(sender As Object, e As EventArgs) Handles mnuFile.DropDownOpening
        Me.mnuFileLoadLastSnippet.Enabled = File.Exists(s_snippetFileWithPath)
        Me.mnuFileSaveSnippet.Enabled = Me.ConversionInput.TextLength > 0
    End Sub

    Private Sub mnuFileExit_Click(sender As Object, e As EventArgs) Handles mnuFileExit.Click
        Me.Close()
        End
    End Sub

    Private Async Sub mnuFileLastFolder_Click(sender As Object, e As EventArgs) Handles mnuFileLastFolder.Click
        Dim folderName As String = CType(sender, ToolStripMenuItem).Text
        If Directory.Exists(folderName) Then
            Dim fullFolderPath As (SolutionRoot As String, ProjectRelativePath As String) = Me.GetSavePath(folderName, promptIfDirExists:=True)
            Dim targetSavePath As String = Path.Combine(fullFolderPath.SolutionRoot, fullFolderPath.ProjectRelativePath)
            If String.IsNullOrWhiteSpace(targetSavePath) Then
                Exit Sub
            End If
            ' This path is a directory.
            Dim lastFileNameWithPath As String = If(My.Settings.StartFolderConvertFromLastFile, My.Settings.MRU_Data.Last, "")
            Dim stats As New ProcessingStats(lastFileNameWithPath)
            SetButtonStopCursorAndCancelToken(Me, stopButtonVisible:=True)
            If Await ProcessFilesAsync(Me, folderName, targetSavePath, "cs", stats, _cancellationTokenSource.Token).ConfigureAwait(True) Then
                MsgBox($"Conversion completed.",
                       MsgBoxStyle.OkOnly Or MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground)
            End If
            SetButtonStopCursorAndCancelToken(Me, stopButtonVisible:=False)
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

    Private Sub mnuFileLoadLastSnippet_Click(sender As Object, e As EventArgs) Handles mnuFileLoadLastSnippet.Click
        If My.Settings.ColorizeInput Then
            SetButtonStopCursorAndCancelToken(Me, True)
            Me.mnuConvertConvertSnippet.Enabled = 0 <> LoadInputBufferFromStream(Me, s_snippetFileWithPath)
            SetButtonStopCursorAndCancelToken(Me, False)
        Else
            Me.ConversionInput.LoadFile(s_snippetFileWithPath, RichTextBoxStreamType.PlainText)
        End If
        Me.mnuCompile.Enabled = True
    End Sub

    Private Sub mnuFileOpen_Click(sender As Object, e As EventArgs) Handles mnuFileOpen.Click
        With Me.OpenFileDialog1
            .AddExtension = True
            .DefaultExt = "cs"
            .InitialDirectory = My.Settings.DefaultProjectDirectory
            .FileName = ""
            .Filter = $"C# Code Files (*.cs)|*.cs"
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
            .Filter = $"C# Project or Solution (*.csproj, *.sln)|*.csproj; *.sln"
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
            My.Settings.ColorizeOutput = colorizeOutput
        End With
    End Sub

    Private Sub mnuFileSaveAs_Click(sender As Object, e As EventArgs) Handles mnuFileSaveAs.Click

        Me.SaveFileDialog1.AddExtension = True
        Me.SaveFileDialog1.CreatePrompt = False
        Me.SaveFileDialog1.DefaultExt = "vb"
        Me.SaveFileDialog1.FileName = Path.ChangeExtension(Me.OpenFileDialog1.SafeFileName, "vb")
        Me.SaveFileDialog1.Filter = $"VB Code Files (*.vb)|*.vb"
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

    Private Sub mnuFileSaveSnippet_Click(sender As Object, e As EventArgs) Handles mnuFileSaveSnippet.Click
        If Me.ConversionInput.TextLength = 0 Then
            Exit Sub
        End If
        Me.ConversionInput.SaveFile(s_snippetFileWithPath, RichTextBoxStreamType.PlainText)
    End Sub

    Private Sub mnuHelpAboutMenuItem_Click(sender As Object, e As EventArgs) Handles mnuHelpAboutMenuItem.Click
        Dim lightMode As Boolean = Me.TSThemeButton.Text.IsLightMode
        Using about As New AboutBox1(lightMode)
            DarkMode.ToggleImmersiveDarkMode(about.Handle, Not lightMode)
            Me.SetColorMode(about, Me.TSThemeButton.Text.IsLightMode)
            about.ShowDialog()
        End Using
    End Sub

    Private Sub mnuHelpCheckForUpdatesMenuItem_Click(sender As Object, e As EventArgs) Handles mnuHelpCheckForUpdatesMenuItem.Click
        CheckForUpdatesAsync(Me, reportResults:=True)
    End Sub

    Private Sub mnuHelpReportIssueMenuItem_Click(sender As Object, e As EventArgs) Handles mnuHelpReportIssueMenuItem.Click
        OpenUrlInBrowser($"{ProjectGitHubUrl}issues")
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
        If My.Settings.IgnoreFileList Is Nothing Then
            My.Settings.IgnoreFileList = New Specialized.StringCollection
        End If

        Dim srcFileNameWithPath As String = My.Settings.MRU_Data.Last
        If Not My.Settings.IgnoreFileList.Contains(srcFileNameWithPath) Then
            My.Settings.IgnoreFileList.Add(srcFileNameWithPath)
            My.Settings.Save()
        End If
    End Sub

    Private Sub mnuOptionsAdvanced_Click(sender As Object, e As EventArgs) Handles mnuOptionsAdvanced.Click
        Dim isLightMode As Boolean = Me.TSThemeButton.Text.IsLightMode
        Using o As New OptionsDialog()
            o.MainForm = Me
            DarkMode.ToggleImmersiveDarkMode(o.Handle, Not isLightMode)
            Me.SetColorMode(o, isLightMode)
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
        Dim lightMode As Boolean = Me.TSThemeButton.Text.IsLightMode
        Using ignoreFilesDialog As New IgnoreFilesWithErrorsListDialog
            DarkMode.ToggleImmersiveDarkMode(ignoreFilesDialog.Handle, Not lightMode)
            Me.SetColorMode(ignoreFilesDialog, lightMode)
            ignoreFilesDialog.ShowDialog(Me)
            If Not String.IsNullOrWhiteSpace(ignoreFilesDialog.FileToLoad) Then
                OpenSourceFile(Me, ignoreFilesDialog.FileToLoad)
            End If

        End Using
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
        Me.LineNumbersForConversionInput.Visible = CType(sender, ToolStripMenuItem).Checked
    End Sub

    Private Sub mnuViewShowSourceLineNumbers_Click(sender As Object, e As EventArgs) Handles mnuViewShowSourceLineNumbers.Click
        My.Settings.ShowSourceLineNumbers = CType(sender, ToolStripMenuItem).Checked
        My.Settings.Save()
    End Sub

    Private Sub SplitContainer1_SplitterMoved(sender As Object, e As SplitterEventArgs) Handles SplitContainer1.SplitterMoved
        Me.FileListPanelEx.Height = Me.SplitContainer1.Panel2.ClientSize.Height
        Me.ErrorListPanelEx.Height = Me.SplitContainer1.Panel2.ClientSize.Height
        Me.ResizeRichTextBuffers()
    End Sub

    Private Sub StatusStripCurrentFileName_MouseDown(sender As Object, e As MouseEventArgs) Handles StatusStripCurrentFileName.MouseDown
        If e.Button = MouseButtons.Right Then
            Clipboard.SetText(text:=CType(sender, ToolStripStatusLabel).Text)
        End If
    End Sub

    Private Sub StatusStripUpdateAvailable_Click(sender As Object, e As EventArgs) Handles StatusStripUpdateAvailable.Click
        OpenUrlInBrowser(ProjectGitHubUrl)
    End Sub

    Protected Overrides Sub OnLoad(e As EventArgs)
        Me.SetStyle(ControlStyles.AllPaintingInWmPaint Or ControlStyles.UserPaint Or ControlStyles.DoubleBuffer, True)
        ' enable events...
        MyBase.OnLoad(e)
    End Sub

#Region "TSFind Events"

    Private Sub TSFindClearHighlightsButton_Click(sender As Object, e As EventArgs) Handles TSFindClearHighlightsButton.Click
        Dim selectionStart As Integer
        _inColorize = True
        If Me.LanguageBuffersToSearch.IsFlagSet(LanguageBufferToSearch.Csharp) Then
            selectionStart = Me.ConversionInput.SelectionStart
            Me.ConversionInput.SelectAll()
            Me.ConversionInput.SelectionBackColor = DefaultColor.Background
            Me.ConversionInput.Select(selectionStart, 0)
            Me.ConversionInput.ScrollToCaret()
        End If
        If Me.LanguageBuffersToSearch.IsFlagSet(LanguageBufferToSearch.VisualBasic) Then
            selectionStart = Me.ConversionOutput.SelectionStart
            Me.ConversionOutput.SelectAll()
            Me.ConversionOutput.SelectionBackColor = DefaultColor.Background
            Me.ConversionOutput.Select(selectionStart, 0)
            Me.ConversionOutput.ScrollToCaret()
        End If
        Application.DoEvents()
        _inColorize = False
    End Sub

    Private Sub TSFindFindNextButton_Click(sender As Object, e As EventArgs) Handles TSFindFindNextButton.Click
        Me.DoFind(searchForward:=True)
    End Sub

    Private Sub TSFindFindPreviousButton_Click(sender As Object, e As EventArgs) Handles TSFindFindPreviousButton.Click
        Me.DoFind(searchForward:=False)
    End Sub

    Private Sub TSFindFindWhatComboBox_Click(sender As Object, e As EventArgs) Handles TSFindFindWhatComboBox.Click
        If Me.TSFindFindWhatComboBox.Text = $"Search..." Then
            Me.TSFindFindWhatComboBox.Text = ""
            Me.TSFindFindWhatComboBox.ForeColor = SystemColors.ControlText
        End If
    End Sub

    Private Sub TSFindFindWhatComboBox_Leave(sender As Object, e As EventArgs) Handles TSFindFindWhatComboBox.Leave
        If Not Me.TSFindFindWhatComboBox.Text.Any Then
            Me.TSFindFindWhatComboBox.ForeColor = SystemColors.GrayText
            Me.TSFindFindWhatComboBox.Text = $"Search..."
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
                Me.LanguageBuffersToSearch = LanguageBufferToSearch.Csharp
            Case 1
                Me.LanguageBuffersToSearch = LanguageBufferToSearch.VisualBasic
            Case 2
                Me.LanguageBuffersToSearch = {LanguageBufferToSearch.Csharp, LanguageBufferToSearch.VisualBasic}.CombineFlags()
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

    Private Sub TSThemeButton_Click(sender As Object, e As EventArgs) Handles TSThemeButton.Click
        Me.ToggleColorMode(Me, Me.TSThemeButton.Text.IsLightMode)
        DefaultColor = GetColorFromName(ThemeDefaultColor)
        If Me.ConversionInput.Text.Any Then
            If Me.mnuOptionsColorizeSource.Checked AndAlso Not _inColorize Then
                Colorize(Me, GetClassifiedRanges(sourceCode:=Me.ConversionInput.Text, LanguageNames.CSharp).ToList(), Me.ConversionInput, Me.ConversionInput.Lines.Length)
                Me.ConversionInput.Select(0, 0)
            End If

        End If
        If Me.ConversionOutput.Text.Any Then
            If Me.mnuOptionsColorizeSource.Checked AndAlso Not _inColorize Then
                Colorize(Me, GetClassifiedRanges(sourceCode:=Me.ConversionOutput.Text, LanguageNames.VisualBasic).ToList(), Me.ConversionOutput, Me.ConversionOutput.Lines.Length)
                Me.ConversionOutput.Select(0, 0)
            End If
        End If
        My.Settings.ColorMode = Me.TSThemeButton.Text
        My.Settings.Save()
    End Sub

#End Region

#Region "Form Support Routines"

    Private Sub SetColorMode(myForm As Form, lightMode As Boolean)

        If lightMode Then
            SetLightMode(myForm.Controls, _mCapturedRenderer)
        Else
            SetDarkMode(myForm.Controls, _mCapturedRenderer)
        End If
    End Sub

    Private Sub ToggleColorMode(myForm As Form, lightMode As Boolean)
        If lightMode Then
            Me.TSThemeButton.Text = DarkModeStr
            _currentThemeDictionary = _darkModeColorDictionary
            SetDarkMode(myForm.Controls, _mCapturedRenderer)
        Else
            Me.TSThemeButton.Text = LightModeStr
            _currentThemeDictionary = _lightModeColorDictionary
            SetLightMode(myForm.Controls, _mCapturedRenderer)
        End If
    End Sub

    ''' <summary>
    ''' This will be used at runtime to add this Event to all items on mnuFileMRU list
    ''' </summary>
    ''' <param name="sender"></param>
    ''' <param name="e"></param>
    Friend Sub mnu_MRUList_Click(sender As Object, e As EventArgs)
        SetButtonStopCursorAndCancelToken(Me, stopButtonVisible:=True)
        ' open the file...
        OpenSourceFile(Me, DirectCast(sender, ToolStripItem).Tag.ToString().Substring(startIndex:=4))
        SetButtonStopCursorAndCancelToken(Me, stopButtonVisible:=False)
    End Sub

    Friend Sub ResizeRichTextBuffers()
        If _loading OrElse _inColorize Then
            Exit Sub
        End If
        Try
            _loading = True
            ' Position all 4 panels
            Dim halfClientWidth As Integer = (Me.SplitContainer1.ClientRectangle.Width - Me.SplitContainer1.SplitterWidth) \ 2
            Me.ConversionInputPanelEx.Width = halfClientWidth
            Me.ConversionInputPanelEx.Height = Me.SplitContainer1.Panel1.ClientRectangle.Height
            Me.ConversionOutputPanelEx.Left = halfClientWidth + Me.SplitContainer1.SplitterWidth + 2
            Me.ConversionOutputPanelEx.Width = halfClientWidth
            Me.ConversionOutputPanelEx.Height = Me.SplitContainer1.Panel1.Height
            Me.FileListPanelEx.Width = halfClientWidth
            Me.FileListPanelEx.Height = Me.SplitContainer1.Panel2.Height
            Me.ErrorListPanelEx.Left = Me.ConversionOutputPanelEx.Left
            Me.ErrorListPanelEx.Width = halfClientWidth
            Me.ErrorListPanelEx.Height = Me.SplitContainer1.Panel2.Height

            Dim lineNumberInputWidth As Integer = If(Me.LineNumbersForConversionInput.Visible AndAlso Me.ConversionInput.TextLength > 0, Me.LineNumbersForConversionInput.Width, 0)
            If lineNumberInputWidth > 0 Then
                Me.ConversionInput.Left = lineNumberInputWidth + 1
            Else
                Me.ConversionInput.Left = 2
            End If
            Me.ConversionInput.Height = Me.ConversionInputPanelEx.Height - 6
            Me.ConversionInput.Width = halfClientWidth - lineNumberInputWidth - 4

            Dim lineNumberOutputWidth As Integer = If(Me.LineNumbersForConversionOutput.Visible AndAlso Me.ConversionOutput.TextLength > 0, Me.LineNumbersForConversionOutput.Width, 0)
            If lineNumberOutputWidth > 0 Then
                Me.ConversionOutput.Left = lineNumberOutputWidth + 1
            Else
                Me.ConversionOutput.Left = 2
            End If
            Me.ConversionOutput.Height = Me.ConversionInputPanelEx.Height - 6
            Me.ConversionOutput.Width = halfClientWidth - lineNumberOutputWidth - 6

            'Me.FileListListBox.Left = 2
            'Me.FileListListBox.Width = Me.FileListPanelEx.ClientRectangle.Width - 4
            'Me.FileListListBox.Height = Me.FileListPanelEx.ClientRectangle.Height - 2

            'Me.ErrorListListBox.Left = 2
            'Me.ErrorListListBox.Width = Me.ErrorListPanelEx.ClientRectangle.Width - 4
            'Me.ErrorListListBox.Height = Me.ErrorListPanelEx.ClientRectangle.Height - 2

            Me.StatusStripCurrentFileName.Width = halfClientWidth
            _loading = False
        Catch ex As ObjectDisposedException
            End
        Catch ex As Exception
            Stop
        End Try
    End Sub

#End Region

End Class
