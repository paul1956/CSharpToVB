' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.ComponentModel
Imports System.Globalization
Imports System.IO
Imports System.Reflection
Imports System.Threading

Imports Buildalyzer

Imports CSharpToVBApp

Imports CSharpToVBCodeConverter
Imports CSharpToVBCodeConverter.ConversionResult

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Emit
Imports Microsoft.VisualBasic.FileIO

#If Not NET5 Then

Imports VBMsgBox

#End If

Partial Public Class Form1

    Private Shared ReadOnly s_snippetFileWithPath As String = Path.Combine(SpecialDirectories.MyDocuments, "CSharpToVBLastSnippet.RTF")

    Private ReadOnly _frameworkTypeList As New List(Of ToolStripMenuItem)

    Private ReadOnly _frameworkVersionList As New Dictionary(Of String, (Item As ToolStripMenuItem, Parent As ToolStripMenuItem))

    Private _cancellationTokenSource As CancellationTokenSource

    Private _currentBuffer As Control

    Private _findDiablog As FindDialog

    Private _inColorize As Boolean

    Private _requestToConvert As ConvertRequest

    Private _resultOfConversion As ConversionResult

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

    Private Sub ButtonStop_Click(sender As Object, e As EventArgs) Handles ButtonStopConversion.Click
        ButtonStopConversion.Visible = False
        _cancellationTokenSource.Cancel()
        Application.DoEvents()
    End Sub

    Private Sub ButtonStop_MouseEnter(sender As Object, e As EventArgs) Handles ButtonStopConversion.MouseEnter
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=False)
    End Sub

    Private Sub ButtonStop_MouseLeave(sender As Object, e As EventArgs) Handles ButtonStopConversion.MouseLeave
        ButtonStopConversion.BackColor = SystemColors.Control
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=ButtonStopConversion.Visible)
    End Sub

    Private Sub ButtonStop_VisibleChanged(sender As Object, e As EventArgs) Handles ButtonStopConversion.VisibleChanged
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=ButtonStopConversion.Visible)
    End Sub

    Private Sub Colorize(FragmentRange As IEnumerable(Of Range), ConversionBuffer As RichTextBox, Lines As Integer, Optional failures As IEnumerable(Of Diagnostic) = Nothing)
        If _inColorize Then
            Exit Sub
        End If
        _inColorize = True
        If ConversionBuffer.Visible Then
            ConversionBuffer.Visible = False
        End If
        Try ' Prevent crash when exiting
            If failures Is Nothing Then
                ListBoxErrorList.Enabled = False
            Else
                ListBoxErrorList.Enabled = True
                For Each dia As Diagnostic In failures
                    ListBoxErrorList.Items.Add($"{dia.Id} Line = {dia.Location.GetLineSpan.StartLinePosition.Line + 1} {dia.GetMessage}")
                Next
            End If
            Dim Progress As New TextProgressBar(ConversionProgressBar)
            Progress.Maximum(Lines)

            With ConversionBuffer
                .Clear()
                .Select(.TextLength, 0)
                For Each range As Range In FragmentRange
                    .Select(.TextLength, 0)
                    .SelectionColor = ColorSelector.GetColorFromName(range.ClassificationType)
                    .AppendText(range.Text)
                    If range.Text.Contains(vbLf, StringComparison.OrdinalIgnoreCase) Then
                        Progress.Increment(range.Text.Count(CType(vbLf, Char)))
                        Application.DoEvents()
                    End If
                    If _requestToConvert?.CancelToken.IsCancellationRequested Then
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
                LineNumbersForConversionInput.Visible = True
                LineNumbersForConversionOutput.Visible = True
            End If
            Progress.Clear()
        Catch ex As Exception
            Stop
            Throw
        End Try
        ConversionBuffer.Visible = True
        _inColorize = False
    End Sub

    Private Sub Compile_Colorize(TextToCompile As String, VBPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)))
        _inColorize = False
        Dim CompileResult As (Success As Boolean, EmitResult As EmitResult) = CompileVisualBasicString(TextToCompile, VBPreprocessorSymbols, DiagnosticSeverity.Error, _resultOfConversion)

        LabelErrorCount.Text = $"Number Of Errors:  {_resultOfConversion.GetFilteredListOfFailures().Count}"
        Dim FragmentRange As IEnumerable(Of Range) = GetClassifiedRanges(TextToCompile, LanguageNames.VisualBasic)

        If CompileResult.Success AndAlso CompileResult.EmitResult.Success Then
            If My.Settings.ColorizeOutput Then
                Me.Colorize(FragmentRange, ConversionOutput, TextToCompile.SplitLines.Length)
            Else
                ConversionOutput.Text = TextToCompile
            End If
        Else
            If Not _resultOfConversion.GetFilteredListOfFailures().Any Then
                _resultOfConversion.ResultStatus = ResultTriState.Success
                If My.Settings.ColorizeOutput Then
                    Me.Colorize(FragmentRange, ConversionOutput, TextToCompile.SplitLines.Length, _resultOfConversion.GetFilteredListOfFailures())
                Else
                    ConversionOutput.Text = TextToCompile
                End If
            Else
                Me.Colorize(FragmentRange, ConversionOutput, TextToCompile.SplitLines.Length, _resultOfConversion.GetFilteredListOfFailures())
            End If
        End If
        ConversionOutput.Visible = True
        Application.DoEvents()
    End Sub

    Private Sub ContextMenuCopy_Click(sender As Object, e As EventArgs) Handles ContextMenuCopy.Click
        If TypeOf ContextMenuStrip1.SourceControl Is RichTextBox Then
            CType(ContextMenuStrip1.SourceControl, RichTextBox).Copy()
        Else
            If ContextMenuStrip1.SourceControl IsNot Nothing Then
                Clipboard.SetText(CType(ContextMenuStrip1.SourceControl, ListBox).SelectedItem.ToString)
            End If
        End If
    End Sub

    Private Sub ContextMenuCut_Click(sender As Object, e As EventArgs) Handles ContextMenuCut.Click
        If TypeOf ContextMenuStrip1.SourceControl Is RichTextBox Then
            CType(ContextMenuStrip1.SourceControl, RichTextBox).Cut()
        End If
    End Sub

    Private Sub ContextMenuPaste_Click(sender As Object, e As EventArgs) Handles ContextMenuPaste.Click
        Dim sourceControl As RichTextBox = CType(ContextMenuStrip1.SourceControl, RichTextBox)
        If sourceControl IsNot Nothing AndAlso sourceControl.CanPaste(DataFormats.GetFormat(DataFormats.Rtf)) Then
            sourceControl.Paste()
        End If
    End Sub

    Private Sub ContextMenuRedo_Click(sender As Object, e As EventArgs) Handles ContextMenuRedo.Click
        Dim sourceControl As RichTextBox = CType(ContextMenuStrip1.SourceControl, RichTextBox)
        If sourceControl IsNot Nothing AndAlso sourceControl.CanRedo Then
            sourceControl.Redo()
        End If
    End Sub

    Private Sub ContextMenuStrip1_Opening(sender As Object, e As CancelEventArgs) Handles ContextMenuStrip1.Opening
        Dim ContextMenu As ContextMenuStrip = CType(sender, ContextMenuStrip)
        If TypeOf CurrentBuffer Is RichTextBox Then
            Dim sourceBuffer As RichTextBox = CType(CurrentBuffer, RichTextBox)
            ContextMenu.Items(IndexOf(ContextMenu, NameOf(ContextMenuCopy))).Enabled = sourceBuffer.TextLength > 0 And sourceBuffer.SelectedText.Length > 0
            ContextMenu.Items(IndexOf(ContextMenu, NameOf(ContextMenuCut))).Enabled = sourceBuffer.TextLength > 0 And sourceBuffer.SelectedText.Length > 0
            ContextMenu.Items(IndexOf(ContextMenu, NameOf(ContextMenuPaste))).Enabled = sourceBuffer.CanPaste(DataFormats.GetFormat(DataFormats.Rtf))
            ContextMenu.Items(IndexOf(ContextMenu, NameOf(ContextMenuRedo))).Enabled = sourceBuffer.CanRedo
            ContextMenu.Items(IndexOf(ContextMenu, NameOf(ContextMenuUndo))).Enabled = sourceBuffer.CanUndo
        Else
            ContextMenu.Items(IndexOf(ContextMenu, NameOf(ContextMenuCut))).Enabled = False
            ContextMenu.Items(IndexOf(ContextMenu, NameOf(ContextMenuPaste))).Enabled = False
            ContextMenu.Items(IndexOf(ContextMenu, NameOf(ContextMenuRedo))).Enabled = False
            ContextMenu.Items(IndexOf(ContextMenu, NameOf(ContextMenuUndo))).Enabled = False
        End If
    End Sub

    Private Sub ContextMenuUndo_Click(sender As Object, e As EventArgs) Handles ContextMenuUndo.Click
        Dim sourceControl As RichTextBox = CType(ContextMenuStrip1.SourceControl, RichTextBox)
        If sourceControl IsNot Nothing AndAlso sourceControl.CanUndo Then
            sourceControl.Undo()
        End If
    End Sub

    Private Sub ConversionInput_Enter(sender As Object, e As EventArgs) Handles ConversionInput.Enter
        CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub ConversionInput_MouseEnter(sender As Object, e As EventArgs) Handles ConversionInput.MouseEnter
        CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub ConversionInput_TextChanged(sender As Object, e As EventArgs) Handles ConversionInput.TextChanged
        Dim InputBufferInUse As Boolean = CType(sender, RichTextBox).TextLength > 0
        mnuViewShowSourceLineNumbers.Checked = InputBufferInUse And My.Settings.ShowSourceLineNumbers
        mnuFileSnippetSave.Enabled = InputBufferInUse
        mnuConvertConvertSnippet.Enabled = InputBufferInUse
        mnuConvertConvertFolder.Enabled = InputBufferInUse
        If mnuOptionsColorizeSource.Checked AndAlso Not _inColorize Then
            Me.Colorize(GetClassifiedRanges(SourceCode:=ConversionInput.Text, LanguageNames.CSharp), ConversionBuffer:=ConversionInput, Lines:=ConversionInput.Lines.Length)
        End If
    End Sub

    Private Sub ConversionOutput_MouseEnter(sender As Object, e As EventArgs) Handles ConversionOutput.MouseEnter
        CurrentBuffer = CType(sender, RichTextBox)
    End Sub

    Private Sub ConversionOutput_TextChanged(sender As Object, e As EventArgs) Handles ConversionOutput.TextChanged
        Dim OutputBufferInUse As Boolean = CType(sender, RichTextBox).TextLength > 0
        mnuViewShowDestinationLineNumbers.Checked = OutputBufferInUse And My.Settings.ShowDestinationLineNumbers
        mnuCompile.Enabled = OutputBufferInUse
    End Sub

    Private Async Function Convert_Compile_ColorizeAsync(RequestToConvert As ConvertRequest,
                                                        CSPreprocessorSymbols As List(Of String),
                                    VBPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)),
                                    OptionalReferences() As MetadataReference,
                                    CancelToken As CancellationToken) As Task(Of Boolean)
        Dim UIContext As SynchronizationContext = SynchronizationContext.Current

        Dim ReportException As Action(Of Exception) =
            Sub(ex As Exception)
                ' Use the Windows Forms synchronization context in order to call MsgBox from the UI thread.
                UIContext.Post(Function(state) MsgBox(ex.Message,
                                                      MsgBoxStyle.OkOnly Or MsgBoxStyle.Critical Or MsgBoxStyle.MsgBoxSetForeground,
                                                      "Stack Overflow"), state:=Nothing)
            End Sub

        Using ProgressBar As TextProgressBar = New TextProgressBar(ConversionProgressBar)
            ' The System.Progress class invokes the callback on the UI thread. It does this because we create the
            ' System.Progress object on the main thread. During creation, it reads SynchronizationContext.Current so
            ' that it knows how to get back to the main thread to invoke the callback there no matter what thread calls
            ' IProgress.Report.
            Dim progress As New Progress(Of ProgressReport)(AddressOf ProgressBar.Update)
            Dim defaultVBOptions As New DefaultVBOptions
            With My.Settings
                defaultVBOptions = New DefaultVBOptions(.OptionCompare, .OptionCompareIncludeInCode, .OptionExplicit, .OptionExplicitIncludeInCode, .OptionInfer, .OptionInferIncludeInCode, .OptionStrict, .OptionStrictIncludeInCode)
            End With
            _resultOfConversion = Await Task.Run(Function() ConvertInputRequest(
                                                    RequestToConvert,
                                                    defaultVBOptions,
                                                    CSPreprocessorSymbols,
                                                    VBPreprocessorSymbols,
                                                    OptionalReferences,
                                                    ReportException,
                                                    progress,
                                                    CancelToken)
                                                    ).ConfigureAwait(True)

        End Using

        If _resultOfConversion Is Nothing Then
            mnuFileSaveAs.Enabled = False
            Return False
        Else
            mnuFileSaveAs.Enabled = Me._resultOfConversion.ResultStatus = ResultTriState.Success
        End If
        Select Case _resultOfConversion.ResultStatus
            Case ResultTriState.Success
                Me.Compile_Colorize(_resultOfConversion.ConvertedCode, VBPreprocessorSymbols)
                Dim FilteredErrorCount As Integer = _resultOfConversion.GetFilteredListOfFailures().Count
                LabelErrorCount.Text = $"Number of Errors: {FilteredErrorCount}"
                Return FilteredErrorCount = 0
            Case ResultTriState.Failure
                If TypeOf _resultOfConversion.Exceptions(0) IsNot OperationCanceledException Then
                    ConversionOutput.SelectionColor = Color.Red
                    ConversionOutput.Text = GetExceptionsAsString(_resultOfConversion.Exceptions)
                End If
            Case ResultTriState.Ignore
                ConversionOutput.Text = ""
                LabelErrorCount.Text = "File Skipped"
        End Select
        Return _resultOfConversion.ResultStatus <> ResultTriState.Failure
    End Function

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles Me.Load
        SplitContainer1.SplitterDistance = SplitContainer1.Height - (ListBoxErrorList.Height + 20)

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

        mnuFileSnippetLoadLast.Enabled = File.Exists(s_snippetFileWithPath)
        mnuOptionsPauseConvertOnSuccess.Checked = My.Settings.PauseConvertOnSuccess
        mnuOptionsSkipSkipAutoGenerated.Checked = My.Settings.SkipAutoGenerated
        mnuOptionsSkipSkipBinAndObjFolders.Checked = My.Settings.SkipBinAndObjFolders
        mnuOptionsSkipSkipTestResourceFiles.Checked = My.Settings.SkipTestResourceFiles

        mnuOptionsStartFolderConvertFromLastFile.Checked = My.Settings.StartFolderConvertFromLastFile

        If String.IsNullOrWhiteSpace(My.Settings.DefaultProjectDirectory) Then
            My.Settings.DefaultProjectDirectory = GetLatestVisualStudioProjectPath()
            My.Settings.Save()
            Application.DoEvents()
        End If

        Width = Screen.PrimaryScreen.Bounds.Width
        Height = CInt(Screen.PrimaryScreen.Bounds.Height * 0.95)

        ListBoxFileList.Height = SplitContainer1.Panel2.ClientSize.Height
        ListBoxErrorList.Height = SplitContainer1.Panel2.ClientSize.Height

        For Each FrameworkType As ToolStripMenuItem In FrameworkToolStripMenuItem.DropDownItems
            If FrameworkType.Text = ".Net Full Framework" Then
                For Each f As String In GetAllFrameworkVersions()
                    AddDropDownMenuItem(FrameworkType.DropDownItems, f)
                Next
            Else
                For Each f As String In GetAllCoreVersions()
                    AddDropDownMenuItem(FrameworkType.DropDownItems, f)
                Next
            End If
        Next
        For Each FrameworkType As ToolStripMenuItem In FrameworkToolStripMenuItem.DropDownItems
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
                AddHandler FrameworkVersion.CheckedChanged, AddressOf Me.ToolStripMenuItem_CheckedChanged
                _frameworkVersionList.Add(FrameworkVersion.Text, (FrameworkVersion, FrameworkType))
            Next
        Next
        If My.Settings.LastProject.Length > 0 Then
            mnuFileLastProject.Text = $"Last Project - {My.Settings.LastProject}"
            mnuFileLastProject.Enabled = True
        End If
        If My.Settings.LastSolution.Length > 0 Then
            mnuFileLastSolution.Text = $"Last Solution - {My.Settings.LastSolution}"
            mnuFileLastSolution.Enabled = True
        End If
        ProgressBar1.Visible = False
        Me.CenterToScreen()
        ProgressBar1.Location = New Point(ClientSize.Width \ 4, ClientSize.Height \ 2)
        LabelProgress.Left = ProgressBar1.Left
        LabelProgress.Top = ProgressBar1.Top - (LabelProgress.Height * 2)
        ToolTipErrorList.SetToolTip(ListBoxErrorList, "Double-Click to scroll to VB error")
        ToolTipFileList.SetToolTip(ListBoxFileList, "Double-Click to open C# and corresponding VB file if available")
        Application.DoEvents()
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
        Dim lineStartPosition As Integer = ConversionOutput.GetFirstCharIndexFromLine(CInt(LineText.Substring(startIndex, count)) - 1)

        If lineStartPosition > 0 AndAlso ConversionOutput.SelectionStart <> lineStartPosition Then
            ConversionOutput.Select(lineStartPosition, 0)
            ConversionOutput.ScrollToCaret()
        End If
    End Sub

    Private Sub ListBoxErrorList_Enter(sender As Object, e As EventArgs) Handles ListBoxErrorList.Enter
        If ListBoxErrorList.Items.Count = 0 Then
            ListBoxErrorList.Enabled = False
            Exit Sub
        End If
        ListBoxErrorList.Enabled = True
        CurrentBuffer = ListBoxErrorList
    End Sub

    Private Sub ListBoxErrorList_MouseEnter(sender As Object, e As EventArgs) Handles ListBoxErrorList.MouseEnter
        If ListBoxErrorList.Items.Count = 0 Then
            ListBoxErrorList.Enabled = False
            Exit Sub
        End If
        ListBoxErrorList.Enabled = True
        CurrentBuffer = ListBoxErrorList
    End Sub

    Private Sub ListBoxErrorList_SelectedValueChanged(sender As Object, e As EventArgs) Handles ListBoxErrorList.SelectedValueChanged
        ListBoxErrorList.Enabled = ListBoxErrorList.Items.Count > 0
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
        Me.LoadInputBufferFromStream(SourceFileNameWithPath)
        Dim ConvertedFileNameWithPath As String = item.ValueItem
        If Not File.Exists(ConvertedFileNameWithPath) Then
            Exit Sub
        End If

        Me.LoadOutputBufferFromStream(ConvertedFileNameWithPath)
    End Sub

    Private Sub ListBoxFileList_Enter(sender As Object, e As EventArgs) Handles ListBoxFileList.Enter
        If ListBoxFileList.Items.Count = 0 Then
            Exit Sub
        End If
        CurrentBuffer = ListBoxFileList
    End Sub

    Private Sub ListBoxFileList_MouseEnter(sender As Object, e As EventArgs) Handles ListBoxFileList.MouseEnter
        If ListBoxFileList.Items.Count = 0 Then
            Exit Sub
        End If
        CurrentBuffer = ListBoxFileList
    End Sub

    Private Sub ListBoxFileList_SelectedValueChanged(sender As Object, e As EventArgs) Handles ListBoxFileList.SelectedValueChanged
        ListBoxFileList.Enabled = ListBoxFileList.Items.Count > 0
    End Sub

    Private Function LoadInputBufferFromStream(SourceFileNameWithPath As String) As Integer
        ConversionInput.Visible = False
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=True)
        Dim SourceText As String
        Using myFileStream As FileStream = File.OpenRead(path:=SourceFileNameWithPath)
            SourceText = GetFileTextFromStream(myFileStream)
        End Using

        Dim InputLines As Integer
        Dim ConversionInputLinesArray() As String = SourceText.SplitLines
        InputLines = ConversionInputLinesArray.Length
        If mnuOptionsColorizeSource.Checked Then
            Me.Colorize(GetClassifiedRanges(ConversionInputLinesArray.Join(vbCrLf), LanguageNames.CSharp), ConversionInput, InputLines)
        Else
            ConversionInput.Text = ConversionInputLinesArray.Join(vbCrLf)
        End If
        ConversionInput.Visible = True
        If mnuViewShowSourceLineNumbers.Checked Then
            LineNumbersForConversionInput.Visible = False
            Application.DoEvents()
            LineNumbersForConversionInput.Visible = True
            Application.DoEvents()
        End If
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=False)
        Return InputLines
    End Function

    Private Sub LoadOutputBufferFromStream(SourceFileNameWithPath As String)
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=True)
        Dim SourceText As String
        Using myFileStream As FileStream = File.OpenRead(path:=SourceFileNameWithPath)
            SourceText = GetFileTextFromStream(myFileStream)
        End Using
        Dim InputLines As Integer
        Dim ConversionInputLinesArray() As String = SourceText.SplitLines
        InputLines = ConversionInputLinesArray.Length
        If mnuOptionsColorizeSource.Checked Then
            Me.Colorize(GetClassifiedRanges(ConversionInputLinesArray.Join(vbCrLf), LanguageNames.VisualBasic), ConversionOutput, InputLines)
        Else
            ConversionOutput.Text = ConversionInputLinesArray.Join(vbCrLf)
        End If
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=False)
    End Sub

    Private Sub mnuCompile_Click(sender As Object, e As EventArgs) Handles mnuCompile.Click
        LineNumbersForConversionInput.Visible = False
        LineNumbersForConversionOutput.Visible = False
        ListBoxErrorList.Items.Clear()

        If String.IsNullOrWhiteSpace(ConversionOutput.Text) Then
            Exit Sub
        End If
        ListBoxErrorList.Text = ""
        Dim VBPreprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
            New KeyValuePair(Of String, Object)(My.Settings.Framework, True)
        }
        Me.Compile_Colorize(ConversionOutput.Text, VBPreprocessorSymbols)
    End Sub

    Private Sub mnuConvert_Click(sender As Object, e As EventArgs) Handles mnuConvert.Click
        mnuConvertConvertSnippet.Enabled = ConversionInput.TextLength > 0
    End Sub

    Private Async Sub mnuConvertConvertSnippet_Click(sender As Object, e As EventArgs) Handles mnuConvertConvertSnippet.Click
        SetButtonStopAndCursor(MeForm:=Me, StopButton:=ButtonStopConversion, StopButtonVisible:=True)
        ListBoxErrorList.Items.Clear()
        ListBoxFileList.Items.Clear()
        LineNumbersForConversionOutput.Visible = False
        StatusStripCurrentFileName.Text = ""
        Me.ResizeRichTextBuffers()
        If _cancellationTokenSource IsNot Nothing Then
            _cancellationTokenSource.Dispose()
        End If
        _cancellationTokenSource = New CancellationTokenSource
        _requestToConvert = New ConvertRequest(My.Settings.SkipAutoGenerated, New Progress(Of ProgressReport)(AddressOf New TextProgressBar(ConversionProgressBar).Update), _cancellationTokenSource.Token) With
            {
            .SourceCode = ConversionInput.Text
            }
        Dim CSPreprocessorSymbols As New List(Of String) From {
            My.Settings.Framework
        }
        Dim VBPreprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
            KeyValuePair.Create(Of String, Object)(My.Settings.Framework, True)
        }
        Dim DontDisplayLineNumbers As Boolean = Await Me.Convert_Compile_ColorizeAsync(_requestToConvert, CSPreprocessorSymbols, VBPreprocessorSymbols, OptionalReferences:=SharedReferences.CSharpReferences(Assembly.Load("System.Windows.Forms").Location, Nothing).ToArray, CancelToken:=_cancellationTokenSource.Token).ConfigureAwait(True)
        If _requestToConvert.CancelToken.IsCancellationRequested Then
            MsgBox($"Conversion canceled.",
                   MsgBoxStyle.OkOnly Or MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground,
                   Title:="C# to Visual Basic")
            ConversionProgressBar.Value = 0
        End If
        mnuConvertConvertFolder.Enabled = True
        SetButtonStopAndCursor(MeForm:=Me, StopButton:=ButtonStopConversion, StopButtonVisible:=False)
        LineNumbersForConversionOutput.Visible = (Not DontDisplayLineNumbers) OrElse My.Settings.ShowDestinationLineNumbers
    End Sub

    Private Async Sub mnuConvertFolder_Click(sender As Object, e As EventArgs) Handles mnuConvertConvertFolder.Click
        LineNumbersForConversionInput.Visible = False
        LineNumbersForConversionOutput.Visible = False
        StatusStripCurrentFileName.Text = ""
        Dim SourceFolderName As String
        Dim solutionSavePath As String
        Using OFD As New FolderBrowserDialog
            With OFD
                .Description = "Select folder to convert..."
                .SelectedPath = My.Settings.DefaultProjectDirectory.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar) & Path.DirectorySeparatorChar
                .ShowNewFolderButton = False
                If .ShowDialog(Me) <> DialogResult.OK Then
                    Exit Sub
                End If
                SourceFolderName = .SelectedPath
                Dim fullFolderPath As (SolutionRoot As String, ProjectRelativePath As String) = GetSavePath(Me, .SelectedPath, PromptIfDirExsits:=True)
                solutionSavePath = Path.Combine(fullFolderPath.SolutionRoot, fullFolderPath.ProjectRelativePath)
            End With
        End Using
        If Not Directory.Exists(SourceFolderName) Then
            MsgBox($"{SourceFolderName} is not a directory.",
                   MsgBoxStyle.OkOnly Or MsgBoxStyle.Exclamation Or MsgBoxStyle.MsgBoxSetForeground,
                   Title:="Convert C# to Visual Basic")
            Exit Sub
        End If
        If String.IsNullOrWhiteSpace(solutionSavePath) Then
            MsgBox($"Conversion aborted.",
                   MsgBoxStyle.OkOnly Or MsgBoxStyle.Exclamation Or MsgBoxStyle.MsgBoxSetForeground,
                   Title:="Convert C# to Visual Basic")
            Exit Sub
        End If
        Dim LastFileNameWithPath As String = If(My.Settings.StartFolderConvertFromLastFile, My.Settings.MRU_Data.Last, "")
        Dim Stats As New ProcessingStats(LastFileNameWithPath)
        _cancellationTokenSource = New CancellationTokenSource
        StatusStripElapasedTimeLabel.Text = ""
        ' Create new stopwatch
        Dim stopwatch As New Stopwatch()
        ' Begin timing
        stopwatch.Start()
        Dim prompt As String
        If Await Me.ProcessAllFilesAsync(SourceFolderName,
                                         solutionSavePath,
                                         "cs",
                                         Stats,
                                         _cancellationTokenSource.Token
                                         ).ConfigureAwait(True) Then
            stopwatch.Stop()
            If _cancellationTokenSource.Token.IsCancellationRequested Then
                prompt = $"Conversion canceled, {Stats.FilesProcessed} files completed successfully."
            Else
                prompt = $"Conversion completed, {Stats.FilesProcessed} files completed successfully."
                mnuOptionsStartFolderConvertFromLastFile.Checked = False
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
               Title:="Convert C# to Visual Basic")
        Dim elapsed As TimeSpan = stopwatch.Elapsed
        StatusStripElapasedTimeLabel.Text = $"Elapsed Time - {elapsed.Hours}: {elapsed.Minutes}:{elapsed.Seconds}.{elapsed.Milliseconds}"

    End Sub

    Private Sub mnuEdit_DropDownOpening(sender As Object, e As EventArgs) Handles mnuEdit.DropDownOpening
        If TypeOf CurrentBuffer Is RichTextBox Then
            Dim sourceBuffer As RichTextBox = CType(CurrentBuffer, RichTextBox)
            mnuEditCopy.Enabled = sourceBuffer.TextLength > 0 And sourceBuffer.SelectedText.Length > 0
            mnuEditCut.Enabled = sourceBuffer.TextLength > 0 And sourceBuffer.SelectedText.Length > 0
            mnuEditPaste.Enabled = sourceBuffer.CanPaste(DataFormats.GetFormat(DataFormats.Rtf))
            mnuEditUndo.Enabled = sourceBuffer.CanUndo
            mnuEditRedo.Enabled = sourceBuffer.CanRedo
        Else
            mnuEditCut.Enabled = False
            mnuEditPaste.Enabled = False
            mnuEditUndo.Enabled = False
            mnuEditRedo.Enabled = False
        End If
    End Sub

    Private Sub mnuEditCopy_Click(sender As Object, e As EventArgs) Handles mnuEditCopy.Click
        If TypeOf CurrentBuffer Is RichTextBox Then
            CType(CurrentBuffer, RichTextBox).Copy()
        Else
            Clipboard.SetText(CType(CurrentBuffer, ListBox).Text)
        End If
    End Sub

    Private Sub mnuEditCut_Click(sender As Object, e As EventArgs) Handles mnuEditCut.Click
        If TypeOf CurrentBuffer Is RichTextBox Then
            CType(CurrentBuffer, RichTextBox).Cut()
        End If
    End Sub

    Private Sub mnuEditFind_Click(sender As Object, e As EventArgs) Handles mnuEditFind.Click
        If _findDiablog Is Nothing Then
            _findDiablog = New FindDialog(ConversionInput, ConversionOutput, Me)
        End If
        mnuEditFind.Enabled = False
        _findDiablog.Show()
    End Sub

    Private Sub mnuEditPaste_Click(sender As Object, e As EventArgs) Handles mnuEditPaste.Click
        If TypeOf CurrentBuffer Is RichTextBox Then
            CType(CurrentBuffer, RichTextBox).SelectedText = Clipboard.GetText(TextDataFormat.Text)
        End If
    End Sub

    Private Sub mnuEditRedo_Click(sender As Object, e As EventArgs) Handles mnuEditRedo.Click
        Dim sourceControl As RichTextBox = TryCast(CurrentBuffer, RichTextBox)
        If sourceControl IsNot Nothing AndAlso sourceControl.CanRedo Then
            sourceControl.Redo()
        End If

    End Sub

    Private Sub mnuEditUndo_Click(sender As Object, e As EventArgs) Handles mnuEditUndo.Click
        Dim sourceControl As RichTextBox = TryCast(CurrentBuffer, RichTextBox)
        If sourceControl IsNot Nothing AndAlso sourceControl.CanUndo Then
            sourceControl.Undo()
        End If
    End Sub

    Private Sub mnuFileExit_Click(sender As Object, e As EventArgs) Handles mnuFileExit.Click
        Me.Close()
        End
    End Sub

    Private Async Sub mnuFileLastFolder_Click(sender As Object, e As EventArgs) Handles mnuFileLastFolder.Click
        Dim FolderName As String = CType(sender, ToolStripMenuItem).Text
        If Directory.Exists(FolderName) Then
            Dim SourceLanguageExtension As String = "cs"
            Dim fullFolderPath As (SolutionRoot As String, ProjectRelativePath As String) = GetSavePath(Me, FolderName, PromptIfDirExsits:=True)
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
            If Await Me.ProcessAllFilesAsync(FolderName, targetSavePath, SourceLanguageExtension, Stats, _cancellationTokenSource.Token).ConfigureAwait(True) Then
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
        Me.ProcessProjectOrSolution(projectFileName)
    End Sub

    Private Sub mnuFileLastSolution_Click(sender As Object, e As EventArgs) Handles mnuFileLastSolution.Click
        Dim solutionFileName As String = My.Settings.LastSolution
        Me.ProcessProjectOrSolution(solutionFileName)
    End Sub

    Private Sub mnuFileOpen_Click(sender As Object, e As EventArgs) Handles mnuFileOpen.Click
        Dim LanguageExtension As String = "cs"
        With OpenFileDialog1
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
                mnuConvertConvertFolder.Enabled = False
                Me.OpenSourceFile(OpenFileDialog1.FileName)
            Else
                mnuConvertConvertFolder.Enabled = True
            End If
        End With
    End Sub

    Private Sub mnuFileOpenProject_Click(sender As Object, e As EventArgs) Handles mnuFileConvertProject.Click
        With OpenFileDialog1
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
            Me.ProcessProjectOrSolution(.FileName)
        End With
    End Sub

    Private Sub mnuFileSaveAs_Click(sender As Object, e As EventArgs) Handles mnuFileSaveAs.Click

        SaveFileDialog1.AddExtension = True
        SaveFileDialog1.CreatePrompt = False
        SaveFileDialog1.DefaultExt = "vb"
        SaveFileDialog1.FileName = Path.ChangeExtension(OpenFileDialog1.SafeFileName, "vb")
        SaveFileDialog1.Filter = "VB Code Files (*.vb)|*.vb"
        SaveFileDialog1.FilterIndex = 0
        SaveFileDialog1.OverwritePrompt = True
        SaveFileDialog1.SupportMultiDottedExtensions = False
        SaveFileDialog1.Title = $"Save {SaveFileDialog1.DefaultExt} Output..."
        SaveFileDialog1.ValidateNames = True
        Dim FileSaveResult As DialogResult = SaveFileDialog1.ShowDialog
        If FileSaveResult = DialogResult.OK Then
            ConversionOutput.SaveFile(SaveFileDialog1.FileName, RichTextBoxStreamType.PlainText)
        End If
    End Sub

    Private Sub mnuFileSnippetLoadLast_Click(sender As Object, e As EventArgs) Handles mnuFileSnippetLoadLast.Click
        If My.Settings.ColorizeInput Then
            mnuConvertConvertSnippet.Enabled = 0 <> Me.LoadInputBufferFromStream(s_snippetFileWithPath)
        Else
            ConversionInput.LoadFile(s_snippetFileWithPath, RichTextBoxStreamType.PlainText)
        End If
        mnuCompile.Enabled = True
    End Sub

    Private Sub mnuFileSnippetSave_Click(sender As Object, e As EventArgs) Handles mnuFileSnippetSave.Click
        If ConversionInput.TextLength = 0 Then
            Exit Sub
        End If
        ConversionInput.SaveFile(s_snippetFileWithPath, RichTextBoxStreamType.PlainText)
    End Sub

    Private Sub mnuFileSnippett_Click(sender As Object, e As EventArgs) Handles mnuFileSnippet.Click
        If Not File.Exists(s_snippetFileWithPath) Then
            Exit Sub
        End If
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
            Cursor = Cursors.AppStarting
            launchBrowser(webAddress)
        Catch ex As Exception
            Stop
        Finally
            Cursor = Cursors.AppStarting
            'Devices.Mouse.OverrideCursor = Nothing
        End Try
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
            Me.OpenSourceFile(IgnoreFilesWithErrorsDialog.FileToLoad)
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

    Private Sub mnuView_DropDownOpening(sender As Object, e As EventArgs) Handles mnuView.DropDownOpening
        mnuViewShowDestinationLineNumbers.Enabled = ConversionOutput.TextLength > 0
        mnuViewShowSourceLineNumbers.Enabled = ConversionInput.TextLength > 0
    End Sub

    Private Sub mnuViewShowDestinationLineNumbers_Click(sender As Object, e As EventArgs) Handles mnuViewShowDestinationLineNumbers.Click
        Dim checked As Boolean = CType(sender, ToolStripMenuItem).Checked
        LineNumbersForConversionOutput.Visible = checked
        My.Settings.ShowDestinationLineNumbers = checked
        My.Settings.Save()
    End Sub

    Private Sub mnuViewShowSourceLineNumbers_CheckStateChanged(sender As Object, e As EventArgs) Handles mnuViewShowSourceLineNumbers.CheckStateChanged
        Dim checked As Boolean = CType(sender, ToolStripMenuItem).Checked
        LineNumbersForConversionInput.Visible = checked
    End Sub

    Private Sub mnuViewShowSourceLineNumbers_Click(sender As Object, e As EventArgs) Handles mnuViewShowSourceLineNumbers.Click
        Dim checked As Boolean = CType(sender, ToolStripMenuItem).Checked
        My.Settings.ShowSourceLineNumbers = checked
        My.Settings.Save()
    End Sub

    Private Sub MRU_UpdateUI(dropDownItems As ToolStripItemCollection)
        ' clear MRU menu items...
        Dim MRUToolStripItems As New List(Of ToolStripItem)
        ' create a temporary collection containing every MRU menu item
        ' (identified by the tag text when added to the list)...
        For Each FileMenuItem As ToolStripItem In dropDownItems
            If Not FileMenuItem.Tag Is Nothing Then
                If FileMenuItem.Tag.ToString().StartsWith("MRU:", StringComparison.Ordinal) Then
                    MRUToolStripItems.Add(FileMenuItem)
                End If
            End If
        Next
        ' iterate through list and remove each from menu...
        For Each MRUToolStripItem As ToolStripItem In MRUToolStripItems
            RemoveHandler MRUToolStripItem.Click, AddressOf Me.mnu_MRUList_Click
            RemoveHandler MRUToolStripItem.MouseDown, AddressOf mnuMRUList_MouseDown
            dropDownItems.Remove(MRUToolStripItem)
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
            AddHandler clsItem.Click, AddressOf Me.mnu_MRUList_Click
            AddHandler clsItem.MouseDown, AddressOf mnuMRUList_MouseDown
            ' insert into DropDownItems list...
            dropDownItems.Insert(dropDownItems.Count - 12, clsItem)
        Next
        ' show separator...
        My.Settings.Save()
        If My.Settings.MRU_Data.Count > 0 Then
            mnuFileLastFolder.Text = Path.GetDirectoryName(My.Settings.MRU_Data.Last)
            mnuFileLastFolder.Visible = True
            mnuFileSep1.Visible = True
            mnuFileSep2.Visible = True
        Else
            mnuFileLastFolder.Visible = False
            mnuFileSep1.Visible = False
            mnuFileSep2.Visible = False
        End If

    End Sub

    Private Sub OpenSourceFile(FileNameWithPath As String)
        mnuConvertConvertSnippet.Enabled = Me.LoadInputBufferFromStream(FileNameWithPath) <> 0
        mnuAddToMRU(My.Settings.MRU_Data, FileNameWithPath)
        Me.MRU_UpdateUI(mnuFile.DropDownItems)
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
    Private Async Function ProcessAllFilesAsync(SourceDirectory As String, TargetDirectory As String, SourceLanguageExtension As String, Stats As ProcessingStats, CancelToken As CancellationToken) As Task(Of Boolean)
        Try
            ListBoxErrorList.Items.Clear()
            ListBoxFileList.Items.Clear()
            SetButtonStopAndCursor(Me,
                                   ButtonStopConversion,
                                   StopButtonVisible:=True)
            Stats.TotalFilesToProcess = GetFileCount(SourceDirectory,
                                                     SourceLanguageExtension,
                                                     My.Settings.SkipBinAndObjFolders,
                                                     My.Settings.SkipTestResourceFiles)
            ' Process the list of files found in the directory.
            Return Await ProcessDirectoryAsync(SourceDirectory,
                                               TargetDirectory,
                                               MeForm:=Me,
                                               ButtonStopConversion,
                                               ListBoxFileList,
                                               SourceLanguageExtension,
                                               Stats,
                                               AddressOf Me.ProcessFileAsync,
                                               CancelToken).ConfigureAwait(True)
        Catch ex As OperationCanceledException
            ConversionProgressBar.Value = 0
        Catch ex As Exception
            ' don't crash on exit
            End
        Finally
            SetButtonStopAndCursor(Me,
                                   ButtonStopConversion,
                                   StopButtonVisible:=False)
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
    Private Async Function ProcessFileAsync(SourceFileNameWithPath As String, TargetDirectory As String, SourceLanguageExtension As String, CSPreprocessorSymbols As List(Of String), VBPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)), OptionalReferences() As MetadataReference, SkipAutoGenerated As Boolean, CancelToken As CancellationToken) As Task(Of Boolean)
        If My.Settings.IgnoreFileList.Contains(SourceFileNameWithPath) Then
            Return True
        End If
        ButtonStopConversion.Visible = True
        ConversionOutput.Text = ""
        mnuAddToMRU(My.Settings.MRU_Data, SourceFileNameWithPath)
        Me.MRU_UpdateUI(mnuFile.DropDownItems)
        Dim lines As Integer = Me.LoadInputBufferFromStream(SourceFileNameWithPath)
        If lines > 0 Then
            _requestToConvert = New ConvertRequest(SkipAutoGenerated, New Progress(Of ProgressReport)(AddressOf New TextProgressBar(ConversionProgressBar).Update), _cancellationTokenSource.Token) With {
                .SourceCode = ConversionInput.Text
            }
            If Not Await Me.Convert_Compile_ColorizeAsync(_requestToConvert, CSPreprocessorSymbols, VBPreprocessorSymbols, OptionalReferences, CancelToken).ConfigureAwait(True) Then
                If _requestToConvert.CancelToken.IsCancellationRequested Then
                    ConversionProgressBar.Value = 0
                    Return False
                End If
                Select Case MsgBox($"Conversion failed, do you want to stop processing this file automatically in the future? Yes and No will continue processing files, Cancel will stop conversions!",
                                   MsgBoxStyle.YesNoCancel Or MsgBoxStyle.Exclamation Or MsgBoxStyle.MsgBoxSetForeground)
                    Case MsgBoxResult.Cancel
                        _cancellationTokenSource.Cancel()
                        Return False
                    Case MsgBoxResult.No
                        Return True
                    Case MsgBoxResult.Yes
                        If Not My.Settings.IgnoreFileList.Contains(SourceFileNameWithPath) Then
                            My.Settings.IgnoreFileList.Add(SourceFileNameWithPath)
                            My.Settings.Save()
                            ListBoxErrorList.Text = ""
                            LineNumbersForConversionInput.Visible = My.Settings.ShowSourceLineNumbers
                            LineNumbersForConversionOutput.Visible = My.Settings.ShowDestinationLineNumbers
                        End If
                        Return True
                End Select
            Else
                If Not String.IsNullOrWhiteSpace(TargetDirectory) Then
                    If _requestToConvert.CancelToken.IsCancellationRequested Then
                        Return False
                    End If
                    If LabelErrorCount.Text = "File Skipped" Then
                        Return True
                    End If
                    Dim NewFileName As String = Path.ChangeExtension(New FileInfo(SourceFileNameWithPath).Name, If(SourceLanguageExtension = "vb", "cs", "vb"))
                    WriteTextToStream(TargetDirectory, NewFileName, ConversionOutput.Text)
                End If
                If My.Settings.PauseConvertOnSuccess Then
                    If MsgBox($"{SourceFileNameWithPath} successfully converted, Continue?",
                              MsgBoxStyle.YesNo Or MsgBoxStyle.Question Or MsgBoxStyle.MsgBoxSetForeground) = MsgBoxResult.No Then
                        Return False
                    End If
                End If
            End If
            ' 5 second delay
            Const LoopSleep As Integer = 25
            Dim Delay As Integer = (1000 * My.Settings.ConversionDelay) \ LoopSleep
            For index As Integer = 0 To Delay
                Application.DoEvents()
                Thread.Sleep(LoopSleep)
                If CancelToken.IsCancellationRequested Then
                    Return False
                End If
            Next
            Application.DoEvents()
        Else
            ConversionOutput.Clear()
        End If
        Return True
    End Function

    ''' <summary>
    '''
    ''' </summary>
    ''' <param name="TaskProjectAnalyzer"></param>
    ''' <param name="SolutionRoot"></param>
    ''' <param name="_cancellationTokenSource"></param>
    ''' <returns></returns>
    Private Async Function ProcessOneProjectAsync(TaskProjectAnalyzer As IProjectAnalyzer, SolutionRoot As String, processedProjects As Integer, totalProjects As Integer, _cancellationTokenSource As CancellationTokenSource) As Task(Of (ErrorPrompt As String, ProjectsToBeAdded As List(Of String)))
        Application.DoEvents()
        Me.UpdateProgressLabels("Getting Analyzer Results", True)
        Dim TaskResults As Task(Of IAnalyzerResults) = GetResultsAsync(CType(TaskProjectAnalyzer, ProjectAnalyzer))
        While Not TaskResults.IsCompleted
            If _cancellationTokenSource.IsCancellationRequested Then
                Return ("", New List(Of String))
            End If
            Await Task.Delay(100).ConfigureAwait(True)
        End While
        Dim results As IAnalyzerResults = TaskResults.Result
        Me.UpdateProgressLabels("Loading Workspace", True)
        Dim TaskWorkspace As Task(Of AdhocWorkspace) = GetWorkspaceAsync(TaskProjectAnalyzer)
        Dim frameworkList As List(Of String) = GetFrameworks(results.TargetFrameworks.ToList)
        ' Under debugger each framework will be processed
        ' Under production the user will select one framework
        While Not TaskWorkspace.IsCompleted
            If _cancellationTokenSource.IsCancellationRequested Then
                Return ("", New List(Of String))
            End If
            Await Task.Delay(100).ConfigureAwait(True)
        End While
        If frameworkList.Count = 0 Then
            Return ($"no framework is specified, processing project will terminate!", New List(Of String))
        End If
        Dim projectsToBeAdd As New List(Of String)
        Using workspace As AdhocWorkspace = TaskWorkspace.Result
            Me.UpdateProgressLabels("", False)

            If workspace.CurrentSolution.Projects.Count <> 1 Then
                Return ($"of an unexpected number of projects {workspace.CurrentSolution.Projects.Count}, processing project will terminate!", New List(Of String))
            End If
            For Each framework As IndexClass(Of String) In frameworkList.WithIndex
                Dim frameworkMsg As String
                If frameworkList.Count = 1 Then
                    frameworkMsg = $"Framework: {framework.Value}"
                Else
                    frameworkMsg = $"Framework {framework.Index + 1} of {frameworkList.Count}: {framework.Value}"
                End If
                Dim currentProject As Project = workspace.CurrentSolution.Projects(0)
                StatusStripCurrentFileName.Text = $"{processedProjects} of {totalProjects} Projects, {frameworkMsg}, {currentProject.FilePath}"
                Application.DoEvents()
                Dim csReferences As MetadataReference() = CSharpReferences(results(framework.Value).References, results(framework.Value).ProjectReferences).ToArray
                Dim taskConvertOneProject As Task(Of Boolean) =
                        Me.ProcessOneProjectCore(currentProject,
                                               SolutionRoot,
                                               framework.Value,
                                               csReferences
                                               )
                Dim projectToBeAdd As String = ConvertProjectFile(currentProject.FilePath, SolutionRoot)
                If projectToBeAdd.Length > 0 AndAlso Not projectsToBeAdd.Contains(projectToBeAdd) Then
                    projectsToBeAdd.Add(projectToBeAdd)
                End If
                While Not taskConvertOneProject.IsCompleted
                    If _cancellationTokenSource.IsCancellationRequested Then
                        Exit For
                    End If
                    Await Task.Delay(100).ConfigureAwait(True)
                End While
                If Not taskConvertOneProject.Result Then
                    Exit For
                End If
            Next
        End Using
        Return ("", projectsToBeAdd)
    End Function

    ''' <summary>
    ''' Converts all the files in a C# project
    ''' </summary>
    ''' <param name="currentProject"></param>
    ''' <param name="solutionRoot"></param>
    ''' <param name="Framework"></param>
    ''' <param name="References"></param>
    ''' <returns>False if failed</returns>
    Private Async Function ProcessOneProjectCore(currentProject As Project, solutionRoot As String, Framework As String, References As MetadataReference()) As Task(Of Boolean)
        If _cancellationTokenSource.IsCancellationRequested Then
            Return False
        End If
        Dim FilesProcessed As Integer = 0
        Dim TotalFilesToProcess As Integer = currentProject.Documents.Count
        Dim convertedFramework As String = FrameworkNameToConstant(Framework)
        Dim CSPreprocessorSymbols As New List(Of String) From {Framework, convertedFramework}
        Dim VBPreprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
                                    KeyValuePair.Create(Of String, Object)(convertedFramework, True)}
        If Not convertedFramework.Equals(Framework, StringComparison.OrdinalIgnoreCase) Then
            VBPreprocessorSymbols.Add(KeyValuePair.Create(Of String, Object)(Framework, True))
        End If

        For Each currentDocument As Document In currentProject.Documents
            If _cancellationTokenSource.IsCancellationRequested Then
                Return False
            End If
            Dim targetFileWithPath As String = DestinationFilePath(currentDocument.FilePath, solutionRoot)
            FilesProcessed += 1
            ListBoxFileList.Items.Add(New NumberedListItem($"{FilesProcessed.ToString(CultureInfo.InvariantCulture),-5} {currentDocument.FilePath}", $"{targetFileWithPath}{Path.DirectorySeparatorChar}{Path.GetFileNameWithoutExtension(currentDocument.Name)}.vb"))
            ListBoxFileList.SelectedIndex = ListBoxFileList.Items.Count - 1
            FilesConversionProgress.Text = $"Processed {FilesProcessed:N0} of {TotalFilesToProcess:N0} Files"
            Application.DoEvents()
            If Not Await Me.ProcessFileAsync(currentDocument.FilePath,
                                             targetFileWithPath,
                                             "cs",
                                             CSPreprocessorSymbols,
                                             VBPreprocessorSymbols,
                                             References,
                                             SkipAutoGenerated:=False,
                                             _cancellationTokenSource.Token).ConfigureAwait(True) _
                            OrElse _requestToConvert.CancelToken.IsCancellationRequested Then
                Return False
            End If
        Next
        Return True
    End Function

    Private Async Sub ProcessProjectOrSolution(fileName As String)
        _cancellationTokenSource = New CancellationTokenSource
        Dim saveSolutionRoot As String = GetSavePath(Me, fileName, PromptIfDirExsits:=True).SolutionRoot
        If String.IsNullOrWhiteSpace(saveSolutionRoot) Then
            LabelProgress.Visible = False
            ProgressBar1.Visible = False
            MsgBox($"Can't find {saveSolutionRoot}, exiting solution conversion")
            Exit Sub
        End If
        SetButtonStopAndCursor(MeForm:=Me, StopButton:=ButtonStopConversion, StopButtonVisible:=True)
        ListBoxErrorList.Items.Clear()
        ListBoxFileList.Items.Clear()
        ConversionInput.Clear()
        ConversionOutput.Clear()
        Me.UpdateProgressLabels($"Getting Analyzer Manger for {fileName}", True)
        Try
            Dim TaskAnalyzerManager As Task(Of AnalyzerManager) = GetManagerAsync(fileName)
            While Not TaskAnalyzerManager.IsCompleted
                If _cancellationTokenSource.IsCancellationRequested Then
                    Exit Try
                End If
                Await Task.Delay(100).ConfigureAwait(True)
            End While
            Dim solutionAnalyzerManager As AnalyzerManager = TaskAnalyzerManager.Result
            LabelProgress.Visible = False
            ProgressBar1.Visible = False

            Dim prompt As String = "Conversion stopped."
            If fileName.EndsWith(".sln", StringComparison.OrdinalIgnoreCase) Then
                mnuFileLastSolution.Enabled = True
                My.Settings.LastSolution = fileName
                mnuFileLastSolution.Text = $"Last Solution - {fileName}"
                My.Settings.Save()
                Dim totalProjects As Integer = solutionAnalyzerManager.Projects.Count
                Dim processedProjects As Integer = 0
                Dim skipProjects As Boolean = My.Settings.StartFolderConvertFromLastFile
                Dim results As (resultsString As String, projectsToBeAdded As List(Of String)) = ("", New List(Of String))
                For Each proj As KeyValuePair(Of String, IProjectAnalyzer) In solutionAnalyzerManager.Projects
                    Dim projectFile As String = proj.Key
                    If Not projectFile.EndsWith(".csproj", StringComparison.OrdinalIgnoreCase) Then
                        totalProjects -= 1
                        Continue For
                    End If
                    processedProjects += 1

                    If skipProjects AndAlso My.Settings.LastProject.Length > 0 Then
                        If projectFile <> My.Settings.LastProject Then
                            Continue For
                        Else
                            skipProjects = False
                        End If
                    End If
                    mnuFileLastProject.Text = $"Last Project - {projectFile}"
                    mnuFileLastProject.Enabled = True
                    My.Settings.LastProject = projectFile
                    My.Settings.Save()
                    Application.DoEvents()
                    results = Await Me.ProcessOneProjectAsync(
                                        TaskProjectAnalyzer:=proj.Value,
                        SolutionRoot:=saveSolutionRoot, processedProjects:=processedProjects,
                        totalProjects:=totalProjects, _cancellationTokenSource:=_cancellationTokenSource).ConfigureAwait(True)
                    If results.resultsString.Length = 0 Then
                        If _cancellationTokenSource.Token.IsCancellationRequested Then
                            prompt = $"Conversion canceled, {processedProjects} of {totalProjects} projects completed successfully."
                            Exit For
                        Else
                            prompt = $"Conversion completed, {totalProjects} projects completed successfully."
                        End If
                    Else
                        prompt = $"Conversion canceled because {results}, {processedProjects} of {totalProjects} projects completed successfully."
                        Exit For
                    End If
                Next
                If prompt.Length > 0 Then
                    MsgBox(prompt,
                           MsgBoxStyle.OkOnly Or If(prompt.Contains("terminated", StringComparison.OrdinalIgnoreCase), MsgBoxStyle.Critical, MsgBoxStyle.Information) Or MsgBoxStyle.MsgBoxSetForeground,
                           Title:="Convert C# to Visual Basic")
                Else
                    ConvertSolutionFile(fileName, saveSolutionRoot, results.projectsToBeAdded)
                End If
            Else
                ' Single project
                Me.UpdateProgressLabels($"Getting Project Analyzer for {fileName}", True)
                Dim TaskProjectAnalyzer As Task(Of IProjectAnalyzer) = GetProjectAnalyzerAsync(fileName, solutionAnalyzerManager)
                While Not TaskProjectAnalyzer.IsCompleted
                    If _cancellationTokenSource.IsCancellationRequested Then
                        Exit Sub
                    End If
                    Await Task.Delay(100).ConfigureAwait(True)
                End While
                Me.UpdateProgressLabels("", False)
                mnuFileLastProject.Text = $"Last Project - {fileName}"
                mnuFileLastProject.Enabled = True
                My.Settings.LastProject = fileName
                My.Settings.Save()
                prompt = (Await Me.ProcessOneProjectAsync(TaskProjectAnalyzer.Result,
                                                          saveSolutionRoot,
                                                          processedProjects:=1,
                                                          totalProjects:=1,
                                                          _cancellationTokenSource:=_cancellationTokenSource).ConfigureAwait(True)).ErrorPrompt

                If prompt.Length = 0 Then
#Disable Warning CA1308 ' Normalize strings to uppercase
                    prompt = $"{If(_cancellationTokenSource.Token.IsCancellationRequested, "Conversion canceled", "Conversion completed")}, {FilesConversionProgress.Text.ToLower(CultureInfo.InvariantCulture)} completed successfully."
#Enable Warning CA1308 ' Normalize strings to uppercase
                End If
                MsgBox(prompt,
                       MsgBoxStyle.OkOnly Or If(prompt.Contains("terminated", StringComparison.OrdinalIgnoreCase), MsgBoxStyle.Critical, MsgBoxStyle.Information) Or MsgBoxStyle.MsgBoxSetForeground,
                       Title:="Convert C# to Visual Basic")

                Dim projectSavePath As String = DestinationFilePath(fileName, saveSolutionRoot)
                If Directory.Exists(projectSavePath) AndAlso Not _cancellationTokenSource.IsCancellationRequested Then
                    Process.Start("explorer.exe", $"/root,{projectSavePath}")
                End If
            End If
        Catch ex As ObjectDisposedException
        Finally
            LabelProgress.Visible = False
            ProgressBar1.Visible = False
            SetButtonStopAndCursor(Me, ButtonStopConversion, StopButtonVisible:=False)
        End Try
    End Sub

    Private Sub ResizeRichTextBuffers()
        Dim LineNumberInputWidth As Integer = If(LineNumbersForConversionInput.Visible AndAlso ConversionInput.TextLength > 0, LineNumbersForConversionInput.Width, 0)
        Dim LineNumberOutputWidth As Integer = If(LineNumbersForConversionOutput.Visible AndAlso ConversionOutput.TextLength > 0, LineNumbersForConversionOutput.Width, 0)
        Dim HalfClientWidth As Integer = ClientSize.Width \ 2
        ConversionInput.Width = HalfClientWidth - LineNumberInputWidth
        ListBoxFileList.Width = HalfClientWidth

        ConversionOutput.Width = ClientSize.Width - (ConversionInput.Width + LineNumberInputWidth + LineNumberOutputWidth)
        ConversionOutput.Left = ConversionInput.Width + LineNumberInputWidth + LineNumberOutputWidth

        ListBoxErrorList.Left = HalfClientWidth
        ListBoxErrorList.Width = HalfClientWidth
        StatusStripCurrentFileName.Width = HalfClientWidth
    End Sub
    Private Sub SplitContainer1_SplitterMoved(sender As Object, e As SplitterEventArgs) Handles SplitContainer1.SplitterMoved
        ListBoxFileList.Height = SplitContainer1.Panel2.ClientSize.Height
        ListBoxErrorList.Height = SplitContainer1.Panel2.ClientSize.Height
    End Sub

    Private Sub StatusStripCurrentFileName_MouseDown(sender As Object, e As MouseEventArgs) Handles StatusStripCurrentFileName.MouseDown
        If e.Button = Windows.Forms.MouseButtons.Right Then
            Clipboard.SetText(text:=CType(sender, ToolStripStatusLabel).Text)
        End If
    End Sub

    Private Sub ToolStripMenuItem_CheckedChanged(sender As Object, e As EventArgs)
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

    Private Sub UpdateProgressLabels(progressStr As String, Value As Boolean)
        If InvokeRequired Then
            Me.Invoke(Sub()
                          LabelProgress.Text = progressStr
                          If Value Then
                              ConversionInput.Text = ""
                              ConversionOutput.Text = ""
                              LabelProgress.Visible = True
                              ProgressBar1.Visible = True
                          Else
                              LabelProgress.Visible = False
                              ProgressBar1.Visible = False
                          End If
                          Application.DoEvents()
                      End Sub)
        Else
            Me.Invoke(Sub()
                          LabelProgress.Text = progressStr
                          If Value Then
                              ConversionInput.Text = ""
                              ConversionOutput.Text = ""
                              LabelProgress.Visible = True
                              ProgressBar1.Visible = True
                          Else
                              LabelProgress.Visible = False
                              ProgressBar1.Visible = False
                          End If
                          Application.DoEvents()
                      End Sub)
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
        ' display MRU if there are any items to display...
        Me.MRU_UpdateUI(mnuFile.DropDownItems)
    End Sub

    Friend Sub mnu_MRUList_Click(sender As Object, e As EventArgs)
        ' open the file...
        Me.OpenSourceFile(DirectCast(sender, ToolStripItem).Tag.ToString().Substring(4))
    End Sub

    <STAThread()>
    Shared Sub main(args As String())
        Application.SetHighDpiMode(HighDpiMode.PerMonitorV2)
        Dim MyApp As New My.MyApplication
        MyApp.Run(args)
        MyApp.Dispose()
    End Sub
End Class
