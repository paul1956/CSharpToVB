' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.ComponentModel
Imports System.Diagnostics.CodeAnalysis
Imports System.IO
Imports System.Reflection
Imports System.Text
Imports System.Threading

Imports Buildalyzer

Imports CSharpToVBApp
Imports CSharpToVBApp.Microsoft.VisualBasic.ApplicationServices

Imports CSharpToVBCodeConverter
Imports CSharpToVBCodeConverter.ConversionResult

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Emit
Imports Microsoft.VisualBasic.FileIO
Imports Microsoft.Win32

Imports VBMsgBox

Partial Public Class Form1

    Private WithEvents MyApplication As New WindowsFormsApplicationBase With {
            .IsSingleInstance = True,
            .EnableVisualStyles = True,
            .SaveMySettingsOnExit = True,
            .ShutdownStyle = ShutdownMode.AfterMainFormCloses
        }

    Private Shared ReadOnly s_snippetFileWithPath As String = Path.Combine(SpecialDirectories.MyDocuments, "CSharpToVBLastSnippet.RTF")

    Private ReadOnly _frameworkTypeList As New Dictionary(Of String, ToolStripMenuItem)

    Private ReadOnly _frameworkVersionList As New Dictionary(Of String, (Item As ToolStripMenuItem, Parent As ToolStripMenuItem))

    Private _cancellationTokenSource As CancellationTokenSource

    Private _currentBuffer As Control

    Private _inColorize As Boolean

    Private _requestToConvert As ConvertRequest

    Private _resultOfConversion As ConversionResult

    Public Sub New()
        InitializeComponent()
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

    <ExcludeFromCodeCoverage>
    Private Shared Function GetExceptionsAsString(Exceptions As IReadOnlyList(Of Exception)) As String
        If Exceptions Is Nothing OrElse Not Exceptions.Any Then
            Return String.Empty
        End If

        Dim builder As New StringBuilder()
        For i As Integer = 0 To Exceptions.Count - 1
            builder.AppendFormat(Globalization.CultureInfo.InvariantCulture, "----- Exception {0} Of {1} -----" & System.Environment.NewLine, i + 1, Exceptions.Count)
            builder.AppendLine(Exceptions(i).ToString())
        Next i
        Return builder.ToString()
    End Function

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
        ConversionBuffer.Visible = False
        Try ' Prevent crash when exiting
            If failures IsNot Nothing Then
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
                LineNumbers_For_RichTextBoxInput.Visible = True
                LineNumbers_For_RichTextBoxOutput.Visible = True
            End If
            Progress.Clear()
        Catch ex As Exception
            Stop
            Throw
        End Try
        ConversionBuffer.Visible = True
        _inColorize = False
    End Sub

    Private Sub Compile_Colorize(TextToCompile As String)
        Dim CompileResult As (Success As Boolean, EmitResult As EmitResult) = CompileVisualBasicString(TextToCompile, DiagnosticSeverity.Error, _resultOfConversion)

        LabelErrorCount.Text = $"Number Of Errors:  {_resultOfConversion.GetFilteredListOfFailures().Count}"
        Dim FragmentRange As IEnumerable(Of Range) = GetClassifiedRanges(TextToCompile, LanguageNames.VisualBasic)

        If CompileResult.Success AndAlso CompileResult.EmitResult.Success Then
            If My.Settings.ColorizeOutput Then
                Colorize(FragmentRange, RichTextBoxConversionOutput, TextToCompile.SplitLines.Length)
            Else
                RichTextBoxConversionOutput.Text = TextToCompile
            End If
        Else
            If Not _resultOfConversion.GetFilteredListOfFailures().Any Then
                _resultOfConversion.ResultStatus = ResultTriState.Success
                If My.Settings.ColorizeOutput Then
                    Colorize(FragmentRange, RichTextBoxConversionOutput, TextToCompile.SplitLines.Length, _resultOfConversion.GetFilteredListOfFailures())
                Else
                    RichTextBoxConversionOutput.Text = TextToCompile
                End If
            Else
                Colorize(FragmentRange, RichTextBoxConversionOutput, TextToCompile.SplitLines.Length, _resultOfConversion.GetFilteredListOfFailures())
            End If
        End If
        RichTextBoxConversionOutput.Visible = True
        Application.DoEvents()
    End Sub

    Private Sub ContextMenuCopy_Click(sender As Object, e As EventArgs) Handles ContextMenuCopy.Click
        If TypeOf ContextMenuStrip1.SourceControl Is RichTextBox Then
            CType(ContextMenuStrip1.SourceControl, RichTextBox).Copy()
        Else
            Clipboard.SetText(CType(ContextMenuStrip1.SourceControl, ListBox).SelectedItem.ToString)
        End If
    End Sub

    Private Sub ContextMenuCut_Click(sender As Object, e As EventArgs) Handles ContextMenuCut.Click
        CType(ContextMenuStrip1.SourceControl, RichTextBox).Cut()
    End Sub

    Private Sub ContextMenuPaste_Click(sender As Object, e As EventArgs) Handles ContextMenuPaste.Click
        CType(ContextMenuStrip1.SourceControl, RichTextBox).Paste()
    End Sub

    Private Sub ContextMenuStrip1_Opening(sender As Object, e As CancelEventArgs) Handles ContextMenuStrip1.Opening
        Dim ContextMenu As ContextMenuStrip = CType(sender, ContextMenuStrip)
        If TypeOf CurrentBuffer Is RichTextBox Then
            ContextMenu.Items(1).Visible = True
            ContextMenu.Items(2).Visible = True
        Else
            ContextMenu.Items(1).Visible = False
            ContextMenu.Items(2).Visible = False
        End If
    End Sub

    Private Async Function Convert_Compile_ColorizeAsync(RequestToConvert As ConvertRequest, CSPreprocessorSymbols As List(Of String), VBPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)), OptionalReferences() As MetadataReference, CancelToken As CancellationToken) As Task(Of Boolean)
        Dim UIContext As SynchronizationContext = SynchronizationContext.Current

        Dim ReportException As Action(Of Exception) =
            Sub(ex As Exception)
                ' Use the Windows Forms synchronization context in order to call MsgBox from the UI thread.
                UIContext.Post(Function(state) MsgBox(ex.Message, MsgBoxStyle.Critical, "Stack Overflow"), state:=Nothing)
            End Sub

        Using ProgressBar As TextProgressBar = New TextProgressBar(ConversionProgressBar)
            ' The System.Progress class invokes the callback on the UI thread. It does this because we create the
            ' System.Progress object on the main thread. During creation, it reads SynchronizationContext.Current so
            ' that it knows how to get back to the main thread to invoke the callback there no matter what thread calls
            ' IProgress.Report.
            Dim progress As New Progress(Of ProgressReport)(AddressOf ProgressBar.Update)

            With My.Settings
                Dim _defaultVBOptions As New DefaultVBOptions(.OptionCompare, .OptionCompareIncludeInCode, .OptionExplicit, .OptionExplicitIncludeInCode, .OptionInfer, .OptionInferIncludeInCode, .OptionStrict, .OptionStrictIncludeInCode)
                _resultOfConversion = Await Task.Run(Function() ConvertInputRequest(RequestToConvert, _defaultVBOptions, CSPreprocessorSymbols, VBPreprocessorSymbols, OptionalReferences, ReportException, progress, CancelToken)).ConfigureAwait(True)
            End With

        End Using
        mnuFileSaveAs.Enabled = Me._resultOfConversion.ResultStatus = ResultTriState.Success
        Select Case _resultOfConversion.ResultStatus
            Case ResultTriState.Success
                Compile_Colorize(_resultOfConversion.ConvertedCode)
                Dim FilteredErrorCount As Integer = _resultOfConversion.GetFilteredListOfFailures().Count
                LabelErrorCount.Text = $"Number of Errors: {FilteredErrorCount}"
                Return FilteredErrorCount = 0
            Case ResultTriState.Failure
                If TypeOf _resultOfConversion.Exceptions(0) IsNot OperationCanceledException Then
                    RichTextBoxConversionOutput.SelectionColor = Color.Red
                    RichTextBoxConversionOutput.Text = GetExceptionsAsString(_resultOfConversion.Exceptions)
                End If
            Case ResultTriState.Ignore
                RichTextBoxConversionOutput.Text = ""
                LabelErrorCount.Text = "File Skipped"
        End Select
        Return _resultOfConversion.ResultStatus <> ResultTriState.Failure
    End Function

    ''' <summary>
    ''' Look in SearchBuffer for text and highlight it
    ''' No error is displayed if not found
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

        ListBoxFileList.Height = SplitContainer1.Panel2.ClientSize.Height
        ListBoxErrorList.Height = SplitContainer1.Panel2.ClientSize.Height

        For Each FrameworkType As ToolStripMenuItem In FrameworkToolStripMenuItem.DropDownItems
            _frameworkTypeList.Add(FrameworkType.Text, FrameworkType)
            FrameworkType.Checked = False
            For Each FrameworkVersion As ToolStripMenuItem In FrameworkType.DropDownItems
                If FrameworkVersion.Text = My.Settings.Framework Then
                    FrameworkType.Checked = True
                    FrameworkVersion.Checked = True
                    FrameworkVersion.Enabled = False
                Else
                    FrameworkVersion.Checked = False
                    FrameworkVersion.Enabled = True
                End If
                AddHandler FrameworkVersion.CheckedChanged, AddressOf ToolStripMenuItem_CheckedChanged
                _frameworkVersionList.Add(FrameworkVersion.Text, (FrameworkVersion, FrameworkType))
            Next
        Next
        ProgressBar1.Visible = False
        CenterToScreen()
        ProgressBar1.Location = New Point(ClientSize.Width \ 4, ClientSize.Height \ 2)
        LabelProgress.Left = ProgressBar1.Left
        LabelProgress.Top = ProgressBar1.Top - (LabelProgress.Height * 2)
        ToolTipErrorList.SetToolTip(ListBoxErrorList, "Double-Click to scroll to VB error")
        ToolTipFileList.SetToolTip(ListBoxFileList, "Double-Click to open C# and corresponding VB file if available")
        Application.DoEvents()
    End Sub

    Private Sub Form1_Resize(sender As Object, e As EventArgs) Handles Me.Resize
        ResizeRichTextBuffers()
    End Sub

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
        Dim PathFromSolutionRoot As List(Of String) = DirectoryToBeTranslatedWithPath.Replace(SolutionRoot, "", StringComparison.OrdinalIgnoreCase) _
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

    Private Sub launchBrowser(url As String)
        Dim browserPath As String = "%ProgramFiles(x86)%\Internet Explorer\iexplore.exe"
        Dim msgResult As MsgBoxResult = MsgBoxResult.Ok
        Using userChoiceKey As RegistryKey = Registry.CurrentUser.OpenSubKey("Software\Microsoft\Windows\Shell\Associations\UrlAssociations\http\UserChoice")
            If userChoiceKey IsNot Nothing Then
                Dim progIdObject As Object = userChoiceKey.GetValue("Progid")
                If progIdObject IsNot Nothing Then
                    Dim progIdValue As String = CStr(progIdObject)
                    If progIdValue IsNot Nothing Then
                        If progIdValue.Contains("chrome", StringComparison.OrdinalIgnoreCase) Then
                            browserPath = "%ProgramFiles(x86)%\Google\Chrome\Application\chrome.exe"
                            'ElseIf progIdValue.Contains("firefox", StringComparison.OrdinalIgnoreCase) Then
                            '    browserPath = "firefox.exe"
                        ElseIf progIdValue.Contains("msedgehtm", StringComparison.OrdinalIgnoreCase) Then
                            browserPath = "%ProgramFiles(x86)%\Microsoft\Edge\Application\msedge.exe"
                            'ElseIf progIdValue.Contains("safari", StringComparison.OrdinalIgnoreCase) Then
                            '    browserPath = "safari.exe"
                            'ElseIf progIdValue.Contains("opera", StringComparison.OrdinalIgnoreCase) Then
                            '    browserPath = "opera.exe"
                        Else
                            msgResult = MsgBox($"Your default browser {progIdValue} is not supported, iExplorer will be used if you select OK!, please enter an issue with its 'Progid' and full path", MsgBoxStyle.OkCancel)
                        End If
                    End If
                End If
            End If
        End Using
        If msgResult = MsgBoxResult.Ok Then
            Dim info As New ProcessStartInfo(System.Environment.ExpandEnvironmentVariables(browserPath), url)
            Process.Start(info)
        End If
    End Sub

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
        Dim lineStartPosition As Integer = RichTextBoxConversionOutput.GetFirstCharIndexFromLine(CInt(LineText.Substring(startIndex, count)) - 1)

        If lineStartPosition > 0 AndAlso RichTextBoxConversionOutput.SelectionStart <> lineStartPosition Then
            RichTextBoxConversionOutput.Select(lineStartPosition, 0)
            RichTextBoxConversionOutput.ScrollToCaret()
        End If
    End Sub

    Private Sub ListBoxErrorList_Enter(sender As Object, e As EventArgs) Handles ListBoxErrorList.Enter
        CurrentBuffer = CType(sender, Control)
    End Sub

    Private Sub ListBoxErrorList_MouseEnter(sender As Object, e As EventArgs) Handles ListBoxErrorList.MouseEnter
        CurrentBuffer = CType(sender, Control)
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
        LoadInputBufferFromStream(SourceFileNameWithPath)
        Dim ConvertedFileNameWithPath As String = item.ValueItem
        If Not File.Exists(ConvertedFileNameWithPath) Then
            Exit Sub
        End If

        LoadOutputBufferFromStream(ConvertedFileNameWithPath)
    End Sub

    Private Sub ListBoxFileList_Enter(sender As Object, e As EventArgs) Handles ListBoxFileList.Enter
        CurrentBuffer = CType(sender, Control)
    End Sub

    Private Sub ListBoxFileList_MouseEnter(sender As Object, e As EventArgs) Handles ListBoxFileList.MouseEnter
        CurrentBuffer = CType(sender, Control)
    End Sub

    Private Function LoadInputBufferFromStream(SourceFileNameWithPath As String) As Integer
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=True)
        Dim SourceText As String
        Using myFileStream As FileStream = File.OpenRead(path:=SourceFileNameWithPath)
            SourceText = GetFileTextFromStream(myFileStream)
        End Using

        Dim InputLines As Integer
        Dim ConversionInputLinesArray() As String = SourceText.SplitLines
        InputLines = ConversionInputLinesArray.Length
        If mnuOptionsColorizeSource.Checked Then
            Colorize(GetClassifiedRanges(ConversionInputLinesArray.Join(vbCrLf), LanguageNames.CSharp), RichTextBoxConversionInput, InputLines)
        Else
            RichTextBoxConversionInput.Text = ConversionInputLinesArray.Join(vbCrLf)
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
            Colorize(GetClassifiedRanges(ConversionInputLinesArray.Join(vbCrLf), LanguageNames.VisualBasic), RichTextBoxConversionOutput, InputLines)
        Else
            RichTextBoxConversionOutput.Text = ConversionInputLinesArray.Join(vbCrLf)
        End If
        LocalUseWaitCursor(MeForm:=Me, WaitCursorEnable:=False)
    End Sub

    Private Sub mnu_MRUList_Click(sender As Object, e As EventArgs)
        ' open the file...
        OpenSourceFile(DirectCast(sender, ToolStripItem).Tag.ToString().Substring(4))
    End Sub

    Private Sub mnu_MRUList_MouseDown(sender As Object, e As MouseEventArgs)
        If e.Button = Windows.Forms.MouseButtons.Right Then
            Clipboard.SetText(text:=CType(sender, ToolStripMenuItem).Text)
        End If
    End Sub

    Private Sub mnuCompile_Click(sender As Object, e As EventArgs) Handles mnuCompile.Click
        LineNumbers_For_RichTextBoxInput.Visible = False
        LineNumbers_For_RichTextBoxOutput.Visible = False

        If String.IsNullOrWhiteSpace(RichTextBoxConversionOutput.Text) Then
            Exit Sub
        End If
        ListBoxErrorList.Text = ""
        Compile_Colorize(RichTextBoxConversionOutput.Text)
    End Sub

    Private Sub mnuConvert_Click(sender As Object, e As EventArgs) Handles mnuConvert.Click
        mnuConvertConvertSnippet.Enabled = RichTextBoxConversionInput.TextLength > 0
    End Sub

    Private Async Sub mnuConvertConvertSnippet_Click(sender As Object, e As EventArgs) Handles mnuConvertConvertSnippet.Click
        SetButtonStopAndCursor(MeForm:=Me, StopButton:=ButtonStopConversion, StopButtonVisible:=True)
        ListBoxErrorList.Items.Clear()
        ListBoxFileList.Items.Clear()
        LineNumbers_For_RichTextBoxOutput.Visible = False
        ResizeRichTextBuffers()
        If _cancellationTokenSource IsNot Nothing Then
            _cancellationTokenSource.Dispose()
        End If
        _cancellationTokenSource = New CancellationTokenSource
        _requestToConvert = New ConvertRequest(My.Settings.SkipAutoGenerated, New Progress(Of ProgressReport)(AddressOf New TextProgressBar(ConversionProgressBar).Update), _cancellationTokenSource.Token) With
            {
            .SourceCode = RichTextBoxConversionInput.Text
            }
        Dim CSPreprocessorSymbols As New List(Of String) From {
            My.Settings.Framework
        }
        Dim VBPreprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
            KeyValuePair.Create(Of String, Object)(My.Settings.Framework, True)
        }
        Dim DontDisplayLineNumbers As Boolean = Await Convert_Compile_ColorizeAsync(_requestToConvert, CSPreprocessorSymbols, VBPreprocessorSymbols, OptionalReferences:=SharedReferences.CSharpReferences(Assembly.Load("System.Windows.Forms").Location, Nothing).ToArray, CancelToken:=_cancellationTokenSource.Token).ConfigureAwait(True)
        If _requestToConvert.CancelToken.IsCancellationRequested Then
            MsgBox($"Conversion canceled.", MsgBoxStyle.Information, Title:="C# to VB")
            ConversionProgressBar.Value = 0
        End If
        mnuConvertConvertFolder.Enabled = True
        SetButtonStopAndCursor(MeForm:=Me, StopButton:=ButtonStopConversion, StopButtonVisible:=False)
        LineNumbers_For_RichTextBoxOutput.Visible = (Not DontDisplayLineNumbers) OrElse My.Settings.ShowDestinationLineNumbers
    End Sub

    Private Async Sub mnuConvertFolder_Click(sender As Object, e As EventArgs) Handles mnuConvertConvertFolder.Click
        LineNumbers_For_RichTextBoxInput.Visible = False
        LineNumbers_For_RichTextBoxOutput.Visible = False
        Dim SourceFolderName As String
        Dim ProjectSavePath As String
        Using OFD As New FolderBrowserDialog
            With OFD
                .Description = "Select folder to convert..."
                .SelectedPath = My.Settings.DefaultProjectDirectory.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar) & Path.DirectorySeparatorChar
                .ShowNewFolderButton = False
                If .ShowDialog(Me) <> DialogResult.OK Then
                    Return
                End If
                SourceFolderName = .SelectedPath
                ProjectSavePath = GetFoldertSavePath((.SelectedPath), "cs", ConvertingProject:=False)
            End With
        End Using
        If Not Directory.Exists(SourceFolderName) Then
            MsgBox($"{SourceFolderName} is not a directory.", Title:="C# to VB")
            Exit Sub
        End If
        If String.IsNullOrWhiteSpace(ProjectSavePath) Then
            MsgBox($"Conversion aborted.", Title:="C# to VB")
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
        If Await ProcessAllFilesAsync(SourceFolderName,
                            ProjectSavePath,
                            "cs",
                            Stats,
                            _cancellationTokenSource.Token
                            ).ConfigureAwait(True) Then
            stopwatch.Stop()
            If _cancellationTokenSource.Token.IsCancellationRequested Then
                MsgBox($"Conversion canceled, {Stats.FilesProcessed} files completed successfully.", MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground Or MsgBoxStyle.OkOnly, Title:="C# to VB")
            Else
                MsgBox($"Conversion completed, {Stats.FilesProcessed} files completed successfully.", MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground Or MsgBoxStyle.OkOnly, Title:="C# to VB")
                mnuOptionsStartFolderConvertFromLastFile.Checked = False
                My.Settings.StartFolderConvertFromLastFile = False
                My.Settings.Save()
                Application.DoEvents()
            End If
        Else
            stopwatch.Stop()
            MsgBox($"Conversion stopped.", MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground Or MsgBoxStyle.OkOnly, Title:="C# to VB")
        End If
        Dim elapsed As TimeSpan = stopwatch.Elapsed
        StatusStripElapasedTimeLabel.Text = $"Elapsed Time - {elapsed.Hours}:{elapsed.Minutes}:{elapsed.Seconds}.{elapsed.Milliseconds}"

    End Sub

    Private Sub mnuEdit_Click(sender As Object, e As EventArgs) Handles mnuEdit.Click
        If TypeOf CurrentBuffer Is RichTextBox Then
            mnuEditCut.Enabled = True
            mnuEditPaste.Enabled = True
            mnuEditUndo.Enabled = True
        Else
            mnuEditCut.Enabled = False
            mnuEditPaste.Enabled = False
            mnuEditUndo.Enabled = False
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
        SearchBoxVisibility(Visible:=True)
    End Sub

    Private Sub mnuEditPaste_Click(sender As Object, e As EventArgs) Handles mnuEditPaste.Click
        If TypeOf CurrentBuffer Is RichTextBox Then
            CType(CurrentBuffer, RichTextBox).SelectedText = Clipboard.GetText(TextDataFormat.Text)
        End If
    End Sub

    Private Sub mnuEditUndo_Click(sender As Object, e As EventArgs) Handles mnuEditUndo.Click
        If TypeOf CurrentBuffer Is RichTextBox Then
            CType(CurrentBuffer, RichTextBox).Undo()
        End If
    End Sub

    Private Sub mnuFileExit_Click(sender As Object, e As EventArgs) Handles mnuFileExit.Click
        Close()
        End
    End Sub

    Private Async Sub mnuFileLastFolder_Click(sender As Object, e As EventArgs) Handles mnuFileLastFolder.Click
        Dim FolderName As String = CType(sender, ToolStripMenuItem).Text
        If Directory.Exists(FolderName) Then
            Dim SourceLanguageExtension As String = "cs"
            Dim ProjectSavePath As String = GetFoldertSavePath(FolderName, SourceLanguageExtension, ConvertingProject:=False)
            If String.IsNullOrWhiteSpace(ProjectSavePath) Then
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
            If Await ProcessAllFilesAsync(FolderName, ProjectSavePath, SourceLanguageExtension, Stats, _cancellationTokenSource.Token).ConfigureAwait(True) Then
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
            .InitialDirectory = My.Settings.DefaultProjectDirectory
            .FileName = ""
            .Filter = "C# Code Files (*.cs)|*.cs"
            SaveFileDialog1.FilterIndex = 0
            .Multiselect = False
            .ReadOnlyChecked = True
            .Title = $"Open C# Source file"
            .ValidateNames = True
            If .ShowDialog = DialogResult.OK Then
                mnuConvertConvertFolder.Enabled = False
                OpenSourceFile(OpenFileDialog1.FileName)
            Else
                mnuConvertConvertFolder.Enabled = True
            End If
        End With
    End Sub

    Private Async Sub mnuFileOpenProject_Click(sender As Object, e As EventArgs) Handles mnuFileConvertProject.Click
        With OpenFileDialog1
            .AddExtension = True
            .CheckFileExists = True
            .CheckPathExists = True
            .DefaultExt = "cs"
            .FileName = ""
            '.Filter = "C# Project or Solution (*.csproj, *.sln)|*.csproj; *.sln"
            .Filter = "C# Project (*.csproj)|*.csproj"
            .FilterIndex = 0
            .Multiselect = False
            .ReadOnlyChecked = True
            .Title = $"Open C# Project file"
            .ValidateNames = True
            ' InputLines is used for future progress bar
            If .ShowDialog = Global.System.Windows.Forms.DialogResult.OK Then
                _cancellationTokenSource = New CancellationTokenSource
                Dim projectSavePath As String = GetFoldertSavePath(Path.GetDirectoryName(.FileName), "cs", ConvertingProject:=True)
                If Not String.IsNullOrWhiteSpace(projectSavePath) Then
                    SetButtonStopAndCursor(MeForm:=Me, StopButton:=ButtonStopConversion, StopButtonVisible:=True)
                    Try
                        ListBoxErrorList.Items.Clear()
                        ListBoxFileList.Items.Clear()
                        RichTextBoxConversionInput.Clear()
                        RichTextBoxConversionOutput.Clear()
                        LabelProgress.Text = "Getting Analyzer Manger"
                        LabelProgress.Visible = True
                        ProgressBar1.Visible = True
                        Dim TaskAnalyzerManager As Task(Of AnalyzerManager) = GetManager()
                        While Not TaskAnalyzerManager.IsCompleted

                            If _cancellationTokenSource.IsCancellationRequested Then
                                LabelProgress.Visible = False
                                ProgressBar1.Visible = False
                                Exit Try
                            End If
                            Await Task.Delay(100).ConfigureAwait(True)
                        End While

                        If Await ProcessOneProjectUI(TaskAnalyzerManager.Result, sourceProjectNameWithPath:= .FileName, projectSavePath, _cancellationTokenSource).ConfigureAwait(True) Then
                            MsgBox($"{If(_cancellationTokenSource.Token.IsCancellationRequested, "Conversion canceled", "Conversion completed")}, {FilesConversionProgress.Text.ToLower(Globalization.CultureInfo.CurrentCulture)} completed successfully.",
                               MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground Or MsgBoxStyle.OkOnly,
                               Title:="C# to VB"
                               )
                        Else
                            MsgBox($"Conversion stopped.", MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground Or MsgBoxStyle.OkOnly, Title:="C# to VB")
                        End If
                        If File.Exists(projectSavePath) Then
                            Process.Start("explorer.exe", $"/root,{projectSavePath}")
                        End If
                    Catch ex As ObjectDisposedException
                    Finally
                        SetButtonStopAndCursor(Me, ButtonStopConversion, StopButtonVisible:=False)
                    End Try
                End If
            End If
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
            RichTextBoxConversionOutput.SaveFile(SaveFileDialog1.FileName, RichTextBoxStreamType.PlainText)
        End If
    End Sub

    Private Sub mnuFileSnippetLoadLast_Click(sender As Object, e As EventArgs) Handles mnuFileSnippetLoadLast.Click
        If My.Settings.ColorizeInput Then
            mnuConvertConvertSnippet.Enabled = 0 <> LoadInputBufferFromStream(s_snippetFileWithPath)
        Else
            RichTextBoxConversionInput.LoadFile(s_snippetFileWithPath, RichTextBoxStreamType.PlainText)
        End If
        mnuCompile.Enabled = True
    End Sub

    Private Sub mnuFileSnippetSave_Click(sender As Object, e As EventArgs) Handles mnuFileSnippetSave.Click
        If RichTextBoxConversionInput.TextLength = 0 Then
            Exit Sub
        End If
        RichTextBoxConversionInput.SaveFile(s_snippetFileWithPath, RichTextBoxStreamType.PlainText)
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
            OpenSourceFile(IgnoreFilesWithErrorsDialog.FileToLoad)
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
        Dim MRUToolStripItems As New List(Of ToolStripItem)
        ' create a temporary collection containing every MRU menu item
        ' (identified by the tag text when added to the list)...
        For Each FileMenuItem As ToolStripItem In mnuFile.DropDownItems
            If Not FileMenuItem.Tag Is Nothing Then
                If (FileMenuItem.Tag.ToString().StartsWith("MRU:", StringComparison.Ordinal)) Then
                    MRUToolStripItems.Add(FileMenuItem)
                End If
            End If
        Next
        ' iterate through list and remove each from menu...
        For Each MRUToolStripItem As ToolStripItem In MRUToolStripItems
            RemoveHandler MRUToolStripItem.Click, AddressOf mnu_MRUList_Click
            RemoveHandler MRUToolStripItem.MouseDown, AddressOf mnu_MRUList_MouseDown
            mnuFile.DropDownItems.Remove(MRUToolStripItem)
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
            mnuFile.DropDownItems.Insert(mnuFile.DropDownItems.Count - 10, clsItem)
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
        mnuConvertConvertSnippet.Enabled = LoadInputBufferFromStream(FileNameWithPath) <> 0
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
    Private Async Function ProcessAllFilesAsync(SourceDirectory As String, TargetDirectory As String, SourceLanguageExtension As String, Stats As ProcessingStats, CancelToken As CancellationToken) As Task(Of Boolean)
        Try
            ListBoxErrorList.Items.Clear()
            ListBoxFileList.Items.Clear()
            SetButtonStopAndCursor(Me, ButtonStopConversion, StopButtonVisible:=True)
            Dim TotalFilesToProcess As Long = GetFileCount(SourceDirectory, SourceLanguageExtension, My.Settings.SkipBinAndObjFolders, My.Settings.SkipTestResourceFiles)
            ' Process the list of files found in the directory.
            Return Await ProcessDirectoryAsync(SourceDirectory, TargetDirectory, MeForm:=Me, ButtonStopConversion, ListBoxFileList, SourceLanguageExtension, Stats, TotalFilesToProcess, AddressOf ProcessFileAsync, CancelToken).ConfigureAwait(True)
        Catch ex As OperationCanceledException
            ConversionProgressBar.Value = 0
        Catch ex As Exception
            ' don't crash on exit
            Stop
            End
        Finally
            SetButtonStopAndCursor(Me, ButtonStopConversion, StopButtonVisible:=False)
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
    Private Async Function ProcessFileAsync(SourceFileNameWithPath As String, TargetDirectory As String, SourceLanguageExtension As String, CSPreprocessorSymbols As List(Of String), VBPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)), OptionalReferences() As MetadataReference, CancelToken As CancellationToken) As Task(Of Boolean)
        If My.Settings.IgnoreFileList.Contains(SourceFileNameWithPath) Then
            Return True
        End If
        ButtonStopConversion.Visible = True
        RichTextBoxConversionOutput.Text = ""
        MRU_AddTo(SourceFileNameWithPath)
        Dim lines As Integer = LoadInputBufferFromStream(SourceFileNameWithPath)
        If lines > 0 Then
            _requestToConvert = New ConvertRequest(My.Settings.SkipAutoGenerated, New Progress(Of ProgressReport)(AddressOf New TextProgressBar(ConversionProgressBar).Update), _cancellationTokenSource.Token) With {
                .SourceCode = RichTextBoxConversionInput.Text
            }
            If Not Await Convert_Compile_ColorizeAsync(_requestToConvert, CSPreprocessorSymbols, VBPreprocessorSymbols, OptionalReferences, CancelToken).ConfigureAwait(True) Then
                If _requestToConvert.CancelToken.IsCancellationRequested Then
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
                            ListBoxErrorList.Text = ""
                            LineNumbers_For_RichTextBoxInput.Visible = My.Settings.ShowSourceLineNumbers
                            LineNumbers_For_RichTextBoxOutput.Visible = My.Settings.ShowDestinationLineNumbers
                        End If
                        Return True
                End Select
            Else
                If Not String.IsNullOrWhiteSpace(TargetDirectory) Then
                    If Not _requestToConvert.CancelToken.IsCancellationRequested Then
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
            Dim Delay As Integer = (1000 * My.Settings.ConversionDelay) \ LoopSleep
            For i As Integer = 0 To Delay
                Application.DoEvents()
                Thread.Sleep(LoopSleep)
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
        ListBoxFileList.Width = CInt(ClientSize.Width / 2 + 0.5)

        RichTextBoxConversionOutput.Width = ClientSize.Width - (RichTextBoxConversionInput.Width + LineNumberInputWidth + LineNumberOutputWidth)
        RichTextBoxConversionOutput.Left = RichTextBoxConversionInput.Width + LineNumberInputWidth + LineNumberOutputWidth

        Dim HalfClientWidth As Integer = ClientSize.Width \ 2
        ListBoxErrorList.Left = HalfClientWidth
        ListBoxErrorList.Width = HalfClientWidth
        StatusStripCurrentFileName.Width = HalfClientWidth

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
        If mnuOptionsColorizeSource.Checked AndAlso Not _inColorize Then
            Colorize(GetClassifiedRanges(SourceCode:=RichTextBoxConversionInput.Text, LanguageNames.CSharp), ConversionBuffer:=RichTextBoxConversionInput, Lines:=RichTextBoxConversionInput.Lines.Length)
        End If
    End Sub

    Private Sub RichTextBoxConversionOutput_HorizScrollBarRightClicked(sender As Object, loc As Point) Handles RichTextBoxConversionOutput.HorizScrollBarRightClicked
        Dim p As Integer = CType(sender, AdvancedRTB).HScrollPos
        MsgBox($"Horizontal Right Clicked At: {loc}, HScrollPos = {p}")
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
        MsgBox($"Vertical Right Clicked At: {loc}, VScrollPos = {p} ")
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
        If Not MenuItem.Checked Then Return
        For Each kvp As KeyValuePair(Of String, (Item As ToolStripMenuItem, Parent As ToolStripMenuItem)) In _frameworkVersionList
            If kvp.Key = MenuItem.Text Then
                MenuItem.Enabled = False
                My.Settings.Framework = MenuItem.Text
                My.Settings.Save()
                kvp.Value.Parent.Checked = True
                For Each ParentItem As KeyValuePair(Of String, ToolStripMenuItem) In _frameworkTypeList
                    ParentItem.Value.Checked = kvp.Value.Parent.Text = ParentItem.Key
                Next
            Else
                kvp.Value.Item.Enabled = True
                If kvp.Value.Item.Checked Then
                    kvp.Value.Item.Checked = False
                End If
            End If
        Next
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

    Shared Sub main()
        Dim MyForm As New Form1
        Application.EnableVisualStyles()
        MyForm.MyApplication.Run({""})
        MyForm.Dispose()
    End Sub

    Public Sub MeStartupNextInstance(
           ByVal sender As Object,
           ByVal e As StartupNextInstanceEventArgs
        ) Handles MyApplication.StartupNextInstance
        ' Put code here that takes command line parameter from additional instances
        ' and allows processing here
        Stop
    End Sub

End Class
