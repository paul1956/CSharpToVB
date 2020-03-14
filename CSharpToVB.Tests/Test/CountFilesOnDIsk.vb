Imports System.IO
Imports System.Reflection
Imports System.Threading
Imports System.Windows.Forms

Imports Microsoft.CodeAnalysis

Imports Xunit
Namespace DiskTest

    Public Class CountFilesOnDIsk
        Private ReadOnly _lastFileProcessed As String

        Private Function ProcessDirectory(SourceDirectory As String, ByRef LastFileNameWithPath As String, SourceLanguageExtension As String, ByRef FilesProcessed As Long, ByRef TotalFilesToProcess As Long, ProcessFile As Func(Of String, CancellationToken, Boolean), CancelToken As CancellationToken) As Boolean
            If String.IsNullOrWhiteSpace(SourceDirectory) OrElse Not Directory.Exists(SourceDirectory) Then
                Return True
            End If
            ' Process the list of files found in the directory.
            Try
                Dim DirectoryList As String() = Directory.GetFiles(path:=SourceDirectory, searchPattern:=$"*.{SourceLanguageExtension}")
                Dim CSPreprocessorSymbols As New List(Of String) From {"Anything"}
                Dim VBPreprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
                                            KeyValuePair.Create(Of String, Object)("Anything", True)
                                        }
                For Each PathWithFileName As String In DirectoryList
                    FilesProcessed += 1
                    If LastFileNameWithPath.Length = 0 OrElse LastFileNameWithPath = PathWithFileName Then
                        LastFileNameWithPath = ""

                        If Not ProcessFile(PathWithFileName, CancelToken) Then
                            Return False
                        End If
                        Application.DoEvents()
                    End If
                Next PathWithFileName
            Catch Ex As Exception
                Stop
                Throw New Exception(Ex.Message)
            End Try
            Dim subdirectoryEntries As String() = Directory.GetDirectories(path:=SourceDirectory)
            Try
                ' Recurse into subdirectories of this directory.
                For Each Subdirectory As String In subdirectoryEntries
                    Dim DirName As String = New DirectoryInfo(Subdirectory).Name.ToUpperInvariant
                    If (DirName = "BIN" OrElse DirName = "OBJ" OrElse DirName = "G") Then
                        Continue For
                    End If

                    If (Subdirectory.EndsWith("Test\Resources", StringComparison.OrdinalIgnoreCase) OrElse Subdirectory.EndsWith("Setup\Templates", StringComparison.OrdinalIgnoreCase)) Then
                        Continue For
                    End If
                    If Not ProcessDirectory(Subdirectory, LastFileNameWithPath, SourceLanguageExtension, FilesProcessed, TotalFilesToProcess, ProcessFile, CancelToken) Then
                        Return False
                    End If
                Next Subdirectory
            Catch ex As Exception
                Stop
                Throw
            End Try
            Return True
        End Function

        Private Function ProcessFile(PathWithFileName As String, CancelToken As CancellationToken) As Boolean
            If CancelToken.IsCancellationRequested OrElse Not File.Exists(PathWithFileName) Then
                Return False
            End If
            Return True
        End Function

        <Fact>
        Public Sub CountFiles()
            Dim FilesProcessed As Long = 0
            Dim S As New Stopwatch
            S.Start()
            Assert.True(ProcessDirectory(GetRoslynRootDirectory(), LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, TotalFilesToProcess:=0, ProcessFile:=AddressOf ProcessFile, CancellationToken.None), $"Failing file {_lastFileProcessed}")
            Dim Elapsed As TimeSpan = S.Elapsed
            Debug.WriteLine(Elapsed.TotalMilliseconds)
        End Sub

    End Class
End Namespace
