Option Explicit On
Option Infer Off
Option Strict On

Imports System.IO
Imports System.Reflection
Imports System.Reflection.Metadata
Imports System.Reflection.PortableExecutable
Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Public Module SharedReferences
    Private ReadOnly _CSharpReferences As New List(Of MetadataReference)
    Private ReadOnly _ReferencePath As New List(Of String)
    Private ReadOnly _VisualBasicReferences As New List(Of MetadataReference)
    Private ReadOnly FrameworkDirectory As String = Directory.GetParent(GetType(Object).Assembly.Location).FullName

    Private Sub BuildReferenceList()
        SyncLock _ReferencePath
            If _ReferencePath.Count > 0 Then
                Return
            End If
            ' CodeAnalysisReference
            Dim Location As String = GetType(Compilation).Assembly.Location
            _ReferencePath.AddReferences(Location)

            'SystemReferences
            For Each DLL_Path As String In Directory.GetFiles(FrameworkDirectory, "*.dll")
                If DLL_Path.EndsWith("System.EnterpriseServices.Wrapper.dll") Then
                    Continue For
                End If
                _ReferencePath.AddReferences(DLL_Path)
            Next

            ' ComponentModelEditorBrowsable
            Location = GetType(ComponentModel.EditorBrowsableAttribute).GetAssemblyLocation
            _ReferencePath.AddReferences(Location)

            ' SystemCore
            Location = GetType(Enumerable).Assembly.Location
            _ReferencePath.AddReferences(Location)

            ' SystemXmlLinq
            Location = GetType(XElement).Assembly.Location
            _ReferencePath.AddReferences(Location)

            ' VBRuntime
            Location = GetType(CompilerServices.StandardModuleAttribute).Assembly.Location
            _VisualBasicReferences.Add(MetadataReference.CreateFromFile(Location))
        End SyncLock
    End Sub

    <Extension>
    Private Sub AddReferences(L As List(Of String), FileNameWithPath As String)
        If L.Contains(FileNameWithPath) Then
            Return
        End If
        Dim hasMetadataIsAssembly As (HasMetadata As Boolean, IsAssembly As Boolean) = FileNameWithPath.HasMetadataIsAssembly

        If Not hasMetadataIsAssembly.HasMetadata Then
            Return
        End If
        L.Add(FileNameWithPath)
        If hasMetadataIsAssembly.IsAssembly Then
            _CSharpReferences.Add(MetadataReference.CreateFromFile(FileNameWithPath))
        End If
        _VisualBasicReferences.Add(MetadataReference.CreateFromFile(FileNameWithPath))
    End Sub

    <Extension>
    Private Function HasMetadataIsAssembly(sourcePath As String) As (HasMetadata As Boolean, IsAssembly As Boolean)
        Using assemblyStream As New FileStream(sourcePath, FileMode.Open, FileAccess.Read, FileShare.Delete Or FileShare.Read)
            Try
                Using peReader As New PEReader(assemblyStream, PEStreamOptions.LeaveOpen)
                    If peReader.HasMetadata Then
                        Dim reader As MetadataReader = peReader.GetMetadataReader()
                        Return (True, reader.IsAssembly)
                    End If
                End Using
            Catch e1 As BadImageFormatException
                ' not a PE
            End Try

            Return (False, False)
        End Using
    End Function

    Public Function CSharpReferences() As List(Of MetadataReference)
        Try
            If _CSharpReferences.Count = 0 Then
                BuildReferenceList()
            End If
            Return _CSharpReferences.ToList
        Catch ex As Exception
            Stop
        End Try
        Return Nothing
    End Function

    Public Function VisualBasicReferences() As List(Of MetadataReference)
        Try
            If _VisualBasicReferences.Count = 0 Then
                BuildReferenceList()
            End If
            Return _VisualBasicReferences.ToList
        Catch ex As Exception
            Stop
        End Try
        Return Nothing
    End Function

    <Extension>
    Public Function GetAssemblyLocation(type As Type) As String
        Dim asm As Assembly = type.GetTypeInfo().Assembly
        Dim locationProperty As PropertyInfo = asm.GetType().GetRuntimeProperties().Single(Function(p As PropertyInfo) p.Name = "Location")
        Return CStr(locationProperty.GetValue(asm))
    End Function

End Module