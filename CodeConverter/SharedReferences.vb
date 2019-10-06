' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.IO
Imports System.Reflection
Imports System.Reflection.Metadata
Imports System.Reflection.PortableExecutable
Imports System.Runtime.CompilerServices
Imports System.Windows.Forms
Imports Microsoft.CodeAnalysis

Public Module SharedReferences
    Private ReadOnly _CSharpReferences As New List(Of MetadataReference)
    Private ReadOnly _ReferencePath As New List(Of String)
    Private ReadOnly _VisualBasicReferences As New List(Of MetadataReference)
    Private ReadOnly FrameworkDirectory As String = Directory.GetParent(GetType(Object).Assembly.Location).FullName

    Private Sub BuildReferenceList(WindowsFormsLocation As String, OptionalReference As IReadOnlyList(Of MetadataReference))
        If _ReferencePath.Any Then
            Return
        End If
        ' CodeAnalysisReference
        Dim Location As String = GetType(Compilation).Assembly.Location
        AddReferences(_ReferencePath, Location)

        'SystemReferences
        For Each DLL_Path As String In Directory.GetFiles(FrameworkDirectory, "*.dll")
            If DLL_Path.EndsWith("System.EnterpriseServices.Wrapper.dll", StringComparison.InvariantCulture) Then
                Continue For
            End If
            AddReferences(_ReferencePath, DLL_Path)
        Next

        ' ComponentModelEditorBrowsable
        Location = GetType(System.ComponentModel.EditorBrowsableAttribute).GetAssemblyLocation
        AddReferences(_ReferencePath, Location)

        ' SystemCore
        Location = GetType(Enumerable).Assembly.Location
        AddReferences(_ReferencePath, Location)

        ' SystemXmlLinq
        Location = GetType(XElement).Assembly.Location
        AddReferences(_ReferencePath, Location)

        ' VBRuntime
        Location = GetType(CompilerServices.StandardModuleAttribute).Assembly.Location
        _VisualBasicReferences.Add(MetadataReference.CreateFromFile(Location))

        ' Windows Forms
        If Not String.IsNullOrWhiteSpace(WindowsFormsLocation) Then
            AddReferences(_ReferencePath, WindowsFormsLocation)
        End If

        ' Optional References
        If OptionalReference?.Any Then
            _VisualBasicReferences.AddRange(OptionalReference)
            _CSharpReferences.AddRange(OptionalReference)
        End If
    End Sub

    Private Sub AddReferences(L As List(Of String), FileNameWithPath As String)
        If L.Contains(FileNameWithPath) Then
            Return
        End If
        Dim hasMetadataOrIsAssembly As (HasMetadata As Boolean, IsAssembly As Boolean) = HasMetadataIsAssembly(FileNameWithPath)

        If Not hasMetadataOrIsAssembly.HasMetadata Then
            Return
        End If
        L.Add(FileNameWithPath)
        Application.DoEvents()
        If hasMetadataOrIsAssembly.IsAssembly Then
            _CSharpReferences.Add(MetadataReference.CreateFromFile(FileNameWithPath))
        End If
        Application.DoEvents()
        _VisualBasicReferences.Add(MetadataReference.CreateFromFile(FileNameWithPath))
    End Sub

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

    Public Function CSharpReferences(WindowsFormsLocation As String, OptionalReference As IReadOnlyList(Of MetadataReference)) As List(Of MetadataReference)
        If WindowsFormsLocation Is Nothing Then
            WindowsFormsLocation = ""
        End If
        Try
            SyncLock _ReferencePath
                If Not _CSharpReferences.Any Then
                    BuildReferenceList(WindowsFormsLocation, OptionalReference)
                End If
                Return _CSharpReferences.ToList
            End SyncLock
        Catch ex As OperationCanceledException
            Throw
        Catch ex As Exception
            Throw
        End Try
        Return Nothing
    End Function

    Public Function VisualBasicReferences(WindowsFormsLocation As String, Optional OptionalReference As IReadOnlyList(Of MetadataReference) = Nothing) As List(Of MetadataReference)
        Try
            If WindowsFormsLocation Is Nothing Then
                WindowsFormsLocation = ""
            End If
            SyncLock _ReferencePath
                If Not _VisualBasicReferences.Any Then
                    BuildReferenceList(WindowsFormsLocation, OptionalReference)
                End If
                Return _VisualBasicReferences.ToList
            End SyncLock
        Catch ex As OperationCanceledException
            Stop
        Catch ex As Exception
            Stop
        End Try
        Return Nothing
    End Function

    <Extension>
    Friend Function GetAssemblyLocation(type As Type) As String
        Dim asm As Assembly = type.GetTypeInfo().Assembly
        Dim locationProperty As PropertyInfo = asm.GetType().GetRuntimeProperties().Single(Function(p As PropertyInfo) p.Name = "Location")
        Return CStr(locationProperty.GetValue(asm))
    End Function

End Module
