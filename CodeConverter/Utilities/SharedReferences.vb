﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Reflection
Imports System.Reflection.Metadata
Imports System.Reflection.PortableExecutable
Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis

Public Module SharedReferences
    Private ReadOnly s_cSharpReferences As New List(Of MetadataReference)
    Private ReadOnly s_frameworkDirectory As String = Directory.GetParent(GetType(Object).Assembly.Location).FullName
    Private ReadOnly s_referencePath As New List(Of String)
    Private ReadOnly s_visualBasicReferences As New List(Of MetadataReference)

    Private Sub AddReferences(l As List(Of String), fileNameWithPath As String)
        If l.Contains(fileNameWithPath) Then
            Exit Sub
        End If
        Dim hasMetadataOrIsAssembly As (HasMetadata As Boolean, IsAssembly As Boolean) = HasMetadataIsAssembly(fileNameWithPath)

        If Not hasMetadataOrIsAssembly.HasMetadata Then
            Exit Sub
        End If
        l.Add(fileNameWithPath)
        If hasMetadataOrIsAssembly.IsAssembly Then
            s_cSharpReferences.Add(MetadataReference.CreateFromFile(fileNameWithPath))
        End If
        s_visualBasicReferences.Add(MetadataReference.CreateFromFile(fileNameWithPath))
    End Sub

    Private Sub BuildReferenceList(windowsFormsLocation As String)
        If s_referencePath.Any Then
            Exit Sub
        End If
        ' CodeAnalysisReference
        Dim location As String = GetType(Compilation).Assembly.Location
        AddReferences(s_referencePath, location)

        'SystemReferences
        For Each dllPath As String In Directory.EnumerateFiles(s_frameworkDirectory, "*.dll")
            If dllPath.EndsWith("System.EnterpriseServices.Wrapper.dll", StringComparison.Ordinal) Then
                Continue For
            End If
            AddReferences(s_referencePath, dllPath)
        Next

        ' ComponentModelEditorBrowsable
        location = GetType(ComponentModel.EditorBrowsableAttribute).GetAssemblyLocation
        AddReferences(s_referencePath, location)

        ' SystemCore
        location = GetType(Enumerable).Assembly.Location
        AddReferences(s_referencePath, location)

        ' SystemXmlLinq
        location = GetType(XElement).Assembly.Location
        AddReferences(s_referencePath, location)

        ' VBRuntime
        location = GetType(CompilerServices.StandardModuleAttribute).Assembly.Location
        s_visualBasicReferences.Add(MetadataReference.CreateFromFile(location))

        ' Windows Forms
        If Not String.IsNullOrWhiteSpace(windowsFormsLocation) Then
            AddReferences(s_referencePath, windowsFormsLocation)
        End If

    End Sub

    <Extension>
    Private Function GetAssemblyLocation(type As Type) As String
        Dim asm As Assembly = type.GetTypeInfo().Assembly
        Dim locationProperty As PropertyInfo = asm.GetType().GetRuntimeProperties().Single(Function(p As PropertyInfo) p.Name = "Location")
        Return CStr(locationProperty.GetValue(asm))
    End Function

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

    Public Function CSharpReferences(windowsFormsLocation As String, optionalReference As IReadOnlyList(Of MetadataReference)) As List(Of MetadataReference)

        If windowsFormsLocation Is Nothing Then
            windowsFormsLocation = ""
        End If
        Try
            SyncLock s_referencePath
                If Not s_cSharpReferences.Any Then
                    BuildReferenceList(windowsFormsLocation)
                End If
                If optionalReference IsNot Nothing Then
                    ' Optional References
                    Dim tempList As New List(Of MetadataReference)
                    tempList.AddRange(s_cSharpReferences)
                    tempList.AddRange(optionalReference)
                    Return tempList
                End If
                Return s_cSharpReferences
            End SyncLock
        Catch ex As OperationCanceledException
            Throw
        Catch ex As Exception
            Throw
        End Try
    End Function

    Public Function VisualBasicReferences(windowsFormsLocation As String, Optional optionalReference As IReadOnlyList(Of MetadataReference) = Nothing) As List(Of MetadataReference)
        Try
            If windowsFormsLocation Is Nothing Then
                windowsFormsLocation = ""
            End If
            SyncLock s_referencePath
                If Not s_visualBasicReferences.Any Then
                    BuildReferenceList(windowsFormsLocation)
                End If
                If optionalReference IsNot Nothing Then
                    ' Optional References
                    Dim tempList As New List(Of MetadataReference)
                    tempList.AddRange(s_visualBasicReferences)
                    tempList.AddRange(optionalReference)
                    Return tempList
                End If
            End SyncLock
        Catch ex As OperationCanceledException
            Stop
        Catch ex As Exception
            Throw
        End Try
        Return s_visualBasicReferences
    End Function

End Module
