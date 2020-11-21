' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Reflection
Imports System.Reflection.Metadata
Imports System.Reflection.PortableExecutable
Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Namespace CSharpToVBConverter

    Public Module SharedReferences
        Private ReadOnly s_cSharpReferences As New List(Of MetadataReference)
        Private ReadOnly s_frameworkDirectory As String = Directory.GetParent(GetType(Object).Assembly.Location).FullName
        Private ReadOnly s_referencePath As New List(Of String)
        Private ReadOnly s_visualBasicReferences As New List(Of MetadataReference)

        Private Sub AddReferences(L As List(Of String), FileNameWithPath As String)
            If L.Contains(FileNameWithPath) Then
                Exit Sub
            End If
            Dim hasMetadataOrIsAssembly As (HasMetadata As Boolean, IsAssembly As Boolean) = HasMetadataIsAssembly(FileNameWithPath)

            If Not hasMetadataOrIsAssembly.HasMetadata Then
                Exit Sub
            End If
            L.Add(FileNameWithPath)
            If hasMetadataOrIsAssembly.IsAssembly Then
                s_cSharpReferences.Add(MetadataReference.CreateFromFile(FileNameWithPath))
            End If
            s_visualBasicReferences.Add(MetadataReference.CreateFromFile(FileNameWithPath))
        End Sub

        Private Sub BuildReferenceList(WindowsFormsLocation As String)
            If s_referencePath.Any Then
                Exit Sub
            End If
            ' CodeAnalysisReference
            Dim location As String = GetType(Compilation).Assembly.Location
            AddReferences(s_referencePath, location)

            'SystemReferences
            For Each dllPath As String In Directory.GetFiles(s_frameworkDirectory, "*.dll")
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
            If Not String.IsNullOrWhiteSpace(WindowsFormsLocation) Then
                AddReferences(s_referencePath, WindowsFormsLocation)
            End If

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

        <Extension>
        Friend Function GetAssemblyLocation(type As Type) As String
            Dim asm As Assembly = type.GetTypeInfo().Assembly
            Dim locationProperty As PropertyInfo = asm.GetType().GetRuntimeProperties().Single(Function(p As PropertyInfo) p.Name = "Location")
            Return CStr(locationProperty.GetValue(asm))
        End Function

        Public Function CSharpReferences(WindowsFormsLocation As String, OptionalReference As IReadOnlyList(Of MetadataReference)) As List(Of MetadataReference)

            If WindowsFormsLocation Is Nothing Then
                WindowsFormsLocation = ""
            End If
            Try
                SyncLock s_referencePath
                    If Not s_cSharpReferences.Any Then
                        BuildReferenceList(WindowsFormsLocation)
                    End If
                    If OptionalReference IsNot Nothing Then
                        ' Optional References
                        Dim tempList As New List(Of MetadataReference)
                        tempList.AddRange(s_cSharpReferences)
                        tempList.AddRange(OptionalReference)
                        Return tempList
                    End If
                    Return s_cSharpReferences
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
                SyncLock s_referencePath
                    If Not s_visualBasicReferences.Any Then
                        BuildReferenceList(WindowsFormsLocation)
                    End If
                    If OptionalReference IsNot Nothing Then
                        ' Optional References
                        Dim tempList As New List(Of MetadataReference)
                        tempList.AddRange(s_visualBasicReferences)
                        tempList.AddRange(OptionalReference)
                        Return tempList
                    End If
                    Return s_visualBasicReferences
                End SyncLock
            Catch ex As OperationCanceledException
                Stop
            Catch ex As Exception
                Stop
                Throw
            End Try
            Return Nothing
        End Function

    End Module
End Namespace
