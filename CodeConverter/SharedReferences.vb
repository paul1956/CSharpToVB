Option Explicit On
Option Infer Off
Option Strict On

Imports System.IO
Imports System.Reflection
Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Public Module SharedReferences
    Private ReadOnly FrameworkDirectory As String = Directory.GetParent(GetType(Object).Assembly.Location).FullName

    Private ReadOnly _References As New List(Of MetadataReference)
    Private ReadOnly _ReferencePath As New List(Of String)
    Private Sub BuildReferenceList()
        If _References.Count > 0 Then
            Return
        End If
        ' CodeAnalysisReference
        Dim Location As String = GetType(Compilation).Assembly.Location
        _ReferencePath.Add(Location)
        _References.Add(MetadataReference.CreateFromFile(GetType(Compilation).Assembly.Location))

        'SystemReferences
        For Each DLL_Path As String In Directory.GetFiles(FrameworkDirectory, "*.dll")
            If _ReferencePath.Contains(DLL_Path) Then
                Stop
            Else
                _ReferencePath.Add(DLL_Path)
                _References.Add(MetadataReference.CreateFromFile(DLL_Path))
            End If
        Next

#If Not NETCOREAPP3_0 Then
        ' ComponentModelEditorBrowsable
        Location = GetType(ComponentModel.EditorBrowsableAttribute).GetAssemblyLocation
        If _ReferencePath.Contains(Location) Then
            Stop
        Else
            _ReferencePath.Add(Location)
            _References.Add(MetadataReference.CreateFromFile(Location))
        End If

        ' SystemCore
        Location = GetType(Enumerable).Assembly.Location
        If _ReferencePath.Contains(Location) Then
            Stop
        Else
            _ReferencePath.Add(Location)
            _References.Add(MetadataReference.CreateFromFile(Location))
        End If

        ' SystemXmlLinq
        Location = GetType(XElement).Assembly.Location
        If _ReferencePath.Contains(Location) Then
            Stop
        Else
            _ReferencePath.Add(Location)
            _References.Add(MetadataReference.CreateFromFile(Location))
        End If
#End If

        ' VBRuntime
        Location = GetType(CompilerServices.StandardModuleAttribute).Assembly.Location
        If _ReferencePath.Contains(Location) Then
            Stop
        Else
            _ReferencePath.Add(Location)
            _References.Add(MetadataReference.CreateFromFile(Location))
        End If

    End Sub

    <Extension>
    Public Function GetAssemblyLocation(type As Type) As String
        Dim asm As Assembly = type.GetTypeInfo().Assembly
        Dim locationProperty As PropertyInfo = asm.GetType().GetRuntimeProperties().Single(Function(p As PropertyInfo) p.Name = "Location")
        Return CStr(locationProperty.GetValue(asm))
    End Function

    Public Function References() As List(Of MetadataReference)
        Try
            If _References.Count = 0 Then
                BuildReferenceList()
            End If
            Return _References.ToList
        Catch ex As Exception
            Stop
        End Try
        Return Nothing
    End Function

End Module
