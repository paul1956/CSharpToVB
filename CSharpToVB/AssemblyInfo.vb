Imports System.Collections.ObjectModel
Imports System.Reflection

Namespace Microsoft.VisualBasic.ApplicationServices
    '
    ' Summary:
    '     Provides properties for getting the information about the application, such as
    '     the version number, description, loaded assemblies, and so on.
    Public Class AssemblyInfo
        Public Sub New(currentAssembly As Assembly)
            If currentAssembly Is Nothing Then
                Throw New ArgumentNullException(NameOf(currentAssembly))
            End If
            Dim Attributes As IEnumerable(Of Object) = currentAssembly.GetCustomAttributes()
            CompanyName = Attributes.OfType(Of AssemblyCompanyAttribute).FirstOrDefault()?.Company
            Title = Attributes.OfType(Of AssemblyTitleAttribute).FirstOrDefault()?.Title
            Copyright = Attributes.OfType(Of AssemblyCopyrightAttribute).FirstOrDefault()?.Copyright
            Description = Attributes.OfType(Of AssemblyDescriptionAttribute).FirstOrDefault()?.Description
            DirectoryPath = Application.ExecutablePath
            ProductName = Attributes.OfType(Of AssemblyProductAttribute).FirstOrDefault()?.Product
            LoadedAssemblies = New ReadOnlyCollection(Of Assembly)(New List(Of Assembly) From {currentAssembly})
            Version = Version.Parse(Attributes.OfType(Of AssemblyFileVersionAttribute).FirstOrDefault()?.Version)
        End Sub

        Public ReadOnly Property Description As String
        Public ReadOnly Property CompanyName As String
        Public ReadOnly Property Title As String
        Public ReadOnly Property Copyright As String
        Public ReadOnly Property Trademark As String
        Public ReadOnly Property ProductName As String
        Public ReadOnly Property Version As Version
        Public ReadOnly Property AssemblyName As String
        Public ReadOnly Property DirectoryPath As String
        Public ReadOnly Property LoadedAssemblies As ReadOnlyCollection(Of Assembly)


        Public Shared Function GetStackTrace() As String
            Throw New NotImplementedException
        End Function

        Public Shared Function GetWorkingSet() As Long
            Return Process.GetCurrentProcess.WorkingSet64
        End Function
    End Class
End Namespace
