Public Module RoslynLocation
    Private _roslynRootDirectory As String = ""

    Public Function GetRoslynRootDirectory() As String
        If _roslynRootDirectory.IsEmptyNullOrWhitespace Then
            _roslynRootDirectory = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) & "\Source\Repos\roslyn\src"
        End If
        Return _roslynRootDirectory
    End Function
End Module