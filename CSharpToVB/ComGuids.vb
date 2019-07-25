' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Public NotInheritable Class ComGuids

    Private Sub New()
    End Sub

    ' IID GUID strings for relevant COM interfaces
    Friend Const IModalWindow As String = "b4db1657-70d7-485e-8e3e-6fcb5a5c1802"

    Friend Const IFileDialog As String = "42f85136-db7e-439c-85f1-e4075d135fc8"
    Friend Const IFileOpenDialog As String = "d57c7288-d4ad-4768-be02-9d969532d960"
    Friend Const IFileSaveDialog As String = "84bccd23-5fde-4cdb-aea4-af64b83d78ab"
    Friend Const IFileDialogEvents As String = "973510DB-7D7F-452B-8975-74A85828D354"
    Friend Const IFileDialogControlEvents As String = "36116642-D713-4b97-9B83-7484A9D00433"
    Friend Const IFileDialogCustomize As String = "8016b7b3-3d49-4504-a0aa-2a37494e606f"
    Friend Const IShellItem As String = "43826D1E-E718-42EE-BC55-A1E261C37BFE"
    Friend Const IShellItemArray As String = "B63EA76D-1F85-456F-A19C-48159EFA858B"
    Friend Const IKnownFolder As String = "38521333-6A87-46A7-AE10-0F16706816C3"
    Friend Const IKnownFolderManager As String = "44BEAAEC-24F4-4E90-B3F0-23D258FBB146"
    Friend Const IPropertyStore As String = "886D8EEB-8CF2-4446-8D02-CDBA1DBDCF99"
End Class

Friend NotInheritable Class CLSIDGuid

    Private Sub New()
    End Sub

    ' CLSID GUID strings for relevant coclasses
    Friend Const FileOpenDialog As String = "DC1C5A9C-E88A-4dde-A5A1-60F82A20AEF7"
    Friend Const FileSaveDialog As String = "C0B4E2F3-BA21-4773-8DBA-335EC946EB8B"
    Friend Const KnownFolderManager As String = "4df0c730-df9d-4ae3-9153-aa6b82e9795a"
End Class

Friend NotInheritable Class KFIDGuid

    Private Sub New()
    End Sub

    Friend Const ComputerFolder As String = "0AC0837C-BBF8-452A-850D-79D08E667CA7"
    Friend Const Favorites As String = "1777F761-68AD-4D8A-87BD-30B759FA33DD"
    Friend Const Documents As String = "FDD39AD0-238F-46AF-ADB4-6C85480369C7"
    Friend Const Profile As String = "5E6C858F-0E22-4760-9AFE-EA3317B67173"
End Class
