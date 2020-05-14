' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Module MRUSupport
    Friend Sub MRU_AddTo(mru_Data As Specialized.StringCollection, Path As String)
        ' remove the item from the collection if exists so that we can
        ' re-add it to the beginning...
        If mru_Data.Contains(Path) Then
            mru_Data.Remove(Path)
        End If
        ' add to MRU list..
        mru_Data.Add(Path)
        ' make sure there are only ever 5 items...
        While mru_Data.Count > 5
            mru_Data.RemoveAt(0)
        End While
    End Sub

End Module
