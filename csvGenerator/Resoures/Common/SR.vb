' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
'
'   This is a stand-in for the SR class used throughout FX.
'
' ReSharper disable once CheckNamespace
' ReSharper disable once InconsistentNaming
Partial Friend Class SR
' ReSharper disable InconsistentNaming
    Friend Const IO_FileNotFound_Path As String = "IO File Not Found Path"
    Friend Const IO_FilePathException As String = "IO File Path Exception"
    Friend Const IO_GetParentPathIsRoot_Path As String = "IO Get Parent Path Is Root Path"
    Friend Const TextFieldParser_BufferExceededMaxSize As String = "Text Field Parser Buffer Exceeded Max Size"
    Friend Const TextFieldParser_DelimiterNothing As String = "Text Field Parser Delimiter Nothing"
    Friend Const TextFieldParser_EndCharsInDelimiter As String = "Text Field Parser End Chars In Delimiter"
    Friend Const TextFieldParser_FieldWidthsMustPositive As String = "Text Field Parser Field Widths Must Positive"
    Friend Const TextFieldParser_FieldWidthsNothing As String = "Text Field Parser Field Widths Nothing"
    Friend Const TextFieldParser_IllegalDelimiter As String = "Text Field Parser Illegal Delimiter"
    Friend Const TextFieldParser_InvalidComment As String = "Text Field Parser Invalid Comment"
    Friend Const TextFieldParser_MalFormedDelimitedLine As String = "Text Field Parser MalFormed Delimited Line"
    Friend Const TextFieldParser_MalformedExtraData As String = "Text Field Parser Malformed Extra Data"
    Friend Const TextFieldParser_MalFormedFixedWidthLine As String = "Text Field Parser MalFormed Fixed Width Line"
    Friend Const TextFieldParser_MaxLineSizeExceeded As String = "Text Field Parser Max Line Size Exceeded"
    Friend Const TextFieldParser_NumberOfCharsMustBePositive As String = "Text Field Parser Number Of Chars Must Be Positive"
    Friend Const TextFieldParser_StreamNotReadable As String = "Text Field Parser Stream Not Readable"
    Friend Const TextFieldParser_WhitespaceInToken As String = "Text Field Parser Whitespace In Token"
' ReSharper restore InconsistentNaming

    Friend Shared Function Format(resourceFormat As String, p1 As Object) As String
        Return String.Format(resourceFormat, p1)
    End Function

End Class
