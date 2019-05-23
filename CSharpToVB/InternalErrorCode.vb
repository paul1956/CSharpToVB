Option Explicit On
Option Infer Off
Option Strict On

''' <summary>
''' Values for ErrorCode/ERRID that are used internally by the compiler but are not exposed.
''' </summary>
Friend Module InternalErrorCode
    ''' <summary>
    ''' The code has yet to be determined.
    ''' </summary>
    Public Const Unknown As Integer = -1

    ''' <summary>
    ''' The code was lazily determined and does not need to be reported.
    ''' </summary>
    Public Const Void As Integer = -2
End Module
