' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.

Friend Enum VbErrors
    ' ReSharper disable InconsistentNaming
    None = 0
    ReturnWOGoSub = 3
    IllegalFuncCall = 5
    Overflow = 6
    OutOfMemory = 7
    OutOfBounds = 9

    ArrayLocked = 10
    DivByZero = 11

    TypeMismatch = 13
    OutOfStrSpace = 14

    ExprTooComplex = 16
    CantContinue = 17
    UserInterrupt = 18
    ResumeWOErr = 20

    OutOfStack = 28

    UNDONE = 29
    UndefinedProc = 35
    TooManyClients = 47
    DLLLoadErr = 48

    DLLBadCallingConv = 49
    InternalError = 51
    BadFileNameOrNumber = 52

    FileNotFound = 53
    BadFileMode = 54
    FileAlreadyOpen = 55
    IOError = 57
    FileAlreadyExists = 58
    BadRecordLen = 59
    DiskFull = 61
    EndOfFile = 62
    BadRecordNum = 63
    TooManyFiles = 67
    DevUnavailable = 68
    PermissionDenied = 70
    DiskNotReady = 71
    DifferentDrive = 74
    PathFileAccess = 75
    PathNotFound = 76
    ObjNotSet = 91

    IllegalFor = 92
    BadPatStr = 93
    CantUseNull = 94

    UserDefined = 95
    AdviseLimit = 96
    BadCallToFriendFunction = 97
    CantPassPrivateObject = 98
    DLLCallException = 99
    DoesntImplementICollection = 100

    Abort = 287

    InvalidFileFormat = 321
    CantCreateTmpFile = 322
    InvalidResourceFormat = 325
    InvalidPropertyValue = 380
    InvalidPropertyArrayIndex = 381
    SetNotSupportedAtRuntime = 382
    SetNotSupported = 383
    NeedPropertyArrayIndex = 385
    SetNotPermitted = 387
    GetNotSupportedAtRuntime = 393
    GetNotSupported = 394
    PropertyNotFound = 422

    NoSuchControlOrProperty = 423
    NotObject = 424
    CantCreateObject = 429

    OLENotSupported = 430
    OLEFileNotFound = 432

    OLENoPropOrMethod = 438

    OLEAutomationError = 440
    LostTLB = 442
    OLENoDefault = 443
    ActionNotSupported = 445
    NamedArgsNotSupported = 446

    LocaleSettingNotSupported = 447
    NamedParamNotFound = 448

    ParameterNotOptional = 449

    FuncArityMismatch = 450
    NotEnum = 451
    InvalidOrdinal = 452
    InvalidDllFunctionName = 453
    CodeResourceNotFound = 454
    CodeResourceLockError = 455
    DuplicateKey = 457
    InvalidTypeLibVariable = 458
    ObjDoesNotSupportEvents = 459
    InvalidClipboardFormat = 460
    IdentNotMember = 461
    ServerNotFound = 462

    ObjNotRegistered = 463
    InvalidPicture = 481
    PrinterError = 482
    CantSaveFileToTemp = 735
    SearchTextNotFound = 744
    ReplacementsTooLong = 746

    NotYetImplemented = 32768
    FileNotFoundWithName = 40243
    CantFindDllEntryPoint = 59201

    SeekErr = 32771
    ReadFault = 32772
    WriteFault = 32773
    BadFunctionId = 32774
    FileLockViolation = 32775
    ShareRequired = 32789
    BufferTooSmall = 32790
    InvDataRead = 32792
    UnsupFormat = 32793
    RegistryAccess = 32796
    LibNotRegistered = 32797
    Usage = 32799
    UndefinedType = 32807
    QualifiedNameDisallowed = 32808
    InvalidState = 32809
    WrongTypeKind = 32810
    ElementNotFound = 32811
    AmbiguousName = 32812
    ModNameConflict = 32813
    UnknownLcid = 32814
    BadModuleKind = 35005
    NoContainingLib = 35009
    BadTypeId = 35010
    BadLibId = 35011
    Eof = 35012

    SizeTooBig = 35013
    ExpectedFuncNotModule = 35015
    ExpectedFuncNotRecord = 35016
    ExpectedFuncNotProject = 35017
    ExpectedFuncNotVar = 35018
    ExpectedTypeNotProj = 35019
    UnsuitableFuncPropMatch = 35020
    BrokenLibRef = 35021
    UnsupportedTypeLibFeature = 35022
    ModuleAsType = 35024
    InvalidTypeInfoKind = 35025
    InvalidTypeLibFunction = 35026
    OperationNotAllowedInDll = 40035
    CompileError = 40036
    CantEvalWatch = 40037
    MissingVbaTypeLib = 40038
    UserReset = 40040
    MissingEndBrack = 40041
    IncorrectTypeChar = 40042
    InvalidNumLit = 40043
    IllegalChar = 40044
    IdTooLong = 40045
    StatementTooComplex = 40046
    ExpectedTokens = 40047
    InconsistentPropFuncs = 40067
    CircularType = 40068
    AccessViolation = &H80004003 'This is E_POINTER.  This is what VB6 returns from err.Number when calling into a .NET assembly that throws an AccessViolation

    LastTrappable = ReplacementsTooLong
    ' ReSharper restore InconsistentNaming
End Enum

' Implements error utilities for Basic
Friend NotInheritable Class ExceptionUtils

    ' Prevent creation.
    Private Sub New()
    End Sub

    Friend Shared Function VbMakeExceptionEx(number As Integer, sMsg As String) As Exception
        Dim vBDefinedError As Boolean

        VbMakeExceptionEx = BuildException(number, sMsg, vBDefinedError)

        If vBDefinedError Then
        End If

    End Function

    Friend Shared Function BuildException(number As Integer, description As String, ByRef vbDefinedError As Boolean) As Exception

        vbDefinedError = True

        Select Case number

            Case VbErrors.None

            Case VbErrors.ReturnWOGoSub,
                VbErrors.ResumeWOErr,
                VbErrors.CantUseNull,
                VbErrors.DoesntImplementICollection
                Return New InvalidOperationException(description)

            Case VbErrors.IllegalFuncCall,
                VbErrors.NamedParamNotFound,
                VbErrors.NamedArgsNotSupported,
                VbErrors.ParameterNotOptional
                Return New ArgumentException(description)

            Case VbErrors.OLENoPropOrMethod
                Return New MissingMemberException(description)

            Case VbErrors.Overflow
                Return New OverflowException(description)

            Case VbErrors.OutOfMemory, VbErrors.OutOfStrSpace
                Return New OutOfMemoryException(description)

            Case VbErrors.OutOfBounds
                Return New IndexOutOfRangeException(description)

            Case VbErrors.DivByZero
                Return New DivideByZeroException(description)

            Case VbErrors.TypeMismatch
                Return New InvalidCastException(description)

            Case VbErrors.OutOfStack
                Return New StackOverflowException(description)

            Case VbErrors.DLLLoadErr
                Return New TypeLoadException(description)

            Case VbErrors.FileNotFound
                Return New IO.FileNotFoundException(description)

            Case VbErrors.EndOfFile
                Return New IO.EndOfStreamException(description)

            Case VbErrors.IOError,
                VbErrors.BadFileNameOrNumber,
                VbErrors.BadFileMode,
                VbErrors.FileAlreadyOpen,
                VbErrors.FileAlreadyExists,
                VbErrors.BadRecordLen,
                VbErrors.DiskFull,
                VbErrors.BadRecordNum,
                VbErrors.TooManyFiles,
                VbErrors.DevUnavailable,
                VbErrors.PermissionDenied,
                VbErrors.DiskNotReady,
                VbErrors.DifferentDrive,
                VbErrors.PathFileAccess
                Return New IO.IOException(description)

            Case VbErrors.PathNotFound,
                VbErrors.OLEFileNotFound
                Return New IO.FileNotFoundException(description)

            Case VbErrors.ObjNotSet
                Return New NullReferenceException(description)

            Case VbErrors.PropertyNotFound
                Return New MissingFieldException(description)

            Case VbErrors.CantCreateObject,
                VbErrors.ServerNotFound
                Return New Exception(description)

            Case VbErrors.AccessViolation
                Return New AccessViolationException() 'We never want a custom description here.  Use the localized message that comes for free inside the exception

            Case Else
                'Fall below to default
                vbDefinedError = False
                Return New Exception(description)
        End Select

        vbDefinedError = False
        Return New Exception(description)

    End Function

    ''' <summary>
    ''' Return a new instance of ArgumentException with the message from resource file and the Exception.ArgumentName property set.
    ''' </summary>
    ''' <param name="argumentName">The name of the argument (parameter). Not localized.</param>
    ''' <param name="resourceId">The resource ID. Use CompilerServices.ResID.xxx</param>
    ''' <param name="placeHolders">Strings that will replace place holders in the resource string, if any.</param>
    ''' <returns>A new instance of ArgumentException.</returns>
    ''' <remarks>This is the preferred way to construct an argument exception.</remarks>
    Friend Shared Function GetArgumentExceptionWithArgName(argumentName As String,
                                                           resourceId As String,
                                                           ParamArray placeHolders() As String) As ArgumentException

        Return New ArgumentException(String.Format(resourceId, placeHolders), argumentName)
    End Function

    ''' <summary>
    ''' Return a new instance of ArgumentNullException with message: "Argument cannot be Nothing."
    ''' </summary>
    ''' <param name="argumentName">The name of the argument (parameter). Not localized.</param>
    ''' <returns>A new instance of ArgumentNullException.</returns>
    Friend Shared Function GetArgumentNullException(argumentName As String) As ArgumentNullException

        Return New ArgumentNullException(argumentName, "Argument Null Exception")
    End Function

    ''' <summary>
    ''' Return a new instance of InvalidOperationException with the message from resource file.
    ''' </summary>
    ''' <param name="resourceId">The resource ID. Use CompilerServices.ResID.xxx</param>
    ''' <param name="placeHolders">Strings that will replace place holders in the resource string, if any.</param>
    ''' <returns>A new instance of InvalidOperationException.</returns>
    Friend Shared Function GetInvalidOperationException(resourceId As String,
                                                        ParamArray placeHolders() As String) As InvalidOperationException

        Return New InvalidOperationException(String.Format(resourceId, placeHolders))
    End Function

End Class
