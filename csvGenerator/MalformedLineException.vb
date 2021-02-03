' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.

Imports System.ComponentModel
Imports System.Globalization

''' <summary>
'''  Indicates a line cannot be parsed into fields
''' </summary>
''' <remarks></remarks>
<Serializable()>
    Public Class MalformedLineException
        Inherits Exception

        ''' <summary>
        '''  Creates a new exception with no properties set
        ''' </summary>
        ''' <remarks></remarks>
        Public Sub New()
            MyBase.New()
        End Sub

    ''' <summary>
    '''  Creates a new exception, setting Message and LineNumber
    ''' </summary>
    ''' <param name="message">The message for the exception</param>
    ''' <param name="lineNumber">The number of the line that is malformed</param>
    ''' <remarks></remarks>
    Public Sub New(message As String, lineNumber As Long)
        MyBase.New(message)
        _lineNumber = lineNumber
    End Sub

    ''' <summary>
    '''  Creates a new exception, setting Message
    ''' </summary>
    ''' <param name="message">The message for the exception</param>
    ''' <remarks></remarks>
    Public Sub New(message As String)
        MyBase.New(message)
    End Sub

    ''' <summary>
    '''  Creates a new exception, setting Message, LineNumber, and InnerException
    ''' </summary>
    ''' <param name="message">The message for the exception</param>
    ''' <param name="lineNumber">The number of the line that is malformed</param>
    ''' <param name="innerException">The inner exception for the exception</param>
    ''' <remarks></remarks>
    Public Sub New(message As String, lineNumber As Long, innerException As Exception)
        MyBase.New(message, innerException)
        _lineNumber = lineNumber
    End Sub

    ''' <summary>
    '''  Creates a new exception, setting Message and InnerException
    ''' </summary>
    ''' <param name="message">The message for the exception</param>
    ''' <param name="innerException">The inner exception for the exception</param>
    ''' <remarks></remarks>
    Public Sub New(message As String, innerException As Exception)
        MyBase.New(message, innerException)
    End Sub

    ''' <summary>
    '''  Constructor used for serialization
    ''' </summary>
    ''' <param name="info"></param>
    ''' <param name="context"></param>
    ''' <remarks></remarks>
    <EditorBrowsable(EditorBrowsableState.Advanced)>
    Protected Sub New(info As Runtime.Serialization.SerializationInfo, context As Runtime.Serialization.StreamingContext)
        MyBase.New(info, context)

        If info IsNot Nothing Then ' Fix FxCop violation ValidateArgumentsOfPublicMethods.
            _lineNumber = info.GetInt32(LINE_NUMBER_PROPERTY)
        Else
            _lineNumber = -1
        End If
    End Sub

    ''' <summary>
    '''  The number of the offending line
    ''' </summary>
    ''' <value>The line number</value>
    ''' <remarks></remarks>
    <EditorBrowsable(EditorBrowsableState.Always)>
    Public Property LineNumber() As Long
        Get
            Return _lineNumber
        End Get
        Set(value As Long)
            _lineNumber = value
        End Set
    End Property

    ''' <summary>
    '''  Supports serialization
    ''' </summary>
    ''' <param name="info"></param>
    ''' <param name="context"></param>
    ''' <remarks></remarks>
    <EditorBrowsable(EditorBrowsableState.Advanced)>
    Public Overrides Sub GetObjectData(info As Runtime.Serialization.SerializationInfo, context As Runtime.Serialization.StreamingContext)
        If info IsNot Nothing Then ' Fix FxCop violation ValidateArgumentsOfPublicMethods.
            info.AddValue(LINE_NUMBER_PROPERTY, _lineNumber, GetType(Long))
        End If

        MyBase.GetObjectData(info, context)
    End Sub

    ''' <summary>
    '''  Appends extra data to string so that it's available when the exception is caught as an Exception
    ''' </summary>
    ''' <returns>The base ToString plus the Line Number</returns>
    ''' <remarks></remarks>
    Public Overrides Function ToString() As String
        Return MyBase.ToString() & " " & SR.Format(SR.TextFieldParser_MalformedExtraData, Me.LineNumber.ToString(CultureInfo.InvariantCulture))
    End Function

    ' Holds the line number
    Private _lineNumber As Long

    ' Name of property used for serialization
    Private Const LINE_NUMBER_PROPERTY As String = "LineNumber"

    End Class
