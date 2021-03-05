' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.

Imports System.ComponentModel
Imports System.Globalization
Imports System.IO
Imports System.Security
Imports System.Text
Imports System.Text.RegularExpressions
Imports System.IO.Path

''' <summary>
'''  Enum used to indicate the kind of file being read, either delimited or fixed length
''' </summary>
''' <remarks></remarks>
Public Enum FieldType As Integer

    '!!!!!!!!!! Changes to this enum must be reflected in ValidateFieldTypeEnumValue()
    Delimited

    FixedWidth
End Enum

''' <summary>
'''  Helper class that when passed a line and an index to a quote delimited field
'''  will build the field and handle escaped quotes
''' </summary>
''' <remarks></remarks>
Friend Class QuoteDelimitedFieldBuilder

    ' The regular expression used to find the next delimiter
    Private ReadOnly _delimiterRegex As Regex

    ' String builder holding the field
    Private ReadOnly _field As New StringBuilder

    ' Chars that should be counted as space (and hence ignored if occurring before or after a delimiter
    Private ReadOnly _spaceChars As String

    ' The length of the closing delimiter if one is found
    Private _delimiterLength As Integer

    ' Indicates m_Field contains the entire field
    Private _fieldFinished As Boolean

    ' The current index on the field
    Private _index As Integer

    ' Indicates the line breaks the csv rules we enforce
    Private _malformedLine As Boolean

    ''' <summary>
    '''  Creates an instance of the class and sets some properties
    ''' </summary>
    ''' <param name="delimiterRegex">The regex used to find any of the delimiters</param>
    ''' <param name="spaceChars">Characters treated as space (usually space and tab)</param>
    ''' <remarks></remarks>
    Public Sub New(delimiterRegex As Regex, spaceChars As String)
        _delimiterRegex = delimiterRegex
        _spaceChars = spaceChars
    End Sub

    ''' <summary>
    '''  The length of the closing delimiter if one was found
    ''' </summary>
    ''' <value>The length of the delimiter</value>
    ''' <remarks></remarks>
    Public ReadOnly Property DelimiterLength As Integer
        Get
            Return _delimiterLength
        End Get
    End Property

    ''' <summary>
    '''  The field being built
    ''' </summary>
    ''' <value>The field</value>
    ''' <remarks></remarks>
    Public ReadOnly Property Field As String
        Get
            Return _field.ToString()
        End Get
    End Property

    ''' <summary>
    '''  Indicates whether or not the field has been built.
    ''' </summary>
    ''' <value>True if the field has been built, otherwise False</value>
    ''' <remarks>If the Field has been built, the Field property will return the entire field</remarks>
    Public ReadOnly Property FieldFinished As Boolean
        Get
            Return _fieldFinished
        End Get
    End Property

    ''' <summary>
    '''  Indicates that the current field breaks the subset of csv rules we enforce
    ''' </summary>
    ''' <value>True if the line is malformed, otherwise False</value>
    ''' <remarks>
    '''  The rules we enforce are:
    '''      Embedded quotes must be escaped
    '''      Only space characters can occur between a delimiter and a quote
    '''</remarks>
    Public ReadOnly Property MalformedLine As Boolean
        Get
            Return _malformedLine
        End Get
    End Property

    ''' <summary>
    '''  The current index on the line. Used to indicate how much of the line was used to build the field
    ''' </summary>
    ''' <value>The current position on the line</value>
    ''' <remarks></remarks>
    Public ReadOnly Property Index As Integer
        Get
            Return _index
        End Get
    End Property

    ''' <summary>
    '''  Builds a field by walking through the passed in line starting at StartAt
    ''' </summary>
    ''' <param name="line">The line containing the data</param>
    ''' <param name="startAt">The index at which we start building the field</param>
    ''' <remarks></remarks>
    Public Sub BuildField(line As String, startAt As Integer)

        _index = startAt
        Dim length As Integer = line.Length

        While _index < length

            If line(_index) = """"c Then

                ' Are we at the end of the file?
                If _index + 1 = length Then
                    ' We've found the end of the field
                    _fieldFinished = True
                    _delimiterLength = 1

                    ' Move index past end of line
                    _index += 1
                    Return
                End If
                ' Check to see if this is an escaped quote
                If _index + 1 < line.Length And line(_index + 1) = """"c Then
                    _field.Append(""""c)
                    _index += 2
                    Continue While
                End If

                ' Find the next delimiter and make sure everything between the quote and
                ' the delimiter is ignorable
                Dim limit As Integer
                Dim delimiterMatch As Match = _delimiterRegex.Match(line, _index + 1)
                If Not delimiterMatch.Success Then
                    limit = length - 1
                Else
                    limit = delimiterMatch.Index - 1
                End If

                For i As Integer = _index + 1 To limit
                    If _spaceChars.IndexOf(line(i)) < 0 Then
                        _malformedLine = True
                        Return
                    End If
                Next

                ' The length of the delimiter is the length of the closing quote (1) + any spaces + the length
                ' of the delimiter we matched if any
                _delimiterLength = 1 + limit - _index
                If delimiterMatch.Success Then
                    _delimiterLength += delimiterMatch.Length
                End If

                _fieldFinished = True
                Return
            Else
                _field.Append(line(_index))
                _index += 1
            End If
        End While
    End Sub

End Class

''' <summary>
'''  Enables parsing very large delimited or fixed width field files
''' </summary>
''' <remarks></remarks>
Public Class TextFieldParser
    Implements IDisposable


    ' Regex pattern to determine if field begins with quotes
    ' ReSharper disable InconsistentNaming
    Private Const BEGINS_WITH_QUOTE As String = "\G[{0}]*"""

    ' The default size for the buffer
    Private Const DEFAULT_BUFFER_LENGTH As Integer = 4096

    ' This is a guess as to how much larger the string builder should be beyond the size of what
    ' we've already read
    Private Const DEFAULT_BUILDER_INCREASE As Integer = 10

    ' Regex pattern to find a quote before a delimiter
    Private Const ENDING_QUOTE As String = """[{0}]*"

    ' Options used for regular expressions
    Private Const REGEX_OPTIONS As RegexOptions = RegexOptions.CultureInvariant
    ' ReSharper restore InconsistentNaming

    ' Indicates passed in stream should be not be closed
    Private ReadOnly _leaveOpen As Boolean

    ' The largest size the buffer can be
    Private ReadOnly _maxBufferSize As Integer = 10000000

    ' The largest size a line can be.
    Private ReadOnly _maxLineSize As Integer = 10000000

    ' Codes for whitespace as used by String.Trim excluding line end chars as those are handled separately
    Private ReadOnly _whitespaceCodes() As Integer = {&H9, &HB, &HC, &H20, &H85, &HA0, &H1680, &H2000, &H2001, &H2002, &H2003, &H2004, &H2005, &H2006, &H2007, &H2008, &H2009, &H200A, &H200B, &H2028, &H2029, &H3000, &HFEFF}

    ' Regular expression for whitespace
    Private ReadOnly _whiteSpaceRegEx As Regex = New Regex("\s", REGEX_OPTIONS)

    ' Regular expression used to find beginning quotes ignore spaces and tabs
    Private _beginQuotesRegex As Regex

    ' Buffer used to hold data read from the file. It holds data that must be read
    ' ahead of the cursor (for PeekChars and EndOfData)
    Private _buffer(DEFAULT_BUFFER_LENGTH - 1) As Char

    ' The number of chars in the buffer
    Private _charsRead As Integer

    ' An array holding the strings that indicate a line is a comment
    Private _commentTokens() As String = Array.Empty(Of String)()

    ' Regular expression used to find delimiters
    Private _delimiterRegex As Regex

    ' An array of the delimiters used for the fields in the file
    Private _delimiters() As String

    ' Holds a copy of the field widths last set so we can respond to changes in the array
    Private _delimitersCopy() As String

    ' Regex used with BuildField
    Private _delimiterWithEndCharsRegex As Regex

    ' Indicates reader has been disposed
    Private _disposed As Boolean

    ' Flags whether or not there is data left to read. Assume there is at creation
    Private _endOfData As Boolean

    ' Holds the last malformed line
    Private _errorLine As String = ""

    ' Holds the line number of the last malformed line
    Private _errorLineNumber As Long = -1

    ' An array of the widths of the fields in a fixed width file
    Private _fieldWidths() As Integer

    ' Holds a copy of the field widths last set so we can respond to changes in the array
    Private _fieldWidthsCopy() As Integer

    ' Indicates whether or not we handle quotes in a csv appropriate way
    Private _hasFieldsEnclosedInQuotes As Boolean = True

    ' The minimum length for a valid fixed width line
    Private _lineLength As Integer

    ' The line last read by either ReadLine or ReadFields
    Private _lineNumber As Long = 1

    ' Indicates that the user has changed properties so that we need to validate before a read
    Private _needPropertyCheck As Boolean = True

    ' The position of the peek cursor
    Private _peekPosition As Integer

    ' The position of the cursor in the buffer
    Private _position As Integer

    ' The internal StreamReader that reads the file
    Private _reader As TextReader

    ' A string of the chars that count as spaces (used for csv format). The norm is spaces and tabs.
    Private _spaceChars As String

    ' Indicates what type of fields are in the file (fixed width or delimited)
    Private _textFieldType As FieldType = FieldType.Delimited

    ' Indicates whether or not white space should be removed from a returned field
    Private _trimWhiteSpace As Boolean = True

    ''' <summary>
    '''  Creates a new TextFieldParser to parse the passed in file
    ''' </summary>
    ''' <param name="path">The path of the file to be parsed</param>
    ''' <remarks></remarks>
    Public Sub New(path As String)

        ' Default to UTF-8 and detect encoding
        Me.InitializeFromPath(path, Encoding.UTF8, True)
    End Sub

    ''' <summary>
    '''  Creates a new TextFieldParser to parse the passed in file
    ''' </summary>
    ''' <param name="path">The path of the file to be parsed</param>
    ''' <param name="defaultEncoding">The decoding to default to if encoding isn't determined from file</param>
    ''' <remarks></remarks>
    Public Sub New(path As String, defaultEncoding As Encoding)

        ' Default to detect encoding
        Me.InitializeFromPath(path, defaultEncoding, True)
    End Sub

    ''' <summary>
    '''  Creates a new TextFieldParser to parse the passed in file
    ''' </summary>
    ''' <param name="path">The path of the file to be parsed</param>
    ''' <param name="defaultEncoding">The decoding to default to if encoding isn't determined from file</param>
    ''' <param name="detectEncoding">Indicates whether or not to try to detect the encoding from the BOM</param>
    ''' <remarks></remarks>
    Public Sub New(path As String, defaultEncoding As Encoding, detectEncoding As Boolean)

        Me.InitializeFromPath(path, defaultEncoding, detectEncoding)
    End Sub

    ''' <summary>
    '''  Creates a new TextFieldParser to parse a file represented by the passed in stream
    ''' </summary>
    ''' <param name="stream"></param>
    ''' <remarks></remarks>
    Public Sub New(stream As Stream)

        ' Default to UTF-8 and detect encoding
        Me.InitializeFromStream(stream, Encoding.UTF8, True)
    End Sub

    ''' <summary>
    '''  Creates a new TextFieldParser to parse a file represented by the passed in stream
    ''' </summary>
    ''' <param name="stream"></param>
    ''' <param name="defaultEncoding">The decoding to default to if encoding isn't determined from file</param>
    ''' <remarks></remarks>
    Public Sub New(stream As Stream, defaultEncoding As Encoding)

        ' Default to detect encoding
        Me.InitializeFromStream(stream, defaultEncoding, True)
    End Sub

    ''' <summary>
    '''  Creates a new TextFieldParser to parse a file represented by the passed in stream
    ''' </summary>
    ''' <param name="stream"></param>
    ''' <param name="defaultEncoding">The decoding to default to if encoding isn't determined from file</param>
    ''' <param name="detectEncoding">Indicates whether or not to try to detect the encoding from the BOM</param>
    ''' <remarks></remarks>
    Public Sub New(stream As Stream, defaultEncoding As Encoding, detectEncoding As Boolean)

        Me.InitializeFromStream(stream, defaultEncoding, detectEncoding)
    End Sub

    ''' <summary>
    '''  Creates a new TextFieldParser to parse a file represented by the passed in stream
    ''' </summary>
    ''' <param name="stream"></param>
    ''' <param name="defaultEncoding">The decoding to default to if encoding isn't determined from file</param>
    ''' <param name="detectEncoding">Indicates whether or not to try to detect the encoding from the BOM</param>
    ''' <param name="leaveOpen">Indicates whether or not to leave the passed in stream open</param>
    ''' <remarks></remarks>
    Public Sub New(stream As Stream, defaultEncoding As Encoding, detectEncoding As Boolean, leaveOpen As Boolean)

        _leaveOpen = leaveOpen
        Me.InitializeFromStream(stream, defaultEncoding, detectEncoding)
    End Sub

    ''' <summary>
    '''  Creates a new TextFieldParser to parse a stream or file represented by the passed in TextReader
    ''' </summary>
    ''' <param name="reader">The TextReader that does the reading</param>
    ''' <remarks></remarks>
    Public Sub New(reader As TextReader)

        If reader Is Nothing Then
            Throw New ArgumentNullException(NameOf(reader))
        End If

        _reader = reader

        Me.ReadToBuffer()

    End Sub

    ''' <summary>
    '''  An array of the strings that indicate a line is a comment
    ''' </summary>
    ''' <value>An array of comment indicators</value>
    ''' <remarks>Returns an empty array if not set</remarks>
    ''' <summary>
    ''' Clean up following dispose pattern
    ''' </summary>
    ''' <remarks></remarks>
    Protected Overrides Sub Finalize()
        ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
        Me.Dispose(False)
        MyBase.Finalize()
    End Sub

    ''' <summary>
    '''  Function to call when we're at the end of the buffer. We either re fill the buffer
    '''  or change the size of the buffer
    ''' </summary>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Private Delegate Function ChangeBufferFunction() As Integer

    ''' <summary>
    '''  Gets the appropriate regex for finding a field beginning with quotes
    ''' </summary>
    ''' <value>The right regex</value>
    ''' <remarks></remarks>
    Private ReadOnly Property BeginQuotesRegex As Regex
        Get
            If _beginQuotesRegex Is Nothing Then
                ' Get the pattern
                Dim pattern As String = String.Format(CultureInfo.InvariantCulture, BEGINS_WITH_QUOTE, Me.WhitespacePattern)
                _beginQuotesRegex = New Regex(pattern, REGEX_OPTIONS)
            End If

            Return _beginQuotesRegex
        End Get
    End Property

    ''' <summary>
    '''  Gets the appropriate expression for finding ending quote of a field
    ''' </summary>
    ''' <value>The expression</value>
    ''' <remarks></remarks>
    Private ReadOnly Property EndQuotePattern As String
        Get
            Return String.Format(CultureInfo.InvariantCulture, ENDING_QUOTE, Me.WhitespacePattern)
        End Get
    End Property

    ''' <summary>
    ''' Returns a string containing all the characters which are whitespace for parsing purposes
    ''' </summary>
    ''' <value></value>
    ''' <remarks></remarks>
    Private ReadOnly Property WhitespaceCharacters As String
        Get
            Dim builder As New StringBuilder
            For Each code As Integer In _whitespaceCodes

                Dim spaceChar As Char = ChrW(code)
                If Not Me.CharacterIsInDelimiter(spaceChar) Then
                    builder.Append(spaceChar)
                End If
            Next

            Return builder.ToString()
        End Get
    End Property

    ''' <summary>
    ''' Gets the character set of white-spaces to be used in a regex pattern
    ''' </summary>
    ''' <value></value>
    ''' <remarks></remarks>
    Private ReadOnly Property WhitespacePattern As String
        Get
            Dim builder As New StringBuilder()
            For Each code As Integer In _whitespaceCodes
                Dim spaceChar As Char = ChrW(code)
                If Not Me.CharacterIsInDelimiter(spaceChar) Then
                    ' Gives us something like \u00A0
                    builder.Append("\u" & code.ToString("X4", CultureInfo.InvariantCulture))
                End If
            Next

            Return builder.ToString()
        End Get
    End Property

    <EditorBrowsable(EditorBrowsableState.Advanced)>
    Public Property CommentTokens As String()
        Get
            Return _commentTokens
        End Get
        Set
            Me.CheckCommentTokensForWhitespace(Value)
            _commentTokens = Value
            _needPropertyCheck = True
        End Set
    End Property

    Public Property Delimiters As String()
        Get
            Return _delimiters
        End Get
        Set
            If Value IsNot Nothing Then
                ValidateDelimiters(Value)

                ' Keep a copy so we can determine if the user changes elements of the array
                _delimitersCopy = DirectCast(Value.Clone(), String())
            Else
                _delimitersCopy = Nothing
            End If

            _delimiters = Value

            _needPropertyCheck = True

            ' Force rebuilding of regex
            _beginQuotesRegex = Nothing

        End Set
    End Property

    ''' <summary>
    '''  Indicates whether or not there is any data (non ignorable lines) left to read in the file
    ''' </summary>
    ''' <value>True if there's more data to read, otherwise False</value>
    ''' <remarks>Ignores comments and blank lines</remarks>
    Public ReadOnly Property EndOfData As Boolean
        Get
            If _endOfData Then
                Return _endOfData
            End If

            ' Make sure we're not at end of file
            If _reader Is Nothing Or _buffer Is Nothing Then
                _endOfData = True
                Return True
            End If

            'See if we can get a data line
            If Me.PeekNextDataLine() IsNot Nothing Then
                Return False
            End If

            _endOfData = True
            Return True
        End Get
    End Property

    ''' <summary>
    '''  Returns the last malformed line if there is one.
    ''' </summary>
    ''' <value>The last malformed line</value>
    ''' <remarks></remarks>
    Public ReadOnly Property ErrorLine As String
        Get
            Return _errorLine
        End Get
    End Property

    ''' <summary>
    '''  Returns the line number of last malformed line if there is one.
    ''' </summary>
    ''' <value>The last malformed line number</value>
    ''' <remarks></remarks>
    Public ReadOnly Property ErrorLineNumber As Long
        Get
            Return _errorLineNumber
        End Get
    End Property

    Public Property FieldWidths As Integer()
        Get
            Return _fieldWidths
        End Get
        Set
            If Value IsNot Nothing Then
                ValidateFieldWidthsOnInput(Value)

                ' Keep a copy so we can determine if the user changes elements of the array
                _fieldWidthsCopy = DirectCast(Value.Clone(), Integer())
            Else
                _fieldWidthsCopy = Nothing
            End If

            _fieldWidths = Value
            _needPropertyCheck = True
        End Set
    End Property

    ''' <summary>
    '''  Indicates whether or not to handle quotes in a csv friendly way
    ''' </summary>
    ''' <value>True if we escape quotes otherwise false</value>
    ''' <remarks></remarks>
    <EditorBrowsable(EditorBrowsableState.Advanced)>
    Public Property HasFieldsEnclosedInQuotes As Boolean
        Get
            Return _hasFieldsEnclosedInQuotes
        End Get
        Set
            _hasFieldsEnclosedInQuotes = Value
        End Set
    End Property

    ''' <summary>
    '''  The line to the right of the cursor.
    ''' </summary>
    ''' <value>The number of the line</value>
    ''' <remarks>LineNumber returns the location in the file and has nothing to do with rows or fields</remarks>
    <EditorBrowsable(EditorBrowsableState.Advanced)>
    Public ReadOnly Property LineNumber As Long
        Get
            If _lineNumber <> -1 Then

                ' See if we're at the end of file
                If _reader.Peek = -1 And _position = _charsRead Then
                    Me.CloseReader()
                End If
            End If

            Return _lineNumber
        End Get
    End Property

    ''' <summary>
    '''  Indicates the type of file being read, either fixed width or delimited
    ''' </summary>
    ''' <value>The type of fields in the file</value>
    ''' <remarks></remarks>
    Public Property TextFieldType As FieldType
        Get
            Return _textFieldType
        End Get
        Set
            ValidateFieldTypeEnumValue(Value, NameOf(Value))
            _textFieldType = Value
            _needPropertyCheck = True
        End Set
    End Property

    ''' <summary>
    '''  Gets or sets the widths of the fields for reading a fixed width file
    ''' </summary>
    ''' <value>An array of the widths</value>
    ''' <remarks></remarks>
    ''' <summary>
    '''  Indicates whether or not leading and trailing white space should be removed when returning a field
    ''' </summary>
    ''' <value>True if white space should be removed, otherwise False</value>
    ''' <remarks></remarks>
    Public Property TrimWhiteSpace As Boolean
        Get
            Return _trimWhiteSpace
        End Get
        Set
            _trimWhiteSpace = Value
        End Set
    End Property

    ''' <summary>
    '''  Gets the index of the first end of line character
    ''' </summary>
    ''' <param name="line"></param>
    ''' <returns></returns>
    ''' <remarks>When there are no end of line characters, the index is the length (one past the end)</remarks>
    Private Shared Function GetEndOfLineIndex(line As String) As Integer

        Debug.Assert(line IsNot Nothing, "We are parsing a Nothing")

        Dim length As Integer = line.Length
        Debug.Assert(length > 0, "A blank line shouldn't be parsed")

        If length = 1 Then
            Debug.Assert(line(0) <> vbCr And line(0) <> vbLf, "A blank line shouldn't be parsed")
            Return length
        End If

        ' Check the next to last and last char for end line characters
        If line(length - 2) = vbCr Or line(length - 2) = vbLf Then
            Return length - 2
        ElseIf line(length - 1) = vbCr Or line(length - 1) = vbLf Then
            Return length - 1
        Else
            Return length
        End If

    End Function

    ''' <summary>
    '''  Returns the given path in long format (v.s 8.3 format) if the path exists.
    ''' </summary>
    ''' <param name="fullPath">The path to resolve to long format.</param>
    ''' <returns>The given path in long format if the path exists.</returns>
    ''' <remarks>
    '''  GetLongPathName is a PInvoke call and requires unmanaged code permission.
    '''  Use DirectoryInfo.GetFiles and GetDirectories (which call FindFirstFile) so that we always have permission.
    '''</remarks>
    Private Shared Function GetLongPath(fullPath As String) As String
        Debug.Assert(Not String.IsNullOrEmpty(fullPath) AndAlso IsPathRooted(fullPath), "Must be full path")
        Try
            ' If root path, return itself. UNC path do not recognize 8.3 format in root path, so this is fine.
            If IsRoot(fullPath) Then
                Return fullPath
            End If

            ' DirectoryInfo.GetFiles and GetDirectories call FindFirstFile which resolves 8.3 path.
            ' Get the DirectoryInfo (user must have code permission or access permission).
            Dim dInfo As New DirectoryInfo(GetParentPath(fullPath))

            If File.Exists(fullPath) Then
                Debug.Assert(dInfo.GetFiles(GetFileName(fullPath)).Length = 1, "Must found exactly 1")
                Return dInfo.GetFiles(GetFileName(fullPath))(0).FullName
            ElseIf Directory.Exists(fullPath) Then
                Debug.Assert(dInfo.GetDirectories(GetFileName(fullPath)).Length = 1,
                                 "Must found exactly 1")
                Return dInfo.GetDirectories(GetFileName(fullPath))(0).FullName
            Else
                Return fullPath ' Path does not exist, cannot resolve.
            End If
        Catch ex As Exception
            ' Ignore these type of exceptions and return FullPath. These type of exceptions should either be caught by calling functions
            ' or indicate that caller does not have enough permission and should get back the 8.3 path.
            If TypeOf ex Is ArgumentException OrElse
                    TypeOf ex Is ArgumentNullException OrElse
                    TypeOf ex Is PathTooLongException OrElse
                    TypeOf ex Is NotSupportedException OrElse
                    TypeOf ex Is DirectoryNotFoundException OrElse
                    TypeOf ex Is SecurityException OrElse
                    TypeOf ex Is UnauthorizedAccessException Then

                Debug.Assert(Not (TypeOf ex Is ArgumentException OrElse
                        TypeOf ex Is ArgumentNullException OrElse
                        TypeOf ex Is PathTooLongException OrElse
                        TypeOf ex Is NotSupportedException), "These exceptions should be caught above")

                Return fullPath
            Else
                Throw
            End If
        End Try
    End Function

    ''' <summary>
    ''' Checks if the full path is a root path.
    ''' </summary>
    ''' <param name="path">The path to check.</param>
    ''' <returns>True if FullPath is a root path, False otherwise.</returns>
    ''' <remarks>
    '''   IO.Path.GetPathRoot: C: -> C:, C:\ -> C:\, \\machine\share -> \\machine\share,
    '''           BUT \\machine\share\ -> \\machine\share (No separator here).
    '''   Therefore, remove any separators at the end to have correct result.
    ''' </remarks>
    Private Shared Function IsRoot(path As String) As Boolean
        ' This function accepts a relative path since GetParentPath will call this,
        ' and GetParentPath accept relative paths.
        If Not IsPathRooted(path) Then
            Return False
        End If

        path = path.TrimEnd(DirectorySeparatorChar, AltDirectorySeparatorChar)
        Return String.Compare(path, GetPathRoot(path),
                    StringComparison.OrdinalIgnoreCase) = 0
    End Function

    ''' <summary>
    ''' Removes all directory separators at the end of a path.
    ''' </summary>
    ''' <param name="path">a full or relative path.</param>
    ''' <returns>If Path is a root path, the same value. Otherwise, removes any directory separators at the end.</returns>
    ''' <remarks>We decided not to return path with separators at the end.</remarks>
    Private Shared Function RemoveEndingSeparator(path As String) As String
        If IsPathRooted(path) Then
            ' If the path is rooted, attempt to check if it is a root path.
            ' Note: IO.Path.GetPathRoot: C: -> C:, C:\ -> C:\, \\myShare\myDir -> \\myShare\myDir
            ' BUT \\myShare\myDir\ -> \\myShare\myDir!!! This function will remove the ending separator of
            ' \\myShare\myDir\ as well. Do not use IsRoot here.
            If path.Equals(GetPathRoot(path), StringComparison.OrdinalIgnoreCase) Then
                Return path
            End If
        End If

        ' Otherwise, remove all separators at the end.
        Return path.TrimEnd(DirectorySeparatorChar, AltDirectorySeparatorChar)
    End Function

    ''' <summary>
    '''  Throws if any of the delimiters contain line end characters
    ''' </summary>
    ''' <param name="delimiterArray">A string array of delimiters</param>
    ''' <remarks></remarks>
    Private Shared Sub ValidateDelimiters(delimiterArray() As String)
        If delimiterArray Is Nothing Then
            Return
        End If
        For Each delimiter As String In delimiterArray
            If String.IsNullOrEmpty(delimiter) Then
                Throw ExceptionUtils.GetArgumentExceptionWithArgName("Delimiters", SR.TextFieldParser_DelimiterNothing, "Delimiters")
            End If
            If delimiter.IndexOfAny(New Char() {Chr(13), Chr(10)}) > -1 Then
                Throw ExceptionUtils.GetArgumentExceptionWithArgName("Delimiters", SR.TextFieldParser_EndCharsInDelimiter)
            End If
        Next
    End Sub

    ''' <summary>
    ''' Validates that the value being passed as an FieldType is a legal value
    ''' </summary>
    ''' <param name="value"></param>
    ''' <remarks></remarks>
    Private Shared Sub ValidateFieldTypeEnumValue(value As FieldType, paramName As String)
        If value < FieldType.Delimited OrElse value > FieldType.FixedWidth Then
            Throw New InvalidEnumArgumentException(paramName, DirectCast(value, Integer), GetType(FieldType))
        End If
    End Sub

    ''' <summary>
    '''  Checks the field widths at input.
    ''' </summary>
    ''' <param name="widths"></param>
    ''' <remarks>
    '''  All field widths, except the last one, must be greater than zero. If the last width is
    '''  less than one it indicates the last field is ragged
    '''</remarks>
    Private Shared Sub ValidateFieldWidthsOnInput(widths() As Integer)

        Debug.Assert(widths IsNot Nothing, "There are no field widths")

        Dim bound As Integer = widths.Length - 1
        For i As Integer = 0 To bound - 1
            If widths(i) < 1 Then
                Throw ExceptionUtils.GetArgumentExceptionWithArgName("FieldWidths", SR.TextFieldParser_FieldWidthsMustPositive, "FieldWidths")
            End If
        Next
    End Sub

    ''' <summary>
    '''  Gets full name and path from passed in path.
    ''' </summary>
    ''' <param name="path">The path to be validated</param>
    ''' <returns>The full name and path</returns>
    ''' <remarks>Throws if the file doesn't exist or if the path is malformed</remarks>
    Private Shared Function ValidatePath(path As String) As String

        ' Validate and get full path
        Dim fullPath As String = NormalizeFilePath(path, NameOf(path))

        ' Make sure the file exists
        If Not File.Exists(fullPath) Then
            Throw New FileNotFoundException(SR.Format(SR.IO_FileNotFound_Path, fullPath))
        End If

        Return fullPath
    End Function

    ''' <summary>
    '''  Gets or sets the delimiters used in a file
    ''' </summary>
    ''' <value>An array of the delimiters</value>
    ''' <remarks></remarks>
    ''' <summary>
    '''  Determines if the FieldWidths or Delimiters arrays have changed.
    ''' </summary>
    ''' <remarks>If the array has changed, we need to re initialize before reading.</remarks>
    Private Function ArrayHasChanged() As Boolean

        Dim lowerBound As Integer
        Dim upperBound As Integer

        Select Case _textFieldType
            Case FieldType.Delimited

                Debug.Assert((_delimitersCopy Is Nothing And _delimiters Is Nothing) Or (_delimitersCopy IsNot Nothing And _delimiters IsNot Nothing), "Delimiters and copy are not both Nothing or both not Nothing")

                ' Check null cases
                If _delimiters Is Nothing Then
                    Return False
                End If

                lowerBound = _delimitersCopy.GetLowerBound(0)
                upperBound = _delimitersCopy.GetUpperBound(0)

                For i As Integer = lowerBound To upperBound
                    If _delimiters(i) <> _delimitersCopy(i) Then
                        Return True
                    End If
                Next i

            Case FieldType.FixedWidth

                Debug.Assert((_fieldWidthsCopy Is Nothing And _fieldWidths Is Nothing) Or (_fieldWidthsCopy IsNot Nothing And _fieldWidths IsNot Nothing), "FieldWidths and copy are not both Nothing or both not Nothing")

                ' Check null cases
                If _fieldWidths Is Nothing Then
                    Return False
                End If

                lowerBound = _fieldWidthsCopy.GetLowerBound(0)
                upperBound = _fieldWidthsCopy.GetUpperBound(0)

                For i As Integer = lowerBound To upperBound
                    If _fieldWidths(i) <> _fieldWidthsCopy(i) Then
                        Return True
                    End If
                Next i

            Case Else
                Debug.Fail("Unknown TextFieldType")
        End Select

        Return False
    End Function

    ''' <summary>
    ''' Checks to see if the passed in character is in any of the delimiters
    ''' </summary>
    ''' <param name="testCharacter">The character to look for</param>
    ''' <returns>True if the character is found in a delimiter, otherwise false</returns>
    ''' <remarks></remarks>
    Private Function CharacterIsInDelimiter(testCharacter As Char) As Boolean

        Debug.Assert(_delimiters IsNot Nothing, "No delimiters set!")

        For Each delimiter As String In _delimiters
            If delimiter.IndexOf(testCharacter) > -1 Then
                Return True
            End If
        Next
        Return False
    End Function

    ''' <summary>
    '''  Throws if any of the comment tokens contain whitespace
    ''' </summary>
    ''' <param name="tokens">A string array of comment tokens</param>
    ''' <remarks></remarks>
    Private Sub CheckCommentTokensForWhitespace(tokens() As String)
        If tokens Is Nothing Then
            Return
        End If
        For Each token As String In tokens
            If _whiteSpaceRegEx.IsMatch(token) Then
                Throw ExceptionUtils.GetArgumentExceptionWithArgName("CommentTokens", SR.TextFieldParser_WhitespaceInToken)
            End If
        Next
    End Sub

    ''' <summary>
    '''  Closes the StreamReader
    ''' </summary>
    ''' <remarks></remarks>
    Private Sub CloseReader()

        Me.FinishReading()
        If _reader IsNot Nothing Then
            If Not _leaveOpen Then
                _reader.Close()
            End If
            _reader = Nothing
        End If
    End Sub

    ''' <summary>
    '''  Cleans up managed resources except the StreamReader and indicates reading is finished
    ''' </summary>
    ''' <remarks></remarks>
    Private Sub FinishReading()

        _lineNumber = -1
        _endOfData = True
        _buffer = Nothing
        _delimiterRegex = Nothing
        _beginQuotesRegex = Nothing

    End Sub

    ''' <summary>
    '''  Returns the field at the passed in index
    ''' </summary>
    ''' <param name="line">The string containing the fields</param>
    ''' <param name="index">The start of the field</param>
    ''' <param name="fieldLength">The length of the field</param>
    ''' <returns>The field</returns>
    ''' <remarks></remarks>
    Private Function GetFixedWidthField(line As StringInfo, index As Integer, fieldLength As Integer) As String

        Dim field As String
        If fieldLength > 0 Then
            field = line.SubstringByTextElements(index, fieldLength)
        Else
            ' Make sure the index isn't past the string
            If index >= line.LengthInTextElements Then
                field = String.Empty
            Else
                field = line.SubstringByTextElements(index).TrimEnd(Chr(13), Chr(10))
            End If
        End If

        If _trimWhiteSpace Then
            Return field.Trim()
        Else
            Return field
        End If
    End Function

    ''' <summary>
    '''  Indicates whether or not the passed in line should be ignored
    ''' </summary>
    ''' <param name="line">The line to be tested</param>
    ''' <returns>True if the line should be ignored, otherwise False</returns>
    ''' <remarks>Lines to ignore are blank lines and comments</remarks>
    Private Function IgnoreLine(line As String) As Boolean

        ' If the Line is Nothing, it has meaning (we've reached the end of the file) so don't
        ' ignore it
        If line Is Nothing Then
            Return False
        End If

        ' Ignore empty or whitespace lines
        Dim trimmedLine As String = line.Trim()
        If trimmedLine.Length = 0 Then
            Return True
        End If

        ' Ignore comments
        If _commentTokens IsNot Nothing Then
            For Each token As String In _commentTokens
                If String.IsNullOrEmpty(token) Then
                    Continue For
                End If

                If trimmedLine.StartsWith(token, StringComparison.Ordinal) Then
                    Return True
                End If

                ' Test original line in case whitespace char is a comment token
                If line.StartsWith(token, StringComparison.Ordinal) Then
                    Return True
                End If
            Next
        End If

        Return False

    End Function

    ''' <summary>
    '''  Increases the size of the buffer. Used when we are at the end of the buffer, we need
    '''  to read more data from the file, and we can't discard what we've already read.
    ''' </summary>
    ''' <returns>The number of characters read to fill the new buffer</returns>
    ''' <remarks>This is needed for PeekChars and EndOfData</remarks>
    Private Function IncreaseBufferSize() As Integer

        Debug.Assert(_buffer IsNot Nothing, "There's no buffer")
        Debug.Assert(_reader IsNot Nothing, "There's no StreamReader")

        ' Set cursor
        _peekPosition = _charsRead

        ' Create a larger buffer and copy our data into it
        Dim bufferSize As Integer = _buffer.Length + DEFAULT_BUFFER_LENGTH

        ' Make sure the buffer hasn't grown too large
        If bufferSize > _maxBufferSize Then
            Throw ExceptionUtils.GetInvalidOperationException(SR.TextFieldParser_BufferExceededMaxSize)
        End If

        Dim tempArray(bufferSize - 1) As Char

        Array.Copy(_buffer, tempArray, _buffer.Length)
        Dim charsRead As Integer = _reader.Read(tempArray, _buffer.Length, DEFAULT_BUFFER_LENGTH)
        _buffer = tempArray
        _charsRead += charsRead

        Debug.Assert(_charsRead <= bufferSize, "We've read more chars than we have space for")

        Return charsRead
    End Function

    ''' <summary>
    '''  Creates a StreamReader for the passed in Path
    ''' </summary>
    ''' <param name="path">The passed in path</param>
    ''' <param name="defaultEncoding">The encoding to default to if encoding can't be detected</param>
    ''' <param name="detectEncoding">Indicates whether or not to detect encoding from the BOM</param>
    ''' <remarks>We validate the arguments here for the three Public constructors that take a Path</remarks>
    Private Sub InitializeFromPath(path As String, defaultEncoding As Encoding, detectEncoding As Boolean)

        If String.IsNullOrEmpty(path) Then
            Throw New ArgumentNullException(NameOf(path))
        End If

        If defaultEncoding Is Nothing Then
            Throw New ArgumentNullException(NameOf(defaultEncoding))
        End If

        Dim fullPath As String = ValidatePath(path)
        Dim fileStreamTemp As New FileStream(fullPath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        _reader = New StreamReader(fileStreamTemp, defaultEncoding, detectEncoding)

        Me.ReadToBuffer()

    End Sub

    ''' <summary>
    '''  Creates a StreamReader for a passed in stream
    ''' </summary>
    ''' <param name="stream">The passed in stream</param>
    ''' <param name="defaultEncoding">The encoding to default to if encoding can't be detected</param>
    ''' <param name="detectEncoding">Indicates whether or not to detect encoding from the BOM</param>
    ''' <remarks>We validate the arguments here for the three Public constructors that take a Stream</remarks>
    Private Sub InitializeFromStream(stream As Stream, defaultEncoding As Encoding, detectEncoding As Boolean)

        If stream Is Nothing Then
            Throw New ArgumentNullException(NameOf(stream))
        End If

        If Not stream.CanRead Then
            Throw ExceptionUtils.GetArgumentExceptionWithArgName("stream", SR.TextFieldParser_StreamNotReadable, "stream")
        End If

        If defaultEncoding Is Nothing Then
            Throw New ArgumentNullException(NameOf(defaultEncoding))
        End If

        _reader = New StreamReader(stream, defaultEncoding, detectEncoding)

        Me.ReadToBuffer()
    End Sub

    ''' <summary>
    '''  Gets the next data line and parses it with the delimiters
    ''' </summary>
    ''' <returns>An array of the fields in the line</returns>
    ''' <remarks></remarks>
    Private Function ParseDelimitedLine() As String()

        Dim line As String = Me.ReadNextDataLine()
        If line Is Nothing Then
            Return Nothing
        End If

        ' The line number is that of the line just read
        Dim currentLineNumber As Long = _lineNumber - 1

        Dim index As Integer = 0
        Dim fields As New List(Of String)
        Dim field As String
        Dim lineEndIndex As Integer = GetEndOfLineIndex(line)

        While index <= lineEndIndex

            ' Is the field delimited in quotes? We only care about this if
            ' EscapedQuotes is True
            Dim matchResult As Match = Nothing
            Dim quoteDelimited As Boolean = False

            If _hasFieldsEnclosedInQuotes Then
                matchResult = Me.BeginQuotesRegex.Match(line, index)
                quoteDelimited = matchResult.Success
            End If

            If quoteDelimited Then

                'Move the Index beyond quote
                index = matchResult.Index + matchResult.Length
                ' Look for the closing "
                Dim endHelper As New QuoteDelimitedFieldBuilder(_delimiterWithEndCharsRegex, _spaceChars)
                endHelper.BuildField(line, index)

                If endHelper.MalformedLine Then
                    _errorLine = line.TrimEnd(Chr(13), Chr(10))
                    _errorLineNumber = currentLineNumber
                    Throw New MalformedLineException(SR.Format(SR.TextFieldParser_MalFormedDelimitedLine, currentLineNumber.ToString(CultureInfo.InvariantCulture)), currentLineNumber)
                End If

                If endHelper.FieldFinished Then
                    field = endHelper.Field
                    index = endHelper.Index + endHelper.DelimiterLength
                Else
                    ' We may have an embedded line end character, so grab next line
                    Dim newLine As String
                    Dim endOfLine As Integer

                    Do
                        endOfLine = line.Length
                        ' Get the next data line
                        newLine = Me.ReadNextDataLine()

                        ' If we didn't get a new line, we're at the end of the file so our original line is malformed
                        If newLine Is Nothing Then
                            _errorLine = line.TrimEnd(Chr(13), Chr(10))
                            _errorLineNumber = currentLineNumber
                            Throw New MalformedLineException(SR.Format(SR.TextFieldParser_MalFormedDelimitedLine, currentLineNumber.ToString(CultureInfo.InvariantCulture)), currentLineNumber)
                        End If

                        If line.Length + newLine.Length > _maxLineSize Then
                            _errorLine = line.TrimEnd(Chr(13), Chr(10))
                            _errorLineNumber = currentLineNumber
                            Throw New MalformedLineException(SR.Format(SR.TextFieldParser_MaxLineSizeExceeded, currentLineNumber.ToString(CultureInfo.InvariantCulture)), currentLineNumber)
                        End If

                        line &= newLine
                        lineEndIndex = GetEndOfLineIndex(line)
                        endHelper.BuildField(line, endOfLine)
                        If endHelper.MalformedLine Then
                            _errorLine = line.TrimEnd(Chr(13), Chr(10))
                            _errorLineNumber = currentLineNumber
                            Throw New MalformedLineException(SR.Format(SR.TextFieldParser_MalFormedDelimitedLine, currentLineNumber.ToString(CultureInfo.InvariantCulture)), currentLineNumber)
                        End If
                    Loop Until endHelper.FieldFinished

                    field = endHelper.Field
                    index = endHelper.Index + endHelper.DelimiterLength
                End If

                If _trimWhiteSpace Then
                    field = field.Trim()
                End If

                fields.Add(field)
            Else
                ' Find the next delimiter
                Dim delimiterMatch As Match = _delimiterRegex.Match(line, index)
                If delimiterMatch.Success Then
                    field = line.Substring(index, delimiterMatch.Index - index)

                    If _trimWhiteSpace Then
                        field = field.Trim()
                    End If

                    fields.Add(field)

                    ' Move the index
                    index = delimiterMatch.Index + delimiterMatch.Length
                Else
                    ' We're at the end of the line so the field consists of all that's left of the line
                    ' minus the end of line chars
                    field = line.Substring(index).TrimEnd(Chr(13), Chr(10))

                    If _trimWhiteSpace Then
                        field = field.Trim()
                    End If
                    fields.Add(field)
                    Exit While
                End If

            End If
        End While

        Return fields.ToArray()

    End Function

    ''' <summary>
    '''  Gets the next data line and parses into fixed width fields
    ''' </summary>
    ''' <returns>An array of the fields in the line</returns>
    ''' <remarks></remarks>
    Private Function ParseFixedWidthLine() As String()

        Debug.Assert(_fieldWidths IsNot Nothing, "No field widths")

        Dim line As String = Me.ReadNextDataLine()

        If line Is Nothing Then
            Return Nothing
        End If

        ' Strip off trailing carriage return or line feed
        line = line.TrimEnd(Chr(13), Chr(10))

        Dim lineInfo As New StringInfo(line)
        Me.ValidateFixedWidthLine(lineInfo, _lineNumber - 1)

        Dim index As Integer = 0
        Dim bound As Integer = _fieldWidths.Length - 1
        Dim fields(bound) As String

        For i As Integer = 0 To bound
            fields(i) = Me.GetFixedWidthField(lineInfo, index, _fieldWidths(i))
            index += _fieldWidths(i)
        Next

        Return fields
    End Function

    ''' <summary>
    '''  Returns the next data line but doesn't move the cursor
    ''' </summary>
    ''' <returns>The next data line, or Nothing if there's no more data</returns>
    ''' <remarks></remarks>
    Private Function PeekNextDataLine() As String

        Dim line As String

        ' Set function to use when we reach the end of the buffer
        Dim bufferFunction As New ChangeBufferFunction(AddressOf Me.IncreaseBufferSize)

        ' Slide the data to the left so that we make maximum use of the buffer
        Me.SlideCursorToStartOfBuffer()
        _peekPosition = 0

        Do
            line = Me.ReadNextLine(_peekPosition, bufferFunction)
        Loop While Me.IgnoreLine(line)

        Return line
    End Function

    ''' <summary>
    '''  Returns the next line of data or nothing if there's no more data to be read
    ''' </summary>
    ''' <returns>The next line of data</returns>
    ''' <remarks>Moves the cursor past the line read</remarks>
    Private Function ReadNextDataLine() As String

        Dim line As String

        ' Set function to use when we reach the end of the buffer
        Dim bufferFunction As New ChangeBufferFunction(AddressOf Me.ReadToBuffer)

        Do
            line = Me.ReadNextLine(_position, bufferFunction)
            _lineNumber += 1
        Loop While Me.IgnoreLine(line)

        If line Is Nothing Then
            Me.CloseReader()
        End If

        Return line

    End Function

    ''' <summary>
    '''  Gets the next line from the file and moves the passed in cursor past the line
    ''' </summary>
    ''' <param name="cursor">Indicates the current position in the buffer</param>
    ''' <param name="changeBuffer">Function to call when we've reached the end of the buffer</param>
    ''' <returns>The next line in the file</returns>
    ''' <remarks>Returns Nothing if we are at the end of the file</remarks>
    Private Function ReadNextLine(ByRef cursor As Integer, changeBuffer As ChangeBufferFunction) As String

        Debug.Assert(_buffer IsNot Nothing, "There's no buffer")
        Debug.Assert(cursor >= 0 And cursor <= _charsRead, "The cursor is out of range")

        ' Check to see if the cursor is at the end of the chars in the buffer. If it is, re fill the buffer
        If cursor = _charsRead Then
            If changeBuffer() = 0 Then

                ' We're at the end of the file
                Return Nothing
            End If
        End If

        Dim builder As StringBuilder = Nothing
        Do
            ' Walk through buffer looking for the end of a line. End of line can be vbLf (\n), vbCr (\r) or vbCrLf (\r\n)
            For i As Integer = cursor To _charsRead - 1

                Dim character As Char = _buffer(i)
                If character = vbCr Or character = vbLf Then

                    ' We've found the end of a line so add everything we've read so far to the
                    ' builder. We include the end of line char because we need to know what it is
                    ' in case it's embedded in a field.
                    If builder IsNot Nothing Then
                        builder.Append(_buffer, cursor, i - cursor + 1)
                    Else
                        builder = New StringBuilder(i + 1)
                        builder.Append(_buffer, cursor, i - cursor + 1)
                    End If
                    cursor = i + 1

                    ' See if vbLf should be added as well
                    If character = vbCr Then
                        If cursor < _charsRead Then
                            If _buffer(cursor) = vbLf Then
                                cursor += 1
                                builder.Append(CChar(vbLf))
                            End If
                        ElseIf changeBuffer() > 0 Then
                            If _buffer(cursor) = vbLf Then
                                cursor += 1
                                builder.Append(CChar(vbLf))
                            End If
                        End If
                    End If

                    Return builder.ToString()
                End If
            Next i

            ' We've searched the whole buffer and haven't found an end of line. Save what we have, and read more to the buffer.
            Dim size As Integer = _charsRead - cursor

            If builder Is Nothing Then
                builder = New StringBuilder(size + DEFAULT_BUILDER_INCREASE)
            End If
            builder.Append(_buffer, cursor, size)

        Loop While changeBuffer() > 0

        Return builder.ToString()
    End Function

    ''' <summary>
    '''  Reads characters from the file into the buffer
    ''' </summary>
    ''' <returns>The number of Chars read. If no Chars are read, we're at the end of the file</returns>
    ''' <remarks></remarks>
    Private Function ReadToBuffer() As Integer

        Debug.Assert(_buffer IsNot Nothing, "There's no buffer")
        Debug.Assert(_reader IsNot Nothing, "There's no StreamReader")

        ' Set cursor to beginning of buffer
        _position = 0
        Dim bufferLength As Integer = _buffer.Length
        Debug.Assert(bufferLength >= DEFAULT_BUFFER_LENGTH, "Buffer shrunk to below default")

        ' If the buffer has grown, shrink it back to the default size
        If bufferLength > DEFAULT_BUFFER_LENGTH Then
            bufferLength = DEFAULT_BUFFER_LENGTH
            ReDim _buffer(bufferLength - 1)
        End If

        ' Read from the stream
        _charsRead = _reader.Read(_buffer, 0, bufferLength)

        ' Return the number of Chars read
        Return _charsRead
    End Function

    ''' <summary>
    '''  Moves the cursor and all the data to the right of the cursor to the front of the buffer. It
    '''  then fills the remainder of the buffer from the file
    ''' </summary>
    ''' <returns>The number of Chars read in filling the remainder of the buffer</returns>
    ''' <remarks>
    '''  This should be called when we want to make maximum use of the space in the buffer. Characters
    '''  to the left of the cursor have already been read and can be discarded.
    '''</remarks>
' ReSharper disable once UnusedMethodReturnValue.Local
    Private Function SlideCursorToStartOfBuffer() As Integer

        Debug.Assert(_buffer IsNot Nothing, "There's no buffer")
        Debug.Assert(_reader IsNot Nothing, "There's no StreamReader")
        Debug.Assert(_position >= 0 And _position <= _buffer.Length, "The cursor is out of range")

        ' No need to slide if we're already at the beginning
        If _position > 0 Then
            Dim bufferLength As Integer = _buffer.Length
            Dim tempArray(bufferLength - 1) As Char
            Array.Copy(_buffer, _position, tempArray, 0, bufferLength - _position)

            ' Fill the rest of the buffer
            Dim charsRead As Integer = _reader.Read(tempArray, bufferLength - _position, _position)
            _charsRead = _charsRead - _position + charsRead

            _position = 0
            _buffer = tempArray

            Return charsRead
        End If

        Return 0
    End Function

    ''' <summary>
    '''  Validates the delimiters and creates the Regex objects for finding delimiters or quotes followed
    '''  by delimiters
    ''' </summary>
    ''' <remarks></remarks>
    Private Sub ValidateAndEscapeDelimiters()
        If _delimiters Is Nothing Then
            Throw ExceptionUtils.GetArgumentExceptionWithArgName("Delimiters", SR.TextFieldParser_DelimiterNothing, "Delimiters")
        End If

        If _delimiters.Length = 0 Then
            Throw ExceptionUtils.GetArgumentExceptionWithArgName("Delimiters", SR.TextFieldParser_DelimiterNothing, "Delimiters")
        End If

        Dim length As Integer = _delimiters.Length

        Dim builder As StringBuilder = New StringBuilder()
        Dim quoteBuilder As StringBuilder = New StringBuilder()

        ' Add ending quote pattern. It will be followed by delimiters resulting in a string like:
        ' "[ ]*(d1|d2|d3)
        quoteBuilder.Append(Me.EndQuotePattern & "(")
        For i As Integer = 0 To length - 1
            If _delimiters(i) IsNot Nothing Then

                ' Make sure delimiter is legal
                If _hasFieldsEnclosedInQuotes Then
                    If _delimiters(i).IndexOf(""""c) > -1 Then
                        Throw ExceptionUtils.GetInvalidOperationException(SR.TextFieldParser_IllegalDelimiter)
                    End If
                End If

                Dim escapedDelimiter As String = Regex.Escape(_delimiters(i))

                builder.Append(escapedDelimiter & "|")
                quoteBuilder.Append(escapedDelimiter & "|")
            Else
                Debug.Fail("Delimiter element is empty. This should have been caught on input")
            End If
        Next

        _spaceChars = Me.WhitespaceCharacters

        ' Get rid of trailing | and set regex
        _delimiterRegex = New Regex(builder.ToString(0, builder.Length - 1), REGEX_OPTIONS)
        builder.Append(vbCr & "|" & vbLf)
        _delimiterWithEndCharsRegex = New Regex(builder.ToString(), REGEX_OPTIONS)

        ' Add end of line (either Cr, Ln, or nothing) and set regex
        quoteBuilder.Append(vbCr & "|" & vbLf & ")|""$")
    End Sub

    ''' <summary>
    '''  Determines whether or not the field widths are valid, and sets the size of a line
    ''' </summary>
    ''' <remarks></remarks>
    Private Sub ValidateFieldWidths()

        If _fieldWidths Is Nothing Then
            Throw ExceptionUtils.GetInvalidOperationException(SR.TextFieldParser_FieldWidthsNothing)
        End If

        If _fieldWidths.Length = 0 Then
            Throw ExceptionUtils.GetInvalidOperationException(SR.TextFieldParser_FieldWidthsNothing)
        End If

        Dim widthBound As Integer = _fieldWidths.Length - 1
        _lineLength = 0

        ' add all but the last element
        For i As Integer = 0 To widthBound - 1
            Debug.Assert(_fieldWidths(i) > 0, "Bad field width, this should have been caught on input")

            _lineLength += _fieldWidths(i)
        Next

        ' add the last field if it's greater than zero (IE not ragged).
        If _fieldWidths(widthBound) > 0 Then
            _lineLength += _fieldWidths(widthBound)
        End If

    End Sub

    ''' <summary>
    '''  Indicates whether or not a line is valid
    ''' </summary>
    ''' <param name="line">The line to be tested</param>
    ''' <param name="lineNum">The line number, used for exception</param>
    ''' <remarks></remarks>
    Private Sub ValidateFixedWidthLine(line As StringInfo, lineNum As Long)
        Debug.Assert(line IsNot Nothing, "No Line sent")

        ' The only malformed line for fixed length fields is one that's too short
        If line.LengthInTextElements < _lineLength Then
            _errorLine = line.String
            _errorLineNumber = _lineNumber - 1
            Throw New MalformedLineException(SR.Format(SR.TextFieldParser_MalFormedFixedWidthLine, lineNum.ToString(CultureInfo.InvariantCulture)), lineNum)
        End If

    End Sub

    ''' <summary>
    '''  Checks property settings to ensure we're able to read fields.
    ''' </summary>
    ''' <remarks>Throws if we're not able to read fields with current property settings</remarks>
    Private Sub ValidateReadyToRead()

        If _needPropertyCheck Or Me.ArrayHasChanged() Then
            Select Case _textFieldType
                Case FieldType.Delimited

                    Me.ValidateAndEscapeDelimiters()
                Case FieldType.FixedWidth

                    ' Check FieldWidths
                    Me.ValidateFieldWidths()

                Case Else
                    Debug.Fail("Unknown TextFieldType")
            End Select

            ' Check Comment Tokens
            If _commentTokens IsNot Nothing Then
                For Each token As String In _commentTokens
                    If Not String.IsNullOrEmpty(token) Then
                        If _hasFieldsEnclosedInQuotes And _textFieldType = FieldType.Delimited Then

                            If String.Compare(token.Trim(), """", StringComparison.Ordinal) = 0 Then
                                Throw ExceptionUtils.GetInvalidOperationException(SR.TextFieldParser_InvalidComment)
                            End If
                        End If
                    End If
                Next
            End If

            _needPropertyCheck = False
        End If
    End Sub

    ''' <summary>
    ''' Standard implementation of IDisposable.Dispose for non sealed classes. Classes derived from
    ''' TextFieldParser should override this method. After doing their own cleanup, they should call
    ''' this method (MyBase.Dispose(disposing))
    ''' </summary>
    ''' <param name="disposing">Indicates we are called by Dispose and not GC</param>
    ''' <remarks></remarks>
    Protected Overridable Sub Dispose(disposing As Boolean)
        If disposing Then
            If Not _disposed Then
                Me.Close()
            End If
            _disposed = True
        End If
    End Sub

    ''' <summary>
    ''' Throw ArgumentException if the file path ends with a separator.
    ''' </summary>
    ''' <param name="path">The file path.</param>
    ''' <param name="paramName">The parameter name to include in ArgumentException.</param>
    Friend Shared Sub CheckFilePathTrailingSeparator(path As String, paramName As String)
        If String.IsNullOrEmpty(path) Then ' Check for argument null
            Throw ExceptionUtils.GetArgumentNullException(paramName)
        End If
        If path.EndsWith(DirectorySeparatorChar, StringComparison.Ordinal) Or
                path.EndsWith(AltDirectorySeparatorChar, StringComparison.Ordinal) Then
            Throw ExceptionUtils.GetArgumentExceptionWithArgName(paramName, SR.IO_FilePathException)
        End If
    End Sub

    ''' <summary>
    ''' Normalize the path, but throw exception if the path ends with separator.
    ''' </summary>
    ''' <param name="path">The input path.</param>
    ''' <param name="paramName">The parameter name to include in the exception if one is raised.</param>
    ''' <returns>The normalized path.</returns>
    Friend Shared Function NormalizeFilePath(path As String, paramName As String) As String
        CheckFilePathTrailingSeparator(path, paramName)
        Return NormalizePath(path)
    End Function

    ''' <summary>
    ''' Get full path, get long format, and remove any pending separator.
    ''' </summary>
    ''' <param name="path">The path to be normalized.</param>
    ''' <returns>The normalized path.</returns>
    ''' <exception cref="Path.GetFullPath">See IO.Path.GetFullPath for possible exceptions.</exception>
    ''' <remarks>Keep this function since we might change the implementation / behavior later.</remarks>
    Friend Shared Function NormalizePath(path As String) As String
        Return GetLongPath(RemoveEndingSeparator(GetFullPath(path)))
    End Function

    ''' <summary>
    ''' Returns the parent directory's path from a specified path.
    ''' </summary>
    ''' <param name="path">The path to a file or directory, this can be absolute or relative.</param>
    ''' <returns>
    ''' The path to the parent directory of that file or directory (whether absolute or relative depends on the input),
    ''' or an empty string if Path is a root directory.
    ''' </returns>
    ''' <exception cref="Path.GetFullPath">See IO.Path.GetFullPath: If path is an invalid path.</exception>
    ''' <remarks>
    ''' The path will be normalized (for example: C:\Dir1////\\\Dir2 will become C:\Dir1\Dir2)
    ''' but will not be resolved (for example: C:\Dir1\Dir2\..\Dir3 WILL NOT become C:\Dir1\Dir3). Use CombinePath.
    ''' </remarks>
    Public Shared Function GetParentPath(path As String) As String
        ' Call IO.Path.GetFullPath to handle exception cases. Don't use the full path returned.
        ' ReSharper disable once ReturnValueOfPureMethodIsNotUsed
        GetFullPath(path)

        If IsRoot(path) Then
            Throw ExceptionUtils.GetArgumentExceptionWithArgName("path", SR.IO_GetParentPathIsRoot_Path, path)
        Else
            Return GetDirectoryName(path.TrimEnd(
                    DirectorySeparatorChar, AltDirectorySeparatorChar))
        End If
    End Function

    ''' <summary>
    '''  Closes the StreamReader
    ''' </summary>
    ''' <remarks></remarks>
    Public Sub Close()
        Me.CloseReader()
    End Sub

    ''' <summary>
    '''  Closes the StreamReader
    ''' </summary>
    ''' <remarks></remarks>
    Public Sub Dispose() Implements IDisposable.Dispose
        Me.Dispose(True)
        GC.SuppressFinalize(Me)
    End Sub

    ''' <summary>
    '''  Enables looking at the passed in number of characters of the next data line without reading the line
    ''' </summary>
    ''' <param name="numberOfChars"></param>
    ''' <returns>A string consisting of the first NumberOfChars characters of the next line</returns>
    ''' <remarks>If numberOfChars is greater than the next line, only the next line is returned</remarks>
    Public Function PeekChars(numberOfChars As Integer) As String

        If numberOfChars <= 0 Then
            Throw ExceptionUtils.GetArgumentExceptionWithArgName("numberOfChars", SR.TextFieldParser_NumberOfCharsMustBePositive, "numberOfChars")
        End If

        If _reader Is Nothing Or _buffer Is Nothing Then
            Return Nothing
        End If

        ' If we know there's no more data return Nothing
        If _endOfData Then
            Return Nothing
        End If

        ' Get the next line without reading it
        Dim line As String = Me.PeekNextDataLine()

        If line Is Nothing Then
            _endOfData = True
            Return Nothing
        End If

        ' Strip of end of line chars
        line = line.TrimEnd(Chr(13), Chr(10))

        ' If the number of chars is larger than the line, return the whole line. Otherwise
        ' return the NumberOfChars characters from the beginning of the line
        If line.Length < numberOfChars Then
            Return line
        Else
            Dim info As New StringInfo(line)
            Return info.SubstringByTextElements(0, numberOfChars)
        End If

    End Function

    ''' <summary>
    '''  Reads a non ignorable line and parses it into fields
    ''' </summary>
    ''' <returns>The line parsed into fields</returns>
    ''' <remarks>This is a data aware method. Comments and blank lines are ignored.</remarks>
    Public Function ReadFields() As String()
        If _reader Is Nothing Or _buffer Is Nothing Then
            Return Nothing
        End If

        Me.ValidateReadyToRead()

        Select Case _textFieldType
            Case FieldType.FixedWidth
                Return Me.ParseFixedWidthLine()
            Case FieldType.Delimited
                Return Me.ParseDelimitedLine()
            Case Else
                Debug.Fail("The TextFieldType is not supported")
        End Select
        Return Nothing
    End Function

    ''' <summary>
    '''  Reads and returns the next line from the file
    ''' </summary>
    ''' <returns>The line read or Nothing if at the end of the file</returns>
    ''' <remarks>This is data unaware method. It simply reads the next line in the file.</remarks>
    <EditorBrowsable(EditorBrowsableState.Advanced)>
    Public Function ReadLine() As String
        If _reader Is Nothing Or _buffer Is Nothing Then
            Return Nothing
        End If

        Dim line As String

        ' Set the method to be used when we reach the end of the buffer
        Dim bufferFunction As New ChangeBufferFunction(AddressOf Me.ReadToBuffer)

        line = Me.ReadNextLine(_position, bufferFunction)

        If line Is Nothing Then
            Me.FinishReading()
            Return Nothing
        Else
            _lineNumber += 1
            Return line.TrimEnd(Chr(13), Chr(10))
        End If
    End Function

    ''' <summary>
    '''  Reads the file starting at the current position and moving to the end of the file
    ''' </summary>
    ''' <returns>The contents of the file from the current position to the end of the file</returns>
    ''' <remarks>This is not a data aware method. Everything in the file from the current position to the end is read</remarks>
    <EditorBrowsable(EditorBrowsableState.Advanced)>
    Public Function ReadToEnd() As String

        If _reader Is Nothing Or _buffer Is Nothing Then
            Return Nothing
        End If

        Dim builder As New StringBuilder(_buffer.Length)

        ' Get the lines in the Buffer first
        builder.Append(_buffer, _position, _charsRead - _position)

        ' Add what we haven't read
        builder.Append(_reader.ReadToEnd())

        Me.FinishReading()

        Return builder.ToString()

    End Function

    ''' <summary>
    ''' Helper function to enable setting delimiters without having to Dim an array
    ''' </summary>
    ''' <param name="delimiter">A list of the delimiters</param>
    ''' <remarks></remarks>
    Public Sub SetDelimiters(ParamArray delimiter As String())
        Me.Delimiters = delimiter
    End Sub

    ''' <summary>
    ''' Helper function to enable setting field widths without having to Dim an array
    ''' </summary>
    ''' <param name="widths">A list of field widths</param>
    ''' <remarks></remarks>
    Public Sub SetFieldWidths(ParamArray widths As Integer())
        Me.FieldWidths = widths
    End Sub

End Class
