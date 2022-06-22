' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.ComponentModel


<DefaultProperty("ParentRichTextBox")>
Partial Public Class LineNumbersForRichTextBox : Inherits Control

    Private WithEvents ZParent As RichTextBox = Nothing

    Private WithEvents ZTimer As New Timer

    Private ReadOnly _zLineNunmberItems As New List(Of LineNumberItem)

    Private _zAutoSizing As Boolean = True

    Private _zAutoSizingSize As New Size(0, 0)

    Private _zBorderLinesColor As Color = Color.SlateGray

    Private _zBorderLinesShow As Boolean = True

    Private _zBorderLinesStyle As Drawing2D.DashStyle = Drawing2D.DashStyle.Dot

    Private _zBorderLinesThickness As Single = 1

    Private _zContentRectangle As Rectangle = Nothing

    Private _zDockSide As LineNumberDockSides = LineNumberDockSides.Left

    Private _zGradientDirection As Drawing2D.LinearGradientMode = Drawing2D.LinearGradientMode.Horizontal

    Private _zGradientEndColor As Color = Color.LightSteelBlue

    Private _zGradientShow As Boolean = True

    Private _zGradientStartColor As Color = Color.FromArgb(0, 0, 0, 0)

    Private _zGridLinesColor As Color = Color.SlateGray

    Private _zGridLinesShow As Boolean = True

    Private _zGridLinesStyle As Drawing2D.DashStyle = Drawing2D.DashStyle.Dot

    Private _zGridLinesThickness As Single = 1

    Private _zLineNumbersAlignment As ContentAlignment = ContentAlignment.TopRight

    Private _zLineNumbersAntiAlias As Boolean = True

    Private _zLineNumbersClipByItemRectangle As Boolean = True

    Private _zLineNumbersFormat As String = "0"

    Private _zLineNumbersOffset As New Size(0, 0)

    Private _zLineNumbersShow As Boolean = True

    Private _zLineNumbersShowAsHexadecimal As Boolean

    Private _zLineNumbersShowLeadingZeroes As Boolean = True

    Private _zMarginLinesColor As Color = Color.SlateGray

    Private _zMarginLinesShow As Boolean = True

    Private _zMarginLinesSide As LineNumberDockSides = LineNumberDockSides.Right

    Private _zMarginLinesStyle As Drawing2D.DashStyle = Drawing2D.DashStyle.Solid

    Private _zMarginLinesThickness As Single = 1

    Private _zParentInMe As Integer

    Private _zParentIsScrolling As Boolean

    Private _zPointInMe As New Point(0, 0)

    Private _zPointInParent As New Point(0, 0)

    Private _zSeeThroughMode As Boolean

    Public Sub New()
        Me.InitializeComponent()

        With Me
            .SetStyle(ControlStyles.OptimizedDoubleBuffer, True)
            .SetStyle(ControlStyles.ResizeRedraw, True)
            .SetStyle(ControlStyles.SupportsTransparentBackColor, True)
            .SetStyle(ControlStyles.UserPaint, True)
            .SetStyle(ControlStyles.AllPaintingInWmPaint, True)
            .Margin = New Padding(0)
            .Padding = New Padding(0, 0, 2, 0)
        End With
        With Me.ZTimer
            .Enabled = True
            .Interval = 200
            .Stop()
        End With
        Me.Update_SizeAndPosition()
        Me.Invalidate()
    End Sub

    <Flags>
    Public Enum LineNumberDockSides
        None = 0
        Left = 1
        Right = 2
        Height = 4
    End Enum

    <Description("Use this property to automatically resize the control (and reposition it if needed).")>
    <Category("Additional Behavior")>
    <DefaultValue(True)>
    Public Property AutoSizing As Boolean
        Get
            Return _zAutoSizing
        End Get
        Set
            _zAutoSizing = Value
            Me.Refresh()
            Me.Invalidate()
        End Set
    End Property

    <Category("Additional Appearance")>
    Public Property BackgroundGradientAlphaColor As Color
        Get
            Return _zGradientStartColor
        End Get
        Set
            _zGradientStartColor = Value
            Me.Invalidate()
        End Set
    End Property

    <Category("Additional Appearance")>
    <DefaultValue(GetType(Color), "LightSteelBlue")>
    Public Property BackgroundGradientBetaColor As Color
        Get
            Return _zGradientEndColor
        End Get
        Set
            _zGradientEndColor = Value
            Me.Invalidate()
        End Set
    End Property

    <Category("Additional Appearance")>
    <DefaultValue(Drawing2D.LinearGradientMode.Horizontal)>
    Public Property BackgroundGradientDirection As Drawing2D.LinearGradientMode
        Get
            Return _zGradientDirection
        End Get
        Set
            _zGradientDirection = Value
            Me.Invalidate()
        End Set
    End Property

    <Category("Additional Appearance")>
    <DefaultValue(GetType(Color), "SlateGray")>
    Public Property BorderLinesColor As Color
        Get
            Return _zBorderLinesColor
        End Get
        Set
            _zBorderLinesColor = Value
            Me.Invalidate()
        End Set
    End Property

    <Category("Additional Appearance")>
    <DefaultValue(Drawing2D.DashStyle.Dot)>
    Public Property BorderLinesStyle As Drawing2D.DashStyle
        Get
            Return _zBorderLinesStyle
        End Get
        Set
            If Value = Drawing2D.DashStyle.Custom Then
                Value = Drawing2D.DashStyle.Solid
            End If
            _zBorderLinesStyle = Value
            Me.Invalidate()
        End Set
    End Property

    <Category("Additional Appearance")>
    <DefaultValue(1)>
    Public Property BorderLinesThickness As Single
        Get
            Return _zBorderLinesThickness
        End Get
        Set
            _zBorderLinesThickness = Math.Max(1, Math.Min(255, Value))
            Me.Invalidate()
        End Set
    End Property

    <Description("Use this property to dock the LineNumbers to a chosen side of the chosen RichTextBox.")>
    <Category("Additional Behavior")>
    <DefaultValue(LineNumberDockSides.Left)>
    Public Property DockSide As LineNumberDockSides
        Get
            Return _zDockSide
        End Get
        Set
            _zDockSide = Value
            Me.Refresh()
            Me.Invalidate()
        End Set
    End Property

    <Category("Additional Appearance")>
    <DefaultValue(GetType(Color), "SlateGray")>
    Public Property GridLinesColor As Color
        Get
            Return _zGridLinesColor
        End Get
        Set
            _zGridLinesColor = Value
            Me.Invalidate()
        End Set
    End Property

    <Category("Additional Appearance")>
    <DefaultValue(Drawing2D.DashStyle.Dot)>
    Public Property GridLinesStyle As Drawing2D.DashStyle
        Get
            Return _zGridLinesStyle
        End Get
        Set
            If Value = Drawing2D.DashStyle.Custom Then
                Value = Drawing2D.DashStyle.Solid
            End If
            _zGridLinesStyle = Value
            Me.Invalidate()
        End Set
    End Property

    <Category("Additional Appearance")>
    <DefaultValue(1)>
    Public Property GridLinesThickness As Single
        Get
            Return _zGridLinesThickness
        End Get
        Set
            _zGridLinesThickness = Math.Max(1, Math.Min(255, Value))
            Me.Invalidate()
        End Set
    End Property

    <Description("Use this to align the LineNumbers to a chosen corner (or center) within their item-area.")>
    <Category("Additional Behavior")>
    <DefaultValue(ContentAlignment.TopRight)>
    Public Property LineNrsAlignment As ContentAlignment
        Get
            Return _zLineNumbersAlignment
        End Get
        Set
            _zLineNumbersAlignment = Value
            Me.Invalidate()
        End Set
    End Property

    <Description("Use this to apply Anti-Aliasing to the LineNumbers (high quality). Some fonts will look better without it, though.")>
    <Category("Additional Behavior")>
    <DefaultValue(True)>
    Public Property LineNrsAntiAlias As Boolean
        Get
            Return _zLineNumbersAntiAlias
        End Get
        Set
            _zLineNumbersAntiAlias = Value
            Me.Refresh()
            Me.Invalidate()
        End Set
    End Property

    <Description("Use this to set whether the LineNumbers should be shown as hexadecimal values.")>
    <Category("Additional Behavior")>
    <DefaultValue(False)>
    Public Property LineNrsAsHexadecimal As Boolean
        Get
            Return _zLineNumbersShowAsHexadecimal
        End Get
        Set
            _zLineNumbersShowAsHexadecimal = Value
            Me.Refresh()
            Me.Invalidate()
        End Set
    End Property

    <Description("Use this to set whether the LineNumbers are allowed to spill out of their item-area, or should be clipped by it.")>
    <Category("Additional Behavior")>
    <DefaultValue(True)>
    Public Property LineNrsClippedByItemRectangle As Boolean
        Get
            Return _zLineNumbersClipByItemRectangle
        End Get
        Set
            _zLineNumbersClipByItemRectangle = Value
            Me.Invalidate()
        End Set
    End Property

    <Description("Use this to set whether the LineNumbers should have leading zeros (based on the total amount of text lines).")>
    <Category("Additional Behavior")>
    <DefaultValue(True)>
    Public Property LineNrsLeadingZeroes As Boolean
        Get
            Return _zLineNumbersShowLeadingZeroes
        End Get
        Set
            _zLineNumbersShowLeadingZeroes = Value
            Me.Refresh()
            Me.Invalidate()
        End Set
    End Property

    <Description("Use this property to manually reposition the LineNumbers, relative to their current location.")>
    <Category("Additional Behavior")>
    Public Property LineNrsOffset As Size
        Get
            Return _zLineNumbersOffset
        End Get
        Set
            _zLineNumbersOffset = Value
            Me.Invalidate()
        End Set
    End Property

    <Category("Additional Appearance")>
    <DefaultValue(GetType(Color), "SlateGray")>
    Public Property MarginLinesColor As Color
        Get
            Return _zMarginLinesColor
        End Get
        Set
            _zMarginLinesColor = Value
            Me.Invalidate()
        End Set
    End Property

    <Category("Additional Appearance")>
    <DefaultValue(LineNumberDockSides.Left)>
    Public Property MarginLinesSide As LineNumberDockSides
        Get
            Return _zMarginLinesSide
        End Get
        Set
            _zMarginLinesSide = Value
            Me.Invalidate()
        End Set
    End Property

    <Category("Additional Appearance")>
    <DefaultValue(Drawing2D.DashStyle.Solid)>
    Public Property MarginLinesStyle As Drawing2D.DashStyle
        Get
            Return _zMarginLinesStyle
        End Get
        Set
            If Value = Drawing2D.DashStyle.Custom Then
                Value = Drawing2D.DashStyle.Solid
            End If
            _zMarginLinesStyle = Value
            Me.Invalidate()
        End Set
    End Property

    <Category("Additional Appearance")>
    <DefaultValue(1)>
    Public Property MarginLinesThickness As Single
        Get
            Return _zMarginLinesThickness
        End Get
        Set
            _zMarginLinesThickness = Math.Max(1, Math.Min(255, Value))
            Me.Invalidate()
        End Set
    End Property

    <Description("Use this property to enable the control to act as an overlay on top of the RichTextBox.")>
    <Category("Additional Behavior")>
    <DefaultValue(False)>
    Public Property SeeThroughMode As Boolean
        Get
            Return _zSeeThroughMode
        End Get
        Set
            _zSeeThroughMode = Value
            Me.Invalidate()
        End Set
    End Property

    <Description("The BackgroundGradient is a gradual blend of two colors, shown in the back of each LineNumber's item-area.")>
    <Category("Additional Behavior")>
    <DefaultValue(True)>
    Public Property ShowBackgroundGradient As Boolean
        Get
            Return _zGradientShow
        End Get
        Set
            _zGradientShow = Value
            Me.Invalidate()
        End Set
    End Property

    <Description("BorderLines are shown on all sides of the LineNumber control.")>
    <Category("Additional Behavior")>
    <DefaultValue(True)>
    Public Property ShowBorderLines As Boolean
        Get
            Return _zBorderLinesShow
        End Get
        Set
            _zBorderLinesShow = Value
            Me.Invalidate()
        End Set
    End Property

    <Description("GridLines are the horizontal divider-lines shown above each LineNumber.")>
    <Category("Additional Behavior")>
    <DefaultValue(True)>
    Public Property ShowGridLines As Boolean
        Get
            Return _zGridLinesShow
        End Get
        Set
            _zGridLinesShow = Value
            Me.Invalidate()
        End Set
    End Property

    <Category("Additional Behavior")>
    <DefaultValue(True)>
    Public Property ShowLineNrs As Boolean
        Get
            Return _zLineNumbersShow
        End Get
        Set
            _zLineNumbersShow = Value
            Me.Invalidate()
        End Set
    End Property

    <Description("MarginLines are shown on the Left or Right (or both in Height-mode) of the LineNumber control.")>
    <Category("Additional Behavior")>
    <DefaultValue(True)>
    Public Property ShowMarginLines As Boolean
        Get
            Return _zMarginLinesShow
        End Get
        Set
            _zMarginLinesShow = Value
            Me.Invalidate()
        End Set
    End Property

    <Browsable(False)>
    Public Overrides Property AutoSize As Boolean
        Get
            Return MyBase.AutoSize
        End Get
        Set
            MyBase.AutoSize = Value
            Me.Invalidate()
        End Set
    End Property

    <Browsable(True)>
    Public Overrides Property Font As Font
        Get
            Return MyBase.Font
        End Get
        Set
            MyBase.Font = Value
            Me.Refresh()
            Me.Invalidate()
        End Set
    End Property

    <Description("Use this property to enable LineNumbers for the chosen RichTextBox.")>
    <Category("Add LineNumbers to")>
    Public Property ParentRichTextBox As RichTextBox
        Get
            Return Me.ZParent
        End Get
        Set
            Me.ZParent = Value
            If Me.ZParent IsNot Nothing Then
                Me.Parent = Me.ZParent.Parent
                Me.ZParent.Refresh()
            End If
            Me.Text = ""
            Me.Refresh()
            Me.Invalidate()
        End Set
    End Property

    <DefaultValue("")>
    <AmbientValue("")>
    <Browsable(False)>
    Public Overrides Property Text As String
        Get
            Return MyBase.Text
        End Get
        Set
            MyBase.Text = ""
            Me.Invalidate()
        End Set
    End Property

    ''' <summary>
    ''' FindStartIndex is a recursive Sub (one that calls itself) to compute the first visible line that should have a LineNumber.
    ''' </summary>
    ''' <param name="zMin"> this will hold the eventual BestStartIndex when the Sub has completed its run.</param>
    ''' <param name="zMax"></param>
    ''' <param name="zTarget"></param>
    ''' <remarks></remarks>
    Private Sub FindStartIndex(ByRef zMin As Integer, ByRef zMax As Integer, ByRef zTarget As Integer)
        '   Recursive Sub to compute best starting index - only run when zParent is known to exist
        If zMax = zMin + 1 Or zMin = (zMax + zMin) \ 2 Then
            Exit Sub
        End If
        Select Case Me.ZParent.GetPositionFromCharIndex((zMax + zMin) \ 2).Y
            Case Is = zTarget
                '   BestStartIndex found
                zMin = (zMax + zMin) \ 2
            Case Is > zTarget
                '   Look again, in lower half
                zMax = (zMax + zMin) \ 2
                Me.FindStartIndex(zMin, zMax, zTarget)
            Case Is < 0
                '   Look again, in top half
                zMin = (zMax + zMin) \ 2
                Me.FindStartIndex(zMin, zMax, zTarget)
        End Select
    End Sub

    ''' <summary>
    ''' This Sub will run whenever Me.Refresh() is called. It applies the AutoSizing and DockSide settings.
    ''' </summary>
    ''' <remarks></remarks>
    Private Sub Update_SizeAndPosition()
        If MyBase.AutoSize = True Then
            Exit Sub
        End If
        If Me.Dock = DockStyle.Bottom Or Me.Dock = DockStyle.Fill Or Me.Dock = DockStyle.Top Then
            Exit Sub
        End If
        Dim zNewLocation As Point = Me.Location
        Dim zNewSize As Size = Me.Size

        If _zAutoSizing = True Then
            Select Case True
                Case Me.ZParent Is Nothing
                    ' --- ReminderMessage sizing
                    If _zAutoSizingSize.Width > 0 Then
                        zNewSize.Width = _zAutoSizingSize.Width
                    End If
                    If _zAutoSizingSize.Height > 0 Then
                        zNewSize.Height = _zAutoSizingSize.Height
                    End If
                    Me.Size = zNewSize

                    '--- zParent isNot Nothing for the following cases
                Case Me.Dock = DockStyle.Left Or Me.Dock = DockStyle.Right
                    If _zAutoSizingSize.Width > 0 Then
                        zNewSize.Width = _zAutoSizingSize.Width
                    End If
                    Me.Width = zNewSize.Width

                    ' --- DockSide is active L/R/H
                Case _zDockSide <> LineNumberDockSides.None
                    If _zAutoSizingSize.Width > 0 Then
                        zNewSize.Width = _zAutoSizingSize.Width
                    End If
                    zNewSize.Height = Me.ZParent.Height
                    If _zDockSide = LineNumberDockSides.Left Then
                        zNewLocation.X = Me.ZParent.Left - zNewSize.Width - 1
                    End If
                    If _zDockSide = LineNumberDockSides.Right Then
                        zNewLocation.X = Me.ZParent.Right + 1
                    End If
                    zNewLocation.Y = Me.ZParent.Top
                    Me.Location = zNewLocation
                    Me.Size = zNewSize

                    ' --- DockSide = None, but AutoSizing is still setting the Width
                Case _zDockSide = LineNumberDockSides.None
                    If _zAutoSizingSize.Width > 0 Then
                        zNewSize.Width = _zAutoSizingSize.Width
                    End If
                    Me.Size = zNewSize

            End Select
        Else
            ' --- No AutoSizing
            Select Case True
                Case Me.ZParent Is Nothing
                    ' --- ReminderMessage sizing
                    If _zAutoSizingSize.Width > 0 Then
                        zNewSize.Width = _zAutoSizingSize.Width
                    End If
                    If _zAutoSizingSize.Height > 0 Then
                        zNewSize.Height = _zAutoSizingSize.Height
                    End If
                    Me.Size = zNewSize

                    ' --- No AutoSizing, but DockSide L/R/H is active so height and position need updates.
                Case _zDockSide <> LineNumberDockSides.None
                    zNewSize.Height = Me.ZParent.Height
                    If _zDockSide = LineNumberDockSides.Left Then
                        zNewLocation.X = Me.ZParent.Left - zNewSize.Width - 1
                    End If
                    If _zDockSide = LineNumberDockSides.Right Then
                        zNewLocation.X = Me.ZParent.Right + 1
                    End If
                    zNewLocation.Y = Me.ZParent.Top
                    Me.Location = zNewLocation
                    Me.Size = zNewSize

            End Select
        End If

    End Sub

    ''' <summary>
    ''' This Sub determines which textlines are visible in the ParentRichTextBox, and makes LineNumberItems (LineNumber + ItemRectangle)
    ''' for each visible line. They are put into the zLNIs List that will be used by the OnPaint event to draw the LineNumberItems.
    ''' </summary>
    ''' <remarks></remarks>
    Private Sub Update_VisibleLineNumberItems()
        _zLineNunmberItems.Clear()
        _zAutoSizingSize = New Size(0, 0)
        _zLineNumbersFormat = "0"  'initial setting
        '   To measure the LineNumber's width, its Format 0 is replaced by w as that is likely to be one of the widest characters in non-mono-space fonts.
        If _zAutoSizing = True Then
            _zAutoSizingSize = New Size(TextRenderer.MeasureText(_zLineNumbersFormat.Replace("0".ToCharArray, "W".ToCharArray, StringComparison.Ordinal), Me.Font).Width, 0)
        End If

        If String.IsNullOrWhiteSpace(Me.ZParent?.Text) Then
            Exit Sub
        End If

        ' --- Make sure the LineNumbers are aligning to the same height as the zParent textlines by converting to screencoordinates
        '   and using that as an offset that gets added to the points for the LineNumberItems
        _zPointInParent = Me.ZParent.PointToScreen(Me.ZParent.ClientRectangle.Location)
        _zPointInMe = Me.PointToScreen(New Point(0, 0))
        '   zParentInMe is the vertical offset to make the LineNumberItems line up with the textlines in zParent.
        _zParentInMe = _zPointInParent.Y - _zPointInMe.Y + 1
        '   The first visible LineNumber may not be the first visible line of text in the RTB if the LineNumbercontrol's .Top is lower on the form than
        '   the .Top of the parent RichTextBox. Therefor, zPointInParent will now be used to find zPointInMe's equivalent height in zParent,
        '   which is needed to find the best StartIndex later on.
        _zPointInParent = Me.ZParent.PointToClient(_zPointInMe)

        ' --- NOTES:
        '   Additional complication is the fact that when word-wrap is enabled on the RTB, the wordwrapped text spills into the RTB.Lines collection,
        '   so we need to split the text into lines ourselves, and use the Index of each zSplit-line's first character instead of the RTB's.
        Dim zSplit() As String = Me.ZParent.Text.Split(vbCrLf.ToCharArray)

        If zSplit.Length < 2 Then
            '   Just one line in the text = one linenumber
            '   NOTE:  zContentRectangle is built by the zParent.ContentsResized event.
            Dim zPoint As Point = Me.ZParent.GetPositionFromCharIndex(0)
            _zLineNunmberItems.Add(New LineNumberItem(1, New Rectangle(New Point(0, zPoint.Y - 1 + _zParentInMe), New Size(Me.Width, _zContentRectangle.Height - zPoint.Y))))
        Else

            '   Multiple lines, but store only those LineNumberItems for lines that are visible.
            Dim zTimeSpan As New TimeSpan(Now.Ticks)
            Dim zPoint As Point
            Dim zStartIndex As Integer = 0
            Dim zA As Integer = Me.ZParent.Text.Length - 1
            Me.FindStartIndex(zStartIndex, zA, _zPointInParent.Y)

            '   zStartIndex now holds the index of a character in the first visible line from zParent.Text
            '   Now it will be pointed at the first character of that line (chr(10) = Linefeed part of the vbCrLf constant)
            zStartIndex = Math.Max(0, Math.Min(Me.ZParent.Text.Length - 1, Me.ZParent.Text.Substring(0, zStartIndex).LastIndexOf(Chr(10)) + 1))

            '   We now need to find out which zSplit-line that character is in, by counting the vbCrlf appearances that come before it.
            Dim zSplitStartLine As Integer = Math.Max(0, Me.ZParent.Text.Substring(0, zStartIndex).Split(vbCrLf.ToCharArray).Length - 1)

            '   zStartIndex starts off pointing at the first character of the first visible line, and will be then be pointed to
            '   the index of the first character on the next line.
            For zA = zSplitStartLine To zSplit.Length - 1
                zPoint = Me.ZParent.GetPositionFromCharIndex(zStartIndex)
                zStartIndex += Math.Max(1, zSplit(zA).Length + 1)
                If zPoint.Y + _zParentInMe > Me.Height Then
                    Exit For
                End If
                '   For performance reasons, the list of LineNumberItems (zLNIs) is first built with only the location of its
                '   itemrectangle being used. The height of those rectangles will be computed afterwards by comparing the items' Y coordinates.
                _zLineNunmberItems.Add(New LineNumberItem(zA + 1, New Rectangle(0, zPoint.Y - 1 + _zParentInMe, Me.Width, 1)))
                If _zParentIsScrolling = True AndAlso Now.Ticks > zTimeSpan.Ticks + 500000 Then
                    '   The more lines there are in the RTB, the slower the RTB's .GetPositionFromCharIndex() method becomes
                    '   To avoid those delays from interfering with the scrollingspeed, this speedbased exit for is applied (0.05 sec)
                    '   zLNIs will have at least 1 item, and if that's the only one, then change its location to 0,0 to make it readable
                    If _zLineNunmberItems.Count = 1 Then
                        _zLineNunmberItems(0)._rectangle.Y = 0
                    End If
                    _zParentIsScrolling = False
                    Me.ZTimer.Start()
                    Exit For
                End If
            Next

            If _zLineNunmberItems.Count = 0 Then
                Exit Sub
            End If

            '   Add an extra placeholder item to the end, to make the heightcomputation easier
            If zA < zSplit.Length Then
                '   getting here means the for/next loop was exited before reaching the last zSplit textline
                '   zStartIndex will still be pointing to the startcharacter of the next line, so we can use that:
                zPoint = Me.ZParent.GetPositionFromCharIndex(Math.Min(zStartIndex, Me.ZParent.Text.Length - 1))
                _zLineNunmberItems.Add(New LineNumberItem(-1, New Rectangle(0, zPoint.Y - 1 + _zParentInMe, 0, 0)))
            Else
                '   getting here means the for/next loop ran to the end (zA is now zSplit.Length).
                _zLineNunmberItems.Add(New LineNumberItem(-1, New Rectangle(0, _zContentRectangle.Bottom, 0, 0)))
            End If

            '   And now we can easily compute the height of the LineNumberItems by comparing each item's Y coordinate with that of the next line.
            '   There's at least two items in the list, and the last item is a "nextline-placeholder" that will be removed.
            For zA = 0 To _zLineNunmberItems.Count - 2
                _zLineNunmberItems(zA)._rectangle.Height = Math.Max(1, _zLineNunmberItems(zA + 1)._rectangle.Y - _zLineNunmberItems(zA)._rectangle.Y)
            Next
            '   Removing the placeholder item
            _zLineNunmberItems.RemoveAt(_zLineNunmberItems.Count - 1)

            ' Set the Format to the width of the highest possible number so that LeadingZeroes shows the correct amount of zeros.
            _zLineNumbersFormat = If(_zLineNumbersShowAsHexadecimal = True, "".PadRight(zSplit.Length.ToString("X", Globalization.CultureInfo.InvariantCulture).Length, "0"c), "".PadRight(zSplit.Length.ToString(Globalization.CultureInfo.InvariantCulture).Length, "0"c))
        End If

        '   To measure the LineNumber's width, its Format 0 is replaced by w as that is likely to be one of the widest characters in non-mono-space fonts.
        If _zAutoSizing = True Then
            _zAutoSizingSize = New Size(TextRenderer.MeasureText(_zLineNumbersFormat.Replace("0".ToCharArray, "W".ToCharArray, StringComparison.Ordinal), Me.Font).Width, 0)
        End If
    End Sub

    Private Sub ZParent_Changed(sender As Object, e As EventArgs) Handles ZParent.LocationChanged, ZParent.Move, ZParent.Resize, ZParent.DockChanged, ZParent.TextChanged, ZParent.MultilineChanged
        Me.Refresh()
        Me.Invalidate()
    End Sub

    Private Sub ZParent_ContentsResized(sender As Object, e As ContentsResizedEventArgs) Handles ZParent.ContentsResized
        _zContentRectangle = e.NewRectangle
        Me.Refresh()
        Me.Invalidate()
    End Sub

    Private Sub ZParent_Disposed(sender As Object, e As EventArgs) Handles ZParent.Disposed
        Me.ParentRichTextBox = Nothing
        Me.Refresh()
        Me.Invalidate()
    End Sub

    Private Sub ZParent_Scroll(sender As Object, e As EventArgs) Handles ZParent.HScroll, ZParent.VScroll
        _zParentIsScrolling = True
        Me.Invalidate()
    End Sub

    Private Sub ZTimer_Tick(sender As Object, e As EventArgs) Handles ZTimer.Tick
        _zParentIsScrolling = False
        Me.ZTimer.Stop()
        Me.Invalidate()
    End Sub

    Protected Overrides Sub OnHandleCreated(e As EventArgs)
        MyBase.OnHandleCreated(e)
        Me.AutoSize = False
    End Sub

    Protected Overrides Sub OnLocationChanged(e As EventArgs)
        If Me.DesignMode = True Then
            Me.Refresh()
        End If
        MyBase.OnLocationChanged(e)
        Me.Invalidate()
    End Sub

    ''' <summary>
    ''' OnPaint will go through the enabled elements (vertical ReminderMessage, GridLines, LineNumbers, BorderLines, MarginLines) and will
    ''' draw them if enabled. At the same time, it will build GraphicsPaths for each of those elements (that are enabled), which will be used
    ''' in SeeThroughMode (if it's active): the figures in the GraphicsPaths will form a customized outline for the control by setting them as the
    ''' Region of the LineNumber control. Note: the vertical ReminderMessages are only drawn during design time.
    ''' </summary>
    ''' <param name="e"></param>
    ''' <remarks></remarks>
    Protected Overrides Sub OnPaint(e As PaintEventArgs)
        Contracts.Contract.Requires(e IsNot Nothing)
        '   Build the list of visible LineNumberItems (= zLNIs) first. (doesn't take long, so it can stay in OnPaint)
        Me.Update_VisibleLineNumberItems()
        MyBase.OnPaint(e)

        ' --- QualitySettings
        e.Graphics.TextRenderingHint = If(_zLineNumbersAntiAlias = True, Drawing.Text.TextRenderingHint.AntiAlias, Drawing.Text.TextRenderingHint.SystemDefault)

        Dim zGraphicsPathLineNumbers As New Drawing2D.GraphicsPath(Drawing2D.FillMode.Winding)
        Dim zTextSize As SizeF
        Dim zPoint As New Point(0, 0)
        Using zStringFormat As New StringFormat
            ' ----------------------------------------------
            ' --- DESIGNTIME / NO VISIBLE ITEMS
            If Me.DesignMode = True Then
                Dim zReminderToShow As String = ""
                '   Show a vertical reminder message
                If Me.ZParent Is Nothing Then
                    zReminderToShow = "-!- Set ParentRichTextBox -!-"
                Else
                    If _zLineNunmberItems.Count = 0 Then
                        zReminderToShow = "LineNrs (  " & Me.ZParent.Name & "  )"
                    End If
                End If
                If zReminderToShow.Length > 0 Then
                    ' --- Centering and Rotation for the reminder message
                    e.Graphics.TranslateTransform(CSng(Me.Width / 2), CSng(Me.Height / 2))
                    e.Graphics.RotateTransform(-90)
                    zStringFormat.Alignment = StringAlignment.Center
                    zStringFormat.LineAlignment = StringAlignment.Center
                    ' --- Show the reminder message (with small shadow)
                    zTextSize = e.Graphics.MeasureString(zReminderToShow, Me.Font, zPoint, zStringFormat)
                    e.Graphics.DrawString(zReminderToShow, Me.Font, Brushes.WhiteSmoke, 1, 1, zStringFormat)
                    e.Graphics.DrawString(zReminderToShow, Me.Font, Brushes.Firebrick, 0, 0, zStringFormat)
                    e.Graphics.ResetTransform()

                    Dim zReminderRectangle As New Rectangle(CInt((Me.Width / 2) - (zTextSize.Height / 2)), CInt((Me.Height / 2) - (zTextSize.Width / 2)), CInt(zTextSize.Height), CInt(zTextSize.Width))
                    zGraphicsPathLineNumbers.AddRectangle(zReminderRectangle)
                    zGraphicsPathLineNumbers.CloseFigure()

                    If _zAutoSizing = True Then
                        zReminderRectangle.Inflate(CInt(zTextSize.Height * 0.2), CInt(zTextSize.Width * 0.1))
                        _zAutoSizingSize = New Size(zReminderRectangle.Width, zReminderRectangle.Height)
                    End If
                End If
            End If

            Dim zPen As New Pen(Me.ForeColor)

            '   NOTE: The GraphicsPaths are only used for SeeThroughMode
            '   FillMode.Winding: combined outline ( Alternate: XOR'ed outline )
            Dim zGpGridLines As New Drawing2D.GraphicsPath(Drawing2D.FillMode.Winding)
            Dim zBrush As New SolidBrush(Me.ForeColor)
            ' ----------------------------------------------
            ' --- DESIGN OR RUNTIME / WITH VISIBLE ITEMS (which means zParent exists)
            If _zLineNunmberItems.Count > 0 Then
                '   The visible LineNumberItems with their BackgroundGradient and GridLines
                '   Loop through every visible LineNumberItem
                Dim zLinearGradientBrush As Drawing2D.LinearGradientBrush = Nothing
                zPen = New Pen(_zGridLinesColor, _zGridLinesThickness) With {
                                    .DashStyle = _zGridLinesStyle
                                }
                zStringFormat.Alignment = StringAlignment.Near
                zStringFormat.LineAlignment = StringAlignment.Near
                zStringFormat.FormatFlags = CType(StringFormatFlags.FitBlackBox + StringFormatFlags.NoClip + StringFormatFlags.NoWrap, StringFormatFlags)

                For zA As Integer = 0 To _zLineNunmberItems.Count - 1

                    ' --- BackgroundGradient
                    If _zGradientShow = True Then
                        zLinearGradientBrush = New Drawing2D.LinearGradientBrush(_zLineNunmberItems(zA)._rectangle, _zGradientStartColor, _zGradientEndColor, _zGradientDirection)
                        e.Graphics.FillRectangle(zLinearGradientBrush, _zLineNunmberItems(zA)._rectangle)
                    End If

                    ' --- GridLines
                    If _zGridLinesShow = True Then
                        e.Graphics.DrawLine(zPen, New Point(0, _zLineNunmberItems(zA)._rectangle.Y), New Point(Me.Width, _zLineNunmberItems(zA)._rectangle.Y))

                        '   NOTE: Every item in a GraphicsPath is a closed figure, so instead of adding gridlines as lines, we'll add them
                        '   as rectangles that loop out of sight. Their height uses the zContentRectangle which is the maxsize of
                        '   the ParentRichTextBox's contents.
                        '   NOTE: Slight adjustment needed when the first item has a negative Y coordinate.
                        '   This explains the " - zLNIs(0).Rectangle.Y" (which adds the negative size to the height
                        '   to make sure the rectangle's bottompart stays out of sight)
                        zGpGridLines.AddRectangle(New Rectangle(CInt(-_zGridLinesThickness), _zLineNunmberItems(zA)._rectangle.Y, CInt(Me.Width + (_zGridLinesThickness * 2)), CInt(Me.Height - _zLineNunmberItems(0)._rectangle.Y + _zGridLinesThickness)))
                        zGpGridLines.CloseFigure()
                    End If

                    ' --- LineNumbers
                    If _zLineNumbersShow = True Then
                        ' --- Local Declarations
                        '   TextFormatting
                        Dim zTextToShow As String = If(_zLineNumbersShowLeadingZeroes, If(_zLineNumbersShowAsHexadecimal, _zLineNunmberItems(zA)._lineNumber.ToString("X", Globalization.CultureInfo.InvariantCulture), _zLineNunmberItems(zA)._lineNumber.ToString(_zLineNumbersFormat, Globalization.CultureInfo.InvariantCulture)), If(_zLineNumbersShowAsHexadecimal, _zLineNunmberItems(zA)._lineNumber.ToString("X", Globalization.CultureInfo.InvariantCulture), _zLineNunmberItems(zA)._lineNumber.ToString(Globalization.CultureInfo.InvariantCulture)))
                        '   TextSizing
                        zTextSize = e.Graphics.MeasureString(zTextToShow, Me.Font, zPoint, zStringFormat)
                        '   TextAlignment and positioning   (zPoint = TopLeftCornerPoint of the text)
                        '   TextAlignment, padding, manual offset (via LineNrs_Offset) and zTextSize are all included in the calculation of zPoint.
                        Select Case _zLineNumbersAlignment
                            Case ContentAlignment.TopLeft
                                zPoint = New Point(_zLineNunmberItems(zA)._rectangle.Left + Me.Padding.Left + _zLineNumbersOffset.Width, _zLineNunmberItems(zA)._rectangle.Top + Me.Padding.Top + _zLineNumbersOffset.Height)
                            Case ContentAlignment.MiddleLeft
                                zPoint = New Point(_zLineNunmberItems(zA)._rectangle.Left + Me.Padding.Left + _zLineNumbersOffset.Width, CInt(_zLineNunmberItems(zA)._rectangle.Top + (_zLineNunmberItems(zA)._rectangle.Height / 2) + _zLineNumbersOffset.Height - (zTextSize.Height / 2)))
                            Case ContentAlignment.BottomLeft
                                zPoint = New Point(_zLineNunmberItems(zA)._rectangle.Left + Me.Padding.Left + _zLineNumbersOffset.Width, CInt(_zLineNunmberItems(zA)._rectangle.Bottom - Me.Padding.Bottom + 1 + _zLineNumbersOffset.Height - zTextSize.Height))
                            Case ContentAlignment.TopCenter
                                zPoint = New Point(CInt((_zLineNunmberItems(zA)._rectangle.Width / 2) + _zLineNumbersOffset.Width - (zTextSize.Width / 2)), _zLineNunmberItems(zA)._rectangle.Top + Me.Padding.Top + _zLineNumbersOffset.Height)
                            Case ContentAlignment.MiddleCenter
                                zPoint = New Point(CInt((_zLineNunmberItems(zA)._rectangle.Width / 2) + _zLineNumbersOffset.Width - (zTextSize.Width / 2)), CInt(_zLineNunmberItems(zA)._rectangle.Top + (_zLineNunmberItems(zA)._rectangle.Height / 2) + _zLineNumbersOffset.Height - (zTextSize.Height / 2)))
                            Case ContentAlignment.BottomCenter
                                zPoint = New Point(CInt((_zLineNunmberItems(zA)._rectangle.Width / 2) + _zLineNumbersOffset.Width - (zTextSize.Width / 2)), CInt(_zLineNunmberItems(zA)._rectangle.Bottom - Me.Padding.Bottom + 1 + _zLineNumbersOffset.Height - zTextSize.Height))
                            Case ContentAlignment.TopRight
                                zPoint = New Point(CInt(_zLineNunmberItems(zA)._rectangle.Right - Me.Padding.Right + _zLineNumbersOffset.Width - zTextSize.Width), _zLineNunmberItems(zA)._rectangle.Top + Me.Padding.Top + _zLineNumbersOffset.Height)
                            Case ContentAlignment.MiddleRight
                                zPoint = New Point(CInt(_zLineNunmberItems(zA)._rectangle.Right - Me.Padding.Right + _zLineNumbersOffset.Width - zTextSize.Width), CInt(_zLineNunmberItems(zA)._rectangle.Top + (_zLineNunmberItems(zA)._rectangle.Height / 2) + _zLineNumbersOffset.Height - (zTextSize.Height / 2)))
                            Case ContentAlignment.BottomRight
                                zPoint = New Point(CInt(_zLineNunmberItems(zA)._rectangle.Right - Me.Padding.Right + _zLineNumbersOffset.Width - zTextSize.Width), CInt(_zLineNunmberItems(zA)._rectangle.Bottom - Me.Padding.Bottom + 1 + _zLineNumbersOffset.Height - zTextSize.Height))
                        End Select
                        '   TextClipping
                        Dim zItemClipRectangle As New Rectangle(zPoint, zTextSize.ToSize)
                        If _zLineNumbersClipByItemRectangle = True Then
                            '   If selected, the text will be clipped so that it doesn't spill out of its own LineNumberItem-area.
                            '   Only the part of the text inside the LineNumberItem.Rectangle should be visible, so intersect with the ItemRectangle
                            '   The SetClip method temporary restricts the drawing area of the control for whatever is drawn next.
                            zItemClipRectangle.Intersect(_zLineNunmberItems(zA)._rectangle)
                            e.Graphics.SetClip(zItemClipRectangle)
                        End If
                        '   TextDrawing
                        e.Graphics.DrawString(zTextToShow, Me.Font, zBrush, zPoint, zStringFormat)
                        e.Graphics.ResetClip()
                        '   The GraphicsPath for the LineNumber is just a rectangle behind the text, to keep the paintingspeed high and avoid ugly artifacts.
                        zGraphicsPathLineNumbers.AddRectangle(zItemClipRectangle)
                        zGraphicsPathLineNumbers.CloseFigure()
                    End If
                Next

                ' --- GridLinesThickness and LineStyle in SeeThroughMode. All GraphicsPath lines are drawn as solid to keep the paintingspeed high.
                If _zGridLinesShow = True Then
                    zPen.DashStyle = Drawing2D.DashStyle.Solid
                    zGpGridLines.Widen(zPen)
                End If

                ' --- Memory CleanUp
                If zLinearGradientBrush IsNot Nothing Then
                    zLinearGradientBrush.Dispose()
                End If
            End If

            ' ----------------------------------------------
            ' --- DESIGN OR RUNTIME / ALWAYS
            Dim zPointLeft As New Point(CInt(Math.Floor(_zBorderLinesThickness / 2)), CInt(Math.Floor(_zBorderLinesThickness / 2)))
            Dim zPointRight As New Point(Me.Width - CInt(Math.Ceiling(_zBorderLinesThickness / 2)), Me.Height - CInt(Math.Ceiling(_zBorderLinesThickness / 2)))

            ' --- BorderLines
            Dim zBorderLinesPoints() As Point = {New Point(zPointLeft.X, zPointLeft.Y), New Point(zPointRight.X, zPointLeft.Y), New Point(zPointRight.X, zPointRight.Y), New Point(zPointLeft.X, zPointRight.Y), New Point(zPointLeft.X, zPointLeft.Y)}

            Dim zGraphicsPathBorderLines As New Drawing2D.GraphicsPath(Drawing2D.FillMode.Winding)
            If _zBorderLinesShow = True Then
                zPen = New Pen(_zBorderLinesColor, _zBorderLinesThickness) With {
                .DashStyle = _zBorderLinesStyle
            }
                e.Graphics.DrawLines(zPen, zBorderLinesPoints)
                zGraphicsPathBorderLines.AddLines(zBorderLinesPoints)
                zGraphicsPathBorderLines.CloseFigure()
                '   BorderThickness and Style for SeeThroughMode
                zPen.DashStyle = Drawing2D.DashStyle.Solid
                zGraphicsPathBorderLines.Widen(zPen)
            End If

            Dim zGraphicsPathMarginLines As New Drawing2D.GraphicsPath(Drawing2D.FillMode.Winding)
            ' --- MarginLines
            If _zMarginLinesShow = True AndAlso _zMarginLinesSide > LineNumberDockSides.None Then
                zPointLeft = New Point(CInt(-_zMarginLinesThickness), CInt(-_zMarginLinesThickness))
                zPointRight = New Point(Me.Width + CInt(_zMarginLinesThickness), Me.Height + CInt(_zMarginLinesThickness))
                zPen = New Pen(_zMarginLinesColor, _zMarginLinesThickness) With {
                .DashStyle = _zMarginLinesStyle
            }
                If _zMarginLinesSide = LineNumberDockSides.Left Or _zMarginLinesSide = LineNumberDockSides.Height Then
                    e.Graphics.DrawLine(zPen, New Point(CInt(Math.Floor(_zMarginLinesThickness / 2)), 0), New Point(CInt(Math.Floor(_zMarginLinesThickness / 2)), Me.Height - 1))
                    zPointLeft = New Point(CInt(Math.Ceiling(_zMarginLinesThickness / 2)), CInt(-_zMarginLinesThickness))
                End If
                If _zMarginLinesSide = LineNumberDockSides.Right Or _zMarginLinesSide = LineNumberDockSides.Height Then
                    e.Graphics.DrawLine(zPen, New Point(CInt(Me.Width - Math.Ceiling(_zMarginLinesThickness / 2)), 0), New Point(CInt(Me.Width - Math.Ceiling(_zMarginLinesThickness / 2)), Me.Height - 1))
                    zPointRight = New Point(Me.Width - CInt(Math.Ceiling(_zMarginLinesThickness / 2)), Me.Height + CInt(_zMarginLinesThickness))
                End If
                '   GraphicsPath for the MarginLines(s):
                '   MarginLines(s) are drawn as a rectangle connecting the zP_Left and zP_Right points, which are either inside or
                '   outside of sight, depending on whether the MarginLines at that side is visible. zP_Left: TopLeft and ZP_Right: BottomRight
                zGraphicsPathMarginLines.AddRectangle(New Rectangle(zPointLeft, New Size(zPointRight.X - zPointLeft.X, zPointRight.Y - zPointLeft.Y)))
                zPen.DashStyle = Drawing2D.DashStyle.Solid
                zGraphicsPathMarginLines.Widen(zPen)
            End If

            Dim zRegion As New Region(Me.ClientRectangle)
            ' ----------------------------------------------
            ' --- SeeThroughMode
            '   combine all the GraphicsPaths (= zGP_... ) and set them as the region for the control.
            If _zSeeThroughMode = True Then
                zRegion.MakeEmpty()
                zRegion.Union(zGraphicsPathBorderLines)
                zRegion.Union(zGraphicsPathMarginLines)
                zRegion.Union(zGpGridLines)
                zRegion.Union(zGraphicsPathLineNumbers)
            End If

            ' --- Region
            If zRegion.GetBounds(e.Graphics).IsEmpty = True Then
                '   Note: If the control is in a condition that would show it as empty, then a border-region is still drawn regardless of it's borders on/off state.
                '   This is added to make sure that the bounds of the control are never lost (it would remain empty if this was not done).
                zGraphicsPathBorderLines.AddLines(zBorderLinesPoints)
                zGraphicsPathBorderLines.CloseFigure()
                zPen = New Pen(_zBorderLinesColor, 1) With {
                .DashStyle = Drawing2D.DashStyle.Solid
            }
                zGraphicsPathBorderLines.Widen(zPen)

                zRegion = New Region(zGraphicsPathBorderLines)
            End If
            Me.Region = zRegion
            ' ----------------------------------------------
            ' --- Memory CleanUp
            If zBrush IsNot Nothing Then
                zBrush.Dispose()
            End If
            If zPen IsNot Nothing Then
                zPen.Dispose()
            End If
            If zRegion IsNot Nothing Then
                zRegion.Dispose()
            End If
            If zGpGridLines IsNot Nothing Then
                zGpGridLines.Dispose()
            End If
            If zGraphicsPathBorderLines IsNot Nothing Then
                zGraphicsPathBorderLines.Dispose()
            End If
            If zGraphicsPathMarginLines IsNot Nothing Then
                zGraphicsPathMarginLines.Dispose()
            End If
        End Using
        If zGraphicsPathLineNumbers IsNot Nothing Then
            zGraphicsPathLineNumbers.Dispose()
        End If
    End Sub

    Protected Overrides Sub OnSizeChanged(e As EventArgs)
        If Me.DesignMode = True Then
            Me.Refresh()
        End If
        MyBase.OnSizeChanged(e)
        Me.Invalidate()
    End Sub

    Public Overrides Sub Refresh()
        '   Note: don't change the order here, first the Mybase.Refresh, then the Update_SizeAndPosition.
        MyBase.Refresh()
        Me.Update_SizeAndPosition()
    End Sub

End Class
