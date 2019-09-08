' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

<ComponentModel.DefaultProperty("ParentRichTextBox")>
Public Class LineNumbersForRichTextBox : Inherits Control

    '   Code provided "as is", with no rights attached, nor liabilities.
    '
    '   Enjoy! --- nogChoco

    Private WithEvents ZParent As RichTextBox = Nothing

    Private WithEvents ZTimer As New Timer

    Private ReadOnly zLNIs As New List(Of LineNumberItem)

    Private zAutoSizing As Boolean = True

    Private zAutoSizing_Size As New Size(0, 0)

    Private zBorderLines_Color As Color = Color.SlateGray

    Private zBorderLines_Show As Boolean = True

    Private zBorderLines_Style As Drawing2D.DashStyle = Drawing2D.DashStyle.Dot

    Private zBorderLines_Thickness As Single = 1

    Private zContentRectangle As Rectangle = Nothing

    Private zDockSide As LineNumberDockSides = LineNumberDockSides.Left

    Private zGradient_Direction As Drawing2D.LinearGradientMode = Drawing2D.LinearGradientMode.Horizontal

    Private zGradient_EndColor As Color = Color.LightSteelBlue

    Private zGradient_Show As Boolean = True

    Private zGradient_StartColor As Color = Color.FromArgb(0, 0, 0, 0)

    Private zGridLines_Color As Color = Color.SlateGray

    Private zGridLines_Show As Boolean = True

    Private zGridLines_Style As Drawing2D.DashStyle = Drawing2D.DashStyle.Dot

    Private zGridLines_Thickness As Single = 1

    Private zLineNumbers_Alignment As ContentAlignment = ContentAlignment.TopRight

    Private zLineNumbers_AntiAlias As Boolean = True

    Private zLineNumbers_ClipByItemRectangle As Boolean = True

    Private zLineNumbersFormat As String = "0"

    Private zLineNumbers_Offset As New Size(0, 0)

    Private zLineNumbers_Show As Boolean = True

    Private zLineNumbers_ShowAsHexadecimal As Boolean = False

    Private zLineNumbers_ShowLeadingZeroes As Boolean = True

    Private zMarginLines_Color As Color = Color.SlateGray

    Private zMarginLines_Show As Boolean = True

    Private zMarginLines_Side As LineNumberDockSides = LineNumberDockSides.Right

    Private zMarginLines_Style As Drawing2D.DashStyle = Drawing2D.DashStyle.Solid

    Private zMarginLines_Thickness As Single = 1

    Private zParentInMe As Integer = 0

    Private zParentIsScrolling As Boolean = False

    Private zPointInMe As New Point(0, 0)

    Private zPointInParent As New Point(0, 0)

    Private zSeeThroughMode As Boolean = False

    Public Sub New()
        With Me
            .SetStyle(ControlStyles.OptimizedDoubleBuffer, True)
            .SetStyle(ControlStyles.ResizeRedraw, True)
            .SetStyle(ControlStyles.SupportsTransparentBackColor, True)
            .SetStyle(ControlStyles.UserPaint, True)
            .SetStyle(ControlStyles.AllPaintingInWmPaint, True)
            .Margin = New Padding(0)
            .Padding = New Padding(0, 0, 2, 0)
        End With
        With ZTimer
            .Enabled = True
            .Interval = 200
            .Stop()
        End With
        Update_SizeAndPosition()
        Invalidate()
    End Sub

    <Flags>
    Public Enum LineNumberDockSides
        None = 0
        Left = 1
        Right = 2
        Height = 4
    End Enum

    <ComponentModel.Description("Use this property to enable the control to act as an overlay on top of the RichTextBox.")>
    <ComponentModel.Category("Additional Behavior")> Public Property SeeThroughMode() As Boolean
        Get
            Return zSeeThroughMode
        End Get
        Set(ByVal value As Boolean)
            zSeeThroughMode = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Browsable(False)> Public Overrides Property AutoSize() As Boolean
        Get
            Return MyBase.AutoSize
        End Get
        Set(ByVal value As Boolean)
            MyBase.AutoSize = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this property to automatically resize the control (and reposition it if needed).")>
    <ComponentModel.Category("Additional Behavior")> Public Property AutoSizing() As Boolean
        Get
            Return zAutoSizing
        End Get
        Set(ByVal value As Boolean)
            zAutoSizing = value
            Refresh()
            Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property BackgroundGradientAlphaColor() As Color
        Get
            Return zGradient_StartColor
        End Get
        Set(ByVal value As Color)
            zGradient_StartColor = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property BackgroundGradientBetaColor() As Color
        Get
            Return zGradient_EndColor
        End Get
        Set(ByVal value As Color)
            zGradient_EndColor = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property BackgroundGradientDirection() As Drawing2D.LinearGradientMode
        Get
            Return zGradient_Direction
        End Get
        Set(ByVal value As Drawing2D.LinearGradientMode)
            zGradient_Direction = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property BorderLinesColor() As Color
        Get
            Return zBorderLines_Color
        End Get
        Set(ByVal value As Color)
            zBorderLines_Color = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property BorderLinesStyle() As Drawing2D.DashStyle
        Get
            Return zBorderLines_Style
        End Get
        Set(ByVal value As Drawing2D.DashStyle)
            If value = Drawing2D.DashStyle.Custom Then value = Drawing2D.DashStyle.Solid
            zBorderLines_Style = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property BorderLinesThickness() As Single
        Get
            Return zBorderLines_Thickness
        End Get
        Set(ByVal value As Single)
            zBorderLines_Thickness = Math.Max(1, Math.Min(255, value))
            Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this property to dock the LineNumbers to a chosen side of the chosen RichTextBox.")>
    <ComponentModel.Category("Additional Behavior")> Public Property DockSide() As LineNumberDockSides
        Get
            Return zDockSide
        End Get
        Set(ByVal value As LineNumberDockSides)
            zDockSide = value
            Refresh()
            Invalidate()
        End Set
    End Property

    <ComponentModel.Browsable(True)> Public Overrides Property Font() As Font
        Get
            Return MyBase.Font
        End Get
        Set(ByVal value As Font)
            MyBase.Font = value
            Refresh()
            Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property GridLinesColor() As Color
        Get
            Return zGridLines_Color
        End Get
        Set(ByVal value As Color)
            zGridLines_Color = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property GridLinesStyle() As Drawing2D.DashStyle
        Get
            Return zGridLines_Style
        End Get
        Set(ByVal value As Drawing2D.DashStyle)
            If value = Drawing2D.DashStyle.Custom Then value = Drawing2D.DashStyle.Solid
            zGridLines_Style = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property GridLinesThickness() As Single
        Get
            Return zGridLines_Thickness
        End Get
        Set(ByVal value As Single)
            zGridLines_Thickness = Math.Max(1, Math.Min(255, value))
            Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this to align the LineNumbers to a chosen corner (or center) within their item-area.")>
    <ComponentModel.Category("Additional Behavior")> Public Property LineNrsAlignment() As ContentAlignment
        Get
            Return zLineNumbers_Alignment
        End Get
        Set(ByVal value As ContentAlignment)
            zLineNumbers_Alignment = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this to apply Anti-Aliasing to the LineNumbers (high quality). Some fonts will look better without it, though.")>
    <ComponentModel.Category("Additional Behavior")> Public Property LineNrsAntiAlias() As Boolean
        Get
            Return zLineNumbers_AntiAlias
        End Get
        Set(ByVal value As Boolean)
            zLineNumbers_AntiAlias = value
            Refresh()
            Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this to set whether the LineNumbers should be shown as hexadecimal values.")>
    <ComponentModel.Category("Additional Behavior")> Public Property LineNrsAsHexadecimal() As Boolean
        Get
            Return zLineNumbers_ShowAsHexadecimal
        End Get
        Set(ByVal value As Boolean)
            zLineNumbers_ShowAsHexadecimal = value
            Refresh()
            Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this to set whether the LineNumbers are allowed to spill out of their item-area, or should be clipped by it.")>
    <ComponentModel.Category("Additional Behavior")> Public Property LineNrsClippedByItemRectangle() As Boolean
        Get
            Return zLineNumbers_ClipByItemRectangle
        End Get
        Set(ByVal value As Boolean)
            zLineNumbers_ClipByItemRectangle = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this to set whether the LineNumbers should have leading zeros (based on the total amount of text lines).")>
    <ComponentModel.Category("Additional Behavior")> Public Property LineNrsLeadingZeroes() As Boolean
        Get
            Return zLineNumbers_ShowLeadingZeroes
        End Get
        Set(ByVal value As Boolean)
            zLineNumbers_ShowLeadingZeroes = value
            Refresh()
            Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this property to manually reposition the LineNumbers, relative to their current location.")>
    <ComponentModel.Category("Additional Behavior")> Public Property LineNrsOffset() As Size
        Get
            Return zLineNumbers_Offset
        End Get
        Set(ByVal value As Size)
            zLineNumbers_Offset = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property MarginLinesColor() As Color
        Get
            Return zMarginLines_Color
        End Get
        Set(ByVal value As Color)
            zMarginLines_Color = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property MarginLinesSide() As LineNumberDockSides
        Get
            Return zMarginLines_Side
        End Get
        Set(ByVal value As LineNumberDockSides)
            zMarginLines_Side = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property MarginLinesStyle() As Drawing2D.DashStyle
        Get
            Return zMarginLines_Style
        End Get
        Set(ByVal value As Drawing2D.DashStyle)
            If value = Drawing2D.DashStyle.Custom Then value = Drawing2D.DashStyle.Solid
            zMarginLines_Style = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property MarginLinesThickness() As Single
        Get
            Return zMarginLines_Thickness
        End Get
        Set(ByVal value As Single)
            zMarginLines_Thickness = Math.Max(1, Math.Min(255, value))
            Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this property to enable LineNumbers for the chosen RichTextBox.")>
    <ComponentModel.Category("Add LineNumbers to")> Public Property ParentRichTextBox() As RichTextBox
        Get
            Return ZParent
        End Get
        Set(ByVal value As RichTextBox)
            ZParent = value
            If ZParent IsNot Nothing Then
                Parent = ZParent.Parent
                ZParent.Refresh()
            End If
            Text = ""
            Refresh()
            Invalidate()
        End Set
    End Property

    <ComponentModel.Description("The BackgroundGradient is a gradual blend of two colors, shown in the back of each LineNumber's item-area.")>
    <ComponentModel.Category("Additional Behavior")> Public Property ShowBackgroundGradient() As Boolean
        Get
            Return zGradient_Show
        End Get
        Set(ByVal value As Boolean)
            zGradient_Show = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Description("BorderLines are shown on all sides of the LineNumber control.")>
    <ComponentModel.Category("Additional Behavior")> Public Property ShowBorderLines() As Boolean
        Get
            Return zBorderLines_Show
        End Get
        Set(ByVal value As Boolean)
            zBorderLines_Show = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Description("GridLines are the horizontal divider-lines shown above each LineNumber.")>
    <ComponentModel.Category("Additional Behavior")> Public Property ShowGridLines() As Boolean
        Get
            Return zGridLines_Show
        End Get
        Set(ByVal value As Boolean)
            zGridLines_Show = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Behavior")> Public Property ShowLineNrs() As Boolean
        Get
            Return zLineNumbers_Show
        End Get
        Set(ByVal value As Boolean)
            zLineNumbers_Show = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.Description("MarginLines are shown on the Left or Right (or both in Height-mode) of the LineNumber control.")>
    <ComponentModel.Category("Additional Behavior")> Public Property ShowMarginLines() As Boolean
        Get
            Return zMarginLines_Show
        End Get
        Set(ByVal value As Boolean)
            zMarginLines_Show = value
            Invalidate()
        End Set
    End Property

    <ComponentModel.DefaultValue("")>
    <ComponentModel.AmbientValue("")>
    <ComponentModel.Browsable(False)>
    Public Overrides Property Text() As String
        Get
            Return MyBase.Text
        End Get
        Set(ByVal value As String)
            MyBase.Text = ""
            Invalidate()
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
        If zMax = zMin + 1 Or zMin = (zMax + zMin) \ 2 Then Exit Sub
        Select Case ZParent.GetPositionFromCharIndex((zMax + zMin) \ 2).Y
            Case Is = zTarget
                '   BestStartIndex found
                zMin = (zMax + zMin) \ 2
            Case Is > zTarget
                '   Look again, in lower half
                zMax = (zMax + zMin) \ 2
                FindStartIndex(zMin, zMax, zTarget)
            Case Is < 0
                '   Look again, in top half
                zMin = (zMax + zMin) \ 2
                FindStartIndex(zMin, zMax, zTarget)
        End Select
    End Sub

    ''' <summary>
    ''' This Sub will run whenever Me.Refresh() is called. It applies the AutoSizing and DockSide settings.
    ''' </summary>
    ''' <remarks></remarks>
    Private Sub Update_SizeAndPosition()
        If AutoSize = True Then Exit Sub
        If Me.Dock = DockStyle.Bottom Or Me.Dock = DockStyle.Fill Or Me.Dock = DockStyle.Top Then Exit Sub
        Dim zNewLocation As Point = Location, zNewSize As Size = Size

        If zAutoSizing = True Then
            Select Case True
                Case ZParent Is Nothing
                    ' --- ReminderMessage sizing
                    If zAutoSizing_Size.Width > 0 Then zNewSize.Width = zAutoSizing_Size.Width
                    If zAutoSizing_Size.Height > 0 Then zNewSize.Height = zAutoSizing_Size.Height
                    Size = zNewSize

                    '--- zParent isNot Nothing for the following cases
                Case Me.Dock = DockStyle.Left Or Me.Dock = DockStyle.Right
                    If zAutoSizing_Size.Width > 0 Then zNewSize.Width = zAutoSizing_Size.Width
                    Width = zNewSize.Width

                    ' --- DockSide is active L/R/H
                Case zDockSide <> LineNumberDockSides.None
                    If zAutoSizing_Size.Width > 0 Then zNewSize.Width = zAutoSizing_Size.Width
                    zNewSize.Height = ZParent.Height
                    If Me.zDockSide = LineNumberDockSides.Left Then zNewLocation.X = ZParent.Left - zNewSize.Width - 1
                    If Me.zDockSide = LineNumberDockSides.Right Then zNewLocation.X = ZParent.Right + 1
                    zNewLocation.Y = ZParent.Top
                    Location = zNewLocation
                    Size = zNewSize

                    ' --- DockSide = None, but AutoSizing is still setting the Width
                Case Me.zDockSide = LineNumberDockSides.None
                    If zAutoSizing_Size.Width > 0 Then zNewSize.Width = zAutoSizing_Size.Width
                    Size = zNewSize

            End Select
        Else
            ' --- No AutoSizing
            Select Case True
                Case ZParent Is Nothing
                    ' --- ReminderMessage sizing
                    If zAutoSizing_Size.Width > 0 Then zNewSize.Width = zAutoSizing_Size.Width
                    If zAutoSizing_Size.Height > 0 Then zNewSize.Height = zAutoSizing_Size.Height
                    Size = zNewSize

                    ' --- No AutoSizing, but DockSide L/R/H is active so height and position need updates.
                Case zDockSide <> LineNumberDockSides.None
                    zNewSize.Height = ZParent.Height
                    If Me.zDockSide = LineNumberDockSides.Left Then zNewLocation.X = ZParent.Left - zNewSize.Width - 1
                    If Me.zDockSide = LineNumberDockSides.Right Then zNewLocation.X = ZParent.Right + 1
                    zNewLocation.Y = ZParent.Top
                    Location = zNewLocation
                    Size = zNewSize

            End Select
        End If

    End Sub

    ''' <summary>
    ''' This Sub determines which textlines are visible in the ParentRichTextBox, and makes LineNumberItems (LineNumber + ItemRectangle)
    ''' for each visible line. They are put into the zLNIs List that will be used by the OnPaint event to draw the LineNumberItems.
    ''' </summary>
    ''' <remarks></remarks>
    Private Sub Update_VisibleLineNumberItems()
        zLNIs.Clear()
        zAutoSizing_Size = New Size(0, 0)
        zLineNumbersFormat = "0"  'initial setting
        '   To measure the LineNumber's width, its Format 0 is replaced by w as that is likely to be one of the widest characters in non-mono-space fonts.
        If zAutoSizing = True Then zAutoSizing_Size = New Size(TextRenderer.MeasureText(zLineNumbersFormat.Replace("0".ToCharArray, "W".ToCharArray, StringComparison.InvariantCulture), Font).Width, 0)

        If String.IsNullOrWhiteSpace(ZParent.Text) Then
            Exit Sub
        End If

        ' --- Make sure the LineNumbers are aligning to the same height as the zParent textlines by converting to screencoordinates
        '   and using that as an offset that gets added to the points for the LineNumberItems
        zPointInParent = ZParent.PointToScreen(ZParent.ClientRectangle.Location)
        zPointInMe = PointToScreen(New Point(0, 0))
        '   zParentInMe is the vertical offset to make the LineNumberItems line up with the textlines in zParent.
        zParentInMe = zPointInParent.Y - zPointInMe.Y + 1
        '   The first visible LineNumber may not be the first visible line of text in the RTB if the LineNumbercontrol's .Top is lower on the form than
        '   the .Top of the parent RichTextBox. Therefor, zPointInParent will now be used to find zPointInMe's equivalent height in zParent,
        '   which is needed to find the best StartIndex later on.
        zPointInParent = ZParent.PointToClient(zPointInMe)

        ' --- NOTES:
        '   Additional complication is the fact that when wordwrap is enabled on the RTB, the wordwrapped text spills into the RTB.Lines collection,
        '   so we need to split the text into lines ourselves, and use the Index of each zSplit-line's first character instead of the RTB's.
        Dim zSplit() As String = ZParent.Text.Split(vbCrLf.ToCharArray)

        If zSplit.Length < 2 Then
            '   Just one line in the text = one linenumber
            '   NOTE:  zContentRectangle is built by the zParent.ContentsResized event.
            Dim zPoint As Point = ZParent.GetPositionFromCharIndex(0)
            zLNIs.Add(New LineNumberItem(1, New Rectangle(New Point(0, zPoint.Y - 1 + zParentInMe), New Size(Width, zContentRectangle.Height - zPoint.Y))))
        Else

            '   Multiple lines, but store only those LineNumberItems for lines that are visible.
            Dim zTimeSpan As New TimeSpan(Now.Ticks)
            Dim zPoint As Point
            Dim zStartIndex As Integer = 0
            Dim zA As Integer = ZParent.Text.Length - 1
            FindStartIndex(zStartIndex, zA, zPointInParent.Y)

            '   zStartIndex now holds the index of a character in the first visible line from zParent.Text
            '   Now it will be pointed at the first character of that line (chr(10) = Linefeed part of the vbCrLf constant)
            zStartIndex = Math.Max(0, Math.Min(ZParent.Text.Length - 1, ZParent.Text.Substring(0, zStartIndex).LastIndexOf(Chr(10)) + 1))

            '   We now need to find out which zSplit-line that character is in, by counting the vbCrlf appearances that come before it.
            Dim zSplitStartLine As Integer = Math.Max(0, ZParent.Text.Substring(0, zStartIndex).Split(vbCrLf.ToCharArray).Length - 1)

            '   zStartIndex starts off pointing at the first character of the first visible line, and will be then be pointed to
            '   the index of the first character on the next line.
            For zA = zSplitStartLine To zSplit.Length - 1
                zPoint = ZParent.GetPositionFromCharIndex(zStartIndex)
                zStartIndex += Math.Max(1, zSplit(zA).Length + 1)
                If zPoint.Y + zParentInMe > Height Then Exit For
                '   For performance reasons, the list of LineNumberItems (zLNIs) is first built with only the location of its
                '   itemrectangle being used. The height of those rectangles will be computed afterwards by comparing the items' Y coordinates.
                zLNIs.Add(New LineNumberItem(zA + 1, New Rectangle(0, zPoint.Y - 1 + zParentInMe, Width, 1)))
                If zParentIsScrolling = True AndAlso Now.Ticks > zTimeSpan.Ticks + 500000 Then
                    '   The more lines there are in the RTB, the slower the RTB's .GetPositionFromCharIndex() method becomes
                    '   To avoid those delays from interfering with the scrollingspeed, this speedbased exit for is applied (0.05 sec)
                    '   zLNIs will have at least 1 item, and if that's the only one, then change its location to 0,0 to make it readable
                    If zLNIs.Count = 1 Then zLNIs(0).Rectangle.Y = 0
                    zParentIsScrolling = False
                    ZTimer.Start()
                    Exit For
                End If
            Next

            If zLNIs.Count = 0 Then Exit Sub

            '   Add an extra placeholder item to the end, to make the heightcomputation easier
            If zA < zSplit.Length Then
                '   getting here means the for/next loop was exited before reaching the last zSplit textline
                '   zStartIndex will still be pointing to the startcharacter of the next line, so we can use that:
                zPoint = ZParent.GetPositionFromCharIndex(Math.Min(zStartIndex, ZParent.Text.Length - 1))
                zLNIs.Add(New LineNumberItem(-1, New Rectangle(0, zPoint.Y - 1 + zParentInMe, 0, 0)))
            Else
                '   getting here means the for/next loop ran to the end (zA is now zSplit.Length).
                zLNIs.Add(New LineNumberItem(-1, New Rectangle(0, zContentRectangle.Bottom, 0, 0)))
            End If

            '   And now we can easily compute the height of the LineNumberItems by comparing each item's Y coordinate with that of the next line.
            '   There's at least two items in the list, and the last item is a "nextline-placeholder" that will be removed.
            For zA = 0 To zLNIs.Count - 2
                zLNIs(zA).Rectangle.Height = Math.Max(1, zLNIs(zA + 1).Rectangle.Y - zLNIs(zA).Rectangle.Y)
            Next
            '   Removing the placeholder item
            zLNIs.RemoveAt(zLNIs.Count - 1)

            ' Set the Format to the width of the highest possible number so that LeadingZeroes shows the correct amount of zeros.
            zLineNumbersFormat = If(zLineNumbers_ShowAsHexadecimal = True, "".PadRight(zSplit.Length.ToString("X", Globalization.CultureInfo.InvariantCulture).Length, "0"c), "".PadRight(zSplit.Length.ToString(Globalization.CultureInfo.InvariantCulture).Length, "0"c))
        End If

        '   To measure the LineNumber's width, its Format 0 is replaced by w as that is likely to be one of the widest characters in non-mono-space fonts.
        If zAutoSizing = True Then zAutoSizing_Size = New Size(TextRenderer.MeasureText(zLineNumbersFormat.Replace("0".ToCharArray, "W".ToCharArray, StringComparison.InvariantCulture), Font).Width, 0)
    End Sub

    Private Sub ZParent_Changed(ByVal sender As Object, ByVal e As EventArgs) Handles ZParent.LocationChanged, ZParent.Move, ZParent.Resize, ZParent.DockChanged, ZParent.TextChanged, ZParent.MultilineChanged
        Refresh()
        Invalidate()
    End Sub

    Private Sub ZParent_ContentsResized(ByVal sender As Object, ByVal e As ContentsResizedEventArgs) Handles ZParent.ContentsResized
        zContentRectangle = e.NewRectangle
        Refresh()
        Invalidate()
    End Sub

    Private Sub ZParent_Disposed(ByVal sender As Object, ByVal e As EventArgs) Handles ZParent.Disposed
        ParentRichTextBox = Nothing
        Refresh()
        Invalidate()
    End Sub

    Private Sub ZParent_Scroll(ByVal sender As Object, ByVal e As EventArgs) Handles ZParent.HScroll, ZParent.VScroll
        zParentIsScrolling = True
        Invalidate()
    End Sub

    Private Sub ZTimer_Tick(ByVal sender As Object, ByVal e As EventArgs) Handles ZTimer.Tick
        zParentIsScrolling = False
        ZTimer.Stop()
        Invalidate()
    End Sub

    Protected Overrides Sub OnHandleCreated(ByVal e As EventArgs)
        MyBase.OnHandleCreated(e)
        AutoSize = False
    End Sub

    Protected Overrides Sub OnLocationChanged(ByVal e As EventArgs)
        If DesignMode = True Then Refresh()
        MyBase.OnLocationChanged(e)
        Invalidate()
    End Sub

    ''' <summary>
    ''' OnPaint will go through the enabled elements (vertical ReminderMessage, GridLines, LineNumbers, BorderLines, MarginLines) and will
    ''' draw them if enabled. At the same time, it will build GraphicsPaths for each of those elements (that are enabled), which will be used
    ''' in SeeThroughMode (if it's active): the figures in the GraphicsPaths will form a customized outline for the control by setting them as the
    ''' Region of the LineNumber control. Note: the vertical ReminderMessages are only drawn during design time.
    ''' </summary>
    ''' <param name="e"></param>
    ''' <remarks></remarks>
    Protected Overrides Sub OnPaint(ByVal e As PaintEventArgs)
        Contracts.Contract.Requires(e IsNot Nothing)
        '   Build the list of visible LineNumberItems (= zLNIs) first. (doesn't take long, so it can stay in OnPaint)
        Update_VisibleLineNumberItems()
        MyBase.OnPaint(e)

        ' --- QualitySettings
        e.Graphics.TextRenderingHint = If(zLineNumbers_AntiAlias = True, Drawing.Text.TextRenderingHint.AntiAlias, Drawing.Text.TextRenderingHint.SystemDefault)

        Dim zGP_LineNumbers As New Drawing2D.GraphicsPath(Drawing2D.FillMode.Winding)
        Dim zTextSize As SizeF
        Dim zPoint As New Point(0, 0)
        Using zSF As New StringFormat
            ' ----------------------------------------------
            ' --- DESIGNTIME / NO VISIBLE ITEMS
            If DesignMode = True Then
                Dim zReminderToShow As String = ""
                '   Show a vertical reminder message
                If ZParent Is Nothing Then
                    zReminderToShow = "-!- Set ParentRichTextBox -!-"
                Else
                    If zLNIs.Count = 0 Then zReminderToShow = "LineNrs (  " & ZParent.Name & "  )"
                End If
                If zReminderToShow.Length > 0 Then
                    ' --- Centering and Rotation for the reminder message
                    e.Graphics.TranslateTransform(CSng(Width / 2), CSng(Height / 2))
                    e.Graphics.RotateTransform(-90)
                    zSF.Alignment = StringAlignment.Center
                    zSF.LineAlignment = StringAlignment.Center
                    ' --- Show the reminder message (with small shadow)
                    zTextSize = e.Graphics.MeasureString(zReminderToShow, Font, zPoint, zSF)
                    e.Graphics.DrawString(zReminderToShow, Font, Brushes.WhiteSmoke, 1, 1, zSF)
                    e.Graphics.DrawString(zReminderToShow, Font, Brushes.Firebrick, 0, 0, zSF)
                    e.Graphics.ResetTransform()

                    Dim zReminderRectangle As New Rectangle(CInt((Width / 2) - (zTextSize.Height / 2)), CInt((Height / 2) - (zTextSize.Width / 2)), CInt(zTextSize.Height), CInt(zTextSize.Width))
                    zGP_LineNumbers.AddRectangle(zReminderRectangle)
                    zGP_LineNumbers.CloseFigure()

                    If zAutoSizing = True Then
                        zReminderRectangle.Inflate(CInt(zTextSize.Height * 0.2), CInt(zTextSize.Width * 0.1))
                        zAutoSizing_Size = New Size(zReminderRectangle.Width, zReminderRectangle.Height)
                    End If
                End If
            End If

            Dim zPen As New Pen(ForeColor)

            '   NOTE: The GraphicsPaths are only used for SeeThroughMode
            '   FillMode.Winding: combined outline ( Alternate: XOR'ed outline )
            Dim zGP_GridLines As New Drawing2D.GraphicsPath(Drawing2D.FillMode.Winding)
            Dim zBrush As New SolidBrush(ForeColor)
            ' ----------------------------------------------
            ' --- DESIGN OR RUNTIME / WITH VISIBLE ITEMS (which means zParent exists)
            If zLNIs.Count > 0 Then
                '   The visible LineNumberItems with their BackgroundGradient and GridLines
                '   Loop through every visible LineNumberItem
                Dim zLGB As Drawing2D.LinearGradientBrush = Nothing
                zPen = New Pen(zGridLines_Color, zGridLines_Thickness) With {
                                    .DashStyle = zGridLines_Style
                                }
                zSF.Alignment = StringAlignment.Near
                zSF.LineAlignment = StringAlignment.Near
                zSF.FormatFlags = CType(StringFormatFlags.FitBlackBox + StringFormatFlags.NoClip + StringFormatFlags.NoWrap, StringFormatFlags)

                For zA As Integer = 0 To zLNIs.Count - 1

                    ' --- BackgroundGradient
                    If zGradient_Show = True Then
                        zLGB = New Drawing2D.LinearGradientBrush(zLNIs(zA).Rectangle, zGradient_StartColor, zGradient_EndColor, zGradient_Direction)
                        e.Graphics.FillRectangle(zLGB, zLNIs(zA).Rectangle)
                    End If

                    ' --- GridLines
                    If zGridLines_Show = True Then
                        e.Graphics.DrawLine(zPen, New Point(0, zLNIs(zA).Rectangle.Y), New Point(Width, zLNIs(zA).Rectangle.Y))

                        '   NOTE: Every item in a GraphicsPath is a closed figure, so instead of adding gridlines as lines, we'll add them
                        '   as rectangles that loop out of sight. Their height uses the zContentRectangle which is the maxsize of
                        '   the ParentRichTextBox's contents.
                        '   NOTE: Slight adjustment needed when the first item has a negative Y coordinate.
                        '   This explains the " - zLNIs(0).Rectangle.Y" (which adds the negative size to the height
                        '   to make sure the rectangle's bottompart stays out of sight)
                        zGP_GridLines.AddRectangle(New Rectangle(CInt(-zGridLines_Thickness), zLNIs(zA).Rectangle.Y, CInt(Width + (zGridLines_Thickness * 2)), CInt(Height - zLNIs(0).Rectangle.Y + zGridLines_Thickness)))
                        zGP_GridLines.CloseFigure()
                    End If

                    ' --- LineNumbers
                    If zLineNumbers_Show = True Then
                        ' --- Local Declarations
                        '   TextFormatting
                        Dim zTextToShow As String = If(zLineNumbers_ShowLeadingZeroes, If(zLineNumbers_ShowAsHexadecimal, zLNIs(zA).LineNumber.ToString("X", Globalization.CultureInfo.InvariantCulture), zLNIs(zA).LineNumber.ToString(zLineNumbersFormat, Globalization.CultureInfo.InvariantCulture)), If(zLineNumbers_ShowAsHexadecimal, zLNIs(zA).LineNumber.ToString("X", Globalization.CultureInfo.InvariantCulture), zLNIs(zA).LineNumber.ToString(Globalization.CultureInfo.InvariantCulture)))
                        '   TextSizing
                        zTextSize = e.Graphics.MeasureString(zTextToShow, Font, zPoint, zSF)
                        '   TextAlignment and positioning   (zPoint = TopLeftCornerPoint of the text)
                        '   TextAlignment, padding, manual offset (via LineNrs_Offset) and zTextSize are all included in the calculation of zPoint.
                        Select Case zLineNumbers_Alignment
                            Case ContentAlignment.TopLeft
                                zPoint = New Point(zLNIs(zA).Rectangle.Left + Padding.Left + zLineNumbers_Offset.Width, zLNIs(zA).Rectangle.Top + Padding.Top + zLineNumbers_Offset.Height)
                            Case ContentAlignment.MiddleLeft
                                zPoint = New Point(zLNIs(zA).Rectangle.Left + Padding.Left + zLineNumbers_Offset.Width, CInt(zLNIs(zA).Rectangle.Top + (zLNIs(zA).Rectangle.Height / 2) + zLineNumbers_Offset.Height - (zTextSize.Height / 2)))
                            Case ContentAlignment.BottomLeft
                                zPoint = New Point(zLNIs(zA).Rectangle.Left + Padding.Left + zLineNumbers_Offset.Width, CInt(zLNIs(zA).Rectangle.Bottom - Padding.Bottom + 1 + zLineNumbers_Offset.Height - zTextSize.Height))
                            Case ContentAlignment.TopCenter
                                zPoint = New Point(CInt((zLNIs(zA).Rectangle.Width / 2) + zLineNumbers_Offset.Width - (zTextSize.Width / 2)), zLNIs(zA).Rectangle.Top + Padding.Top + zLineNumbers_Offset.Height)
                            Case ContentAlignment.MiddleCenter
                                zPoint = New Point(CInt((zLNIs(zA).Rectangle.Width / 2) + zLineNumbers_Offset.Width - (zTextSize.Width / 2)), CInt(zLNIs(zA).Rectangle.Top + (zLNIs(zA).Rectangle.Height / 2) + zLineNumbers_Offset.Height - (zTextSize.Height / 2)))
                            Case ContentAlignment.BottomCenter
                                zPoint = New Point(CInt((zLNIs(zA).Rectangle.Width / 2) + zLineNumbers_Offset.Width - (zTextSize.Width / 2)), CInt(zLNIs(zA).Rectangle.Bottom - Padding.Bottom + 1 + zLineNumbers_Offset.Height - zTextSize.Height))
                            Case ContentAlignment.TopRight
                                zPoint = New Point(CInt(zLNIs(zA).Rectangle.Right - Padding.Right + zLineNumbers_Offset.Width - zTextSize.Width), zLNIs(zA).Rectangle.Top + Padding.Top + zLineNumbers_Offset.Height)
                            Case ContentAlignment.MiddleRight
                                zPoint = New Point(CInt(zLNIs(zA).Rectangle.Right - Padding.Right + zLineNumbers_Offset.Width - zTextSize.Width), CInt(zLNIs(zA).Rectangle.Top + (zLNIs(zA).Rectangle.Height / 2) + zLineNumbers_Offset.Height - (zTextSize.Height / 2)))
                            Case ContentAlignment.BottomRight
                                zPoint = New Point(CInt(zLNIs(zA).Rectangle.Right - Padding.Right + zLineNumbers_Offset.Width - zTextSize.Width), CInt(zLNIs(zA).Rectangle.Bottom - Padding.Bottom + 1 + zLineNumbers_Offset.Height - zTextSize.Height))
                        End Select
                        '   TextClipping
                        Dim zItemClipRectangle As Rectangle = New Rectangle(zPoint, zTextSize.ToSize)
                        If zLineNumbers_ClipByItemRectangle = True Then
                            '   If selected, the text will be clipped so that it doesn't spill out of its own LineNumberItem-area.
                            '   Only the part of the text inside the LineNumberItem.Rectangle should be visible, so intersect with the ItemRectangle
                            '   The SetClip method temporary restricts the drawing area of the control for whatever is drawn next.
                            zItemClipRectangle.Intersect(zLNIs(zA).Rectangle)
                            e.Graphics.SetClip(zItemClipRectangle)
                        End If
                        '   TextDrawing
                        e.Graphics.DrawString(zTextToShow, Font, zBrush, zPoint, zSF)
                        e.Graphics.ResetClip()
                        '   The GraphicsPath for the LineNumber is just a rectangle behind the text, to keep the paintingspeed high and avoid ugly artifacts.
                        zGP_LineNumbers.AddRectangle(zItemClipRectangle)
                        zGP_LineNumbers.CloseFigure()
                    End If
                Next

                ' --- GridLinesThickness and Linestyle in SeeThroughMode. All GraphicsPath lines are drawn as solid to keep the paintingspeed high.
                If zGridLines_Show = True Then
                    zPen.DashStyle = Drawing2D.DashStyle.Solid
                    zGP_GridLines.Widen(zPen)
                End If

                ' --- Memory CleanUp
                If zLGB IsNot Nothing Then zLGB.Dispose()
            End If

            ' ----------------------------------------------
            ' --- DESIGN OR RUNTIME / ALWAYS
            Dim zP_Left As New Point(CInt(Math.Floor(zBorderLines_Thickness / 2)), CInt(Math.Floor(zBorderLines_Thickness / 2)))
            Dim zP_Right As New Point(CInt(Width - Math.Ceiling(zBorderLines_Thickness / 2)), CInt(Height - Math.Ceiling(zBorderLines_Thickness / 2)))

            ' --- BorderLines
            Dim zBorderLines_Points() As Point = {New Point(zP_Left.X, zP_Left.Y), New Point(zP_Right.X, zP_Left.Y), New Point(zP_Right.X, zP_Right.Y), New Point(zP_Left.X, zP_Right.Y), New Point(zP_Left.X, zP_Left.Y)}

            Dim zGP_BorderLines As New Drawing2D.GraphicsPath(Drawing2D.FillMode.Winding)
            If zBorderLines_Show = True Then
                zPen = New Pen(zBorderLines_Color, zBorderLines_Thickness) With {
                .DashStyle = zBorderLines_Style
            }
                e.Graphics.DrawLines(zPen, zBorderLines_Points)
                zGP_BorderLines.AddLines(zBorderLines_Points)
                zGP_BorderLines.CloseFigure()
                '   BorderThickness and Style for SeeThroughMode
                zPen.DashStyle = Drawing2D.DashStyle.Solid
                zGP_BorderLines.Widen(zPen)
            End If

            Dim zGP_MarginLines As New Drawing2D.GraphicsPath(Drawing2D.FillMode.Winding)
            ' --- MarginLines
            If zMarginLines_Show = True AndAlso zMarginLines_Side > LineNumberDockSides.None Then
                zP_Left = New Point(CInt(-zMarginLines_Thickness), CInt(-zMarginLines_Thickness))
                zP_Right = New Point(CInt(Width + zMarginLines_Thickness), CInt(Height + zMarginLines_Thickness))
                zPen = New Pen(zMarginLines_Color, zMarginLines_Thickness) With {
                .DashStyle = zMarginLines_Style
            }
                If Me.zMarginLines_Side = LineNumberDockSides.Left Or Me.zMarginLines_Side = LineNumberDockSides.Height Then
                    e.Graphics.DrawLine(zPen, New Point(CInt(Math.Floor(zMarginLines_Thickness / 2)), 0), New Point(CInt(Math.Floor(zMarginLines_Thickness / 2)), Height - 1))
                    zP_Left = New Point(CInt(Math.Ceiling(zMarginLines_Thickness / 2)), CInt(-zMarginLines_Thickness))
                End If
                If Me.zMarginLines_Side = LineNumberDockSides.Right Or Me.zMarginLines_Side = LineNumberDockSides.Height Then
                    e.Graphics.DrawLine(zPen, New Point(CInt(Width - Math.Ceiling(zMarginLines_Thickness / 2)), 0), New Point(CInt(Width - Math.Ceiling(zMarginLines_Thickness / 2)), Height - 1))
                    zP_Right = New Point(CInt(Width - Math.Ceiling(zMarginLines_Thickness / 2)), CInt(Height + zMarginLines_Thickness))
                End If
                '   GraphicsPath for the MarginLines(s):
                '   MarginLines(s) are drawn as a rectangle connecting the zP_Left and zP_Right points, which are either inside or
                '   outside of sight, depending on whether the MarginLines at that side is visible. zP_Left: TopLeft and ZP_Right: BottomRight
                zGP_MarginLines.AddRectangle(New Rectangle(zP_Left, New Size(zP_Right.X - zP_Left.X, zP_Right.Y - zP_Left.Y)))
                zPen.DashStyle = Drawing2D.DashStyle.Solid
                zGP_MarginLines.Widen(zPen)
            End If

            Dim zRegion As New Region(MyBase.ClientRectangle)
            ' ----------------------------------------------
            ' --- SeeThroughMode
            '   combine all the GraphicsPaths (= zGP_... ) and set them as the region for the control.
            If zSeeThroughMode = True Then
                zRegion.MakeEmpty()
                zRegion.Union(zGP_BorderLines)
                zRegion.Union(zGP_MarginLines)
                zRegion.Union(zGP_GridLines)
                zRegion.Union(zGP_LineNumbers)
            End If

            ' --- Region
            If zRegion.GetBounds(e.Graphics).IsEmpty = True Then
                '   Note: If the control is in a condition that would show it as empty, then a border-region is still drawn regardless of it's borders on/off state.
                '   This is added to make sure that the bounds of the control are never lost (it would remain empty if this was not done).
                zGP_BorderLines.AddLines(zBorderLines_Points)
                zGP_BorderLines.CloseFigure()
                zPen = New Pen(zBorderLines_Color, 1) With {
                .DashStyle = Drawing2D.DashStyle.Solid
            }
                zGP_BorderLines.Widen(zPen)

                zRegion = New Region(zGP_BorderLines)
            End If
            Region = zRegion
            ' ----------------------------------------------
            ' --- Memory CleanUp
            If zBrush IsNot Nothing Then zBrush.Dispose()
            If zPen IsNot Nothing Then zPen.Dispose()
            If zRegion IsNot Nothing Then zRegion.Dispose()
            If zGP_GridLines IsNot Nothing Then zGP_GridLines.Dispose()
            If zGP_BorderLines IsNot Nothing Then zGP_BorderLines.Dispose()
            If zGP_MarginLines IsNot Nothing Then zGP_MarginLines.Dispose()
        End Using
        If zGP_LineNumbers IsNot Nothing Then zGP_LineNumbers.Dispose()
    End Sub

    Protected Overrides Sub OnSizeChanged(ByVal e As EventArgs)
        If DesignMode = True Then Refresh()
        MyBase.OnSizeChanged(e)
        Invalidate()
    End Sub

    Public Overrides Sub Refresh()
        '   Note: don't change the order here, first the Mybase.Refresh, then the Update_SizeAndPosition.
        MyBase.Refresh()
        Update_SizeAndPosition()
    End Sub

    Private Class LineNumberItem
        Friend LineNumber As Integer
        Friend Rectangle As Rectangle

        Friend Sub New(ByVal zLineNumber As Integer, ByVal zRectangle As Rectangle)
            LineNumber = zLineNumber
            Rectangle = zRectangle
        End Sub

    End Class

End Class
