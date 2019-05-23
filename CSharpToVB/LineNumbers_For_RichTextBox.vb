Option Explicit On
Option Infer Off
Option Strict On

<ComponentModel.DefaultProperty("ParentRichTextBox")>
Public Class LineNumbers_For_RichTextBox : Inherits Control

    '//////////////////////////////////////////////////////////////////////////////////////////////////

    '   // Code provided "as is", with no rights attached, nor liabilities.
    '   //
    '   // Enjoy! --- nogChoco

    '//////////////////////////////////////////////////////////////////////////////////////////////////

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

    Private zDockSide As LineNumberDockSide = LineNumberDockSide.Left

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

    Private zLineNumbers_Format As String = "0"

    Private zLineNumbers_Offset As New Size(0, 0)

    Private zLineNumbers_Show As Boolean = True

    Private zLineNumbers_ShowAsHexadecimal As Boolean = False

    Private zLineNumbers_ShowLeadingZeroes As Boolean = True

    Private zMarginLines_Color As Color = Color.SlateGray

    Private zMarginLines_Show As Boolean = True

    Private zMarginLines_Side As LineNumberDockSide = LineNumberDockSide.Right

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
        With Me.ZTimer
            .Enabled = True
            .Interval = 200
            .Stop()
        End With
        Me.Update_SizeAndPosition()
        Me.Invalidate()
    End Sub

    <Flags>
    Public Enum LineNumberDockSide As Byte
        None = 0
        Left = 1
        Right = 2
        Height = 4
    End Enum

    <ComponentModel.Description("Use this property to enable the control to act as an overlay on top of the RichTextBox.")>
    <ComponentModel.Category("Additional Behavior")> Public Property SeeThroughMode_() As Boolean
        Get
            Return Me.zSeeThroughMode
        End Get
        Set(ByVal value As Boolean)
            Me.zSeeThroughMode = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Browsable(False)> Public Overrides Property AutoSize() As Boolean
        Get
            Return MyBase.AutoSize
        End Get
        Set(ByVal value As Boolean)
            MyBase.AutoSize = value
            Me.Invalidate()
        End Set
    End Property

    '//////////////////////////////////////////////////////////////////////////////////////////////////
    <ComponentModel.Description("Use this property to automatically resize the control (and reposition it if needed).")>
    <ComponentModel.Category("Additional Behavior")> Public Property AutoSizing() As Boolean
        Get
            Return Me.zAutoSizing
        End Get
        Set(ByVal value As Boolean)
            Me.zAutoSizing = value
            Me.Refresh()
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property BackgroundGradient_AlphaColor() As Color
        Get
            Return Me.zGradient_StartColor
        End Get
        Set(ByVal value As Color)
            Me.zGradient_StartColor = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property BackgroundGradient_BetaColor() As Color
        Get
            Return Me.zGradient_EndColor
        End Get
        Set(ByVal value As Color)
            Me.zGradient_EndColor = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property BackgroundGradient_Direction() As Drawing2D.LinearGradientMode
        Get
            Return Me.zGradient_Direction
        End Get
        Set(ByVal value As Drawing2D.LinearGradientMode)
            Me.zGradient_Direction = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property BorderLines_Color() As Color
        Get
            Return Me.zBorderLines_Color
        End Get
        Set(ByVal value As Color)
            Me.zBorderLines_Color = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property BorderLines_Style() As Drawing2D.DashStyle
        Get
            Return Me.zBorderLines_Style
        End Get
        Set(ByVal value As Drawing2D.DashStyle)
            If value = Drawing2D.DashStyle.Custom Then value = Drawing2D.DashStyle.Solid
            Me.zBorderLines_Style = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property BorderLines_Thickness() As Single
        Get
            Return Me.zBorderLines_Thickness
        End Get
        Set(ByVal value As Single)
            Me.zBorderLines_Thickness = Math.Max(1, Math.Min(255, value))
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this property to dock the LineNumbers to a chosen side of the chosen RichTextBox.")>
    <ComponentModel.Category("Additional Behavior")> Public Property DockSide() As LineNumberDockSide
        Get
            Return Me.zDockSide
        End Get
        Set(ByVal value As LineNumberDockSide)
            Me.zDockSide = value
            Me.Refresh()
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Browsable(True)> Public Overrides Property Font() As Font
        Get
            Return MyBase.Font
        End Get
        Set(ByVal value As Font)
            MyBase.Font = value
            Me.Refresh()
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property GridLines_Color() As Color
        Get
            Return Me.zGridLines_Color
        End Get
        Set(ByVal value As Color)
            Me.zGridLines_Color = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property GridLines_Style() As Drawing2D.DashStyle
        Get
            Return Me.zGridLines_Style
        End Get
        Set(ByVal value As Drawing2D.DashStyle)
            If value = Drawing2D.DashStyle.Custom Then value = Drawing2D.DashStyle.Solid
            Me.zGridLines_Style = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property GridLines_Thickness() As Single
        Get
            Return Me.zGridLines_Thickness
        End Get
        Set(ByVal value As Single)
            Me.zGridLines_Thickness = Math.Max(1, Math.Min(255, value))
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this to align the LineNumbers to a chosen corner (or center) within their item-area.")>
    <ComponentModel.Category("Additional Behavior")> Public Property LineNrs_Alignment() As ContentAlignment
        Get
            Return Me.zLineNumbers_Alignment
        End Get
        Set(ByVal value As ContentAlignment)
            Me.zLineNumbers_Alignment = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this to apply Anti-Aliasing to the LineNumbers (high quality). Some fonts will look better without it, though.")>
    <ComponentModel.Category("Additional Behavior")> Public Property LineNrs_AntiAlias() As Boolean
        Get
            Return Me.zLineNumbers_AntiAlias
        End Get
        Set(ByVal value As Boolean)
            Me.zLineNumbers_AntiAlias = value
            Me.Refresh()
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this to set whether the LineNumbers should be shown as hexadecimal values.")>
    <ComponentModel.Category("Additional Behavior")> Public Property LineNrs_AsHexadecimal() As Boolean
        Get
            Return Me.zLineNumbers_ShowAsHexadecimal
        End Get
        Set(ByVal value As Boolean)
            Me.zLineNumbers_ShowAsHexadecimal = value
            Me.Refresh()
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this to set whether the LineNumbers are allowed to spill out of their item-area, or should be clipped by it.")>
    <ComponentModel.Category("Additional Behavior")> Public Property LineNrs_ClippedByItemRectangle() As Boolean
        Get
            Return Me.zLineNumbers_ClipByItemRectangle
        End Get
        Set(ByVal value As Boolean)
            Me.zLineNumbers_ClipByItemRectangle = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this to set whether the LineNumbers should have leading zeros (based on the total amount of text lines).")>
    <ComponentModel.Category("Additional Behavior")> Public Property LineNrs_LeadingZeroes() As Boolean
        Get
            Return Me.zLineNumbers_ShowLeadingZeroes
        End Get
        Set(ByVal value As Boolean)
            Me.zLineNumbers_ShowLeadingZeroes = value
            Me.Refresh()
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this property to manually reposition the LineNumbers, relative to their current location.")>
    <ComponentModel.Category("Additional Behavior")> Public Property LineNrs_Offset() As Size
        Get
            Return Me.zLineNumbers_Offset
        End Get
        Set(ByVal value As Size)
            Me.zLineNumbers_Offset = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property MarginLines_Color() As Color
        Get
            Return Me.zMarginLines_Color
        End Get
        Set(ByVal value As Color)
            Me.zMarginLines_Color = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property MarginLines_Side() As LineNumberDockSide
        Get
            Return Me.zMarginLines_Side
        End Get
        Set(ByVal value As LineNumberDockSide)
            Me.zMarginLines_Side = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property MarginLines_Style() As Drawing2D.DashStyle
        Get
            Return Me.zMarginLines_Style
        End Get
        Set(ByVal value As Drawing2D.DashStyle)
            If value = Drawing2D.DashStyle.Custom Then value = Drawing2D.DashStyle.Solid
            Me.zMarginLines_Style = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Appearance")> Public Property MarginLines_Thickness() As Single
        Get
            Return Me.zMarginLines_Thickness
        End Get
        Set(ByVal value As Single)
            Me.zMarginLines_Thickness = Math.Max(1, Math.Min(255, value))
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Description("Use this property to enable LineNumbers for the chosen RichTextBox.")>
    <ComponentModel.Category("Add LineNumbers to")> Public Property ParentRichTextBox() As RichTextBox
        Get
            Return Me.ZParent
        End Get
        Set(ByVal value As RichTextBox)
            Me.ZParent = value
            If Me.ZParent IsNot Nothing Then
                Me.Parent = Me.ZParent.Parent
                Me.ZParent.Refresh()
            End If
            Me.Text = ""
            Me.Refresh()
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Description("The BackgroundGradient is a gradual blend of two colors, shown in the back of each LineNumber's item-area.")>
    <ComponentModel.Category("Additional Behavior")> Public Property Show_BackgroundGradient() As Boolean
        Get
            Return Me.zGradient_Show
        End Get
        Set(ByVal value As Boolean)
            Me.zGradient_Show = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Description("BorderLines are shown on all sides of the LineNumber control.")>
    <ComponentModel.Category("Additional Behavior")> Public Property Show_BorderLines() As Boolean
        Get
            Return Me.zBorderLines_Show
        End Get
        Set(ByVal value As Boolean)
            Me.zBorderLines_Show = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Description("GridLines are the horizontal divider-lines shown above each LineNumber.")>
    <ComponentModel.Category("Additional Behavior")> Public Property Show_GridLines() As Boolean
        Get
            Return Me.zGridLines_Show
        End Get
        Set(ByVal value As Boolean)
            Me.zGridLines_Show = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Category("Additional Behavior")> Public Property Show_LineNrs() As Boolean
        Get
            Return Me.zLineNumbers_Show
        End Get
        Set(ByVal value As Boolean)
            Me.zLineNumbers_Show = value
            Me.Invalidate()
        End Set
    End Property

    <ComponentModel.Description("MarginLines are shown on the Left or Right (or both in Height-mode) of the LineNumber control.")>
    <ComponentModel.Category("Additional Behavior")> Public Property Show_MarginLines() As Boolean
        Get
            Return Me.zMarginLines_Show
        End Get
        Set(ByVal value As Boolean)
            Me.zMarginLines_Show = value
            Me.Invalidate()
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
        If zMax = zMin + 1 Or zMin = (zMax + zMin) \ 2 Then Exit Sub
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
        If Me.AutoSize = True Then Exit Sub
        If Me.Dock = DockStyle.Bottom Or Me.Dock = DockStyle.Fill Or Me.Dock = DockStyle.Top Then Exit Sub
        Dim zNewLocation As Point = Me.Location, zNewSize As Size = Me.Size

        If Me.zAutoSizing = True Then
            Select Case True
                Case Me.ZParent Is Nothing
                    ' --- ReminderMessage sizing
                    If Me.zAutoSizing_Size.Width > 0 Then zNewSize.Width = Me.zAutoSizing_Size.Width
                    If Me.zAutoSizing_Size.Height > 0 Then zNewSize.Height = Me.zAutoSizing_Size.Height
                    Me.Size = zNewSize

                    '--- zParent isNot Nothing for the following cases
                Case Me.Dock = DockStyle.Left Or Me.Dock = DockStyle.Right
                    If Me.zAutoSizing_Size.Width > 0 Then zNewSize.Width = Me.zAutoSizing_Size.Width
                    Me.Width = zNewSize.Width

                    ' --- DockSide is active L/R/H
                Case Me.zDockSide <> LineNumberDockSide.None
                    If Me.zAutoSizing_Size.Width > 0 Then zNewSize.Width = Me.zAutoSizing_Size.Width
                    zNewSize.Height = Me.ZParent.Height
                    If Me.zDockSide = LineNumberDockSide.Left Then zNewLocation.X = Me.ZParent.Left - zNewSize.Width - 1
                    If Me.zDockSide = LineNumberDockSide.Right Then zNewLocation.X = Me.ZParent.Right + 1
                    zNewLocation.Y = Me.ZParent.Top
                    Me.Location = zNewLocation
                    Me.Size = zNewSize

                    ' --- DockSide = None, but AutoSizing is still setting the Width
                Case Me.zDockSide = LineNumberDockSide.None
                    If Me.zAutoSizing_Size.Width > 0 Then zNewSize.Width = Me.zAutoSizing_Size.Width
                    Me.Size = zNewSize

            End Select
        Else
            ' --- No AutoSizing
            Select Case True
                Case Me.ZParent Is Nothing
                    ' --- ReminderMessage sizing
                    If Me.zAutoSizing_Size.Width > 0 Then zNewSize.Width = Me.zAutoSizing_Size.Width
                    If Me.zAutoSizing_Size.Height > 0 Then zNewSize.Height = Me.zAutoSizing_Size.Height
                    Me.Size = zNewSize

                    ' --- No AutoSizing, but DockSide L/R/H is active so height and position need updates.
                Case Me.zDockSide <> LineNumberDockSide.None
                    zNewSize.Height = Me.ZParent.Height
                    If Me.zDockSide = LineNumberDockSide.Left Then zNewLocation.X = Me.ZParent.Left - zNewSize.Width - 1
                    If Me.zDockSide = LineNumberDockSide.Right Then zNewLocation.X = Me.ZParent.Right + 1
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
        Me.zLNIs.Clear()
        Me.zAutoSizing_Size = New Size(0, 0)
        Me.zLineNumbers_Format = "0"  'initial setting
        '   To measure the LineNumber's width, its Format 0 is replaced by w as that is likely to be one of the widest characters in non-mono-space fonts.
        If Me.zAutoSizing = True Then Me.zAutoSizing_Size = New Size(TextRenderer.MeasureText(Me.zLineNumbers_Format.Replace("0".ToCharArray, "W".ToCharArray), Me.Font).Width, 0)

        If Me.ZParent Is Nothing OrElse Me.ZParent.Text.IsEmptyNullOrWhitespace Then
            Exit Sub
        End If

        ' --- Make sure the LineNumbers are aligning to the same height as the zParent textlines by converting to screencoordinates
        '   and using that as an offset that gets added to the points for the LineNumberItems
        Me.zPointInParent = Me.ZParent.PointToScreen(Me.ZParent.ClientRectangle.Location)
        Me.zPointInMe = Me.PointToScreen(New Point(0, 0))
        '   zParentInMe is the vertical offset to make the LineNumberItems line up with the textlines in zParent.
        Me.zParentInMe = Me.zPointInParent.Y - Me.zPointInMe.Y + 1
        '   The first visible LineNumber may not be the first visible line of text in the RTB if the LineNumbercontrol's .Top is lower on the form than
        '   the .Top of the parent RichTextBox. Therefor, zPointInParent will now be used to find zPointInMe's equivalent height in zParent,
        '   which is needed to find the best StartIndex later on.
        Me.zPointInParent = Me.ZParent.PointToClient(Me.zPointInMe)

        ' --- NOTES:
        '   Additional complication is the fact that when wordwrap is enabled on the RTB, the wordwrapped text spills into the RTB.Lines collection,
        '   so we need to split the text into lines ourselves, and use the Index of each zSplit-line's first character instead of the RTB's.
        Dim zSplit() As String = Me.ZParent.Text.Split(vbCrLf.ToCharArray)

        If zSplit.Length < 2 Then
            '   Just one line in the text = one linenumber
            '   NOTE:  zContentRectangle is built by the zParent.ContentsResized event.
            Dim zPoint As Point = Me.ZParent.GetPositionFromCharIndex(0)
            Me.zLNIs.Add(New LineNumberItem(1, New Rectangle(New Point(0, zPoint.Y - 1 + Me.zParentInMe), New Size(Me.Width, Me.zContentRectangle.Height - zPoint.Y))))
        Else

            '   Multiple lines, but store only those LineNumberItems for lines that are visible.
            Dim zTimeSpan As New TimeSpan(Now.Ticks)
            Dim zPoint As Point
            Dim zStartIndex As Integer = 0
            Dim zA As Integer = Me.ZParent.Text.Length - 1
            Me.FindStartIndex(zStartIndex, zA, Me.zPointInParent.Y)

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
                If zPoint.Y + Me.zParentInMe > Me.Height Then Exit For
                '   For performance reasons, the list of LineNumberItems (zLNIs) is first built with only the location of its
                '   itemrectangle being used. The height of those rectangles will be computed afterwards by comparing the items' Y coordinates.
                Me.zLNIs.Add(New LineNumberItem(zA + 1, New Rectangle(0, zPoint.Y - 1 + Me.zParentInMe, Me.Width, 1)))
                If Me.zParentIsScrolling = True AndAlso Now.Ticks > zTimeSpan.Ticks + 500000 Then
                    '   The more lines there are in the RTB, the slower the RTB's .GetPositionFromCharIndex() method becomes
                    '   To avoid those delays from interfering with the scrollingspeed, this speedbased exit for is applied (0.05 sec)
                    '   zLNIs will have at least 1 item, and if that's the only one, then change its location to 0,0 to make it readable
                    If Me.zLNIs.Count = 1 Then Me.zLNIs(0).Rectangle.Y = 0
                    Me.zParentIsScrolling = False
                    Me.ZTimer.Start()
                    Exit For
                End If
            Next

            If Me.zLNIs.Count = 0 Then Exit Sub

            '   Add an extra placeholder item to the end, to make the heightcomputation easier
            If zA < zSplit.Length Then
                '   getting here means the for/next loop was exited before reaching the last zSplit textline
                '   zStartIndex will still be pointing to the startcharacter of the next line, so we can use that:
                zPoint = Me.ZParent.GetPositionFromCharIndex(Math.Min(zStartIndex, Me.ZParent.Text.Length - 1))
                Me.zLNIs.Add(New LineNumberItem(-1, New Rectangle(0, zPoint.Y - 1 + Me.zParentInMe, 0, 0)))
            Else
                '   getting here means the for/next loop ran to the end (zA is now zSplit.Length).
                Me.zLNIs.Add(New LineNumberItem(-1, New Rectangle(0, Me.zContentRectangle.Bottom, 0, 0)))
            End If

            '   And now we can easily compute the height of the LineNumberItems by comparing each item's Y coordinate with that of the next line.
            '   There's at least two items in the list, and the last item is a "nextline-placeholder" that will be removed.
            For zA = 0 To Me.zLNIs.Count - 2
                Me.zLNIs(zA).Rectangle.Height = Math.Max(1, Me.zLNIs(zA + 1).Rectangle.Y - Me.zLNIs(zA).Rectangle.Y)
            Next
            '   Removing the placeholder item
            Me.zLNIs.RemoveAt(Me.zLNIs.Count - 1)

            ' Set the Format to the width of the highest possible number so that LeadingZeroes shows the correct amount of zeros.
            Me.zLineNumbers_Format = If(Me.zLineNumbers_ShowAsHexadecimal = True, "".PadRight(zSplit.Length.ToString("X").Length, "0"c), "".PadRight(zSplit.Length.ToString.Length, "0"c))
        End If

        '   To measure the LineNumber's width, its Format 0 is replaced by w as that is likely to be one of the widest characters in non-mono-space fonts.
        If Me.zAutoSizing = True Then Me.zAutoSizing_Size = New Size(TextRenderer.MeasureText(Me.zLineNumbers_Format.Replace("0".ToCharArray, "W".ToCharArray), Me.Font).Width, 0)
    End Sub

    Private Sub ZParent_Changed(ByVal sender As Object, ByVal e As EventArgs) Handles ZParent.LocationChanged, ZParent.Move, ZParent.Resize, ZParent.DockChanged, ZParent.TextChanged, ZParent.MultilineChanged
        Me.Refresh()
        Me.Invalidate()
    End Sub

    Private Sub ZParent_ContentsResized(ByVal sender As Object, ByVal e As ContentsResizedEventArgs) Handles ZParent.ContentsResized
        Me.zContentRectangle = e.NewRectangle
        Me.Refresh()
        Me.Invalidate()
    End Sub

    Private Sub ZParent_Disposed(ByVal sender As Object, ByVal e As EventArgs) Handles ZParent.Disposed
        Me.ParentRichTextBox = Nothing
        Me.Refresh()
        Me.Invalidate()
    End Sub

    Private Sub ZParent_Scroll(ByVal sender As Object, ByVal e As EventArgs) Handles ZParent.HScroll, ZParent.VScroll
        Me.zParentIsScrolling = True
        Me.Invalidate()
    End Sub

    Private Sub ZTimer_Tick(ByVal sender As Object, ByVal e As EventArgs) Handles ZTimer.Tick
        Me.zParentIsScrolling = False
        Me.ZTimer.Stop()
        Me.Invalidate()
    End Sub

    '//////////////////////////////////////////////////////////////////////////////////////////////////
    Protected Overrides Sub OnHandleCreated(ByVal e As EventArgs)
        MyBase.OnHandleCreated(e)
        Me.AutoSize = False
    End Sub

    Protected Overrides Sub OnLocationChanged(ByVal e As EventArgs)
        If Me.DesignMode = True Then Me.Refresh()
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
    Protected Overrides Sub OnPaint(ByVal e As PaintEventArgs)
        '   Build the list of visible LineNumberItems (= zLNIs) first. (doesn't take long, so it can stay in OnPaint)
        Me.Update_VisibleLineNumberItems()
        MyBase.OnPaint(e)

        ' --- QualitySettings
        e.Graphics.TextRenderingHint = If(Me.zLineNumbers_AntiAlias = True, Drawing.Text.TextRenderingHint.AntiAlias, Drawing.Text.TextRenderingHint.SystemDefault)

        ' --- Local Declarations
        Dim zTextToShow As String
        Dim zReminderToShow As String = ""
        Dim zSF As New StringFormat
        Dim zTextSize As SizeF
        Dim zPen As New Pen(Me.ForeColor)
        Dim zBrush As New SolidBrush(Me.ForeColor)
        Dim zPoint As New Point(0, 0)
        Dim zItemClipRectangle As Rectangle

        '   NOTE: The GraphicsPaths are only used for SeeThroughMode
        '   FillMode.Winding: combined outline ( Alternate: XOR'ed outline )
        Dim zGP_GridLines As New Drawing2D.GraphicsPath(Drawing2D.FillMode.Winding)
        Dim zGP_BorderLines As New Drawing2D.GraphicsPath(Drawing2D.FillMode.Winding)
        Dim zGP_MarginLines As New Drawing2D.GraphicsPath(Drawing2D.FillMode.Winding)
        Dim zGP_LineNumbers As New Drawing2D.GraphicsPath(Drawing2D.FillMode.Winding)
        Dim zRegion As New Region(MyBase.ClientRectangle)

        ' ----------------------------------------------
        ' --- DESIGNTIME / NO VISIBLE ITEMS
        If Me.DesignMode = True Then
            '   Show a vertical reminder message
            If Me.ZParent Is Nothing Then
                zReminderToShow = "-!- Set ParentRichTextBox -!-"
            Else
                If Me.zLNIs.Count = 0 Then zReminderToShow = "LineNrs (  " & Me.ZParent.Name & "  )"
            End If
            If zReminderToShow.Length > 0 Then
                ' --- Centering and Rotation for the reminder message
                e.Graphics.TranslateTransform(CSng(Me.Width / 2), CSng(Me.Height / 2))
                e.Graphics.RotateTransform(-90)
                zSF.Alignment = StringAlignment.Center
                zSF.LineAlignment = StringAlignment.Center
                ' --- Show the reminder message (with small shadow)
                zTextSize = e.Graphics.MeasureString(zReminderToShow, Me.Font, zPoint, zSF)
                e.Graphics.DrawString(zReminderToShow, Me.Font, Brushes.WhiteSmoke, 1, 1, zSF)
                e.Graphics.DrawString(zReminderToShow, Me.Font, Brushes.Firebrick, 0, 0, zSF)
                e.Graphics.ResetTransform()

                Dim zReminderRectangle As New Rectangle(CInt((Me.Width / 2) - (zTextSize.Height / 2)), CInt((Me.Height / 2) - (zTextSize.Width / 2)), CInt(zTextSize.Height), CInt(zTextSize.Width))
                zGP_LineNumbers.AddRectangle(zReminderRectangle)
                zGP_LineNumbers.CloseFigure()

                If Me.zAutoSizing = True Then
                    zReminderRectangle.Inflate(CInt(zTextSize.Height * 0.2), CInt(zTextSize.Width * 0.1))
                    Me.zAutoSizing_Size = New Size(zReminderRectangle.Width, zReminderRectangle.Height)
                End If
            End If
        End If

        ' ----------------------------------------------
        ' --- DESIGN OR RUNTIME / WITH VISIBLE ITEMS (which means zParent exists)
        If Me.zLNIs.Count > 0 Then
            '   The visible LineNumberItems with their BackgroundGradient and GridLines
            '   Loop through every visible LineNumberItem
            Dim zLGB As Drawing2D.LinearGradientBrush = Nothing
            zPen = New Pen(Me.zGridLines_Color, Me.zGridLines_Thickness) With {
                                .DashStyle = Me.zGridLines_Style
                            }
            zSF.Alignment = StringAlignment.Near
            zSF.LineAlignment = StringAlignment.Near
            zSF.FormatFlags = CType(StringFormatFlags.FitBlackBox + StringFormatFlags.NoClip + StringFormatFlags.NoWrap, StringFormatFlags)

            For zA As Integer = 0 To Me.zLNIs.Count - 1

                ' --- BackgroundGradient
                If Me.zGradient_Show = True Then
                    zLGB = New Drawing2D.LinearGradientBrush(Me.zLNIs(zA).Rectangle, Me.zGradient_StartColor, Me.zGradient_EndColor, Me.zGradient_Direction)
                    e.Graphics.FillRectangle(zLGB, Me.zLNIs(zA).Rectangle)
                End If

                ' --- GridLines
                If Me.zGridLines_Show = True Then
                    e.Graphics.DrawLine(zPen, New Point(0, Me.zLNIs(zA).Rectangle.Y), New Point(Me.Width, Me.zLNIs(zA).Rectangle.Y))

                    '   NOTE: Every item in a GraphicsPath is a closed figure, so instead of adding gridlines as lines, we'll add them
                    '   as rectangles that loop out of sight. Their height uses the zContentRectangle which is the maxsize of
                    '   the ParentRichTextBox's contents.
                    '   NOTE: Slight adjustment needed when the first item has a negative Y coordinate.
                    '   This explains the " - zLNIs(0).Rectangle.Y" (which adds the negative size to the height
                    '   to make sure the rectangle's bottompart stays out of sight)
                    zGP_GridLines.AddRectangle(New Rectangle(CInt(-Me.zGridLines_Thickness), Me.zLNIs(zA).Rectangle.Y, CInt(Me.Width + (Me.zGridLines_Thickness * 2)), CInt(Me.Height - Me.zLNIs(0).Rectangle.Y + Me.zGridLines_Thickness)))
                    zGP_GridLines.CloseFigure()
                End If

                ' --- LineNumbers
                If Me.zLineNumbers_Show = True Then
                    '   TextFormatting
                    zTextToShow = If(Me.zLineNumbers_ShowLeadingZeroes, If(Me.zLineNumbers_ShowAsHexadecimal, Me.zLNIs(zA).LineNumber.ToString("X"), Me.zLNIs(zA).LineNumber.ToString(Me.zLineNumbers_Format)), If(Me.zLineNumbers_ShowAsHexadecimal, Me.zLNIs(zA).LineNumber.ToString("X"), Me.zLNIs(zA).LineNumber.ToString))
                    '   TextSizing
                    zTextSize = e.Graphics.MeasureString(zTextToShow, Me.Font, zPoint, zSF)
                    '   TextAlignment and positioning   (zPoint = TopLeftCornerPoint of the text)
                    '   TextAlignment, padding, manual offset (via LineNrs_Offset) and zTextSize are all included in the calculation of zPoint.
                    Select Case Me.zLineNumbers_Alignment
                        Case ContentAlignment.TopLeft
                            zPoint = New Point(Me.zLNIs(zA).Rectangle.Left + Me.Padding.Left + Me.zLineNumbers_Offset.Width, Me.zLNIs(zA).Rectangle.Top + Me.Padding.Top + Me.zLineNumbers_Offset.Height)
                        Case ContentAlignment.MiddleLeft
                            zPoint = New Point(Me.zLNIs(zA).Rectangle.Left + Me.Padding.Left + Me.zLineNumbers_Offset.Width, CInt(Me.zLNIs(zA).Rectangle.Top + (Me.zLNIs(zA).Rectangle.Height / 2) + Me.zLineNumbers_Offset.Height - (zTextSize.Height / 2)))
                        Case ContentAlignment.BottomLeft
                            zPoint = New Point(Me.zLNIs(zA).Rectangle.Left + Me.Padding.Left + Me.zLineNumbers_Offset.Width, CInt(Me.zLNIs(zA).Rectangle.Bottom - Me.Padding.Bottom + 1 + Me.zLineNumbers_Offset.Height - zTextSize.Height))
                        Case ContentAlignment.TopCenter
                            zPoint = New Point(CInt((Me.zLNIs(zA).Rectangle.Width / 2) + Me.zLineNumbers_Offset.Width - (zTextSize.Width / 2)), Me.zLNIs(zA).Rectangle.Top + Me.Padding.Top + Me.zLineNumbers_Offset.Height)
                        Case ContentAlignment.MiddleCenter
                            zPoint = New Point(CInt((Me.zLNIs(zA).Rectangle.Width / 2) + Me.zLineNumbers_Offset.Width - (zTextSize.Width / 2)), CInt(Me.zLNIs(zA).Rectangle.Top + (Me.zLNIs(zA).Rectangle.Height / 2) + Me.zLineNumbers_Offset.Height - (zTextSize.Height / 2)))
                        Case ContentAlignment.BottomCenter
                            zPoint = New Point(CInt((Me.zLNIs(zA).Rectangle.Width / 2) + Me.zLineNumbers_Offset.Width - (zTextSize.Width / 2)), CInt(Me.zLNIs(zA).Rectangle.Bottom - Me.Padding.Bottom + 1 + Me.zLineNumbers_Offset.Height - zTextSize.Height))
                        Case ContentAlignment.TopRight
                            zPoint = New Point(CInt(Me.zLNIs(zA).Rectangle.Right - Me.Padding.Right + Me.zLineNumbers_Offset.Width - zTextSize.Width), Me.zLNIs(zA).Rectangle.Top + Me.Padding.Top + Me.zLineNumbers_Offset.Height)
                        Case ContentAlignment.MiddleRight
                            zPoint = New Point(CInt(Me.zLNIs(zA).Rectangle.Right - Me.Padding.Right + Me.zLineNumbers_Offset.Width - zTextSize.Width), CInt(Me.zLNIs(zA).Rectangle.Top + (Me.zLNIs(zA).Rectangle.Height / 2) + Me.zLineNumbers_Offset.Height - (zTextSize.Height / 2)))
                        Case ContentAlignment.BottomRight
                            zPoint = New Point(CInt(Me.zLNIs(zA).Rectangle.Right - Me.Padding.Right + Me.zLineNumbers_Offset.Width - zTextSize.Width), CInt(Me.zLNIs(zA).Rectangle.Bottom - Me.Padding.Bottom + 1 + Me.zLineNumbers_Offset.Height - zTextSize.Height))
                    End Select
                    '   TextClipping
                    zItemClipRectangle = New Rectangle(zPoint, zTextSize.ToSize)
                    If Me.zLineNumbers_ClipByItemRectangle = True Then
                        '   If selected, the text will be clipped so that it doesn't spill out of its own LineNumberItem-area.
                        '   Only the part of the text inside the LineNumberItem.Rectangle should be visible, so intersect with the ItemRectangle
                        '   The SetClip method temporary restricts the drawing area of the control for whatever is drawn next.
                        zItemClipRectangle.Intersect(Me.zLNIs(zA).Rectangle)
                        e.Graphics.SetClip(zItemClipRectangle)
                    End If
                    '   TextDrawing
                    e.Graphics.DrawString(zTextToShow, Me.Font, zBrush, zPoint, zSF)
                    e.Graphics.ResetClip()
                    '   The GraphicsPath for the LineNumber is just a rectangle behind the text, to keep the paintingspeed high and avoid ugly artifacts.
                    zGP_LineNumbers.AddRectangle(zItemClipRectangle)
                    zGP_LineNumbers.CloseFigure()
                End If
            Next

            ' --- GridLinesThickness and Linestyle in SeeThroughMode. All GraphicsPath lines are drawn as solid to keep the paintingspeed high.
            If Me.zGridLines_Show = True Then
                zPen.DashStyle = Drawing2D.DashStyle.Solid
                zGP_GridLines.Widen(zPen)
            End If

            ' --- Memory CleanUp
            If zLGB IsNot Nothing Then zLGB.Dispose()
        End If

        ' ----------------------------------------------
        ' --- DESIGN OR RUNTIME / ALWAYS
        Dim zP_Left As New Point(CInt(Math.Floor(Me.zBorderLines_Thickness / 2)), CInt(Math.Floor(Me.zBorderLines_Thickness / 2)))
        Dim zP_Right As New Point(CInt(Me.Width - Math.Ceiling(Me.zBorderLines_Thickness / 2)), CInt(Me.Height - Math.Ceiling(Me.zBorderLines_Thickness / 2)))

        ' --- BorderLines
        Dim zBorderLines_Points() As Point = {New Point(zP_Left.X, zP_Left.Y), New Point(zP_Right.X, zP_Left.Y), New Point(zP_Right.X, zP_Right.Y), New Point(zP_Left.X, zP_Right.Y), New Point(zP_Left.X, zP_Left.Y)}
        If Me.zBorderLines_Show = True Then
            zPen = New Pen(Me.zBorderLines_Color, Me.zBorderLines_Thickness) With {
                .DashStyle = Me.zBorderLines_Style
            }
            e.Graphics.DrawLines(zPen, zBorderLines_Points)
            zGP_BorderLines.AddLines(zBorderLines_Points)
            zGP_BorderLines.CloseFigure()
            '   BorderThickness and Style for SeeThroughMode
            zPen.DashStyle = Drawing2D.DashStyle.Solid
            zGP_BorderLines.Widen(zPen)
        End If

        ' --- MarginLines
        If Me.zMarginLines_Show = True AndAlso Me.zMarginLines_Side > LineNumberDockSide.None Then
            zP_Left = New Point(CInt(-Me.zMarginLines_Thickness), CInt(-Me.zMarginLines_Thickness))
            zP_Right = New Point(CInt(Me.Width + Me.zMarginLines_Thickness), CInt(Me.Height + Me.zMarginLines_Thickness))
            zPen = New Pen(Me.zMarginLines_Color, Me.zMarginLines_Thickness) With {
                .DashStyle = Me.zMarginLines_Style
            }
            If Me.zMarginLines_Side = LineNumberDockSide.Left Or Me.zMarginLines_Side = LineNumberDockSide.Height Then
                e.Graphics.DrawLine(zPen, New Point(CInt(Math.Floor(Me.zMarginLines_Thickness / 2)), 0), New Point(CInt(Math.Floor(Me.zMarginLines_Thickness / 2)), Me.Height - 1))
                zP_Left = New Point(CInt(Math.Ceiling(Me.zMarginLines_Thickness / 2)), CInt(-Me.zMarginLines_Thickness))
            End If
            If Me.zMarginLines_Side = LineNumberDockSide.Right Or Me.zMarginLines_Side = LineNumberDockSide.Height Then
                e.Graphics.DrawLine(zPen, New Point(CInt(Me.Width - Math.Ceiling(Me.zMarginLines_Thickness / 2)), 0), New Point(CInt(Me.Width - Math.Ceiling(Me.zMarginLines_Thickness / 2)), Me.Height - 1))
                zP_Right = New Point(CInt(Me.Width - Math.Ceiling(Me.zMarginLines_Thickness / 2)), CInt(Me.Height + Me.zMarginLines_Thickness))
            End If
            '   GraphicsPath for the MarginLines(s):
            '   MarginLines(s) are drawn as a rectangle connecting the zP_Left and zP_Right points, which are either inside or
            '   outside of sight, depending on whether the MarginLines at that side is visible. zP_Left: TopLeft and ZP_Right: BottomRight
            zGP_MarginLines.AddRectangle(New Rectangle(zP_Left, New Size(zP_Right.X - zP_Left.X, zP_Right.Y - zP_Left.Y)))
            zPen.DashStyle = Drawing2D.DashStyle.Solid
            zGP_MarginLines.Widen(zPen)
        End If

        ' ----------------------------------------------
        ' --- SeeThroughMode
        '   combine all the GraphicsPaths (= zGP_... ) and set them as the region for the control.
        If Me.zSeeThroughMode = True Then
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
            zPen = New Pen(Me.zBorderLines_Color, 1) With {
                .DashStyle = Drawing2D.DashStyle.Solid
            }
            zGP_BorderLines.Widen(zPen)

            zRegion = New Region(zGP_BorderLines)
        End If
        Me.Region = zRegion

        ' ----------------------------------------------
        ' --- Memory CleanUp
        If zPen IsNot Nothing Then zPen.Dispose()
        If zBrush IsNot Nothing Then zPen.Dispose()
        If zRegion IsNot Nothing Then zRegion.Dispose()
        If zGP_GridLines IsNot Nothing Then zGP_GridLines.Dispose()
        If zGP_BorderLines IsNot Nothing Then zGP_BorderLines.Dispose()
        If zGP_MarginLines IsNot Nothing Then zGP_MarginLines.Dispose()
        If zGP_LineNumbers IsNot Nothing Then zGP_LineNumbers.Dispose()
    End Sub

    Protected Overrides Sub OnSizeChanged(ByVal e As EventArgs)
        If Me.DesignMode = True Then Me.Refresh()
        MyBase.OnSizeChanged(e)
        Me.Invalidate()
    End Sub

    '//////////////////////////////////////////////////////////////////////////////////////////////////
    Public Overrides Sub Refresh()
        '   Note: don't change the order here, first the Mybase.Refresh, then the Update_SizeAndPosition.
        MyBase.Refresh()
        Me.Update_SizeAndPosition()
    End Sub

    Private Class LineNumberItem
        Friend LineNumber As Integer
        Friend Rectangle As Rectangle

        Friend Sub New(ByVal zLineNumber As Integer, ByVal zRectangle As Rectangle)
            Me.LineNumber = zLineNumber
            Me.Rectangle = zRectangle
        End Sub

    End Class

    '//////////////////////////////////////////////////////////////////////////////////////////////////
End Class