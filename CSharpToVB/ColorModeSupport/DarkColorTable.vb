' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Class DarkColorTable
  Inherits ProfessionalColorTable

  ' **************************************************************
  ' * Main Menu and Sub Menus
  ' **************************************************************

  ' The border of the currently highlighted (selected/hovered) menu item.
  Public Overrides ReadOnly Property MenuItemBorder() As Color = Color.FromArgb(40, 40, 40) 'Color.FromArgb(&H31, &H6A, &HC5)

  ' **************************************************************
  ' * Main Menu
  ' **************************************************************

  ' The background color of the actual menu strip...
  Public Overrides ReadOnly Property MenuStripGradientBegin() As Color = Color.FromArgb(40, 40, 40) 'Color.FromArgb(&HE5, &HE5, &HD7)
  Public Overrides ReadOnly Property MenuStripGradientEnd() As Color = Color.FromArgb(40, 40, 40) 'Color.FromArgb(&HF4, &HF2, &HE8)

  ' The background of the main menu (menuStrip) items while hovering (before click/expanded)...
  Public Overrides ReadOnly Property MenuItemSelectedGradientBegin() As Color = Color.FromArgb(90, 90, 90) 'Color.FromArgb(&HC1, 210, &HEE)
  Public Overrides ReadOnly Property MenuItemSelectedGradientEnd() As Color = Color.FromArgb(90, 90, 90) 'Color.FromArgb(&HC1, 210, &HEE)

  ' The background of a selected (expanded) main menu item...
  Public Overrides ReadOnly Property MenuItemPressedGradientBegin() As Color = Color.FromArgb(90, 90, 90) 'MyBase.MenuItemPressedGradientBegin
  Public Overrides ReadOnly Property MenuItemPressedGradientEnd() As Color = Color.FromArgb(90, 90, 90) 'MyBase.MenuItemPressedGradientEnd
  Public Overrides ReadOnly Property MenuItemPressedGradientMiddle() As Color = Color.FromArgb(90, 90, 90) 'MyBase.MenuItemPressedGradientMiddle

  ' **************************************************************
  ' * Sub Menus
  ' **************************************************************

  ' The color of the "separator" (line) between menu sub items...
  Public Overrides ReadOnly Property SeparatorDark() As Color = Color.FromArgb(90, 90, 90) 'Color.FromArgb(&HC5, &HC2, &HB8)

  ' The border of the menu when "expanded" (dropped-down)...
  Public Overrides ReadOnly Property MenuBorder() As Color = Color.FromArgb(40, 40, 40) 'Color.FromArgb(&H8A, &H86, &H7A)

  ' The background color of the currently highlighted (selected/hovered) menu sub item.
  Public Overrides ReadOnly Property MenuItemSelected() As Color = Color.FromArgb(90, 90, 90) 'Color.FromArgb(&HC1, 210, &HEE)

  Public Overrides ReadOnly Property ToolStripDropDownBackground() As Color = Color.FromArgb(40, 40, 40) 'Color.FromArgb(&HFC, &HFC, &HF9)

  ' The icon / check mark "column" of the drop down menu...
  Public Overrides ReadOnly Property ImageMarginGradientBegin() As Color = Color.FromArgb(50, 50, 50) 'Color.FromArgb(&HFE, &HFE, &HFB)
  Public Overrides ReadOnly Property ImageMarginGradientEnd() As Color = Color.FromArgb(50, 50, 50) 'Color.FromArgb(&HBD, &HBD, &HA3)
  Public Overrides ReadOnly Property ImageMarginGradientMiddle() As Color = Color.FromArgb(50, 50, 50) 'Color.FromArgb(&HEC, &HE7, &HE0)

  ' The border around the check mark?
  Public Overrides ReadOnly Property ButtonSelectedBorder() As Color = Color.FromArgb(40, 40, 40) 'MyBase.ButtonSelectedBorder
  ' The background of the check mark...
  Public Overrides ReadOnly Property CheckBackground() As Color = Color.FromArgb(90, 90, 90) 'Color.FromArgb(&HE1, 230, &HE8)
  ' The background of the check mark when pressed (mouse)...
  Public Overrides ReadOnly Property CheckPressedBackground() As Color = Color.FromArgb(&H31, &H6A, &HC5)
  ' The background of the check mark when selected (keyboard) / hovered (mouse)...
  Public Overrides ReadOnly Property CheckSelectedBackground() As Color = Color.FromArgb(&H31, &H6A, &HC5)

  ' --------------------------------------------

  Public Overrides ReadOnly Property ButtonSelectedGradientBegin() As Color = Color.FromArgb(&HC1, 210, &HEE)
  Public Overrides ReadOnly Property ButtonSelectedGradientEnd() As Color = Color.FromArgb(&HC1, 210, &HEE)
  Public Overrides ReadOnly Property ButtonSelectedGradientMiddle() As Color = Color.FromArgb(&HC1, 210, &HEE)

  Public Overrides ReadOnly Property GripDark() As Color = Color.FromArgb(40, 40, 40) ' Color.FromArgb(&HC1, 190, &HB3)
  Public Overrides ReadOnly Property GripLight() As Color = Color.Silver ' Color.FromArgb(&HFF, &HFF, &HFF)

  Public Overrides ReadOnly Property ImageMarginRevealedGradientBegin() As Color = Color.FromArgb(&HF7, &HF6, &HEF)
  Public Overrides ReadOnly Property ImageMarginRevealedGradientEnd() As Color = Color.FromArgb(230, &HE3, 210)
  Public Overrides ReadOnly Property ImageMarginRevealedGradientMiddle() As Color = Color.FromArgb(&HF2, 240, &HE4)

  Public Overrides ReadOnly Property SeparatorLight() As Color = Color.FromArgb(60, 60, 60) ' Color.FromArgb(&HFF, &HFF, &HFF)

  Public Overrides ReadOnly Property OverflowButtonGradientBegin() As Color = Color.FromArgb(&HF3, &HF2, 240)
  Public Overrides ReadOnly Property OverflowButtonGradientEnd() As Color = Color.FromArgb(&H92, &H92, &H76)
  Public Overrides ReadOnly Property OverflowButtonGradientMiddle() As Color = Color.FromArgb(&HE2, &HE1, &HDB)
  Public Overrides ReadOnly Property RaftingContainerGradientBegin() As Color = Color.FromArgb(&HE5, &HE5, &HD7)
  Public Overrides ReadOnly Property RaftingContainerGradientEnd() As Color = Color.FromArgb(&HF4, &HF2, &HE8)

  Public Overrides ReadOnly Property ToolStripBorder() As Color = Color.FromArgb(40, 40, 40) 'Color.FromArgb(&HA3, &HA3, &H7C)
  Public Overrides ReadOnly Property ToolStripGradientBegin() As Color = Color.FromArgb(40, 40, 40) 'Color.FromArgb(&HF7, &HF6, &HEF)
  Public Overrides ReadOnly Property ToolStripGradientEnd() As Color = Color.FromArgb(40, 40, 40) 'Color.FromArgb(192, 192, 168) 'Color.FromArgb(230, &HE3, 210)
  Public Overrides ReadOnly Property ToolStripGradientMiddle() As Color = Color.FromArgb(40, 40, 40) 'Color.FromArgb(&HF2, 240, &HE4)

  Public Overrides ReadOnly Property ButtonCheckedHighlight As Color = MyBase.ButtonCheckedHighlight
  Public Overrides ReadOnly Property ButtonCheckedHighlightBorder As Color = MyBase.ButtonCheckedHighlightBorder
  Public Overrides ReadOnly Property ButtonPressedHighlight As Color = MyBase.ButtonPressedHighlight
  Public Overrides ReadOnly Property ButtonPressedHighlightBorder As Color = MyBase.ButtonPressedHighlightBorder
  Public Overrides ReadOnly Property ButtonSelectedHighlight As Color = MyBase.ButtonSelectedHighlight
  Public Overrides ReadOnly Property ButtonSelectedHighlightBorder As Color = MyBase.ButtonSelectedHighlightBorder
  Public Overrides ReadOnly Property StatusStripGradientBegin As Color = MyBase.StatusStripGradientBegin
  Public Overrides ReadOnly Property StatusStripGradientEnd As Color = MyBase.StatusStripGradientEnd
  Public Overrides ReadOnly Property ToolStripContentPanelGradientBegin As Color = MyBase.ToolStripContentPanelGradientBegin
  Public Overrides ReadOnly Property ToolStripContentPanelGradientEnd As Color = MyBase.ToolStripContentPanelGradientEnd
  Public Overrides ReadOnly Property ToolStripPanelGradientBegin As Color = MyBase.ToolStripPanelGradientBegin
  Public Overrides ReadOnly Property ToolStripPanelGradientEnd As Color = MyBase.ToolStripPanelGradientEnd

  Public Overrides ReadOnly Property ButtonCheckedGradientBegin() As Color = Color.FromArgb(&HC1, 210, &HEE)
  Public Overrides ReadOnly Property ButtonCheckedGradientEnd() As Color = Color.FromArgb(&HC1, 210, &HEE)
  Public Overrides ReadOnly Property ButtonCheckedGradientMiddle() As Color = Color.FromArgb(&HC1, 210, &HEE)

  Public Overrides ReadOnly Property ButtonPressedBorder() As Color = Color.FromArgb(&H31, &H6A, &HC5)
  Public Overrides ReadOnly Property ButtonPressedGradientBegin() As Color = Color.FromArgb(&H98, &HB5, &HE2)
  Public Overrides ReadOnly Property ButtonPressedGradientEnd() As Color = Color.FromArgb(&H98, &HB5, &HE2)
  Public Overrides ReadOnly Property ButtonPressedGradientMiddle() As Color = Color.FromArgb(&H98, &HB5, &HE2)

End Class
