' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Reflection
Imports System.Runtime.CompilerServices

Public Module ColorExtensions

    <Extension>
    Public Function ToColorName(DrawingColor As Color) As String
        Dim col As Windows.Media.Color = Windows.Media.Color.FromArgb(DrawingColor.A,
                                                                      DrawingColor.R,
                                                                      DrawingColor.G,
                                                                      DrawingColor.B)
        Dim coltype As Type = GetType(Windows.Media.Colors)
        Dim colproplist() As PropertyInfo = coltype.GetProperties

        Try

            Dim colorproperty As PropertyInfo = colproplist.FirstOrDefault(Function(p As PropertyInfo) Windows.Media.Color.AreClose(CType(p.GetValue(DrawingColor, Nothing), Windows.Media.Color), col))

            Return colorproperty.Name
        Catch ex As Exception

            Return ""

        End Try

    End Function

    ''' <summary>
    ''' Convert Media Color (WPF) to Drawing Color (WinForm)
    ''' </summary>
    ''' <param name="mediaColor"></param>
    ''' <returns></returns>
    <Extension()>
    <MethodImpl(MethodImplOptions.AggressiveInlining)>
    Public Function ToDrawingColor(mediaColor As Windows.Media.Color) As Color
        Return Color.FromArgb(mediaColor.A, mediaColor.R, mediaColor.G, mediaColor.B)
    End Function

    ''' <summary>
    ''' Convert Drawing Color (WPF) to Media Color (WinForm)
    ''' </summary>
    ''' <param name="drawingColor"></param>
    ''' <returns></returns>
    <Extension()>
    <MethodImpl(MethodImplOptions.AggressiveInlining)>
    Public Function ToMediaColor(drawingColor As Color) As Windows.Media.Color
        Return Windows.Media.Color.FromArgb(drawingColor.A, drawingColor.R, drawingColor.G, drawingColor.B)
    End Function

End Module
