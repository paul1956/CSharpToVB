' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.ComponentModel
Imports System.Diagnostics.CodeAnalysis
Imports System.Reflection
Imports System.Runtime.CompilerServices

Namespace CSharpToVBConverter

    ''' <summary>
    ''' This code is currently not used but is left here for future purposes
    ''' </summary>
    <ExcludeFromCodeCoverage>
    Public Module EnumExtensions

        Private Sub CheckIsEnum(Of T)(withFlags As Boolean)
            If Not GetType(T).IsEnum Then
                Throw New ArgumentException(String.Format(Globalization.CultureInfo.InvariantCulture, "Type '{0}' is not an enum", GetType(T).FullName))
            End If
            If withFlags AndAlso Not Attribute.IsDefined(GetType(T), GetType(FlagsAttribute)) Then
                Throw New ArgumentException(String.Format(Globalization.CultureInfo.InvariantCulture, "Type '{0}' doesn't have the 'Flags' attribute", GetType(T).FullName))
            End If
        End Sub

        <Extension>
        Public Function ClearFlags(Of T As Structure)(value As T, flags As T) As T
            Return value.SetFlags(flags, False)
        End Function

        <Extension>
        Public Function ClearFlags(Of T As Structure)(value As T) As T
            CheckIsEnum(Of T)(withFlags:=True)
            For Each flag As T In [Enum].GetValues(GetType(T)).Cast(Of T)()
                value = value.ClearFlags(flag)
            Next flag
            Return value
        End Function

        <Extension>
        Public Function CombineFlags(Of T As Structure)(flags As IEnumerable(Of T)) As T
            If flags Is Nothing Then
                Throw New ArgumentNullException(NameOf(flags))
            End If
            CheckIsEnum(Of T)(withFlags:=True)
            Dim lValue As Long = 0
            For Each flag As T In flags
                Dim lFlag As Long = Convert.ToInt64(flag, Globalization.CultureInfo.InvariantCulture)
                lValue = lValue Or lFlag
            Next flag
            Return DirectCast([Enum].ToObject(GetType(T), lValue), T)
        End Function

        <Extension>
        Public Function GetDescription(Of T As Structure)(value As T) As String
            CheckIsEnum(Of T)(withFlags:=False)
            Dim name As String = [Enum].GetName(GetType(T), value)
            If name IsNot Nothing Then
                Dim field As FieldInfo = GetType(T).GetField(name)
                If field IsNot Nothing Then
                    Dim attr As DescriptionAttribute = TryCast(Attribute.GetCustomAttribute(field, GetType(DescriptionAttribute)), DescriptionAttribute)
                    If attr IsNot Nothing Then
                        Return attr.Description
                    End If
                End If
            End If
            Return Nothing
        End Function

        <Extension>
        Public Iterator Function GetFlags(Of T As Structure)(value As T) As IEnumerable(Of T)
            CheckIsEnum(Of T)(withFlags:=True)
            For Each flag As T In [Enum].GetValues(GetType(T)).Cast(Of T)()
                If value.IsFlagSet(flag) Then
                    Yield flag
                End If
            Next flag
        End Function

        <Extension>
        Public Function IsFlagSet(Of T As Structure)(value As T, flag As T) As Boolean
            CheckIsEnum(Of T)(withFlags:=True)
            Dim lValue As Long = Convert.ToInt64(value, Globalization.CultureInfo.InvariantCulture)
            Dim lFlag As Long = Convert.ToInt64(flag, Globalization.CultureInfo.InvariantCulture)
            Return (lValue And lFlag) <> 0
        End Function

        <Extension>
        Public Function SetFlags(Of T As Structure)(value As T, flags As T, [on] As Boolean) As T
            CheckIsEnum(Of T)(withFlags:=True)
            Dim lValue As Long = Convert.ToInt64(value, Globalization.CultureInfo.InvariantCulture)
            Dim lFlag As Long = Convert.ToInt64(flags, Globalization.CultureInfo.InvariantCulture)
            If [on] Then
                lValue = lValue Or lFlag
            Else
                lValue = lValue And (Not lFlag)
            End If
            Return DirectCast([Enum].ToObject(GetType(T), lValue), T)
        End Function

        <Extension>
        Public Function SetFlags(Of T As Structure)(value As T, flags As T) As T
            Return value.SetFlags(flags, True)
        End Function

    End Module
End Namespace
