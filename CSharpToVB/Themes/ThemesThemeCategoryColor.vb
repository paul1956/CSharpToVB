' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Xml.Serialization

'''<remarks/>
<Serializable(),
 ComponentModel.DesignerCategory("code"),
 XmlType(AnonymousType:=True)>
Partial Public Class ThemesThemeCategoryColor

    '''<remarks/>
    Public Property Background As ThemesThemeCategoryColorBackground

    '''<remarks/>
    Public Property Foreground As ThemesThemeCategoryColorForeground

    '''<remarks/>
    <XmlAttribute()>
    Public Property Name As String

End Class
