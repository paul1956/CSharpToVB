﻿'------------------------------------------------------------------------------
' <auto-generated>
'     This code was generated by a tool.
'     Runtime Version:4.0.30319.42000
'
'     Changes to this file may cause incorrect behavior and will be lost if
'     the code is regenerated.
' </auto-generated>
'------------------------------------------------------------------------------

Option Strict On
Option Explicit On


Namespace My

    <Global.System.Runtime.CompilerServices.CompilerGeneratedAttribute(),
     Global.System.CodeDom.Compiler.GeneratedCodeAttribute("Microsoft.VisualStudio.Editors.SettingsDesigner.SettingsSingleFileGenerator", "16.4.0.0")>
    Partial Friend NotInheritable Class MySettings
        Inherits Global.System.Configuration.ApplicationSettingsBase

        Private Shared defaultInstance As MySettings = CType(Global.System.Configuration.ApplicationSettingsBase.Synchronized(New MySettings()), MySettings)

#Region "My.Settings Auto-Save Functionality"
#If _MyType = "WindowsForms" Then
    Private Shared addedHandler As Boolean

    Private Shared addedHandlerLockObject As New Object

    <Global.System.Diagnostics.DebuggerNonUserCodeAttribute(), Global.System.ComponentModel.EditorBrowsableAttribute(Global.System.ComponentModel.EditorBrowsableState.Advanced)> _
    Private Shared Sub AutoSaveSettings(sender As Global.System.Object, e As Global.System.EventArgs)
        If My.Application.SaveMySettingsOnExit Then
            My.Settings.Save()
        End If
    End Sub
#End If
#End Region

        Public Shared ReadOnly Property [Default]() As MySettings
            Get

#If _MyType = "WindowsForms" Then
               If Not addedHandler Then
                    SyncLock addedHandlerLockObject
                        If Not addedHandler Then
                            AddHandler My.Application.Shutdown, AddressOf AutoSaveSettings
                            addedHandler = True
                        End If
                    End SyncLock
                End If
#End If
                Return defaultInstance
            End Get
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("False")>
        Public Property ColorizeInput() As Boolean
            Get
                Return CType(Me("ColorizeInput"), Boolean)
            End Get
            Set
                Me("ColorizeInput") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("False")>
        Public Property ColorizeOutput() As Boolean
            Get
                Return CType(Me("ColorizeOutput"), Boolean)
            End Get
            Set
                Me("ColorizeOutput") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("0")>
        Public Property ConversionDelay() As Integer
            Get
                Return CType(Me("ConversionDelay"), Integer)
            End Get
            Set
                Me("ConversionDelay") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("")>
        Public Property DefaultProjectDirectory() As String
            Get
                Return CType(Me("DefaultProjectDirectory"), String)
            End Get
            Set
                Me("DefaultProjectDirectory") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("")>
        Public Property LastPath() As String
            Get
                Return CType(Me("LastPath"), String)
            End Get
            Set
                Me("LastPath") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("False")>
        Public Property PauseConvertOnSuccess() As Boolean
            Get
                Return CType(Me("PauseConvertOnSuccess"), Boolean)
            End Get
            Set
                Me("PauseConvertOnSuccess") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("True")>
        Public Property SkipAutoGenerated() As Boolean
            Get
                Return CType(Me("SkipAutoGenerated"), Boolean)
            End Get
            Set
                Me("SkipAutoGenerated") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("True")>
        Public Property SkipBinAndObjFolders() As Boolean
            Get
                Return CType(Me("SkipBinAndObjFolders"), Boolean)
            End Get
            Set
                Me("SkipBinAndObjFolders") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("True")>
        Public Property SkipTestResourceFiles() As Boolean
            Get
                Return CType(Me("SkipTestResourceFiles"), Boolean)
            End Get
            Set
                Me("SkipTestResourceFiles") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("False")>
        Public Property StartFolderConvertFromLastFile() As Boolean
            Get
                Return CType(Me("StartFolderConvertFromLastFile"), Boolean)
            End Get
            Set
                Me("StartFolderConvertFromLastFile") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("True")>
        Public Property UpgradeRequired() As Boolean
            Get
                Return CType(Me("UpgradeRequired"), Boolean)
            End Get
            Set
                Me("UpgradeRequired") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute()>
        Public Property MRU_Data() As Global.System.Collections.Specialized.StringCollection
            Get
                Return CType(Me("MRU_Data"), Global.System.Collections.Specialized.StringCollection)
            End Get
            Set
                Me("MRU_Data") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("False")>
        Public Property ShowSourceLineNumbers() As Boolean
            Get
                Return CType(Me("ShowSourceLineNumbers"), Boolean)
            End Get
            Set
                Me("ShowSourceLineNumbers") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("False")>
        Public Property ShowDestinationLineNumbers() As Boolean
            Get
                Return CType(Me("ShowDestinationLineNumbers"), Boolean)
            End Get
            Set
                Me("ShowDestinationLineNumbers") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute()>
        Public Property IgnoreFileList() As Global.System.Collections.Specialized.StringCollection
            Get
                Return CType(Me("IgnoreFileList"), Global.System.Collections.Specialized.StringCollection)
            End Get
            Set
                Me("IgnoreFileList") = Value
            End Set
        End Property
    End Class
End Namespace

Namespace My

    <Global.Microsoft.VisualBasic.HideModuleNameAttribute(),
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
     Global.System.Runtime.CompilerServices.CompilerGeneratedAttribute()>
    Friend Module MySettingsProperty

        <Global.System.ComponentModel.Design.HelpKeywordAttribute("My.Settings")>
        Friend ReadOnly Property Settings() As Global.CSharpToVBApp.My.MySettings
            Get
                Return Global.CSharpToVBApp.My.MySettings.Default
            End Get
        End Property
    End Module
End Namespace
