﻿'------------------------------------------------------------------------------
' <auto-generated>
'     This code was generated by a tool.
'     Runtime Version:4.0.30319.42000
'
'     Changes to this file may cause incorrect behavior and will be lost if
'     the code is regenerated.
' </auto-generated>
'------------------------------------------------------------------------------
Namespace My

    <HideModuleName(),
     DebuggerNonUserCode(),
     Runtime.CompilerServices.CompilerGenerated()>
    Friend Module MySettingsProperty

        <ComponentModel.Design.HelpKeyword("My.Settings")>
        Friend ReadOnly Property Settings() As My_Project.Settings
            Get
                Return My_Project.Settings.Default
            End Get
        End Property

    End Module
End Namespace

Namespace My_Project

    <Global.System.Runtime.CompilerServices.CompilerGeneratedAttribute(),
     Global.System.CodeDom.Compiler.GeneratedCodeAttribute("Microsoft.VisualStudio.Editors.SettingsDesigner.SettingsSingleFileGenerator", "16.6.0.0")>
    Partial Friend NotInheritable Class Settings
        Inherits Global.System.Configuration.ApplicationSettingsBase

        Private Shared defaultInstance As Settings = CType(Global.System.Configuration.ApplicationSettingsBase.Synchronized(New Settings()), Settings)

        Public Shared ReadOnly Property [Default]() As Settings
            Get
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
         Global.System.Configuration.DefaultSettingValueAttribute("Light Mode")>
        Public Property ColorMode() As String
            Get
                Return CType(Me("ColorMode"), String)
            End Get
            Set
                Me("ColorMode") = Value
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
         Global.System.Configuration.DefaultSettingValueAttribute("NETCOREAPP")>
        Public Property EditorFont() As Font
            Get
                Return CType(Me("EditorFont"), Font)
            End Get
            Set
                Me("EditorFont") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("Consolas")>
        Public Property EditorFontName() As String
            Get
                Return CType(Me("EditorFontName"), String)
            End Get
            Set
                Me("EditorFontName") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("NETCOREAPP")>
        Public Property Framework() As String
            Get
                Return CType(Me("Framework"), String)
            End Get
            Set
                Me("Framework") = Value
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
         Global.System.Configuration.DefaultSettingValueAttribute("")>
        Public Property LastProject() As String
            Get
                Return CType(Me("LastProject"), String)
            End Get
            Set
                Me("LastProject") = Value
            End Set
        End Property


        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("")>
        Public Property LastSolution() As String
            Get
                Return CType(Me("LastSolution"), String)
            End Get
            Set
                Me("LastSolution") = Value
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
         Global.System.Configuration.DefaultSettingValueAttribute("Text")>
        Public Property OptionCompare() As String
            Get
                Return CType(Me("OptionCompare"), String)
            End Get
            Set
                Me("OptionCompare") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("True")>
        Public Property OptionCompareIncludeInCode() As Boolean
            Get
                Return CType(Me("OptionCompareIncludeInCode"), Boolean)
            End Get
            Set
                Me("OptionCompareIncludeInCode") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("On")>
        Public Property OptionExplicit() As String
            Get
                Return CType(Me("OptionExplicit"), String)
            End Get
            Set
                Me("OptionExplicit") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("True")>
        Public Property OptionExplicitIncludeInCode() As Boolean
            Get
                Return CType(Me("OptionExplicitIncludeInCode"), Boolean)
            End Get
            Set
                Me("OptionExplicitIncludeInCode") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("On")>
        Public Property OptionInfer() As String
            Get
                Return CType(Me("OptionInfer"), String)
            End Get
            Set
                Me("OptionInfer") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("True")>
        Public Property OptionInferIncludeInCode() As Boolean
            Get
                Return CType(Me("OptionInferIncludeInCode"), Boolean)
            End Get
            Set
                Me("OptionInferIncludeInCode") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("On")>
        Public Property OptionStrict() As String
            Get
                Return CType(Me("OptionStrict"), String)
            End Get
            Set
                Me("OptionStrict") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("True")>
        Public Property OptionStrictIncludeInCode() As Boolean
            Get
                Return CType(Me("OptionStrictIncludeInCode"), Boolean)
            End Get
            Set
                Me("OptionStrictIncludeInCode") = Value
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
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute()>
        Public Property TSFindMRU_Data() As Global.System.Collections.Specialized.StringCollection
            Get
                Return CType(Me("TSFindMRU_Data"), Global.System.Collections.Specialized.StringCollection)
            End Get
            Set
                Me("TSFindMRU_Data") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("False")>
        Public Property TSFindMatchCase() As Boolean
            Get
                Return CType(Me("TSFindMatchCase"), Boolean)
            End Get
            Set
                Me("TSFindMatchCase") = Value
            End Set
        End Property

        <Global.System.Configuration.UserScopedSettingAttribute(),
         Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),
         Global.System.Configuration.DefaultSettingValueAttribute("False")>
        Public Property TSFindMatchWholeWord() As Boolean
            Get
                Return CType(Me("TSFindMatchWholeWord"), Boolean)
            End Get
            Set
                Me("TSFindMatchWholeWord") = Value
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
    End Class
End Namespace
