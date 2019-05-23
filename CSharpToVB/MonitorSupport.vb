Option Explicit On
Option Infer Off
Option Strict On

Imports System.Runtime.InteropServices
Module MonitorSupport

    <FlagsAttribute()>
    Public Enum EXECUTION_STATE As UInteger ' Determine Monitor State
        ES_AWAYMODE_REQUIRED = &H40
        ES_CONTINUOUS = &H80000000UI
        ES_DISPLAY_REQUIRED = &H2
        ES_SYSTEM_REQUIRED = &H1
        ' Legacy flag, should not be used.
        ' ES_USER_PRESENT = 0x00000004
    End Enum

    'Enables an application to inform the system that it is in use, thereby preventing the system from entering sleep or turning off the display while the application is running.
    <DllImport("kernel32.dll", CharSet:=CharSet.Auto, SetLastError:=True)>
    Private Function SetThreadExecutionState(ByVal esFlags As EXECUTION_STATE) As EXECUTION_STATE
    End Function

    'This function queries or sets system-wide parameters, and updates the user profile during the process.
    <DllImport("user32", EntryPoint:=NameOf(SystemParametersInfo), CharSet:=CharSet.Unicode, SetLastError:=True)>
    Public Function SystemParametersInfo(
            ByVal intAction As Integer,
            ByVal intParam As Integer,
            ByVal strParam As String,
            ByVal intWinIniFlag As Integer) As Integer
        ' returns non-zero value if function succeeds
    End Function

    Public Sub KeepMonitorActive()
        'SetThreadExecutionState(CType(EXECUTION_STATE.ES_DISPLAY_REQUIRED + EXECUTION_STATE.ES_CONTINUOUS, EXECUTION_STATE)) 'Do not Go To Sleep
    End Sub

    Public Sub RestoreMonitorSettings()
        'SetThreadExecutionState(EXECUTION_STATE.ES_CONTINUOUS) 'Restore Previous Settings, ie, Go To Sleep Again
    End Sub

End Module
