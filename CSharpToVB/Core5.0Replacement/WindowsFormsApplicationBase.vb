' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Option Strict On
Option Explicit On

Imports System.Collections.ObjectModel
Imports System.ComponentModel
Imports System.IO.Pipes
Imports System.Reflection
Imports System.Runtime.InteropServices
Imports System.Security
Imports System.Security.Permissions
Imports System.Security.Principal
Imports System.Threading

Namespace Microsoft.VisualBasic.ApplicationServices

    ''' <summary>
    ''' Signature for the Shutdown event handler
    ''' </summary>
    <EditorBrowsable(EditorBrowsableState.Advanced)>
    Public Delegate Sub ShutdownEventHandler(sender As Object, e As EventArgs)

    ''' <summary>
    ''' Signature for the Startup event handler
    ''' </summary>
    <EditorBrowsable(EditorBrowsableState.Advanced)>
    Public Delegate Sub StartupEventHandler(sender As Object, e As StartupEventArgs)

    ''' <summary>
    ''' Signature for the StartupNextInstance event handler
    ''' </summary>
    <EditorBrowsable(EditorBrowsableState.Advanced)>
    Public Delegate Sub StartupNextInstanceEventHandler(sender As Object, e As StartupNextInstanceEventArgs)

    ''' <summary>
    ''' Signature for the UnhandledException event handler
    ''' </summary>
    <EditorBrowsable(EditorBrowsableState.Advanced)>
    Public Delegate Sub UnhandledExceptionEventHandler(sender As Object, e As UnhandledExceptionEventArgs)

    ' Any changes to this enum must be reflected in ValidateAuthenticationModeEnumValue()
    Public Enum AuthenticationMode
        Windows
        ApplicationDefined
    End Enum

    ' Any changes to this enum must be reflected in ValidateShutdownModeEnumValue()
    Public Enum ShutdownMode
        AfterMainFormCloses
        AfterAllFormsClose
    End Enum

    ''' <summary>
    ''' Exception for when the WinForms VB application model isn't supplied with a startup form
    ''' </summary>
    <EditorBrowsable(EditorBrowsableState.Never)>
    <Serializable()>
    Public Class NoStartupFormException : Inherits Exception

        ' De-serialization constructor must be defined since we are serializable
        <EditorBrowsable(EditorBrowsableState.Advanced)>
        Protected Sub New(info As Runtime.Serialization.SerializationInfo, context As Runtime.Serialization.StreamingContext)
            MyBase.New(info, context)
        End Sub

        ''' <summary>
        '''  Creates a new exception
        ''' </summary>
        Public Sub New()
            MyBase.New("No Startup Form")
        End Sub

        Public Sub New(message As String)
            MyBase.New(message)
        End Sub

        Public Sub New(message As String, inner As Exception)
            MyBase.New(message, inner)
        End Sub

    End Class

    ''' <summary>
    ''' Provides context for the Startup event.
    ''' </summary>
    <EditorBrowsable(EditorBrowsableState.Advanced), ComVisible(False)>
    Public Class StartupEventArgs : Inherits CancelEventArgs

        ''' <summary>
        ''' Creates a new instance of the StartupEventArgs.
        ''' </summary>
        Public Sub New(args As ReadOnlyCollection(Of String))
            If args Is Nothing Then
                args = New ReadOnlyCollection(Of String)(Nothing)
            End If

            CommandLine = args
        End Sub

        ''' <summary>
        ''' Returns the command line sent to this application
        ''' </summary>
        Public ReadOnly Property CommandLine() As ReadOnlyCollection(Of String)

    End Class

    ''' <summary>
    ''' Provides context for the StartupNextInstance event.
    ''' </summary>
    <EditorBrowsable(EditorBrowsableState.Advanced)>
    Public Class StartupNextInstanceEventArgs : Inherits EventArgs

        ''' <summary>
        ''' Creates a new instance of the StartupNextInstanceEventArgs.
        ''' </summary>
        Public Sub New(args As ReadOnlyCollection(Of String), bringToForegroundFlag As Boolean)
            If args Is Nothing Then
                args = New ReadOnlyCollection(Of String)(Nothing)
            End If

            CommandLine = args
            BringToForeground = bringToForegroundFlag
        End Sub

        ''' <summary>
        ''' Indicates whether we will bring the application to the foreground when processing the
        ''' StartupNextInstance event.
        ''' </summary>
        Public Property BringToForeground() As Boolean

        ''' <summary>
        ''' Returns the command line sent to this application
        ''' </summary>
        ''' <remarks>I'm using Me.CommandLine so that it is consistent with my.net and to assure they
        ''' always return the same values</remarks>
        Public ReadOnly Property CommandLine() As ReadOnlyCollection(Of String)

    End Class

    ''' <summary>
    ''' Provides the exception encountered along with a flag on whether to abort the program
    ''' </summary>
    <EditorBrowsable(EditorBrowsableState.Advanced), ComVisible(False)>
    Public Class UnhandledExceptionEventArgs : Inherits ThreadExceptionEventArgs

        Sub New(exitApplication As Boolean, exception As Exception)
            MyBase.New(exception)
            Me.ExitApplication = exitApplication
        End Sub

        ''' <summary>
        ''' Indicates whether the application should exit upon exiting the exception handler
        ''' </summary>
        Public Property ExitApplication() As Boolean

    End Class

    ''' <summary>
    ''' Provides the infrastructure for the VB Windows Forms application model
    ''' </summary>
    ''' <remarks>Don't put access on this definition.</remarks>
    Public Class WindowsFormsApplicationBase : Inherits ConsoleApplicationBase
        Implements IDisposable

        ' How long a subsequent instance will wait for the original instance to get on its feet.
        Private Const SECOND_INSTANCE_TIMEOUT As Integer = 2500 'milliseconds.

        Private ReadOnly _appContext As WinFormsAppContext

        Private ReadOnly _splashLock As New Object

        Private _appSyncronizationContext As SynchronizationContext

        Private _didSplashScreen As Boolean

        Private _isDisposed As Boolean

        Private _isSingleInstance As Boolean
        Private _ok2CloseSplashScreen As Boolean

        Private _processingUnhandledExceptionEvent As Boolean

        'Whether we have made it through the processing of OnInitialize
        Private _shutdownStyle As ShutdownMode

        'For splash screens with a minimum display time, this let's us know when that time has expired and it is OK to close the splash screen.
        Private _splashScreen As Form

        Private _splashTimer As Timers.Timer

        Private _unhandledExceptionHandlers As ArrayList

        ''' <summary>
        ''' Constructs the application Shutdown/Startup model object
        ''' </summary>
        ''' <remarks>
        ''' We have to have a parameterless ctor because the platform specific Application object
        ''' derives from this one and it doesn't define a ctor because the partial class generated by the
        ''' designer does that to configure the application. </remarks>
        Public Sub New()
            Me.New(AuthenticationMode.Windows)
        End Sub

        ''' <summary>
        ''' Constructs the application Shutdown/Startup model object
        ''' </summary>
        <SecuritySafeCritical()>
        Public Sub New(authenticationMode As AuthenticationMode)
            MyBase.New()
            _ok2CloseSplashScreen = True 'Default to true in case there is no splash screen so we won't block forever waiting for it to appear.
            ValidateAuthenticationModeEnumValue(authenticationMode, NameOf(authenticationMode))

            'Setup Windows Authentication if that's what the user wanted.  Note, we want to do this now, before the Network object gets created because
            'the network object will be doing a AsyncOperationsManager.CreateOperation() which captures the execution context.  So we have to have our
            'principal on the thread before that happens.
            If authenticationMode = AuthenticationMode.Windows Then
                Try
                    'Consider:  - sadly, a call to: System.Security.SecurityManager.IsGranted(New SecurityPermission(SecurityPermissionFlag.ControlPrincipal))
                    'will only check THIS caller so you'll always get TRUE.  What is needed is a way to get to the value of this on a demand basis.  So I try/catch instead for now
                    'but would rather be able to IF my way around this block.
                    Thread.CurrentPrincipal = New WindowsPrincipal(WindowsIdentity.GetCurrent)
                Catch ex As SecurityException
                End Try
            End If

            _appContext = New WinFormsAppContext(Me)

            'We need to set the WindowsFormsSynchronizationContext because the network object is going to get created after this ctor runs
            '(network gets created during event hookup) and we need the context in place for it to latch on to.  The WindowsFormsSynchronizationContext
            'won't otherwise get created until OnCreateMainForm() when the startup form is created and by then it is too late.
            'When the startup form gets created, WinForms is going to push our context into the previous context and then restore it when Application.Run() exits.
            Call New UIPermission(UIPermissionWindow.AllWindows).Assert()
            _appSyncronizationContext = AsyncOperationManager.SynchronizationContext
            AsyncOperationManager.SynchronizationContext = New WindowsFormsSynchronizationContext()
            PermissionSet.RevertAssert() 'CLR also reverts if we throw or when we return from this function
        End Sub

        'defines when the application decides to close
        'we only need to show the splash screen once.  Protect the user from himself if they are overriding our app model.
        Private Delegate Sub DisposeDelegate()

        Public Event Shutdown As ShutdownEventHandler

        Public Event Startup As StartupEventHandler

        Public Event StartupNextInstance As StartupNextInstanceEventHandler

        Public Custom Event UnhandledException As UnhandledExceptionEventHandler
            'This is a custom event because we want to hook up System.Windows.Forms.Application.ThreadException only if the user writes a
            'handler for this event.  We only want to hook the ThreadException event if the user is handling this event because the act of listening to
            'Application.ThreadException causes WinForms to snuff exceptions and we only want WinForms to do that if we are assured that the user wrote their own handler
            'to deal with the error instead
            AddHandler(value As UnhandledExceptionEventHandler)
                If _unhandledExceptionHandlers Is Nothing Then _unhandledExceptionHandlers = New ArrayList
                _unhandledExceptionHandlers.Add(value)
                'Only add the listener once so we don't fire the UnHandledException event over and over for the same exception
                If _unhandledExceptionHandlers.Count = 1 Then AddHandler Application.ThreadException, AddressOf OnUnhandledExceptionEventAdaptor
            End AddHandler

            RemoveHandler(value As UnhandledExceptionEventHandler)
                If _unhandledExceptionHandlers IsNot Nothing AndAlso _unhandledExceptionHandlers.Count > 0 Then
                    _unhandledExceptionHandlers.Remove(value)
                    'Last one to leave, turn out the lights...
                    If _unhandledExceptionHandlers.Count = 0 Then RemoveHandler Application.ThreadException, AddressOf OnUnhandledExceptionEventAdaptor
                End If
            End RemoveHandler

            RaiseEvent(sender As Object, e As UnhandledExceptionEventArgs)
                If _unhandledExceptionHandlers IsNot Nothing Then
                    _processingUnhandledExceptionEvent = True 'In the case that we throw from the unhandled exception handler, we don't want to run the unhandled exception handler again
                    For Each handler As UnhandledExceptionEventHandler In _unhandledExceptionHandlers
                        If handler IsNot Nothing Then handler.Invoke(sender, e)
                    Next
                    _processingUnhandledExceptionEvent = False 'Now that we are out of the unhandled exception handler, treat exceptions normally again.
                End If
            End RaiseEvent
        End Event

        ''' <summary>
        ''' Use GDI for the text rendering engine by default.
        ''' The user can shadow this function to return True if they want their app
        ''' to use the GDI+ render.  We read this function in Main() (My template) to
        ''' determine how to set the text rendering flag on the WinForms application object.
        ''' </summary>
        ''' <returns>True - Use GDI+ renderer.  False - use GDI renderer</returns>
        <EditorBrowsable(EditorBrowsableState.Advanced)>
        Protected Shared ReadOnly Property UseCompatibleTextRendering() As Boolean
            Get
                Return False
            End Get
        End Property

        ''' <summary>
        ''' Provides access to the main form for this application
        ''' </summary>
        Protected Property MainForm() As Form
            Get
                Return _appContext?.MainForm
            End Get
            Set(value As Form)
                If value Is Nothing Then
                    Throw New ArgumentNullException("MainForm")
                End If
                If value Is _splashScreen Then
                    Throw New ArgumentException("Splash And Main Form The Same")
                End If
                _appContext.MainForm = value
            End Set
        End Property

        ''' <summary>
        '''  Determines when this application will terminate (when the main form goes down, all forms)
        ''' </summary>
        Protected Friend Property ShutdownStyle() As ShutdownMode
            Get
                Return _shutdownStyle
            End Get
            Set(value As ShutdownMode)
                ValidateShutdownModeEnumValue(value, NameOf(value))
                _shutdownStyle = value
            End Set
        End Property

        ''' <summary>
        ''' Returns the collection of forms that are open.  We no longer have thread
        ''' affinity meaning that this is the WinForms collection that contains Forms that may
        ''' have been opened on another thread then the one we are calling in on right now.
        ''' </summary>
        Public Shared ReadOnly Property OpenForms() As FormCollection
            Get
                Return Application.OpenForms
            End Get
        End Property

        ''' <summary>
        ''' Provides the WinForms application context that we are running on
        ''' </summary>
        <EditorBrowsable(EditorBrowsableState.Advanced)>
        Public ReadOnly Property ApplicationContext() As ApplicationContext
            Get
                Return _appContext
            End Get
        End Property

        ''' <summary>
        ''' Determines whether this application will use the XP Windows styles for windows, controls, etc.
        ''' </summary>
        Public Property EnableVisualStyles() As Boolean

        <EditorBrowsable(EditorBrowsableState.Advanced)>
        Public Property IsSingleInstance() As Boolean
            Get
                Return _isSingleInstance
            End Get
            Set(value As Boolean)
                If value Then
                    _isSingleInstance = value
                End If
            End Set
        End Property

        ''' <summary>
        ''' The splash screen timeout specifies whether there is a minimum time that the splash
        ''' screen should be displayed for.  When not set then the splash screen is hidden
        ''' as soon as the main form becomes active.
        ''' </summary>
        ''' <value>The minimum amount of time, in milliseconds, to display the splash screen.</value>
        Public Property MinimumSplashScreenDisplayTime() As Integer = 2000

        'Minimum amount of time to show the splash screen.  0 means hide as soon as the app comes up.
        ''' <summary>
        ''' Informs My.Settings whether to save the settings on exit or not
        ''' </summary>
        Public Property SaveMySettingsOnExit() As Boolean

        ''' <summary>
        ''' Provides access to the splash screen for this application
        ''' </summary>
        Public Property SplashScreen() As Form
            Get
                Return _splashScreen
            End Get
            Set(value As Form)
                If value IsNot Nothing AndAlso value Is _appContext.MainForm Then 'allow for the case where they set splash screen = nothing and mainForm is currently nothing
                    Throw New ArgumentException("Splash And Main Form The Same")
                End If
                _splashScreen = value
            End Set
        End Property

        Private Shared Function GetTypeLibGuidForAssembly(_assembly As Assembly) As Guid
            Dim customAttributes As Object() = _assembly.GetCustomAttributes(GetType(GuidAttribute), True)
            If customAttributes.Any Then
                Dim attribute As GuidAttribute = CType(customAttributes(0), GuidAttribute)
                Return New Guid(attribute.Value)
            End If
            Return Nothing
        End Function

        ''' <summary>
        ''' Validates that the value being passed as an AuthenticationMode enum is a legal value
        ''' </summary>
        ''' <param name="value"></param>
        Private Shared Sub ValidateAuthenticationModeEnumValue(value As AuthenticationMode, paramName As String)
            If value < AuthenticationMode.Windows OrElse value > AuthenticationMode.ApplicationDefined Then
                Throw New InvalidEnumArgumentException(paramName, value, GetType(AuthenticationMode))
            End If
        End Sub

        ''' <summary>
        ''' Validates that the value being passed as an ShutdownMode enum is a legal value
        ''' </summary>
        ''' <param name="value"></param>
        Private Shared Sub ValidateShutdownModeEnumValue(value As ShutdownMode, paramName As String)
            If value < ShutdownMode.AfterMainFormCloses OrElse value > ShutdownMode.AfterAllFormsClose Then
                Throw New InvalidEnumArgumentException(paramName, value, GetType(ShutdownMode))
            End If
        End Sub

        ''' <summary>
        ''' Displays the splash screen.  We get called here from a different thread than what the
        ''' main form is starting up on.  This allows us to process events for the Splash screen so
        ''' it doesn't freeze up while the main form is getting it together.
        ''' </summary>
        Private Sub DisplaySplash()
            Debug.Assert(_splashScreen IsNot Nothing, "We should have never get here if there is no splash screen")
            If _splashTimer IsNot Nothing Then 'We only have a timer if there is a minimum time that the splash screen is supposed to be displayed.
                _splashTimer.Enabled = True 'enable the timer now that we are about to show the splash screen
            End If
            Application.Run(_splashScreen)
        End Sub

        ''' <summary>
        ''' Runs the user's program through the VB Startup/Shutdown application model
        ''' </summary>
        Private Sub DoApplicationModel()

            Dim EventArgs As New StartupEventArgs(CommandLineArgs)

            'Only do the try/catch if we aren't running under the debugger.  If we do try/catch under the debugger the debugger never gets a crack at exceptions which breaks the exception helper
            If Not Debugger.IsAttached Then
                'NO DEBUGGER ATTACHED - we use a catch so that we can run our UnhandledException code
                'Note - Sadly, code changes within this IF (that don't pertain to exception handling) need to be mirrored in the ELSE debugger attached clause below
                Try
                    If OnInitialize(CommandLineArgs) Then
                        If OnStartup(EventArgs) = True Then
                            OnRun()
                            OnShutdown()
                        End If
                    End If
                Catch ex As Exception
                    'This catch is for exceptions that happen during the On* methods above, but have occurred outside of the message pump (which exceptions we would
                    'have already seen via our hook of System.Windows.Forms.Application.ThreadException)
                    If _processingUnhandledExceptionEvent Then
                        Throw 'If the UnhandledException handler threw for some reason, throw that error out to the system.
                    Else 'We had an exception, but not during the OnUnhandledException handler so give the user a chance to look at what happened in the UnhandledException event handler
                        If Not OnUnhandledException(New UnhandledExceptionEventArgs(True, ex)) = True Then
                            Throw 'the user didn't write a handler so throw the error out to the system
                        End If
                    End If
                End Try
            Else 'DEBUGGER ATTACHED - we don't have an uber catch when debugging so the exception will bubble out to the exception helper
                'We also don't hook up the Application.ThreadException event because WinForms ignores it when we are running under the debugger
                If OnInitialize(CommandLineArgs) Then
                    If OnStartup(EventArgs) = True Then
                        OnRun()
                        OnShutdown()
                    End If
                End If
            End If
        End Sub

        ''' <summary>
        ''' The Load() event happens before the Shown and Paint events.  When we get called here
        ''' we know that the form load event is done and that the form is about to paint
        ''' itself for the first time.
        ''' We can now hide the splash screen.
        ''' Note that this function gets called from the main thread - the same thread
        ''' that creates the startup form.
        ''' </summary>
        ''' <param name="sender"></param>
        ''' <param name="e"></param>
        Private Sub MainFormLoadingDone(sender As Object, e As EventArgs)
            RemoveHandler MainForm.Load, AddressOf MainFormLoadingDone 'We don't want this event to call us again.

            'block until the splash screen time is up.  See MinimumSplashExposureTimeIsUp() which releases us
            While Not _ok2CloseSplashScreen
                DoEvents() 'In case Load() event, which we are waiting for, is doing stuff that requires windows messages.  our timer message doesn't count because it is on another thread.
            End While

            HideSplashScreen()
        End Sub

        ''' <summary>
        ''' If a splash screen has a minimum time out, then once that is up we check to see whether
        ''' we should close the splash screen.  If the main form has activated then we close it.
        ''' Note that we are getting called on a secondary thread here which isn't necessarily
        ''' associated with any form.  Don't touch forms from this function.
        ''' </summary>
        Private Sub MinimumSplashExposureTimeIsUp(sender As Object, e As Timers.ElapsedEventArgs)
            If _splashTimer IsNot Nothing Then 'We only have a timer if there was a minimum timeout on the splash screen
                _splashTimer.Dispose()
                _splashTimer = Nothing
            End If
            _ok2CloseSplashScreen = True
        End Sub

        Private Sub OnStartupNextInstanceMarshallingAdaptor(ByVal args As String())
            If MainForm Is Nothing Then
                Return
            End If
            Dim invoked As Boolean = False
            Try
                MainForm.Invoke(
                    Sub()
                        invoked = True
                        OnStartupNextInstance(New StartupNextInstanceEventArgs(New ReadOnlyCollection(Of String)(args), True)) 'by default, we set BringToFront as True since that's the behavior most people will want
                    End Sub)
            Catch ex As Exception When Not invoked
                ' Only catch exceptions thrown when the UI thread is not available, before
                ' the UI thread has been created or after it has been terminated. Exceptions
                ' thrown from OnStartupNextInstance() should be allowed to propagate.
            End Try
        End Sub

        ''' <summary>
        ''' Handles the Windows.Forms.Application.ThreadException event and raises our Unhandled
        ''' exception event
        ''' </summary>
        ''' <param name="e"></param>
        ''' <remarks>Our UnHandledException event has a different signature then the Windows.Forms.Application
        ''' unhandled exception event so we do the translation here before raising our event.
        ''' </remarks>
        Private Sub OnUnhandledExceptionEventAdaptor(sender As Object, e As ThreadExceptionEventArgs)
            OnUnhandledException(New UnhandledExceptionEventArgs(True, e.Exception))
        End Sub

        Protected Overridable Sub Dispose(disposing As Boolean)
            If _isDisposed Then
                Return
            End If

            If disposing Then
                ' free managed resources
                If _splashTimer IsNot Nothing Then
                    _splashTimer.Dispose()
                End If
                If _appContext IsNot Nothing Then
                    _appContext.Dispose()
                End If
            End If
            _isDisposed = True
        End Sub

        ''' <summary>
        ''' Hide the splash screen.  The splash screen was created on another thread
        ''' thread (main thread) than the one it was run on (secondary thread for the
        ''' splash screen so it doesn't block app startup. We need to invoke the close.
        ''' This function gets called from the main thread by the app fx.
        ''' </summary>
        <EditorBrowsable(EditorBrowsableState.Advanced)>
        <SecuritySafeCritical()>
        Protected Sub HideSplashScreen()
            SyncLock _splashLock 'This ultimately wasn't necessary.  I suppose we better keep it for backwards comparability
                'Dev10 590587 - we now activate the main form before calling Dispose on the Splash screen. (we're just
                '       swapping the order of the two If blocks). This is to fix the issue where the main form
                '       doesn't come to the front after the Splash screen disappears
                If MainForm IsNot Nothing Then
                    Call New UIPermission(UIPermissionWindow.AllWindows).Assert()
                    MainForm.Activate()
                    PermissionSet.RevertAssert() 'CLR also reverts if we throw or when we return from this function
                End If
                If _splashScreen IsNot Nothing AndAlso Not _splashScreen.IsDisposed Then
                    Dim TheBigGoodbye As New DisposeDelegate(AddressOf _splashScreen.Dispose)
                    _splashScreen.Invoke(TheBigGoodbye)
                    _splashScreen = Nothing
                End If
            End SyncLock
        End Sub

        ''' <summary>
        ''' Provides a hook that designers will override to set the main form.
        ''' </summary>
        <EditorBrowsable(EditorBrowsableState.Advanced)>
        Protected Overridable Sub OnCreateMainForm()
            MainForm = New Form1
        End Sub

        ''' <summary>
        ''' A designer will override this method and provide a splash screen if this application has one.
        ''' </summary>
        ''' <remarks>For instance, a designer would override this method and emit: Me.Splash = new Splash
        ''' where Splash was designated in the application designer as being the splash screen for this app</remarks>
        <EditorBrowsable(EditorBrowsableState.Advanced)>
        Protected Overridable Sub OnCreateSplashScreen()
            SplashScreen = New SplashScreen1
        End Sub

        ''' <summary>
        ''' This exposes the first in a series of extensibility points for the Startup process.  By default, it shows
        ''' the splash screen and does rudimentary processing of the command line to see if /nosplash or its
        ''' variants was passed in.
        ''' </summary>
        ''' <param name="commandLineArgs"></param>
        ''' <returns>Returning True indicates that we should continue on with the application Startup sequence</returns>
        ''' <remarks>This extensibility point is exposed for people who want to override the Startup sequence at the earliest possible point
        ''' to </remarks>
        <EditorBrowsable(EditorBrowsableState.Advanced), STAThread()>
        Protected Overridable Function OnInitialize(commandLineArgs As ReadOnlyCollection(Of String)) As Boolean
            If commandLineArgs Is Nothing Then
                Throw New ArgumentNullException(NameOf(commandLineArgs))
            End If
            ' EnableVisualStyles
            If EnableVisualStyles Then
                Application.EnableVisualStyles()
            End If

            'We'll handle /nosplash for you
            If Not (commandLineArgs.Contains("/nosplash") OrElse Me.CommandLineArgs.Contains("-nosplash")) Then
                ShowSplashScreen()
            End If

            Return True 'true means to not bail out but keep on running after OnIntiailize() finishes
        End Function

        ''' <summary>
        ''' At this point, the command line args should have been processed and the application will create the
        ''' main form and enter the message loop.
        ''' </summary>
        <SecuritySafeCritical()>
        <EditorBrowsable(EditorBrowsableState.Advanced)>
        Protected Overridable Sub OnRun()
            If MainForm Is Nothing Then
                OnCreateMainForm() 'A designer overrides OnCreateMainForm() to set the main form we are supposed to use
                If MainForm Is Nothing Then
                    Throw New NoStartupFormException
                End If

                'When we have a splash screen that hasn't timed out before the main form is ready to paint, we want to
                'block the main form from painting.  To do that I let the form get past the Load() event and hold it until
                'the splash screen goes down.  Then I let the main form continue it's startup sequence.  The ordering of
                'Form startup events for reference is: Ctor(), Load Event, Layout event, Shown event, Activated event, Paint event
                AddHandler MainForm.Load, AddressOf MainFormLoadingDone
            End If

            'Run() eats all exceptions (unless running under the debugger) If the user wrote an UnhandledException handler we will hook
            'the System.Windows.Forms.Application.ThreadException event (see Public Custom Event UnhandledException) which will raise our
            'UnhandledException Event.  If our user didn't write an UnhandledException event, then we land in the try/catch handler for Forms.Application.Run()
            Try
                Application.Run(_appContext)
            Finally
                'When Run() returns, the context we pushed in our ctor (which was a WindowsFormsSynchronizationContext) is restored.  But we are going to dispose it
                'so we need to disconnect the network listener so that it can't fire any events in response to changing network availability conditions through a dead context.  VSWHIDBEY #343374

                AsyncOperationManager.SynchronizationContext = _appSyncronizationContext 'Restore the prior sync context
                _appSyncronizationContext = Nothing
            End Try
        End Sub

        ''' <summary>
        ''' The last in a series of extensibility points for the Shutdown process
        ''' </summary>
        <EditorBrowsable(EditorBrowsableState.Advanced)>
        Protected Overridable Sub OnShutdown()
            RaiseEvent Shutdown(Me, EventArgs.Empty)
        End Sub

        ''' <summary>
        ''' Extensibility point which raises the Startup event
        ''' </summary>
        ''' <param name="eventArgs"></param>
        <EditorBrowsable(EditorBrowsableState.Advanced)>
        Protected Overridable Function OnStartup(eventArgs As StartupEventArgs) As Boolean
            If eventArgs Is Nothing Then
                Throw New ArgumentNullException(NameOf(eventArgs))
            End If

            eventArgs.Cancel = False
            'It is important not to create the network object until the ExecutionContext has everything on it.  By now the principal will be on the thread so
            'we can create the network object.  The timing is important because the network object has an AsyncOperationsManager in it that marshals
            'the network changed event to the main thread.  The asycnOperationsManager does a CreateOperation() which makes a copy of the executionContext
            'That execution context shows up on your thread during the callback so I delay creating the network object (and consequently the capturing of the
            'execution context) until the principal has been set on the thread.
            'this avoid the problem where My.User isn't set during the NetworkAvailabilityChanged event.  This problem would just extend
            'itself to any future callback that involved the asyncOperationsManager so this is where we need to create objects that have a asyncOperationsContext
            'in them.
            RaiseEvent Startup(Me, eventArgs)
            Return Not eventArgs.Cancel
        End Function

        ''' <summary>
        ''' Extensibility point which raises the StartupNextInstance
        ''' </summary>
        ''' <param name="eventArgs"></param>
        <EditorBrowsable(EditorBrowsableState.Advanced)>
        <SecuritySafeCritical()>
        Protected Overridable Sub OnStartupNextInstance(eventArgs As StartupNextInstanceEventArgs)
            If eventArgs Is Nothing Then
                Throw New ArgumentNullException(NameOf(eventArgs))
            End If

            'Activate the original instance
            RaiseEvent StartupNextInstance(Me, eventArgs)

            Call New UIPermission(UIPermissionWindow.SafeSubWindows Or UIPermissionWindow.SafeTopLevelWindows).Assert()
            If eventArgs.BringToForeground = True AndAlso MainForm IsNot Nothing Then
                If MainForm.WindowState = System.Windows.Forms.FormWindowState.Minimized Then
                    MainForm.WindowState = FormWindowState.Normal
                End If
                MainForm.Activate()
            End If
        End Sub

        ''' <summary>
        ''' Raises the UnHandled exception event and exits the application if the event handler indicated
        ''' that execution shouldn't continue
        ''' </summary>
        ''' <param name="e"></param>
        ''' <returns>True indicates the exception event was raised / False it was not</returns>
        <EditorBrowsable(EditorBrowsableState.Advanced)>
        Protected Overridable Function OnUnhandledException(e As UnhandledExceptionEventArgs) As Boolean
            If e Is Nothing Then
                Throw New ArgumentNullException(NameOf(e))
            End If

            If _unhandledExceptionHandlers IsNot Nothing AndAlso _unhandledExceptionHandlers.Count > 0 Then 'Does the user have a handler for this event?
                'We don't put a try/catch around the handler event so that exceptions in there will bubble out - else we will have a recursive exception handler
                RaiseEvent UnhandledException(Me, e)
                If e.ExitApplication = True Then Application.Exit()
                Return True 'User handled the event
            End If
            Return False 'Nobody was listening to the UnhandledException event
        End Function

        ''' <summary>
        ''' Uses the extensibility model to see if there is a splash screen provided for this app and if there is,
        ''' displays it.
        ''' </summary>
        <EditorBrowsable(EditorBrowsableState.Advanced)>
        Protected Sub ShowSplashScreen()
            If Not _didSplashScreen Then
                _didSplashScreen = True
                If _splashScreen Is Nothing Then
                    OnCreateSplashScreen() 'If the user specified a splash screen, the designer will have overridden this method to set it
                End If
                If _splashScreen IsNot Nothing Then
                    'Some splash screens have minimum face time they are supposed to get.  We'll set up a time to let us know when we can take it down.
                    If MinimumSplashScreenDisplayTime > 0 Then
                        _ok2CloseSplashScreen = False 'Don't close until the timer expires.
                        _splashTimer = New Timers.Timer(MinimumSplashScreenDisplayTime)
                        AddHandler _splashTimer.Elapsed, AddressOf MinimumSplashExposureTimeIsUp
                        _splashTimer.AutoReset = False
                        'We'll enable it in DisplaySplash() once the splash screen thread gets running
                    Else
                        _ok2CloseSplashScreen = True 'No timeout so just close it when then main form comes up
                    End If
                    'Run the splash screen on another thread so we don't starve it for events and painting while the main form gets its act together
                    Dim SplashThread As New Thread(AddressOf DisplaySplash)
                    SplashThread.Start()
                End If
            End If
        End Sub

        ''' <summary>
        '''  Processes all windows messages currently in the message queue
        ''' </summary>
        Public Shared Sub DoEvents()
            Application.DoEvents()
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            Dispose(True)
            GC.SuppressFinalize(Me)
        End Sub

        ''' <summary>
        ''' Generates the name for the remote singleton that we use to channel multiple instances
        ''' to the same application model thread.
        ''' </summary>
        ''' <returns>GUID String that should be the same for versions of the application
        ''' that have the same Major and Minor Version Number
        ''' </returns>
        ''' <remarks></remarks>
        <SecurityCritical()>
        Public Overridable Function GetApplicationInstanceID(Entry As Assembly) As String
            If Entry Is Nothing Then
                Throw New ArgumentNullException(NameOf(Entry))
            End If

            Dim permissions As New PermissionSet(PermissionState.None)
            permissions.AddPermission(New FileIOPermission(PermissionState.Unrestricted)) 'Chicken and egg problem.  All I need is PathDiscovery for the location of this assembly but to get the location of the assembly (see GetName below) I need to know the path which I can't get without asserting...
            permissions.AddPermission(New SecurityPermission(SecurityPermissionFlag.UnmanagedCode))
            permissions.Assert()

            Dim typeLibGuid As Guid = GetTypeLibGuidForAssembly(Entry)
            If typeLibGuid = Nothing Then
                Return Entry.ManifestModule.ModuleVersionId.ToString
            End If
            Dim Version As String = Entry.GetName.Version.ToString
            Dim VersionParts As String() = Version.Split(CType(".", Char()))
            Dim SemiUniqueApplicationID As String = typeLibGuid.ToString + VersionParts(0) + "." + VersionParts(1)
            PermissionSet.RevertAssert()

            'Note: We used to make the terminal server session ID part of the key.  It turns out to be unnecessary and the call to
            'NativeMethods.ProcessIdToSessionId(System.Diagnostics.Process.GetCurrentProcess.Id, TerminalSessionID) was not supported on Win98, anyway.
            'It turns out that terminal server sessions, even when you are logged in as the same user to multiple terminal server sessions on the same
            'machine, are separate.  So you can have session 1 running as  and have a global system object named "FOO" that won't conflict with
            'any other global system object named "FOO" whether it be in session 2 running as  or session n running as whoever.
            'So it isn't necessary to make the session id part of the unique name that identifies a

            Return SemiUniqueApplicationID  'Re: version parts, we have the major, minor, build, revision.  We key off major+minor.
        End Function

        ''' <summary>
        ''' Entry point to kick off the VB Startup/Shutdown Application model
        ''' </summary>
        ''' <param name="commandLine">The command line from Main()</param>
        <SecuritySafeCritical()>
        Public Sub Run(commandLine As String())
            'Prime the command line args with what we receive from Main() so that Click-Once windows apps don't have to do a System.Environment call which would require permissions.
            InternalCommandLine = New ReadOnlyCollection(Of String)(commandLine)

            If Not IsSingleInstance Then
                DoApplicationModel()
            Else 'This is a Single-Instance application
                Dim ApplicationInstanceID As String = GetApplicationInstanceID(Assembly.GetCallingAssembly) 'Note: Must pass the calling assembly from here so we can get the running app.  Otherwise, can break single instance.
                Dim pipeServer As NamedPipeServerStream = CreatePipeServer(ApplicationInstanceID)
                If pipeServer IsNot Nothing Then
                    '--- This is the first instance of a single-instance application to run.  This is the instance that subsequent instances will attach to.
                    Using pipeServer
                        WaitForClientConnectionAsync(pipeServer, AddressOf OnStartupNextInstanceMarshallingAdaptor)
                        DoApplicationModel()
                    End Using
                Else '--- We are launching a subsequent instance.
                    If Not SendSecondInstanceArgs(ApplicationInstanceID, SECOND_INSTANCE_TIMEOUT, commandLine) Then
                        Throw New CantStartSingleInstanceException()
                    End If
                End If
            End If 'Single-Instance application
        End Sub

        ''' <summary>
        ''' Encapsulates an ApplicationContext.  We have our own to get the shutdown behaviors we
        ''' offer in the application model.  This derivation of the ApplicationContext listens for when
        ''' the main form closes and provides for shutting down when the main form closes or the
        ''' last form closes, depending on the mode this application is running in.
        ''' </summary>
        Private Class WinFormsAppContext : Inherits ApplicationContext
            Private ReadOnly _app As WindowsFormsApplicationBase

            Sub New(App As WindowsFormsApplicationBase)
                _app = App
            End Sub

            ''' <summary>
            ''' Handles the two types of application shutdown:
            '''   1 - shutdown when the main form closes
            '''   2 - shutdown only after the last form closes
            ''' </summary>
            ''' <param name="sender"></param>
            ''' <param name="e"></param>
            <SecuritySafeCritical()>
            Protected Overrides Sub OnMainFormClosed(sender As Object, e As EventArgs)
                If _app.ShutdownStyle = ShutdownMode.AfterMainFormCloses Then
                    MyBase.OnMainFormClosed(sender, e)
                Else 'identify a new main form so we can keep running
                    Call New UIPermission(UIPermissionWindow.AllWindows).Assert()
                    Dim forms As FormCollection = Application.OpenForms
                    PermissionSet.RevertAssert() 'CLR also reverts if we throw or when we return from this function.
                    If forms.Count > 0 Then
                        'Note: Initially I used Process::MainWindowHandle to obtain an open form.  But that is bad for two reasons:
                        '1 - It appears to be broken and returns NULL sometimes even when there is still a window around.  WinForms people are looking at that issue.
                        '2 - It returns the first window it hits from enum thread windows, which is not necessarily a windows forms form, so that doesn't help us even if it did work
                        'all the time.  So I'll use one of our open forms.  We may not necessarily get a visible form here but that's OK.  Some apps may run on an invisible window
                        'and we need to keep them going until all windows close.
                        MainForm = forms(0)
                    Else
                        MyBase.OnMainFormClosed(sender, e)
                    End If
                End If
            End Sub

        End Class 'WinFormsAppContext

    End Class 'WindowsFormsApplicationBase

End Namespace
