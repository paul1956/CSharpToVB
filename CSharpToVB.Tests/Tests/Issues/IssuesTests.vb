' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports Helpers
Imports Xunit

Namespace Tests.Issues

    <TestClass()> Public Class IssueTests
        Inherits ConverterTestBase

        <Fact> '(Skip:="Not implemented yet")>
        Public Shared Sub CustomEventsConversionIssue76()
            TestConversionCSharpToVisualBasic("namespace Test
{

    public class C
    {
        private static event SmallBasicCallback _buttonClicked;

            /// <summary>
            /// Raises an event when any button control is clicked.
            /// </summary>
            public static event SmallBasicCallback ButtonClicked
            {
                add
                {
                    Controls._buttonClicked = null;
                    _buttonClicked += value;
                }
                remove
                {
                    _buttonClicked -= value;
                }
            }

    }
}", "Namespace Test

    Public Class C

        Private Shared _buttonClicked As New ComponentModel.EventHandlerList

        ''' <summary>
        ''' Raises an event when any button control is clicked.
        ''' </summary>
        Public Shared Custom Event ButtonClicked As SmallBasicCallback
            AddHandler(Value As SmallBasicCallback)
                Dim tempVar As SmallBasicCallback = TryCast(_buttonClicked(""ButtonClicked""), SmallBasicCallback)
                If tempVar IsNot Nothing Then _buttonClicked.RemoveHandler(""ButtonClicked"", Value)
                _buttonClicked.AddHandler(""ButtonClicked"", Value)
            End AddHandler

            RemoveHandler(Value As SmallBasicCallback)
                _buttonClicked.RemoveHandler(""ButtonClicked"", Value)
            End RemoveHandler

            RaiseEvent()
                Dim tempVar3 As SmallBasicCallback = TryCast(_buttonClicked(""ButtonClicked""), SmallBasicCallback)
                If tempVar3 IsNot Nothing Then tempVar3.Invoke()
            End RaiseEvent
        End Event

    End Class
End Namespace
")
        End Sub

        <Fact>
        Public Shared Sub MissingAddressOfIssue77()
            TestConversionCSharpToVisualBasic("namespace Test
{
    public sealed class Timer
        {
            private static void ThreadTimerCallback(object tag)
            {
                if (Timer._tick != null)
                {
                    Timer._tick();
                }
            }

            static Timer()
            {
                _interval = 100000000;
                _threadTimer = new System.Threading.Timer(ThreadTimerCallback);
            }
    }
}", "Namespace Test

    Public NotInheritable Class Timer

        Private Shared Sub ThreadTimerCallback(tag As Object)
            If Timer._tick IsNot Nothing Then
                Timer._tick()
            End If
        End Sub

        Shared Sub New()
            _interval = 100000000
            _threadTimer = New Threading.Timer(AddressOf ThreadTimerCallback)
        End Sub
    End Class
End Namespace
")
        End Sub

        <Fact>
        Public Shared Sub MissingAddressOfIssue77A()
            TestConversionCSharpToVisualBasic("namespace Test
{
    public sealed class C
        {
        private void M(object tag)
            {
                _applicationThread = new Thread((ThreadStart)delegate
                {
                    Application application = new Application();
                    autoEvent.Set();
                   _application = application;
                   application.Run();
                });
            }
        }
}", "Namespace Test

    Public NotInheritable Class C

        Private Sub M(tag As Object)
            _applicationThread = New Thread(CType(Sub()
                                                      Dim application1 As New Application
                                                      autoEvent.[Set]()
                                                      _application = application1
                                                      application1.Run()
                                                  End Sub, ThreadStart))
        End Sub
    End Class
End Namespace
")
        End Sub

    End Class

End Namespace
