Namespace Microsoft.VisualBasic.CompilerServices
    Public NotInheritable Class ExceptionUtils
        ''' <summary>
        ''' Returns a new instance of ArgumentNullException with message: "Argument cannot be Nothing."
        ''' </summary>
        ''' <param name="ArgumentName">The name of the argument (parameter). Not localized.</param>
        ''' <returns>A new instance of ArgumentNullException.</returns>
        Friend Shared Function GetArgumentNullException(ArgumentName As String) As ArgumentNullException
            Return New ArgumentNullException(ArgumentName)
        End Function

    End Class
End Namespace
