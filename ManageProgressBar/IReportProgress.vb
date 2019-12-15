Public Interface IReportProgress

    Sub Clear()

    Sub SetTotalItems(TotalItems As Integer)

    Sub UpdateProgress(Increment As Integer)

End Interface
