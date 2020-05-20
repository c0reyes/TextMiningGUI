as.data.frame.HJBiplot <- function (X, ...) as.data.frame.Biplot(X, ...)

as.data.frame.JKBiplot <- function (X, ...) as.data.frame.Biplot(X, ...)

as.data.frame.GHBiplot <- function (X, ...) as.data.frame.Biplot(X, ...)

as.data.frame.Biplot <- function (X, row.lab = "Rows", col.lab = "Columns") {
  df <- data.frame(
    Label = c(rownames(X$ColCoordinates), rownames(X$RowCoordinates)),
    Dim1 = c(X$ColCoordinates[,1], X$RowCoordinates[,1]),
    Dim2 = c(X$ColCoordinates[,2], X$RowCoordinates[,2]),
    Con1 = c(X$ColContributions[,1], X$RowContributions[,1]),
    Con2 = c(X$ColContributions[,2], X$RowContributions[,2]),
    Variable = c(rep(col.lab, nrow(X$ColCoordinates)), rep(row.lab, nrow(X$RowCoordinates)))
  )
  rownames(df) <- 1:nrow(df)
  return(df)
}