as.data.frame.HJBiplot <- function (x, row.names, optional, row.lab = "Rows", col.lab = "Columns", ...) {
  df <- data.frame(
    Label = c(rownames(x$ColCoordinates), rownames(x$RowCoordinates)),
    Dim1 = c(x$ColCoordinates[,1], x$RowCoordinates[,1]),
    Dim2 = c(x$ColCoordinates[,2], x$RowCoordinates[,2]),
    Con1 = c(x$ColContributions[,1], x$RowContributions[,1]),
    Con2 = c(x$ColContributions[,2], x$RowContributions[,2]),
    Variable = c(rep(col.lab, nrow(x$ColCoordinates)), rep(row.lab, nrow(x$RowCoordinates)))
  )
  rownames(df) <- 1:nrow(df)
  return(df)
}