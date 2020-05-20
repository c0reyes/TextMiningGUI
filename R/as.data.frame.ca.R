as.data.frame.ca <- function (X, row.lab = "Rows", col.lab = "Columns") {
  df <- data.frame(
    Label = c(rownames(X$colcoord), rownames(X$rowcoord)),
    Dim1 = c(X$colcoord[,1], X$rowcoord[,1]),
    Dim2 = c(X$colcoord[,2], X$rowcoord[,2]),
    Con1 = c(X$colcontrib[,1], X$rowcontrib[,1]),
    Con2 = c(X$colcontrib[,2], X$rowcontrib[,2]),
    Variable = c(rep(col.lab, nrow(X$colcoord)), rep(row.lab, nrow(X$rowcoord)))
  )
  rownames(df) <- 1:nrow(df)
  return(df)
}