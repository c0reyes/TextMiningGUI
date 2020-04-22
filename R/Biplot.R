HJBiplot <- function(X, ...) Biplot(X, name = "HJBiplot", A = TRUE, B = TRUE, scale = 2)

JKBiplot <- function(X, ...) Biplot(X, name = "JKBiplot", A = TRUE, B = FALSE, scale = 1)

GHBiplot <- function(X, ...) Biplot(X, name = "GHBiplot", A = FALSE, B = TRUE, scale = 1)

Biplot <- function(X, dimension = 3, name = "Biplot", A = TRUE, B = TRUE, scale = 2) {
  if(is.data.frame(X)) 
    X <- as.matrix(X)
  
  X <- scale(X)
  
  biplot <- list()
  biplot$name = name
  
  if (is.null(rownames(X))) 
    rownames(X) <- rownames(X, do.NULL = FALSE, prefix = "I")
  if (is.null(colnames(X))) 
    colnames(X) <- colnames(X, do.NULL = FALSE, prefix = "V")
  
  RowNames <- rownames(X)
  VarNames <- colnames(X)
  DimNames <- unlist(lapply(1:dimension, function(x) paste0("Dim", x)))
  
  SD <- svd(X, nu = dimension, nv = dimension)
  EV <- SD$d^2
  
  biplot$inertia <- round((EV/sum(EV)) * 100, digits = 3)
  biplot$cuminertia <- cumsum(biplot$inertia)
  
  if(A)
    biplot$RowCoordinates <- (SD$u %*% diag(SD$d[1:dimension])) * scale
  else
    biplot$RowCoordinates <- SD$u
  
  if(B)
    biplot$ColCoordinates <- (SD$v %*% diag(SD$d[1:dimension])) / scale
  else
    biplot$ColCoordinates <- SD$v
  
  rownames(biplot$RowCoordinates) <- RowNames
  colnames(biplot$RowCoordinates) <- DimNames
  rownames(biplot$ColCoordinates) <- VarNames
  colnames(biplot$ColCoordinates) <- DimNames
  
  class(biplot) <- name
  
  return(biplot)
}