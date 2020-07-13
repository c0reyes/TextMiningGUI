HJBiplot <- function(X, name = "HJBiplot", dimension = 3, scale = 2) {
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
  
  X[is.na(X)] <- 0
  SD <- svd(X, nu = dimension, nv = dimension)
  EV <- SD$d^2
  
  biplot$inertia <- round((EV/sum(EV)) * 100, digits = 3)
  biplot$cuminertia <- cumsum(biplot$inertia)
  
  biplot$RowCoordinates <- SD$u * rep(SD$d[1:dimension], rep.int(nrow(SD$u), dimension))
  biplot$ColCoordinates <- SD$v * rep(SD$d[1:dimension], rep.int(nrow(SD$v), dimension))

  sf <- apply((X^2), 1, sum)
  biplot$RowContributions <- matrix(0, nrow(X), dimension)
  for (k in 1:dimension)
    biplot$RowContributions[,k] <- round((biplot$RowCoordinates[,k]^2 / sf) * 100, digits = 2)

  sc <- apply((X^2), 2, sum)
  biplot$ColContributions <- round(((diag(1/sc)) %*% biplot$ColCoordinates^2) * 100, digits = 2)

  biplot$RowCoordinates <- biplot$RowCoordinates * scale
  biplot$ColCoordinates <- biplot$ColCoordinates / scale

  rownames(biplot$RowCoordinates) <- RowNames
  colnames(biplot$RowCoordinates) <- DimNames
  
  rownames(biplot$ColCoordinates) <- VarNames
  colnames(biplot$ColCoordinates) <- DimNames
  
  rownames(biplot$RowContributions) <- RowNames
  colnames(biplot$RowContributions) <- DimNames

  rownames(biplot$ColContributions) <- VarNames
  colnames(biplot$ColContributions) <- DimNames
  
  class(biplot) <- "HJBiplot"
  
  return(biplot)
}