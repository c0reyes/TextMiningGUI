CA <- function(X, dimension = 2) {
  	diagonal <- function(d) {                                                        
    	n <- length(d)                                                                
    	D <- diag(1, n, n)                                                            
    	diag(D) <- d                                                                 
    	return(D)                                                                            
  	}  
  
	if(is.data.frame(X)) 
    	X <- as.matrix(X)

  	if (is.null(rownames(X))) 
    	rownames(X) <- rownames(X, do.NULL = FALSE, prefix = "I")
  	if (is.null(colnames(X))) 
    	colnames(X) <- colnames(X, do.NULL = FALSE, prefix = "V")

  	RowNames <- rownames(X)
  	VarNames <- colnames(X)
  	DimNames <- unlist(lapply(1:dimension, function(x) paste0("Dim", x)))

  	n <- dim(X)[1]
	p <- dim(X)[2]
	nt <- sum(X)
	X <- X/nt
	dr <- matrix(rowSums(X), n, 1)
	dc <- matrix(colSums(X), p, 1)
	X <- X - dr %*% t(dc)

	Dr <- diag(1, n, n)
	diag(Dr) <- 1/sqrt(dr)
	Dc <- diag(1, p, p)
	diag(Dc) <- 1/sqrt(dc)
	X <- Dr %*% X %*% Dc
	UDV <- svd(X)

	r <- min(c(n, p))
	d <- UDV$d[1:r]
	iner <- ((d^2)/sum((d^2))) * 100
	U <- diagonal(sqrt(1/dr)) %*% UDV$u[, 1:r]
	V <- diagonal(sqrt(1/dc)) %*% UDV$v[, 1:r]
	
	D <- diagonal(d)
	A <- U %*% D
	B <- V %*% D

	sf <- rowSums(A^2)
	cf <- solve(diagonal(sf)) %*% (A^2)
	sc <- rowSums(B^2)
	cc <- solve(diagonal(sc)) %*% (B^2)

	afc <- list()
	afc$title <- "Correspondence Analysis"

	afc$inertia = iner
	afc$cuminertia = cumsum(iner)

	afc$RowCoordinates <- A[, 1:dimension]
	rownames(afc$RowCoordinates) <- RowNames
	colnames(afc$RowCoordinates) <- DimNames

	afc$ColCoordinates <- B[, 1:dimension]
  	rownames(afc$ColCoordinates) <- VarNames
	colnames(afc$ColCoordinates) <- DimNames
	
	afc$RowContributions <- round(cf[, 1:dimension] * 100, digits = 2)
	rownames(afc$RowContributions) <- RowNames
	colnames(afc$RowContributions) <- DimNames
	
	afc$ColContributions <- round(cc[, 1:dimension] * 100, digits = 2)
	rownames(afc$ColContributions) <- VarNames
	colnames(afc$ColContributions) <- DimNames

	class(afc) <- "CA"

	return(afc)
}