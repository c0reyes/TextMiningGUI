Distance <- function(X, slope) {
    xyend <- function(X, m) {
        p <- -1/m
        b <- X[2] - p * X[1]
        x <- b / (m - p)
        y <- m * x
        return(c(x, y))
    }

    end <- t(apply(X[,1:2], 1, xyend, m = slope))
    colnames(end) <- c("xend", "yend")

    df <- cbind(X, end)
    df <- as.data.frame(df)
    return(df)
}