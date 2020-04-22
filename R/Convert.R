Convert <- function(DF) {
    colMax <- function(data) sapply(data, max, na.rm = TRUE)
    
    calc <- function(x, maxj) {
        return( x / (sqrt(max(x)) * sqrt(maxj)) )
    }

    df.col.max <- colMax(DF)
    df <- t(apply(DF, 1, calc, maxj = df.col.max))
    return(df)
}