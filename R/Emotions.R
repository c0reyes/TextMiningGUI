Emotions <- function(X, language) {
    X <- as.vector(X)

    cores <- detectCores() - 1
    cl <- makeCluster(cores)
    emotion.df <- get_nrc_sentiment(char_v = X, language = language, cl = cl)
    stopCluster(cl)
    
    emotion.df <- data.frame(t(emotion.df))
    emotion.df <- data.frame(rowSums(emotion.df))
    names(emotion.df)[1] <- "count"
    emotion.df <- cbind("emotion" = rownames(emotion.df), emotion.df)

    return(emotion.df)
}