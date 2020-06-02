Emotions <- function(X, language) {
    X <- as.vector(X)

    if(require(parallel)) {
        cores <- if(detectCores() - 1 > 0)  detectCores() - 1 else 1
        cl <- makeCluster(cores)
        clusterExport(cl = cl, c("get_sentiment", "get_sent_values", "get_nrc_sentiment", "get_nrc_values", "parLapply"))
        emotion.df <- get_nrc_sentiment(char_v = X, language = language, cl = cl)
        stopCluster(cl)
    }else{
        emotion.df <- get_nrc_sentiment(char_v = X, language = language)
    }

    emotion.df <- data.frame(t(emotion.df))
    emotion.df <- data.frame(row_sums(emotion.df))
    names(emotion.df)[1] <- "count"
    emotion.df <- cbind("emotion" = rownames(emotion.df), emotion.df)

    return(emotion.df)
}