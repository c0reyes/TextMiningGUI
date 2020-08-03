Emotions <- function(X, language) {
    X <- as.vector(X)

    if(requireNamespace("parallel", quietly = TRUE)) {
        cores <- max(1, parallel::detectCores() - 1)
        cl <- parallel::makeCluster(cores)
        parallel::clusterExport(cl = cl, c("get_sentiment", "get_sent_values", "get_nrc_sentiment", "get_nrc_values"))
        emotion.df <- syuzhet::get_nrc_sentiment(char_v = X, language = language, cl = cl)
        parallel::stopCluster(cl)
    }else{
        emotion.df <- syuzhet::get_nrc_sentiment(char_v = X, language = language)
    }

    emotion.df <- data.frame(t(emotion.df))
    emotion.df <- data.frame(row_sums(emotion.df))
    names(emotion.df)[1] <- "count"
    emotion.df <- cbind("emotion" = rownames(emotion.df), emotion.df)

    return(emotion.df)
}