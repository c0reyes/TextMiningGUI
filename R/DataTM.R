DataTM <- function(DF, language, steam = TRUE, sparse = 1, normalize = "char-value") {
    tm_group <- function(texto) {
        text = iconv(texto, to = "ASCII//TRANSLIT")
        corpus <- Corpus(VectorSource(text))
        corpus

        d <- tm_map(corpus, tolower)
        d <- tm_map(d, stripWhitespace)
        d <- tm_map(d, removePunctuation)
        d <- tm_map(d, removeNumbers)

        if(steam)
            d <- tm_map(d, stemDocument) # SnowballC

        d <- tm_map(d, removeWords, stopwords(language))

        tdm <- TermDocumentMatrix(d, control = list(weighting = (if(normalize == "tf-idf") weightTfIdf else weightTf)))

        if(sparse < 1)
            tdm = removeSparseTerms(tdm, sparse)

        console(cmds = "inspect(tdm)", e = environment())
            
        v <- sort(row_sums(tdm), decreasing = TRUE)

        df <- data.frame(word = names(v), freq = v)
        return(df)
    }

    df <- DF %>% group_by(GROUP) %>% group_modify(~ tm_group(.x$TEXT)) 
    TM <- df %>% group_by(GROUP) %>% pivot_wider(names_from = GROUP, values_from = freq) %>% column_to_rownames(var = "word")
    TM[is.na(TM)] <- 0

    tm <- list()
    tm$data <- if(normalize == "char-value") Convert(TM) else TM
    tm$freq <- df
    tm$lang <- language
    tm$dist <- df %>% group_by(GROUP) %>% summarise(sum = n()) 
    class(tm) <- "DataTM"

    return(tm)
}