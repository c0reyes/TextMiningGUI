DataTM <- function(DF, language) {
    .tm_group <- function(texto, language) {
        text = iconv(texto, to="ASCII//TRANSLIT")
        corpus <- Corpus(VectorSource(text))
        corpus

        d <- tm_map(corpus, tolower)
        d <- tm_map(d, stripWhitespace)
        d <- tm_map(d, removePunctuation)
        d <- tm_map(d, removeNumbers)
        d <- tm_map(d, removeWords, stopwords(language))

        tdm <- TermDocumentMatrix(d)
        v <- sort(row_sums(tdm), decreasing=TRUE)

        df <- data.frame(word = names(v), freq = v)
        return(df)
    }

    df <- DF %>% group_by(GROUP) %>% group_modify(~ .tm_group(.x$TEXT, language = language)) 
    TM <- df %>% group_by(GROUP) %>% pivot_wider(names_from = GROUP, values_from = freq) %>% column_to_rownames(var = "word")
    TM[is.na(TM)] <- 0

    tm <- list()
    tm$data <- TM
    tm$token <- df
    tm$sum <- df %>% group_by(GROUP) %>% summarise(sum = n()) 
    tm$dist <- df %>% select(GROUP, word) %>% distinct() %>% group_by(GROUP) %>% summarise(distinct = n())
    class(tm) <- "DataTM"

    return(tm)
}