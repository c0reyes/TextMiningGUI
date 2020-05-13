DataTM <- function(DF, language, steam = TRUE, sparse = 1, normalize = "chara-value") {
    tm_group <- function(texto) {
        text = iconv(texto, to = "ASCII//TRANSLIT")
        corpus <- Corpus(VectorSource(text))

        d <- tm_map(corpus, tolower)
        d <- tm_map(d, stripWhitespace)
        d <- tm_map(d, removePunctuation)
        d <- tm_map(d, removeNumbers)
        d <- tm_map(d, removeWords, stopwords(language))

        if(steam)
            d <- tm_map(d, stemDocument) # SnowballC

        txt <<- rbind(txt, data.frame(txt = sapply(d, as.character), stringsAsFactors = FALSE))

        tdm <- TermDocumentMatrix(d, control = list(weighting = (if(normalize == "tf-idf") weightTfIdf else weightTf)))

        if(sparse < 1)
            tdm = removeSparseTerms(tdm, sparse)

        console(cmds = "inspect(tdm)", e = environment())
            
        v <- sort(row_sums(tdm), decreasing = TRUE)

        df <- data.frame(word = names(v), freq = v)
        return(df)
    }

    bigrams <- function() {
        if(!require(tidytext)) return(NULL)
        bigrams <- txt %>% unnest_tokens(input = txt, output = "bigram", token = "ngrams", n = 2, drop = TRUE)
        return(bigrams)
    }

    txt <- data.frame()

    df <- DF %>% group_by(GROUP) %>% group_modify(~ tm_group(.x$TEXT)) 
    TM <- df %>% group_by(GROUP) %>% pivot_wider(names_from = GROUP, values_from = freq) %>% column_to_rownames(var = "word")
    TM[is.na(TM)] <- 0

    tm <- list()
    tm$normalize <- normalize
    
    if(normalize == "chara-value") 
        tm$data <- Convert(TM) 
    else if(normalize == "media")
        tm$data <- TM / rowSums(TM)
    else 
        tm$data <- TM
    
    tm$bigrams <- bigrams()
    tm$freq <- df
    tm$lang <- language
    tm$dist <- df %>% group_by(GROUP) %>% summarise(sum = n()) 
    class(tm) <- "DataTM"

    return(tm)
}