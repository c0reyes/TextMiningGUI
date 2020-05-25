DataTM <- function(DF, language, steam = TRUE, sparse = 1, normalize = "chara-value", ngrams = FALSE, steamcomp = FALSE) {
    tm_group <- function(texto) {
        text = iconv(texto, to = "ASCII//TRANSLIT")
        corpus <- Corpus(VectorSource(text))

        d <- tm_map(corpus, tolower)
        d <- tm_map(d, removePunctuation)
        d <- tm_map(d, removeNumbers)
        d <- tm_map(d, removeWords, stopwords(language))
        d <- tm_map(d, stripWhitespace)

        if(steam) {
            if(steamcomp) dCopy <- d
            d <- tm_map(d, stemDocument)
            d <- tm_map(d, stripWhitespace)
            if(steamcomp) {
                time <- system.time({
                        stemCompletion2 <- function(x, dictionary) {   
                            x <- unlist(strsplit(as.character(x), " "))
                            x <- x[x != ""]
                            x <- stemCompletion(x, dictionary = dictionary)
                            x <- paste(x, sep="", collapse=" ")
                            stripWhitespace(x)
                        }

                        d <- d %>% lapply(stemCompletion2, dictionary = dCopy) %>% VectorSource %>% Corpus()
                    })
                console(cmds = "time", envir = environment())
            }
        }

        tdm <- TermDocumentMatrix(d, control = list(weighting = (if(normalize == "tf-idf") weightTfIdf else weightTf)))

        txt <<- rbind(txt, data.frame(txt = sapply(d, as.character), stringsAsFactors = FALSE))
        tdm_global <<- if(!is.null(tdm_global)) c(tdm_global, tdm) else tdm

        if(sparse < 1)
            tdm = removeSparseTerms(tdm, sparse)
        
        console(cmds = "inspect(tdm)", envir = environment())
            
        v <- sort(row_sums(tdm), decreasing = TRUE)

        df <- data.frame(word = names(v), freq = v)
        return(df)
    }

    bigrams <- function() {
        if(!ngrams) return(NULL)
        bigrams <- txt %>% unnest_tokens(input = txt, output = "bigram", token = "ngrams", n = 2, drop = TRUE)
        return(bigrams)
    }

    txt <- data.frame()
    tdm_global <- c()

    df <- DF %>% group_by(GROUP) %>% group_modify(~ tm_group(.x$TEXT)) 
    TM <- df %>% group_by(GROUP) %>% pivot_wider(names_from = GROUP, values_from = freq) %>% column_to_rownames(var = "word")
    TM[is.na(TM)] <- 0

    tm <- list()
    tm$lang <- language
    tm$normalize <- normalize
    tm$steam <- steam
    tm$sparse <- sparse
    tm$ngrams <- ngrams
    
    if(normalize == "chara-value") 
        tm$data <- Convert(TM) 
    else if(normalize == "media")
        tm$data <- TM / rowSums(TM)
    else 
        tm$data <- TM
    
    tm$bigrams <- bigrams()
    tm$freq <- df
    tm$dist <- df %>% group_by(GROUP) %>% summarise(sum = n()) 

    tm$dtm <- as.DocumentTermMatrix(tdm_global)
    tm$df <- DF %>% select(-TEXT) %>% add_column(txt) %>% rename(TEXT = txt)

    class(tm) <- "DataTM"

    return(tm)
}