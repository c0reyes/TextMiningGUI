DataTM <- function(DF, language, steam = TRUE, sparse = 1, normalize = "chara-value", ngrams = FALSE, steamcomp = FALSE) {
    tm_group <- function(texto) {
        text = iconv(texto, to = "ASCII//TRANSLIT")
        corpus <- Corpus(VectorSource(text))

        d <- tm_map(corpus, tolower)
        d <- tm_map(d, removePunctuation)
        d <- tm_map(d, removeNumbers)
        d <- tm_map(d, removeWords, stopwords(language))
    
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

                        if(require(parallel)) {
                            cores <- if(detectCores() - 1 > 0)  detectCores() - 1 else 1
                            cl <- makeCluster(cores)
                            clusterExport(cl = cl, c("stemCompletion", "stripWhitespace"))
                            d <- parLapply(cl, d, stemCompletion2, dCopy)
                            stopCluster(cl)
                        }else{
                            d <- lapply(d, stemCompletion2, dictionary = dCopy)
                        }

                        d <- Corpus(VectorSource(d))
                    })
                console(cmds = "time", envir = environment())
            }
        }else{
            d <- tm_map(d, stripWhitespace)
        }

        tdm <- TermDocumentMatrix(d, control = list(weighting = (if(normalize == "tf-idf") weightTfIdf else weightTf)))

        txt <<- rbind(txt, data.frame(txt = sapply(d, as.character), stringsAsFactors = FALSE))
        corpus_global <<- if(!is.null(corpus_global)) c(corpus_global, d) else d

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
    corpus_global <- c()

    df <- DF %>% group_by(GROUP) %>% group_modify(~ tm_group(.x$TEXT)) 
    TM <- df %>% group_by(GROUP) %>% pivot_wider(names_from = GROUP, values_from = freq) %>% column_to_rownames(var = "word")
    TM[is.na(TM)] <- 0

    tm <- list()
    tm$lang <- language
    tm$normalize <- normalize
    tm$steam <- steam
    tm$steamcomp <- steamcomp
    tm$sparse <- sparse
    tm$ngrams <- ngrams
    
    if(normalize == "chara-value") 
        tm$data <- Convert(TM) 
    else if(normalize == "media")
        tm$data <- TM / rowSums(TM)
    else 
        tm$data <- TM
    
    tm$bigrams <- bigrams()
    tm$freq <- df %>% select(GROUP, word, freq)
    tm$dist <- df %>% group_by(GROUP) %>% summarise(sum = n()) 

    tm$dtm <- DocumentTermMatrix(corpus_global, control = list(weighting = weightTf))
    tm$df <- DF %>% select(-TEXT) %>% add_column(txt) %>% rename(TEXT = txt)

    class(tm) <- "DataTM"

    return(tm)
}