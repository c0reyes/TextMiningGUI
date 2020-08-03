DataTM <- function(DF, language, steam = TRUE, sparse = 1, normalize = "chara-value", ngrams = FALSE, steamcomp = FALSE, stopwords = TRUE, otherstopwords = "") {
    GROUP <- TEXT <- ID <- freq <- word <- txt <- NULL
    env <- new.env()

    env$txt <- data.frame()
    env$tdm_global <- c()
    
    tm_group <- function(X) {
        X$TEXT <- iconv(X$TEXT, to = "ASCII//TRANSLIT")
        corpus <- Corpus(VectorSource(X$TEXT))

        d <- tm_map(corpus, content_transformer(tolower))
        d <- tm_map(d, removePunctuation)
        d <- tm_map(d, removeNumbers)
        
        if(stopwords)
            d <- tm_map(d, removeWords, stopwords(language))
        
        if(otherstopwords != "")
            d <- tm_map(d, removeWords, otherstopwords)

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

                        d <- lapply(d, stemCompletion2, dictionary = dCopy)
                        d <- Corpus(VectorSource(d))
                    })
                console(cmds = "time", envir = environment())
            }
        }else{
            d <- tm_map(d, stripWhitespace)
        }

        tdm <- TermDocumentMatrix(d, control = list(weighting = (if(normalize == "tf-idf") weightTfIdf else weightTf)))

        env$txt <- rbind(env$txt, data.frame(txt = sapply(d, as.character), stringsAsFactors = FALSE))

        t <- TermDocumentMatrix(d)
        t$dimnames$Docs <- X$ID
        env$tdm_global <- if(!is.null(env$tdm_global)) c(env$tdm_global, t) else t

        if(sparse < 1) {
            tdm <- removeSparseTerms(tdm, sparse)
            env$tdm_global <- removeSparseTerms(env$tdm_global, sparse)
        }
        
        console(cmds = "inspect(tdm)", envir = environment())
            
        v <- sort(row_sums(tdm), decreasing = TRUE)

        df <- data.frame(word = names(v), freq = v)
        return(df)
    }

    bigrams <- function() {
        bigrams <- env$txt %>% unnest_tokens(input = txt, output = "bigram", token = "ngrams", n = 2, drop = TRUE)
        return(bigrams)
    }

    otherstopwords <- if(otherstopwords != "") unlist(strsplit(otherstopwords, ",")) else ""

    time <- system.time({
        df <- DF %>% mutate(ID = rownames(DF)) %>% group_by(GROUP) %>% select(TEXT, GROUP, ID) %>% group_modify(~ tm_group(.x)) 
    })
    console(cmds = "time", envir = environment())
    TM <- df %>% group_by(GROUP) %>% pivot_wider(names_from = GROUP, values_from = freq) %>% column_to_rownames(var = "word")
    TM[is.na(TM)] <- 0

    tm <- list()
    tm$lang <- language
    tm$normalize <- normalize
    tm$steam <- steam
    tm$steamcomp <- steamcomp
    tm$stopwords <- stopwords
    tm$otherstopwords <- otherstopwords
    tm$sparse <- sparse
    tm$ngrams <- ngrams
    
    if(normalize == "chara-value") 
        tm$data <- charavalue(TM) 
    else if(normalize == "media")
        tm$data <- TM / row_means(TM)
    else 
        tm$data <- TM
    
    tm$bigrams <- if(ngrams) bigrams() else NULL
    tm$freq <- df %>% select(GROUP, word, freq)
    tm$dist <- df %>% group_by(GROUP) %>% summarise(sum = n()) 

    tm$dtm <- as.DocumentTermMatrix(env$tdm_global, weighting = weightTf)
    tm$df <- DF %>% select(-TEXT) %>% add_column(env$txt) %>% rename(TEXT = txt)
    tm$len <- if(class(tm$df$GROUP) == "factor") nlevels(tm$df$GROUP) else tm$df$GROUP %>% unique() %>% length()

    class(tm) <- "DataTM"

    return(tm)
}