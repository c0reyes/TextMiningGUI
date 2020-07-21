#' @export
print.DataTM <- function(x, ...) {
    GROUP <- freq <- total <- NULL
    
    cat("\n### Language ###\n\n")
    print(x$lang)
    
    cat("\n### Normalize ###\n\n")
    print(x$normalize)
    
    cat("\n### Steam ###\n\n")
    print(x$steam)

    cat("\n### Completion ###\n\n")
    print(x$steamcomp)
    
    cat("\n### Sparse ###\n\n")
    print(x$sparse)
    
    cat("\n### Summary ###\n\n")
    print(summary(x$freq[1:2]))

    cat("\n### Words by groups ###\n\n")
    t <- x$freq %>% group_by(GROUP) %>% summarise(total = sum(freq)) %>% arrange(desc(total))
    print(t)
    
    cat("\n### Distinct words by groups ###\n\n")
    print(x$dist)
    
    cat("\n### Lexical table - head() ###\n\n")
    print(head(x$data))

    cat("\n### str() ###\n\n")
    print(str(x$freq))

    if(!is.null(x$bigrams)) {
        cat("\n### Bigrams ###\n\n")
        print(head(x$bigrams))
    }

    cat("\n### DTM ###\n\n")
    print(inspect(x$dtm))

    cat("\n### DF ###\n\n")
    print(head(x$df))
}