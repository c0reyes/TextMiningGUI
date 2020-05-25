#' @export
print.DataTM <- function(obj) {
    cat("\n### Language ###\n\n")
    print(obj$lang)
    
    cat("\n### Normalize ###\n\n")
    print(obj$normalize)
    
    cat("\n### Steam ###\n\n")
    print(obj$steam)
    
    cat("\n### Sparse ###\n\n")
    print(obj$sparse)
    
    cat("\n### Summary ###\n\n")
    print(summary(obj$freq[1:2]))
    
    cat("\n### Words Total ###\n\n")
    print(sum(obj$freq[3]))
    
    cat("\n### Distinct words by groups ###\n\n")
    print(obj$dist)
    
    cat("\n### Lexical table - head() ###\n\n")
    print(head(obj$data))

    cat("\n### str() ###\n\n")
    print(str(obj$freq))

    if(!is.null(obj$bigrams)) {
        cat("\n### Bigrams ###\n\n")
        print(head(obj$bigrams))
    }

    cat("\n### DTM ###\n\n")
    print(inspect(obj$dtm))

    cat("\n### DF ###\n\n")
    print(head(obj$df))
}