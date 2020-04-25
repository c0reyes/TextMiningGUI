print.DataTM <- function(obj) {
    cat("\n### Language ###\n\n")
    print(obj$lang)
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
}