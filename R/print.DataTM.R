print.DataTM <- function(obj) {
    cat("### Words by groups ###\n\n")
    print(obj$sum)
    cat("\n### Distinct words by groups ###\n\n")
    print(obj$dist)
    cat("\n### Tokenizers ###\n\n")
    print(head(obj$token))
}