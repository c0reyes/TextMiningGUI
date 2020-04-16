print.DataTM <- function(obj) {
    cat("\n### Distinct words by groups ###\n\n")
    print(obj$dist)
    cat("\n### Frequency table ###\n\n")
    print(head(obj$freq))
}