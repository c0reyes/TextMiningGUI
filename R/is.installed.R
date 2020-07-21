is.installed <- function(x) {
    x %in% rownames(installed.packages())
}