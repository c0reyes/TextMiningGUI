Export <- function(X) {
    GROUP <- freq <- total <- NULL
    file <- "output.Rmd"

    if(!requireNamespace("knitr", quietly = TRUE)) tkmessageBox(title = "Error", message = "Error:", icon = "error", detail = "require knitr library.", type = "ok")
    if(!requireNamespace("rmarkdown", quietly = TRUE)) tkmessageBox(title = "Error", message = "Error:", icon = "error", detail = "require rmarkdown library.", type = "ok")

    cat("---\ntitle: TextMiningGUI\n---\n\n", file = file)
    time <- format(Sys.time(), "%a %b %d %X %Y")
    cat(time, file = file, append = TRUE)

    cat("\n\n# Language\n\n", file = file, append = TRUE)
    cat(X$lang, file = file, append = TRUE)

    cat("\n\n# Normalize\n\n", file = file, append = TRUE)
    cat(X$normalize, file = file, append = TRUE)

    cat("\n\n# Steam\n\n", file = file, append = TRUE)
    cat(X$steam, file = file, append = TRUE)

    cat("\n\n# Completion\n\n", file = file, append = TRUE)
    cat(X$steamcomp, file = file, append = TRUE)

    cat("\n\n# Sparse\n\n", file = file, append = TRUE)
    cat(X$sparse, file = file, append = TRUE)

    cat("\n\n# Summary\n\n", file = file, append = TRUE)
    table <- knitr::kable(summary(X$freq[1:2]), format = "markdown")
    cat(table, sep = "\n", file = file, append = TRUE)

    cat("\n# Words by groups\n\n", file = file, append = TRUE)
    t <- X$freq %>% group_by(GROUP) %>% summarise(total = sum(freq)) %>% arrange(desc(total))
    table <- knitr::kable(t, format = "markdown")
    cat(table, sep = "\n", file = file, append = TRUE)

    cat("\n\n# Distinct words by groups\n\n", file = file, append = TRUE)
    table <- knitr::kable(X$dist, format = "markdown")
    cat(table, sep = "\n", file = file, append = TRUE)

    cat("\n\n# Lexical table\n\n", file = file, append = TRUE)
    table <- knitr::kable(head(X$data), format = "markdown")
    cat(table, sep = "\n", file = file, append = TRUE)

    cat("\n\n# str()\n\n", file = file, append = TRUE)
    i <- capture.output(str(X$freq))
    cat(paste("> ", i, "\n\n"), file = file, append = TRUE)

    if(!is.null(X$bigrams)) {
        cat("\n\n# Bigrams\n\n", file = file, append = TRUE)
        table <- knitr::kable(head(X$bigrams), format = "markdown")
        cat(table, sep="\n", file = file, append = TRUE)
    }

    cat("\n\n# DTM\n\n", file = file, append = TRUE)
    i <- capture.output(inspect(X$dtm))
    cat(paste("> ", i, "\n\n"), file = file, append = TRUE)

    cat("\n\n# DF\n\n", file = file, append = TRUE)
    table <- knitr::kable(head(X$df), format = "markdown")
    cat(table, sep = "\n", file = file, append = TRUE)

    cat(paste0("\n\n# Analysis\n\n"), file = file, append = TRUE)

    params <- list()
    x <- as.integer(1)
    for(i in ls(envir = toprint)) {
        i <- get(i, envir = toprint)
        cat(paste0("\n\n## ", i$name, "\n\n"), file = file, append = TRUE)
        
        if(!is.null(getElement(i, "table"))) {
            if(!is.null(getElement(i, "more"))) {
                for(tt in i$table) {
                    table <- knitr::kable(tt, format = "markdown")
                    cat(table, sep = "\n", file = file, append = TRUE)
                    cat("\n\n", file = file, append = TRUE)
                }
            }else{
                table <- knitr::kable(i$table, format = "markdown")
                cat(table, sep = "\n", file = file, append = TRUE)
            }
        }

        if(!is.null(getElement(i, "plot"))) {
            cat(paste0("\n\n```{r echo=FALSE, warning=FALSE, message=FALSE}\nparams[", x ,"]\n```"), file = file, append = TRUE) 
            params[[x]] <- i$plot
            x <- x + 1
        }
    }

    rmarkdown::render("output.Rmd", output_format = "pdf_document", params = list(params))
    file.remove(file)
    tkmessageBox(title = "Export", message = "Export:", detail = "output.pdf was created.", type = "ok") 
}