TopicModelsPage <- function(X, parent, notebook, envir) {
    Plot <- function(graph) {
        lda <- LDA(X$dtm, k = graph$limit, method = "VEM", control = NULL)
        topics <- tidy(lda, matrix = "beta")

        top_terms <- topics %>%
            group_by(topic) %>%
            top_n(10, beta) %>%
            ungroup() %>%
            arrange(topic, -beta)

        term <- terms(lda, 5)
        term <- apply(term, MARGIN = 2, paste, collapse = ", ")

        top_terms <- top_terms %>% 
                        select(topic, beta) %>% 
                        group_by(topic) %>% 
                        summarize(beta = sum(beta)) 
        top_terms$term <- term

        plot <- top_terms %>%
                    ggplot(aes(topic, beta, fill = factor(topic))) +
                    geom_col(show.legend = FALSE) +
                    geom_text(aes(label = term), 
                              hjust = 1.05, 
                              size = 4) +
                    coord_flip()
            
        save$plot <<- plot
        save$table <<- top_terms
        assign(name, save, envir = toprint)
        
        plot(plot)
    }

    rowTotals <- tryCatch({ 
            row_sums(X$dtm, na.rm = TRUE)
        }, error = function(cond) {
            tkmessageBox(title = "Error", message = "Error:", icon = "error", detail = "Some error occurred verify your data.", type = "ok")
        })
    X$dtm <- X$dtm[rowTotals > 0, ]

    name <- as.character(runif(1))
    save <- list()
    save$name <- "Topic Models"
    class(save) <- "save"

    PageGUI("Topic Models", Plot, id = as.character(match.call()[[1]]), envir = envir, limit = 4, parent = parent, notebook = notebook, 
        to = 10, from = 2, resolution = 1)
}