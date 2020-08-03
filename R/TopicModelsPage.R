TopicModelsPage <- function(X, parent, notebook, envir) {
    ..count.. <- NULL
    lda <- NULL

    Plot <- function(graph) {
        if(!is.null(graph$reload)) { 
            plot(env$save$plot)
            return(NULL)
        }

        t <- match.fun(graph$theme)

        if(limit != graph$limit) {
            lda <- topicmodels::LDA(X$dtm, k = graph$limit, control = list(seed = X$seed))
        }
        limit <- graph$limit

        term <- topicmodels::terms(lda, 6)
        term <- apply(term, MARGIN = 2, paste, collapse = ", ")

        if(graph$time == TRUE && requireNamespace("data.table", quietly = TRUE)) {
            topic <- topicmodels::topics(lda, 1)
            topics <- data.frame(date = data.table::as.IDate(X$df[X$dtm$dimnames$Docs,]$TIME), topic)
            plot <- qplot(date, ..count.., data = topics, geom = "density", fill = term[topic], position = "stack") +
                        labs(title = graph$title, subtitle = graph$subtitle, caption = graph$caption) + t() +
                        theme(legend.title = element_blank(), legend.position = "bottom") +
                        scale_fill_manual(values = tmPalette(graph$palette, graph$limit))
        }else {
            topics <- tidy(lda, matrix = "beta")

            top_terms <- topics %>%
                group_by(topic) %>%
                top_n(10, beta) %>%
                ungroup() %>%
                arrange(topic, -beta)

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
                        labs(title = graph$title, subtitle = graph$subtitle, caption = graph$caption) + t() +
                        scale_fill_brewer(palette = graph$palette) +
                        coord_flip()

            save$table <- top_terms
        }
            
        env$save$plot <- plot
        assign(name, env$save, envir = toprint)
        
        plot(plot)
    }

    rowTotals <- tryCatch({ 
            row_sums(X$dtm, na.rm = TRUE)
        }, error = function(cond) {
            tkmessageBox(title = "Error", message = "Error:", icon = "error", detail = "Some error occurred verify your data.", type = "ok")
        })
    X$dtm <- X$dtm[rowTotals > 0, ]

    name <- as.character(runif(1))
    env = environment()
    env$save <- list()
    env$save$name <- "Topic Models"
    class(env$save) <- "save"

    time <- if("TIME" %in% colnames(X$df)) " " else ""
    limit <- 4
    lda <- topicmodels::LDA(X$dtm, k = limit, control = list(seed = X$seed))

    PageGUI("Topic Models", Plot, id = as.character(match.call()[[1]]), envir = envir, limit = limit, parent = parent, notebook = notebook, 
        to = 10, from = 2, resolution = 1, time = time, theme = "theme_gray", title = "Topic Models", palette = "Dark2", subtitle = " ", caption = " ")
}