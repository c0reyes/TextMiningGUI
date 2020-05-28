ExplorerPage <- function(X, parent, notebook, envir) {
    Plot <- function(graph) {
        t <- match.fun(graph$theme)
        
        plot <- X$freq %>% ggplot(aes(x = GROUP)) + geom_bar(fill = graph$color) + 
            labs(title = graph$title, subtitle = graph$subtitle, caption = graph$caption) + 
            xlab(graph$xlab) + 
            ylab(graph$ylab)

        plot <- plot + theme(text = element_text(size = 12)) + t() +
            geom_text(stat='count', aes(label=..count..), position = position_dodge(width = 0.9), vjust = 1.5, color = graph$tcolor, size = 5)

        if(graph$flip == TRUE)
            plot <- plot + coord_flip()
        
        save$plot <<- plot
        assign(name, save, envir = toprint)
        
        plot(plot)
    }

    name <- as.character(runif(1))
    save <- list()
    save$name <- "Word Counter"
    class(save) <- "save"

    PageGUI("Words by Groups", Plot, id = as.character(match.call()[[1]]), envir = envir, color = "lightblue", theme = "theme_gray", title = "Distinct words by groups", 
            xlab = "Groups", ylab = "Counts", flip = FALSE, subtitle = " ", caption = " ", text_color = "#323232",
            parent = parent, notebook = notebook)
}