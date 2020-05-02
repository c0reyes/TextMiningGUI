ExplorerPage <- function(X, parent, notebook) {
    Plot <- function(graph) {
        t <- match.fun(graph$theme)
        pe <- X$freq %>% ggplot(aes(x = GROUP)) + geom_bar(fill = graph$color) + 
            labs(title = graph$title, subtitle = graph$subtitle, caption = graph$caption) + 
            xlab(graph$xlab) + 
            ylab(graph$ylab)
        pe <- pe + theme(text = element_text(size = 12)) + t()
        if(graph$flip == TRUE)
            pe <- pe + coord_flip()
        return(pe)
    }

    PageGUI("Words by Groups", Plot, color = "#323232", theme = "theme_gray", title = "Distinct words by groups", 
            xlab = "Groups", ylab = "Counts", flip = FALSE, subtitle = " ", caption = " ",
            parent = parent, notebook = notebook)
}