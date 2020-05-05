ExplorerPage <- function(X, parent, notebook) {
    Plot <- function(graph) {
        t <- match.fun(graph$theme)
        
        plot <- X$freq %>% ggplot(aes(x = GROUP)) + geom_bar(fill = graph$color) + 
            labs(title = graph$title, subtitle = graph$subtitle, caption = graph$caption) + 
            xlab(graph$xlab) + 
            ylab(graph$ylab)
        plot <- plot + theme(text = element_text(size = 12)) + t()

        if(graph$flip == TRUE)
            plot <- plot + coord_flip()
        
        return(plot)
    }

    PageGUI("Words by Groups", Plot, color = "#323232", theme = "theme_gray", title = "Distinct words by groups", 
            xlab = "Groups", ylab = "Counts", flip = FALSE, subtitle = " ", caption = " ",
            parent = parent, notebook = notebook)
}