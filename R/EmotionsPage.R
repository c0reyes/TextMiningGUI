EmotionsPage <- function() {
    Plot <- function(graph) {
        t <- match.fun(graph$theme)

        w <<- Convert(emotions)
        b <<- HJBiplot(w)
        plotdf <- as.data.frame(b)

        plotdf$sum <- 1
        plotdf[which(plotdf$Variable == "Rows"),] <- plotdf[which(plotdf$Variable == "Rows"),] %>% mutate(sum = row_number() + 1)
        plotdf$sum <- factor(plotdf$sum)
        plotdf$Variable <- factor(plotdf$Variable)
        
        console_chunk("print(head(emotions))")
        console_chunk("print(head(w))")
        console_chunk("print(b)")

        line_alpha <- 0.50
        vector_alpha <- 0.75
        if(graph$alpha == 1) {
            vector_alpha <- 1
            line_alpha <- 1
        }
        
        p <- ggplot(plotdf, aes(x = Dim1, y = Dim2,
                        col = sum, shape = Variable,
                        label = Label)) +
                    geom_vline(xintercept = 0, lty = "dashed", alpha = line_alpha) +
                    geom_hline(yintercept = 0, lty = "dashed", alpha = line_alpha) +
                    geom_segment(data = plotdf[which(plotdf$Variable == "Columns"),], 
                        aes(x = 0, y = 0, xend = Dim1, yend = Dim2), arrow = arrow(length = unit(0.2, "cm")), 
                        alpha = vector_alpha, color = graph$vcolor, size = graph$vsize) +
                    geom_point(size = graph$psize) +
                    scale_shape_manual(values = c(4, 17))

        if(graph$vtext == TRUE) 
            p <- p + geom_text(data = plotdf[which(plotdf$Variable == "Columns"),],
                         aes(x = Dim1, y = Dim2, col = sum, shape = Variable,
                             label = Label), vjust = -0.5)

        if(graph$ptext == TRUE) 
            p <- p + geom_text(data = plotdf[which(plotdf$Variable == "Rows"),],
                         aes(x = Dim1, y = Dim2, col = sum, shape = Variable,
                             label = Label), vjust = -0.5)

        p <- p +
            labs(x = paste0("Dimension 1 (", round(b$inertia[1]), "%)"),
                y = paste0("Dimension 2 (", round(b$inertia[2]), "%)"),
                title = graph$title) 

        #p <- p + scale_color_manual(values=c(graph$vcolor, graph$pcolor)) 
        p <- p + t() + theme(legend.position = "none")

        return(p)
    }

    emotions <<- tm$freq %>% select(GROUP, word) %>% group_by(GROUP) %>% group_modify(~ Emotions(.x$word, language = tm$lang))
    emotions <<- emotions %>% group_by(GROUP) %>% pivot_wider(names_from = GROUP, values_from = count) 
    emotions <<- as.data.frame(emotions)
    rownames(emotions) <<- emotions[,1]
    emotions[,1] <<- NULL
    console_chunk("print(emotions)")

    #point_color = "#E69F00"
    PageGUI("Emotions - HJ-Biplot", Plot, theme = "theme_white", limit = 100, vector_color = "#f8766d", 
        title = "Emotions - HJ-Biplot", vector_text = " ", point_text = " ", vector_size = 1, point_size = 3)
}