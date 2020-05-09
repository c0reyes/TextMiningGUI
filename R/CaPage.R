CaPage <- function(X, parent, notebook) {
    Plot <- function(graph) {
        t <- match.fun(graph$theme)
        
        w <- X$data[1:graph$limit,]
       
        ca <- CA(w)

        plotdf <- as.data.frame(ca)
        plotdf$Variable <- factor(plotdf$Variable)

        console(cmds = "ca", e = environment())

        line_alpha <- 0.50
        vector_alpha <- 0.75
        if(graph$alpha == 1) {
            vector_alpha <- 1
            line_alpha <- 1
        }
        
        plot <- ggplot(plotdf, aes(x = Dim1, y = Dim2,
                        col = (if(graph$cluster < 2) Variable else kcluster), shape = Variable,
                        label = Label)) +
                    geom_vline(xintercept = 0, lty = "dashed", alpha = line_alpha) +
                    geom_hline(yintercept = 0, lty = "dashed", alpha = line_alpha) +
                    geom_segment(data = plotdf[which(plotdf$Variable == "Columns"),], 
                        aes(x = 0, y = 0, xend = Dim1, yend = Dim2), arrow = arrow(length = unit(0.2, "cm")), 
                            alpha = vector_alpha, color = graph$vcolor, size = graph$vsize) +
                    geom_point(size = graph$psize) +
                    scale_shape_manual(values = c(4, 17))

        if(graph$vtext == TRUE) 
            plot <- plot + geom_text(data = plotdf[which(plotdf$Variable == "Columns"),],
                         aes(x = Dim1, y = Dim2, col = (if(graph$cluster < 2) Variable else kcluster), shape = Variable,
                             label = Label), vjust = -0.5)

        if(graph$ptext == TRUE) 
            plot <- plot + geom_text(data = plotdf[which(plotdf$Variable == "Rows"),],
                         aes(x = Dim1, y = Dim2, col = (if(graph$cluster < 2) Variable else kcluster), shape = Variable,
                             label = Label), vjust = -0.5)
    
        plot <- plot +
            labs(x = paste0("Dimension 1 (", round(ca$inertia[1]), "%)"),
                y = paste0("Dimension 2 (", round(ca$inertia[2]), "%)"),
                title = graph$title) 

        color <- c(graph$vcolor, graph$pcolor)     
        plot <- plot + scale_color_manual(values = color) 
        plot <- plot + t() + theme(legend.position = "none")

        return(plot) 
    }

    PageGUI("CA - Biplot", Plot, theme = "theme_white", title = "CA - Biplot", limit = 100, vector_color = "red", point_color = "blue",
        vector_text = " ", point_text = " ", vector_size = 1, point_size = 2,
        parent = parent, notebook = notebook, to = nrow(X$data))
}
