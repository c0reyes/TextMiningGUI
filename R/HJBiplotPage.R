HJBiplotPage <- function(X, parent, notebook) {
     Plot <- function(graph) {
        t <- match.fun(graph$theme)

        w <- X$data[1:graph$limit,]

        x <- w
        w <- Convert(w)
        b <- HJBiplot(w)
        plotdf <- as.data.frame(b)
        plotdf$Variable <- factor(plotdf$Variable)

        if(graph$cluster > 1) {
            kcluster <- kmeans(w, graph$cluster)
            kcluster <- kcluster$cluster
            kcluster <- c(rep(0, ncol(w)), kcluster)
            plotdf <- cbind(plotdf, kcluster)
            plotdf$kcluster <- factor(plotdf$kcluster)
        }

        console(cmds = "head(x)", e = environment())
        console(cmds = "head(w)", e = environment())
        console(cmds = "b", e = environment())

        line_alpha <- 0.50
        vector_alpha <- 0.75
        if(graph$alpha == 1) {
            vector_alpha <- 1
            line_alpha <- 1
        }
        
        p <- ggplot(plotdf, aes(x = Dim1, y = Dim2,
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
            p <- p + geom_text(data = plotdf[which(plotdf$Variable == "Columns"),],
                         aes(x = Dim1, y = Dim2, col = (if(graph$cluster < 2) Variable else kcluster), shape = Variable,
                             label = Label), vjust = -0.5)

        if(graph$ptext == TRUE) 
            p <- p + geom_text(data = plotdf[which(plotdf$Variable == "Rows"),],
                         aes(x = Dim1, y = Dim2, col = (if(graph$cluster < 2) Variable else kcluster), shape = Variable,
                             label = Label), vjust = -0.5)

        if(graph$distance != "") {
            r <- b$ColCoordinates[graph$distance,]
            slope <- r[2] / r[1]
            distance <- Distance(b$RowCoordinates, slope = slope)

            console(cmds = "slope", e = environment())
            console(cmds = "distance", e = environment())

            p <- p + geom_abline(intercept = 0, slope = slope, linetype = "dashed", color = graph$vcolor, alpha = vector_alpha) +
                     geom_segment(data = distance, aes(x = Dim1, y = Dim2, xend = xend, yend = yend), 
                                  inherit.aes = FALSE, linetype = "dotted")
        }

        p <- p +
            labs(x = paste0("Dimension 1 (", round(b$inertia[1]), "%)"),
                y = paste0("Dimension 2 (", round(b$inertia[2]), "%)"),
                title = graph$title) 

        color <- c(graph$vcolor, graph$pcolor)
        if(graph$cluster > 1) 
            color <- c(graph$vcolor, brewer.pal(n = graph$cluster, name = graph$palette))
        
        p <- p + scale_color_manual(values = color) 
        p <- p + t() + theme(legend.position = "none")

        return(p)
    }

    PageGUI("HJ-Biplot", Plot, theme = "theme_white", limit = 100, vector_color = "#f8766d", point_color = "#00bfc4", 
        title = "HJ-Biplot", vector_text = " ", point_text = " ", vector_size = 1, point_size = 2,
        parent = parent, notebook = notebook, to = nrow(X$data), distances = c(colnames(X$data), ""), cluster = 1, palette = "Dark2")
}