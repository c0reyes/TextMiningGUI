HJBiplotPage <- function() {
     Plot <- function(graph) {
        t <- match.fun(graph$theme)

        w <- tm$data[1:graph$limit,]
        hjbiplot <- HJBiplot(w)
        plotdf <- as.data.frame(hjbiplot)

        console_chunk("print(hjbiplot)")

        line_alpha <- 0.50
        vector_alpha <- 0.75
        if(graph$alpha == 1) {
            vector_alpha <- 1
            line_alpha <- 1
        }
        
        p <- ggplot(plotdf, aes(x = Dim1, y = Dim2,
                        col = Variable, shape = Variable,
                        label = Label)) +
                    geom_vline(xintercept = 0, lty = "dashed", alpha = line_alpha) +
                    geom_hline(yintercept = 0, lty = "dashed", alpha = line_alpha) +
                    geom_segment(data = plotdf[which(plotdf$Variable == "Columns"),], aes(x = 0, y = 0, xend = Dim1, yend = Dim2), arrow = arrow(length = unit(0.2,"cm")), alpha = vector_alpha, color = graph$vcolor) +
                    geom_point()

        #p <- p +
        #  scale_size(range = c(4, 7), guide = F) +
        #  geom_label_repel(show.legend = F, segment.alpha = .5, point.padding = unit(5, "points")) +
        #  guides(colour = guide_legend(override.aes = list(size = 4)))

        p <- p + geom_text_repel(data = plotdf[which(plotdf$Variable == "Columns"),],
                         aes(x = Dim1, y = Dim2, col = Variable, shape = Variable,
                             label = Label))
        # geom_text_repel(data = cnames, aes(x = long, y = lat, label = Provincia), segment.color = "black", size = 2, inherit.aes = FALSE) +
  
        p <- p +
            labs(x = paste0("Dimension 1 (", round(hjbiplot$inertia[1]), "%)"),
                 y = paste0("Dimension 2 (", round(hjbiplot$inertia[2]), "%)"),
                col = "", shape = "") 

        # p <- p + scale_color_manual(values=c("#999999", "#E69F00")) 
        p <- p + scale_color_manual(values=c(graph$vcolor, graph$pcolor)) 
        p <- p + t() + theme(legend.position = "none")

        return(p)
    }

    PageGUI("HJ-Biplot", Plot, theme = "theme_minimal", limit = 100, point_color = "#00bfc4", vector_color = "#f8766d")
}