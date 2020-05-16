CaPage <- function(X, parent, notebook, env) {
    Plot <- function(graph) {
        t <- match.fun(graph$theme)
        
        w <- plotdf[1:(graph$limit+ncol(X$data)),] 

        line_alpha <- 0.50
        vector_alpha <- 0.75
        if(graph$alpha == 1) {
            vector_alpha <- 1
            line_alpha <- 1
        }
        
        plot <- ggplot(plotdf) +
                    geom_vline(xintercept = 0, lty = "dashed", alpha = line_alpha) +
                    geom_hline(yintercept = 0, lty = "dashed", alpha = line_alpha) +
                    geom_segment(data = plotdf[which(plotdf$Variable == "Columns"),], 
                        aes(x = 0, y = 0, xend = Dim1, yend = Dim2), arrow = arrow(length = unit(0.2, "cm")), 
                            alpha = vector_alpha, color = graph$vcolor, size = graph$vsize) +
                    scale_shape_manual(values = c(4, 17))
        
        plot <- plot + geom_point(data = w, aes(x = Dim1, y = Dim2, 
                                  col = Variable, shape = Variable,
                                  label = Label), size = graph$psize)

        g_text <- if(graph$repel == TRUE && require(ggrepel)) geom_text_repel else geom_text

        if(graph$vtext == TRUE) 
            plot <- plot + g_text(data = w[which(plotdf$Variable == "Columns"),],
                         aes(x = Dim1, y = Dim2, col = Variable, shape = Variable,
                             label = Label), vjust = -0.5)

        if(graph$ptext == TRUE) 
            plot <- plot + g_text(data = w[which(plotdf$Variable == "Rows"),],
                         aes(x = Dim1, y = Dim2, col = Variable, shape = Variable,
                             label = Label), vjust = -0.5)

        plot <- plot +
            labs(x = paste0("Dim 1 (", round(ca$inertia[1]), "%)"),
                y = paste0("Dim 2 (", round(ca$inertia[2]), "%)"),
                title = graph$title) 

        color <- c(graph$vcolor, graph$pcolor)     
        plot <- plot + scale_color_manual(values = color) 
        plot <- plot + t() + theme(legend.position = "none")

        plot(plot)
    }

    ca <- tryCatch({ 
            ca(X$data)
        }, error = function(cond) {
            tkmessageBox(title = "Error", message = "Error:", icon = "error", detail = "Some error occurred verify your data.", type = "ok")
        })

    ca$inertia <- round(100 * ca$sv / sum(ca$sv), 2)
    plotdf <- tryCatch({ 
            as.data.frame(ca)
        }, error = function(cond) {
            tkmessageBox(title = "Error", message = "Error:", icon = "error", detail = "Some error occurred verify your data.", type = "ok")
        })
    plotdf$Variable <- factor(plotdf$Variable)
    console(cmds = "ca", e = environment())

    PageGUI("CA - Biplot", Plot, id = as.character(match.call()[[1]]), e = env, theme = "theme_white", title = "CA - Biplot", limit = 100, vector_color = "red", point_color = "blue",
        vector_text = " ", point_text = " ", vector_size = 1, point_size = 2, repel = " ",
        parent = parent, notebook = notebook, to = nrow(X$data))
}
