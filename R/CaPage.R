CaPage <- function(X, parent, notebook, envir) {
    Plot <- function(graph) {
        t <- match.fun(graph$theme)

        if(graph$dim %in% c("dim1", "dim2")) {
            w <- plotdf[which(plotdf$Variable == "Rows"),]
            w <- w[order(w[[(if(graph$dim == "dim1") "Con1" else "Con2")]], decreasing = TRUE),]
            w <- rbind(plotdf[which(plotdf$Variable == "Columns"),], w[1:graph$limit,])
        }else {
            w <- plotdf[1:(graph$limit+ncol(X$data)),]
        }

        line_alpha <- 0.50
        vector_alpha <- 0.75
        if(graph$alpha == 1) {
            vector_alpha <- 1
            line_alpha <- 1
        }
        
        plot <- ggplot(plotdf) +
                    geom_vline(xintercept = 0, lty = "dashed", alpha = line_alpha) +
                    geom_hline(yintercept = 0, lty = "dashed", alpha = line_alpha) +
                    geom_segment(data = plotdf[which(w$Variable == "Columns"),], 
                        aes(x = 0, y = 0, xend = Dim1, yend = Dim2), arrow = arrow(length = unit(0.2, "cm")), 
                            alpha = vector_alpha, color = graph$vcolor, size = graph$vsize) +
                    scale_shape_manual(values = c(4, 17))
        
        plot <- plot + geom_point(data = w, aes(x = Dim1, y = Dim2, 
                                  col = Variable, shape = Variable,
                                  label = Label), size = graph$psize)

        g_text <- if(graph$repel == TRUE && require(ggrepel)) geom_text_repel else geom_text

        if(graph$vtext == TRUE) 
            plot <- plot + g_text(data = w[which(w$Variable == "Columns"),],
                         aes(x = Dim1, y = Dim2, col = Variable, shape = Variable,
                             label = Label), vjust = -0.5)

        if(graph$ptext == TRUE) 
            plot <- plot + g_text(data = w[which(w$Variable == "Rows"),],
                         aes(x = Dim1, y = Dim2, col = Variable, shape = Variable,
                             label = Label), vjust = -0.5)

        plot <- plot +
            labs(x = paste0("Dim 1 (", round(ca$inertia[1]), "%)"),
                y = paste0("Dim 2 (", round(ca$inertia[2]), "%)"),
                title = graph$title) 

        color <- c(graph$vcolor, graph$pcolor)     
        plot <- plot + scale_color_manual(values = color) 
        plot <- plot + t() + theme(legend.position = "none")

        save$plot <<- plot
        assign(name, save, envir = print)

        plot(plot)
    }

    ca <- tryCatch({ 
            ca(X$data, nd = 3)
        }, error = function(cond) {
            tkmessageBox(title = "Error", message = "Error:", icon = "error", detail = "Some error occurred verify your data.", type = "ok")
        })

    ca$inertia <- round(100 * ca$sv / sum(ca$sv), 2)
    ca$rowcontrib <- ca$rowmass * ca$rowcoord^2
    ca$colcontrib <- ca$colmass * ca$colcoord^2

    plotdf <- tryCatch({ 
            as.data.frame(ca)
        }, error = function(cond) {
            tkmessageBox(title = "Error", message = "Error:", icon = "error", detail = "Some error occurred verify your data.", type = "ok")
        })
    plotdf$Variable <- factor(plotdf$Variable)
    console(cmds = "ca", envir = environment())

    name <- as.character(runif(1))
    save <- list()
    save$name <- as.character(match.call()[[1]])

    colnames(ca$colcontrib) <- c("Con1", "Con2")
    colnames(ca$rowcontrib) <- c("Con1", "Con2")
    save$table <- list(cbind(ca$colcoord, ca$colcontrib), cbind(ca$rowcoord, ca$rowcontrib))
    save$more <- TRUE

    class(save) <- "save"

    PageGUI("CA - Biplot", Plot, id = as.character(match.call()[[1]]), envir = envir, theme = "theme_white", title = "CA - Biplot", limit = 100, vector_color = "red", point_color = "blue",
        vector_text = " ", point_text = " ", vector_size = 1, point_size = 2, repel = " ", dim = "all",
        parent = parent, notebook = notebook, to = nrow(X$data))
}
