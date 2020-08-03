CaPage <- function(X, parent, notebook, envir) {
    Plot <- function(graph) {
        Dim1 <- Dim2 <- Variable <- Label <- NULL

        if(!is.null(graph$reload)) { 
            plot(env$save$plot)
            return(NULL)
        }
        
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

        g_text <- if(graph$repel == TRUE && requireNamespace("ggrepel", quietly = TRUE)) ggrepel::geom_text_repel else geom_text

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

        env$save$plot <- plot
        assign(name, env$save, envir = toprint)

        plot(plot)
    }

    if(X$normalize != "none") {
        tkmessageBox(title = "Warning", message = "Warning:", icon = "warning", detail = "It is recommended not to use normalize for AFC.", type = "ok")
    }

    ca <- tryCatch({ 
            ca::ca(X$data, nd = 3)
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
    env = environment()
    env$save <- list()
    env$save$name <- "AFC"

    colnames(ca$colcontrib) <- unlist(lapply(1:ncol(ca$colcontrib), function(x) paste0("Con", x)))
    colnames(ca$rowcontrib) <- unlist(lapply(1:ncol(ca$rowcontrib), function(x) paste0("Con", x)))
    env$save$table <- list(cbind(ca$colcoord, ca$colcontrib), cbind(ca$rowcoord, ca$rowcontrib))
    env$save$more <- TRUE

    class(env$save) <- "save"

    PageGUI("AFC", Plot, id = as.character(match.call()[[1]]), envir = envir, theme = "theme_white", title = "AFC", limit = 100, vector_color = "red", point_color = "blue",
        vector_text = " ", point_text = " ", vector_size = 1, point_size = 2, repel = " ", dim = "all",
        parent = parent, notebook = notebook, to = nrow(X$data))
}
