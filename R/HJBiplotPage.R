HJBiplotPage <- function(X, parent, notebook, envir) {
    Dim1 <- Dim2 <- Variable <- Label <- xend <- yend <- NULL
    
    Plot <- function(graph) {
        if(!is.null(graph$reload)) { 
            plot(env$save$plot)
            return(NULL)
        }
        
        t <- match.fun(graph$theme)

        if(graph$cluster > 1) {
            X$data[is.na(X$data)] <- 0
            kcluster <- kmeans(X$data, graph$cluster)
            kcluster <- kcluster$cluster
            kcluster <- c(rep(0, ncol(X$data)), kcluster)
            plotdf <- cbind(plotdf, kcluster)
            plotdf$kcluster <- factor(plotdf$kcluster)
        }

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
                                  col = (if(graph$cluster < 2) Variable else kcluster), shape = Variable,
                                  label = Label), size = graph$psize)

        g_text <- if(graph$repel == TRUE && requireNamespace("ggrepel", quietly = TRUE)) ggrepel::geom_text_repel else geom_text

        if(graph$vtext == TRUE) 
            plot <- plot + g_text(data = w[which(w$Variable == "Columns"),],
                         aes(x = Dim1, y = Dim2, col = (if(graph$cluster < 2) Variable else kcluster), shape = Variable,
                             label = Label), vjust = -0.5)

        if(graph$ptext == TRUE) 
            plot <- plot + g_text(data = w[which(w$Variable == "Rows"),],
                         aes(x = Dim1, y = Dim2, col = (if(graph$cluster < 2) Variable else kcluster), shape = Variable,
                             label = Label), vjust = -0.5)

        if(graph$distance != "") {
            r <- biplot$ColCoordinates[graph$distance,]
            slope <- r[2] / r[1]
            distance <- Distance(biplot$RowCoordinates[1:graph$limit,], slope = slope)

            console(cmds = "slope", envir = environment())
            console(cmds = "distance", envir = environment())

            plot <- plot + geom_abline(intercept = 0, slope = slope, linetype = "dashed", color = graph$vcolor, alpha = vector_alpha) +
                     geom_segment(data = distance, aes(x = Dim1, y = Dim2, xend = xend, yend = yend), 
                                  inherit.aes = FALSE, linetype = "dotted") + coord_fixed()
        }

        plot <- plot +
            labs(x = paste0("Dim 1 (", round(biplot$inertia[1]), "%)"),
                y = paste0("Dim 2 (", round(biplot$inertia[2]), "%)"),
                title = graph$title) 

        color <- c(graph$vcolor, graph$pcolor)
        if(graph$cluster > 1) 
            color <- c(graph$vcolor, brewer.pal(n = graph$cluster, name = graph$palette))
        
        plot <- plot + scale_color_manual(values = color) 
        plot <- plot + t() + theme(legend.position = "none") 

        env$save$plot <- plot
        assign(name, env$save, envir = toprint)

        plot(plot)
    }

    biplot <- tryCatch({ 
            HJBiplot(X$data) 
        }, error = function(cond) {
            tkmessageBox(title = "Error", message = "Error:", icon = "error", detail = "Some error occurred verify your data.", type = "ok")
        })

    plotdf <- as.data.frame(biplot)
    plotdf$Variable <- factor(plotdf$Variable)

    console(cmds = "head(X$data)", envir = environment())
    console(cmds = "head(biplot)", envir = environment())
    console(cmds = "head(plotdf)", envir = environment())

    name <- as.character(runif(1))
    env = environment()
    env$save <- list()
    env$save$name <- "HJ-Biplot"
    env$save$table <- plotdf
    class(env$save) <- "save"

    assign(name, env$save, envir = toprint)

    PageGUI("HJ-Biplot", Plot, id = as.character(match.call()[[1]]), envir = envir, theme = "theme_white", limit = 100, vector_color = "#f8766d", point_color = "#00bfc4", 
        title = "HJ-Biplot", vector_text = " ", point_text = " ", vector_size = 1, point_size = 2, repel = " ", dim = "all",
        parent = parent, notebook = notebook, to = nrow(X$data), distances = c(colnames(X$data), ""), cluster = 1, palette = "Dark2")
}