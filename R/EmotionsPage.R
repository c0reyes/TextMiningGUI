EmotionsPage <- function(X, parent, notebook, envir) {
    Plot <- function(graph) {
        t <- match.fun(graph$theme)

        line_alpha <- 0.50
        vector_alpha <- 0.75
        if(graph$alpha == 1) {
            vector_alpha <- 1
            line_alpha <- 1
        }
        
        plot <- ggplot(plotdf, aes(x = Dim1, y = Dim2,
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
            plot <- plot + geom_text(data = plotdf[which(plotdf$Variable == "Columns"),],
                         aes(x = Dim1, y = Dim2, col = sum, shape = Variable,
                             label = Label), vjust = -0.5)

        if(graph$ptext == TRUE) 
            plot <- plot + geom_text(data = plotdf[which(plotdf$Variable == "Rows"),],
                         aes(x = Dim1, y = Dim2, col = sum, shape = Variable,
                             label = Label), vjust = -0.5)

        if(graph$distance != "") {
            r <- biplot$ColCoordinates[graph$distance,]
            slope <- r[2] / r[1]
            distance <- Distance(biplot$RowCoordinates, slope = slope)

            console(cmds = "slope", envir = environment())
            console(cmds = "distance", envir = environment())

            plot <- plot + geom_abline(intercept = 0, slope = slope, linetype = "dashed", color = graph$vcolor, alpha = vector_alpha) +
                     geom_segment(data = distance, aes(x = Dim1, y = Dim2, xend = xend, yend = yend), 
                                  inherit.aes = FALSE, linetype = "dotted")
        }

        plot <- plot +
            labs(x = paste0("Dimension 1 (", round(biplot$inertia[1]), "%)"),
                y = paste0("Dimension 2 (", round(biplot$inertia[2]), "%)"),
                title = graph$title) 

        plot <- plot + t() + theme(legend.position = "none")

        name <- as.character(runif(1))
        save <- list()
        save$name <- "EmotionsPage"
        save$plot <- plot
        class(save) <- "save"

        plot(plot)
    }

    PlotS <- function(graph) {
        t <- match.fun(graph$theme)

        plot <-  ggplot(sentiments, 
                    aes(x = group, y = count, fill = sentiment, label = count)) + 
                    geom_bar(position = "dodge", stat="identity") +
                    labs(title = graph$title, 
                        x = graph$xlab, y = graph$ylab) +
                geom_text(position = position_dodge(width = 0.9), vjust = 1.5, color = "black", size = 5) +
                t() +
                theme(plot.title = element_text(hjust = 0.5),
                    axis.text = element_text(size = 12),
                    axis.title = element_text(size = 14,face = "bold"),
                    title = element_text(size = 20,face = "bold"))

        name <- as.character(runif(1))
        save <- list()
        save$name <- "SentimentsPage"
        save$plot <- plot
        class(save) <- "save"

        plot(plot)
    }

    PlotS2 <- function(graph) {
        t <- match.fun(graph$theme)

        plot <-  ggplot(emotions, 
                    aes(x = group, y = count, fill = emotions, label = count)) + 
                    geom_bar(position = "dodge", stat="identity") +
                    labs(title = graph$title, 
                        x = graph$xlab, y = graph$ylab) +
                geom_text(position = position_dodge(width = 0.9), vjust = 1.5, color = "black", size = 5) +
                t() +
                theme(plot.title = element_text(hjust = 0.5),
                    axis.text = element_text(size = 12),
                    axis.title = element_text(size = 14,face = "bold"),
                    title = element_text(size = 20,face = "bold"))

        name <- as.character(runif(1))
        save <- list()
        save$name <- "EmotionsPage"
        save$plot <- plot
        class(save) <- "save"

        plot(plot)
    }

    e <- environment()

    ok_callback <- function() {
        emotions <- X$df %>% select(GROUP, TEXT) %>% group_by(GROUP) %>% group_modify(~ Emotions(.x$TEXT, language = X$lang))
        emotions <- emotions %>% group_by(GROUP) %>% pivot_wider(names_from = GROUP, values_from = count) 
        emotions <- as.data.frame(emotions)
        rownames(emotions) <- emotions[,1]
        emotions[,1] <- NULL

        sentiments <- emotions
        emotions <- emotions[1:8,,drop = FALSE]

        sentiments <- sentiments[9:10,,drop = FALSE]
        sentiments <- as.data.frame(t(sentiments))
        sentiments <- cbind(group = rownames(sentiments), sentiments)
        sentiments <- gather(sentiments, "sentiment", "count", -group)

        assign("emotions", emotions, envir = e)
        assign("sentiments", sentiments, envir = e)

        console(cmds = "emotions", envir = e)
        console(cmds = "sentiments", envir = e)

        if(ncol(X$data) >= 3) {
            biplot <- HJBiplot(emotions)
            plotdf <- as.data.frame(biplot)

            console(cmds = "biplot", envir = environment())
            console(cmds = "plotdf", envir = environment())

            plotdf$sum <- 1
            plotdf[which(plotdf$Variable == "Rows"),] <- plotdf[which(plotdf$Variable == "Rows"),] %>% mutate(sum = row_number() + 1)
            plotdf$sum <- factor(plotdf$sum)
            plotdf$Variable <- factor(plotdf$Variable)

            assign("plotdf", plotdf, envir = e)
            assign("biplot", biplot, envir = e)

            PageGUI("Emotions - HJ-Biplot", Plot, id = "EmotionsPage", envir = envir, theme = "theme_white", vector_color = "#f8766d", 
                title = "Emotions - HJ-Biplot", vector_text = " ", point_text = " ", vector_size = 1, point_size = 3,
                parent = parent, notebook = notebook, distances = c(colnames(emotions), ""))
        }else{
            emotions <- as.data.frame(t(emotions))
            emotions <- cbind(group = rownames(emotions), emotions)
            emotions <- gather(emotions, "emotions", "count", -group)
            assign("emotions", emotions, envir = e)

            PageGUI("Emotions", PlotS2, id = "EmotionsPage", envir = envir, theme = "theme_light",  title = "Emotions", 
                xlab = "Groups", ylab = "Count",
                parent = parent, notebook = notebook)
        }

        PageGUI("Sentiments", PlotS, id = "SentimentsPage", envir = envir, theme = "theme_light",  title = "Sentiments", 
            xlab = "Groups", ylab = "Count",
            parent = parent, notebook = notebook)
    }

    dialog <- function() {
        window <- tktoplevel(width = 300, height = 175)
        tkwm.title(window, "Emotions")
        tkwm.state(window, "withdrawn")
        frame <- ttkframe(window,  padding = c(3, 3, 12, 12))
        tkpack(frame, expand = TRUE, fill = "both")

        frame_1 <- ttkframe(frame)
        tkpack(frame_1, expand = TRUE, fill = "both")
        m <- ttklabel(frame_1, text = "This can take few minutes and use a lot of cpu.\nAre you sure?")
        tkpack(m, expand = TRUE, fill = "both")

        frame_2 <- ttkframe(frame_1)
        tkpack(frame_2)

        ok_button <- ttkbutton(frame_2, text = "OK", 
                       command = function() {
                                time <- system.time({ ok_callback() })
                                console(cmds = "time", envir = environment())
                                tkdestroy(window)
                            })
        cancel_button <- ttkbutton(frame_2, text = "Cancel", 
                    command = function() tkdestroy(window))

        tkpack(ok_button, side = "left", padx = 12)
        tkpack(cancel_button)

        tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
        tkbind("TButton", "<FocusIn>", function(W) tcl(W, "state", "active"))
        tkbind("TButton", "<FocusOut>", function(W) tcl(W, "state", "!active"))

        tkwm.state(window, "normal")
        tkwm.resizable(window, FALSE, FALSE)
        tkfocus(ok_button)
    }

    dialog()
}