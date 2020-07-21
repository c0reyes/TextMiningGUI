EmotionsPage <- function(X, parent, notebook, envir) {
    GROUP <- TEXT <- GROUP <- emotions <- emotion <- sentiments <- sentiment <- group <- NULL
    
    Plot <- function(graph) {
        t <- match.fun(graph$theme)

        plot <- ggplot(data = emotions, aes(x = emotion, y = count, group = GROUP, color = GROUP)) +
            geom_polygon(fill = NA, size = 1) +
            scale_color_manual(values = tmPalette(graph$palette, X$len)) +
            coord_polar() + 
            labs(title = graph$title, x = "", y = "")

        plot <- plot + t()

        name <- as.character(runif(1))
        save <- list()
        save$name <- "Emotions"
        save$plot <- plot
        class(save) <- "save"
        assign(name, save, envir = toprint)

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
        save$name <- "Sentiments"
        save$plot <- plot
        class(save) <- "save"
        assign(name, save, envir = toprint)

        plot(plot)
    }

    e <- environment()

    ok_callback <- function() {
        emotions <- X$df %>% select(GROUP, TEXT) %>% group_by(GROUP) %>% group_modify(~ Emotions(.x$TEXT, language = X$lang))
        emotions <- emotions %>% group_by(GROUP) %>% pivot_wider(names_from = GROUP, values_from = count) 
        emotions <- as.data.frame(emotions)
        rownames(emotions) <- emotions[,1]
        sentiments <- emotions
        sentiments[,1] <- NULL

        emotions <- emotions[1:8,,drop = FALSE]
        sentiments <- sentiments[9:10,,drop = FALSE]

        sentiments <- as.data.frame(t(sentiments))
        sentiments <- cbind(group = rownames(sentiments), sentiments)
        sentiments <- gather(sentiments, "sentiment", "count", -group)

        emotions <- gather(emotions, "GROUP", "count", -emotion)

        assign("emotions", emotions, envir = e)
        assign("sentiments", sentiments, envir = e)

        console(cmds = "emotions", envir = e)
        console(cmds = "sentiments", envir = e)

        PageGUI("Emotions", Plot, id = "EmotionsPage", envir = envir, theme = "theme_minimal", title = "Emotions", palette = "Dark2",
            parent = parent, notebook = notebook)

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