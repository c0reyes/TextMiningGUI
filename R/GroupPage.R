GroupPage <- function(X, parent, notebook, envir) {
    GROUP <- TIME <- COUNT <- freq <- ..count.. <- NULL
    
    Plot <- function(graph) {
        if(!is.null(graph$reload)) { 
            plot(env$save$plot)
            return(NULL)
        }
        
        t <- match.fun(graph$theme)
        
        if(graph$time == TRUE) {
            total <- tryCatch({ 
                row_sums(X$dtm, na.rm = TRUE)
            }, error = function(cond) {
                tkmessageBox(title = "Error", message = "Error:", icon = "error", detail = "Some error occurred verify your data.", type = "ok")
            })

            df <- data.frame(TIME = X$df[X$dtm$dimnames$Docs,]$TIME, GROUP = X$df[X$dtm$dimnames$Docs,]$GROUP, total) %>% group_by(GROUP, TIME) %>% summarise(COUNT = sum(total))

            plot <- ggplot(data = df, aes(x = TIME, y = COUNT, group = GROUP)) +
                geom_line(aes(color = GROUP)) +
                geom_point(aes(color = GROUP)) +
                labs(title = graph$title, subtitle = graph$subtitle, caption = graph$caption) + t() +
                scale_color_manual(values = tmPalette(graph$palette, X$len))
        }else {
            plot <- X$freq %>% ggplot(aes(weight = freq, x = GROUP)) + geom_bar(fill = graph$color) + 
                labs(title = graph$title, subtitle = graph$subtitle, caption = graph$caption) + 
                xlab(graph$xlab) + 
                ylab(graph$ylab)

            plot <- plot + theme(text = element_text(size = 12)) + t() +
                geom_text(stat='count', aes(label=..count..), position = position_dodge(width = 0.9), vjust = 1.5, color = graph$tcolor, size = 5)

            if(graph$flip == TRUE)
                plot <- plot + coord_flip()
        }

        env$save$plot <- plot
        assign(name, env$save, envir = toprint)
        
        plot(plot)
    }

    name <- as.character(runif(1))
    env = environment()
    env$save <- list()
    env$save$name <- "Word Group"
    class(env$save) <- "save"

    time <- if("TIME" %in% colnames(X$df)) " " else ""

    PageGUI("Word Group", Plot, id = as.character(match.call()[[1]]), envir = envir, color = "lightblue", theme = "theme_gray", title = "Word Group", 
            xlab = "Groups", ylab = "Freq", flip = FALSE, subtitle = " ", caption = " ", text_color = "#323232", palette = "Dark2",
            parent = parent, notebook = notebook, time = time)
}