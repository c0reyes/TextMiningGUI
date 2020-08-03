WordCloudPage <- function(X, parent, notebook, envir) {
    word <- freq <- NULL
    
    Plot <- function(graph) {
        if(!is.null(graph$reload)) { 
            plot(env$save$plot)
            return(NULL)
        }

        w <- X$freq[1:graph$limit,]
        plot <- w %>% ggplot(aes(label = word, size = freq, color = freq)) +
            geom_text_wordcloud(aes(angle = 45 * sample(-2:2, nrow(w),
                                    replace = TRUE,
                                    prob = c(1, 1, 4, 1, 1)))) +
            scale_size_area(max_size = 12) + scale_color_fermenter(palette = graph$palette) + 
            theme(
                plot.background = element_rect(fill = graph$background),
                panel.background = element_rect(fill = graph$background),
                panel.border = element_rect(color = graph$background, fill = NA),
                axis.line = element_line(color = graph$background),
                axis.ticks = element_line(color = graph$background),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())

        env$save$plot <- plot
        assign(name, env$save, envir = toprint)

        plot(plot)
    }

    name <- as.character(runif(1))
    env = environment()
    env$save <- list()
    env$save$name <- "Word Cloud"
    class(env$save) <- "save"

    PageGUI("Word Cloud", Plot, id = as.character(match.call()[[1]]), envir = envir, palette = "Dark2", background = "#ffffff", limit = 100,
        parent = parent, notebook = notebook, to = nrow(X$data))
}