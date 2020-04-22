WordCloudPage <- function() {
    Plot <- function(graph) {
        t <- match.fun(graph$theme)

        set.seed(0)
        w <- tm$freq[1:graph$limit,]
        pw <- w %>% ggplot(aes(label = word, size = freq, color = freq)) +
            geom_text_wordcloud(aes(angle = 45 * sample(-2:2, nrow(w),
                                    replace = TRUE,
                                    prob = c(1, 1, 4, 1, 1)))) +
            scale_size_area(max_size = 12) + scale_color_fermenter(palette = graph$palette) +
                theme(plot.background = element_rect(fill = "#ffffff", size = 0),
                      panel.background = element_rect(fill ="#ffffff", size = 0)) + t()

        return(pw)
    }

    PageGUI("Word Cloud", Plot, theme = "theme_void", palette = "Dark2", background = "#ffffff", limit = 100)
}