WorldCloud <- function() {
    Plot <- function(theme, palette, ...) {
        t <- match.fun(theme)

        set.seed(0)
        w <- tm$freq[1:100,]
        pw <- w %>% ggplot(aes(label = word, size = freq, color = freq)) +
            geom_text_wordcloud(aes(angle = 45 * sample(-2:2, nrow(w),
                                    replace = TRUE,
                                    prob = c(1, 1, 4, 1, 1)))) +
            scale_size_area(max_size = 12) + t() + scale_color_fermenter(palette = palette)

        return(pw)
    }

    PageGUI("World Cloud", Plot, 1.25, 1.25, theme = "theme_void", palette = "Dark2")
}