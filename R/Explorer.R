Explorer <- function() {
    Plot <- function(color, theme, title, xlab, ylab, flip = FALSE, ...) {
        t <- match.fun(theme)
        pe <- tm$freq %>% ggplot(aes(x = GROUP)) + geom_bar(fill = color) + labs(title = title) + xlab(xlab) + ylab(ylab)
        pe <- pe + theme(text = element_text(size = 12)) + t()
        if(flip == TRUE)
            pe <- pe + coord_flip()
        return(pe)
    }

    PageGUI("Distinct words by groups", Plot, color = "#000000", theme = "theme_classic", title = "Distinct words by groups", xlab = "Groups", ylab = "Counts", flip = FALSE)
}