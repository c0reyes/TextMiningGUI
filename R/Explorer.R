Explorer <- function() {
    Plot <- function(color = "#000000", theme = "theme_classic", title = "Distinct words by groups", xlab = "Groups", ylab = "Counts", flip = FALSE) {
        t <- match.fun(theme)
        p <- tm$freq %>% ggplot(aes(x = GROUP)) + geom_bar(fill = color) + labs(title = title) + xlab(xlab) + ylab(ylab)
        p <- p + theme(text = element_text(size = 12)) + t()
        if(flip == TRUE)
            p <- p + coord_flip()
        return(p)
    }

    PageGUI("Distinct words by groups", Plot)
}