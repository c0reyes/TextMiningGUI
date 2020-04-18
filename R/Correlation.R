Correlation <- function() {
    Plot <- function(graph) {
        cor <- tm$data %>% correlate() %>% network_plot(min_cor = .1)

        return(cor)
    }

    PageGUI("Correlation", Plot)
}