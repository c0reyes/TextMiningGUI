Correlation <- function() {
    Plot <- function(graph) {
        cor <- tm$data %>% correlate() %>% network_plot(min_cor = .5, repel = TRUE)
        
        if(graph$alpha == 1) {
            cor <- cor + scale_alpha_continuous(range = c(1,1))
        }

        return(cor)
    }

    console_chunk("print(tm$data %>% correlate() %>% fashion())")
    PageGUI("Correlation", Plot)
}