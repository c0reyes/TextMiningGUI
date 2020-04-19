BalloonPlot <- function() {
    Plot <- function(graph) {
        colors <- c("#0D0887FF", "#6A00A8FF", "#B12A90FF", "#E16462FF", "#FCA636FF", "#F0F921FF")

        set.seed(0)
        w <- tm$data[1:graph$limit,]
        pb <- ggballoonplot(w, fill = "value") +
                scale_fill_gradientn(colors = colors) +
                guides(size = FALSE) +
                theme(plot.background = element_rect(fill = "#ffffff", size = 0),
                      panel.background = element_rect(fill ="#ffffff", size = 0))

        return(pb)
    }

    PageGUI("Balloon Plot", Plot, limit = 50)
}