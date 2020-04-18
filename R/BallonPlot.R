BallonPlot <- function() {
    Plot <- function(graph) {
        colors <- c("#0D0887FF", "#6A00A8FF", "#B12A90FF", "#E16462FF", "#FCA636FF", "#F0F921FF")

        set.seed(0)
        w <- tm$data[1:50,]
        pb <- ggballoonplot(w, fill = "value") +
                scale_fill_gradientn(colors = colors) +
                guides(size = FALSE)

        return(pb)
    }

    PageGUI("Ballon Plot", Plot)
}