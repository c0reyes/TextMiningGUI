BalloonPlotPage <- function() {
    Plot <- function(graph) {
        colors <- c("#0D0887FF", "#6A00A8FF", "#B12A90FF", "#E16462FF", "#FCA636FF", "#F0F921FF")

        set.seed(0)
        w <- tm$data[1:graph$limit,]
        pb <- ggballoonplot(w, fill = "value") +
                scale_fill_gradientn(colors = colors) +
                theme_minimal() +
                guides(size = FALSE) + 
                theme(axis.text.x = element_blank(),
                      axis.text.y = element_blank())
            
        return(pb)
    }

    PageGUI("Words Most Used", Plot, limit = 50)
}