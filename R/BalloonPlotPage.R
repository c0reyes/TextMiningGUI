BalloonPlotPage <- function(X, parent, notebook, envir) {
    Plot <- function(graph) {
        colors <- c("#0D0887FF", "#6A00A8FF", "#B12A90FF", "#E16462FF", "#FCA636FF", "#F0F921FF")
        
        w <- X$data[1:graph$limit,]
        
        plot <- ggballoonplot(w, fill = "value") +
                scale_fill_gradientn(colors = colors) +
                theme_minimal() +
                guides(size = FALSE) + 
                theme_white() +
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      panel.grid.major = element_line(color = "lightgray")) 
            
        plot(plot)
    }

    if(!require(ggpubr)) return(NULL)

    PageGUI("Words Most Used", Plot, id = as.character(match.call()[[1]]), envir = envir, limit = 50, parent = parent, notebook = notebook, to = nrow(X$data))
}