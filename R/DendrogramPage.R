DendrogramPage <- function(X, parent, notebook) {
    Plot <- function(graph) {
        w <- X$data[1:graph$limit,]
        w <- Convert(w)
        
        w_dist <- dist(w, method = "euclidean")
        plot <- hclust(w_dist, method = "ward.D2")

        return(plot)
    }

    PageGUI("Dendrogram", Plot, limit = 100,
        parent = parent, notebook = notebook, to = nrow(X$data), title = "Cluster Dendrogram", type = "hclust")
}