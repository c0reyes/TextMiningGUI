ClusterPage <- function(X, parent, notebook) {
    Plot <- function(graph) {
        w <- X$data[1:graph$limit,]
        w <- Convert(w)
        
        w_dist <- dist(w, method = "euclidean")
        plot <- hclust(w_dist, method = "ward.D2")

        if(graph$clustert != "rectangle") {
            colors = c("black", "red", "blue", "green", "orange", "purple", "brown", "gray") 
            if(graph$cluster > 1)
                clus = cutree(plot, graph$cluster)
            else
                clus = rep(1, graph$limit)

            plot(as.phylo(plot), main = graph$title, type = graph$clustert, tip.color = colors[clus], cex = 0.8)
        } else {
            plot(plot, sub = "", xlab = "", main = graph$title)
            if(graph$cluster > 1)
                rect.hclust(plot, k = graph$cluster, border = "red")
        }
    }

    PageGUI("Cluster", Plot, limit = 100,
        parent = parent, notebook = notebook, to = nrow(X$data), title = "Cluster", type = "plot", cluster = 1, clustert = "rectangle")
}