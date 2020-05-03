CaPage <- function(X, parent, notebook) {
    Plot <- function(graph) {
        w <- X$data[1:graph$limit,]
        chisq <- chisq.test(w)
        tm.ca <- FactoMineR::CA(w, graph = FALSE)

        cb <- fviz_ca_biplot(tm.ca, arrow = c(FALSE, TRUE), 
                repel = graph$repel, 
                col.col = graph$vcolor, col.row = graph$pcolor, map = "symbiplot") +
                labs(title = graph$title) + theme_white()
    
        return(cb) 
    }

    PageGUI("CA - Biplot", Plot, title = "CA - Biplot", limit = 100, 
        vector_color = "red", point_color = "blue", repel = FALSE,
        parent = parent, notebook = notebook, to = nrow(X$data))
}
