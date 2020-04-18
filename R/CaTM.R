CaTM <- function() {
    Plot <- function(graph) {
        bp <- fviz_screeplot(tm.ca, addlabels = TRUE) +
                labs(title = graph$title)
        return(bp)
    }

    w <- tm$data[1:100,]
    chisq <- chisq.test(w)
    
    tkmessageBox(title = "CA", message = "Chisq test", detail = paste("P.Value", as.character(chisq$p.value)), type = "ok")

    tm.ca <<- FactoMineR::CA(w, graph = FALSE)

    PageGUI("CA - Scree plot", Plot, 1.5, 1.5, title = "CA - Scree plot")
}

CaBiplot <- function() {
    Plot <- function(graph) {
        cb <- fviz_ca_biplot(tm.ca, arrow = c(FALSE, TRUE), 
                repel = graph$repel, map = "rowgreen", 
                col.col = graph$vcolor, col.row = graph$pcolor) +
                labs(title = graph$title)
    }

    PageGUI("CA - Biplot", Plot, 1.5, 1.5, title = "CA - Biplot", vector_color = "red", point_color = "blue", repel = FALSE)
}

QualityRow <- function() {
    Plot <- function(graph) {
        qr <- fviz_ca_row(tm.ca, col.row = "cos2",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                repel = graph$repel) + 
                labs(title = graph$title)
        return(qr)
    }

    PageGUI("CA - Row points", Plot, 1.5, 1.5, title = "CA - Row points", repel = FALSE)
}