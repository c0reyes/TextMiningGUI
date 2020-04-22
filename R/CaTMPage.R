CaTM <- function() {
    .CA <- function(limit) {
        w <- tm$data[1:limit,]
        chisq <- chisq.test(w)

        tkmessageBox(title = "CA", message = "Chisq test", detail = paste("P.Value", as.character(chisq$p.value)), type = "ok")

        tm.ca <<- FactoMineR::CA(w, graph = FALSE)
        console_chunk("summary(tm.ca)")
    }

    Plot <- function(graph) {
        .CA(graph$limit)
        
        bp <- fviz_screeplot(tm.ca, addlabels = TRUE, linecolor = "red") +
                labs(title = graph$title) +
                theme(plot.background = element_rect(fill = "#ffffff", size = 0),
                      panel.background = element_rect(fill ="#ffffff", size = 0))
        return(bp)
    }

    tkentryconfigure(ca_menu, 1, state = "normal")
    tkentryconfigure(ca_menu, 2, state = "normal")
    tkentryconfigure(ca_menu, 3, state = "normal")

    PageGUI("CA - Scree plot", Plot, title = "CA - Scree plot", limit = 100)
}

CaBiplot <- function() {
    Plot <- function(graph) {
        cb <- fviz_ca_biplot(tm.ca, arrow = c(FALSE, TRUE), 
                repel = graph$repel, 
                col.col = graph$vcolor, col.row = graph$pcolor, map = "colgreen") +
                labs(title = graph$title)

        return(cb)        
    }

    PageGUI("CA - Biplot", Plot, title = "CA - Biplot", vector_color = "red", point_color = "blue", repel = FALSE)
}

QualityRow <- function() {
    Plot <- function(graph) {
        qr <- fviz_ca_row(tm.ca, col.row = "cos2",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                repel = graph$repel) + 
                labs(title = graph$title) 

        return(qr)
    }

    PageGUI("CA - Row points", Plot, title = "CA - Row points", repel = FALSE)
}

QualityCol <- function() {
    Plot <- function(graph) {
        qr <- fviz_ca_col(tm.ca, col.col = "cos2", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = graph$repel) + 
                labs(title = graph$title) 
                
        return(qr)
    }

    PageGUI("CA - Quality and contribution", Plot, title = "CA - Column points", repel = FALSE)
}