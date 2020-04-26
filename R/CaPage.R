CaPage <- function() {
    .CA <- function(limit) {
        w <- tm$data[1:limit,]
        chisq <- chisq.test(w)

        tkmessageBox(title = "CA", message = "Chisq test", detail = paste("P.Value", as.character(chisq$p.value)), type = "ok")

        tm.ca <<- FactoMineR::CA(w, graph = FALSE)
        console_chunk("summary(tm.ca)")
    }

    CaBiPlot <- function(graph) {
        cb <- fviz_ca_biplot(tm.ca, arrow = c(FALSE, TRUE), 
                repel = graph$repel, 
                col.col = graph$vcolor, col.row = graph$pcolor, map = "symbiplot") +
                labs(title = graph$title) + theme_white()

        return(cb)        
    }

    QualityRow <- function(graph) {
        qr <- fviz_ca_row(tm.ca, col.row = "cos2",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                repel = graph$repel) + 
                labs(title = graph$title) + theme_white()

        return(qr)
    }

    QualityCol <- function(graph) {
        qr <- fviz_ca_col(tm.ca, col.col = "cos2", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = graph$repel) + 
                labs(title = graph$title) + theme_white()
                
        return(qr)
    }

    Plot <- function(graph) {
        if(graph$reload == 0) {
            .CA(graph$limit)
            if(exists("cabiplot")) {
                cabiplot()
            }else{
                cabiplot <<- PageGUI("CA - Biplot", CaBiPlot, title = "CA - Biplot", vector_color = "red", point_color = "blue", repel = FALSE)
            }

            if(exists("qualityrow")) {
                qualityrow()
            }else{
                qualityrow <<- PageGUI("CA - Row points", QualityRow, title = "CA - Row points", repel = FALSE)
            }
            
            if(exists("qualitycol")) {
                qualitycol()
            }else{
                qualitycol <<- PageGUI("CA - Quality and contribution", QualityCol, title = "CA - Column points", repel = FALSE)
            }
        }else{
            if(exists("cabiplot")) cabiplot()
            if(exists("qualityrow")) qualityrow()
            if(exists("qualitycol")) qualitycol()
        }

        bp <- fviz_screeplot(tm.ca, addlabels = TRUE, linecolor = "red") +
                labs(title = graph$title) + theme_white()

        return(bp)
    }

    PageGUI("CA - Scree plot", Plot, title = "CA - Scree plot", limit = 100)
}
