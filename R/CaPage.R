CaPage <- function(X, parent, notebook) {
    Plot <- function(graph) {
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

        w <- X$data[1:graph$limit,]
        chisq <- chisq.test(w)
        tm.ca <- FactoMineR::CA(w, graph = FALSE)
            
        if(graph$reload == 0) {
            tkmessageBox(title = "CA", message = "Chisq test", detail = paste("P.Value", as.character(chisq$p.value)), type = "ok") 
            console(cmds = "summary(tm.ca)", e = environment())

            if(exists("cabiplot", envir = environment())) {
                cabiplot()
            }else{
                assign("cabiplot", PageGUI("CA - Biplot", CaBiPlot, title = "CA - Biplot", vector_color = "red", point_color = "blue", repel = FALSE, parent = parent, notebook = notebook), envir = environment())
            }

            if(exists("qualityrow", envir = environment())) {
                qualityrow()
            }else{
                assign("qualityrow", PageGUI("CA - Row points", QualityRow, title = "CA - Row points", repel = FALSE, parent = parent, notebook = notebook), envir = environment())
            }
            
            if(exists("qualitycol", envir = environment())) {
                qualitycol()
            }else{
                assign("qualitycol", PageGUI("CA - Quality and contribution", QualityCol, title = "CA - Column points", repel = FALSE, parent = parent, notebook = notebook), envir = environment())
            }
        }else{
            if(exists("cabiplot", envir = environment())) cabiplot()
            if(exists("qualityrow", envir = environment())) qualityrow()
            if(exists("qualitycol", envir = environment())) qualitycol()
        }

        bp <- fviz_screeplot(tm.ca, addlabels = TRUE, linecolor = "red") +
                labs(title = graph$title) + theme_white()

        return(bp)
    }

    PageGUI("CA - Scree plot", Plot, title = "CA - Scree plot", limit = 100, parent = parent, notebook = notebook, to = nrow(X$data))
}
