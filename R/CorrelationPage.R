CorrelationPage <- function(X, parent, notebook, envir) {
    Plot <- function(graph) {
        if(!is.null(graph$reload)) { 
            plot(env$save$plot)
            return(NULL)
        }
        
        plot <- cor %>% corrr::network_plot(min_cor = graph$limit, repel = TRUE) 
        plot <- plot + theme_white() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                                           axis.title.y = element_blank(), axis.text.y = element_blank())

        if(graph$alpha == 1) {
            plot <- plot + scale_alpha_continuous(range = c(1,1))
        }

        env$save$plot <- plot
        assign(name, env$save, envir = toprint)
        
        plot(plot)
    }

    cor <- tryCatch({
            X$data %>% corrr::correlate() 
        }, error = function(cond) {
            tkmessageBox(title = "Error", message = "Error:", icon = "error", detail = "Some error occurred verify your data.", type = "ok")
        })

    console(cmds = "cor %>% fashion()", envir = environment())

    name <- as.character(runif(1))
    env = environment()
    env$save <- list()
    env$save$name <- "Correlation"
    env$save$table <- X$data %>% corrr::correlate()
    class(env$save) <- "save"

    PageGUI("Correlation", Plot, id = as.character(match.call()[[1]]), envir = envir, parent = parent, notebook = notebook, from = 0.1, to = 0.9, resolution = 0.1, limit = 0.5)
}