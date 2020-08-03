CommonPage <- function(X, parent, notebook, envir) {
    word <- freq <- NULL
    
    Plot <- function(graph) {
        if(!is.null(graph$reload)) { 
            plot(save$plot)
            return(NULL)
        }
        
        t <- match.fun(graph$theme)

        colors <- c("#0D0887FF", "#6A00A8FF", "#B12A90FF", "#E16462FF", "#FCA636FF", "#F0F921FF")
        
        w <- X$data[1:graph$limit,,drop = FALSE]
        
        if(requireNamespace("ggpubr", quietly = TRUE)) 
            plot <- ggpubr::ggballoonplot(w, fill = "value") +
                scale_fill_gradientn(colors = colors) +
                theme_minimal() +
                guides(size = FALSE) + 
                theme_white() +
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      panel.grid.major = element_line(color = "lightgray")) 
        else {
            tt <- X$freq %>% group_by(word) %>% summarise(freq = sum(freq)) %>% arrange(desc(freq))
            plot <- ggplot(tt[1:graph$limit,,drop = FALSE], aes(x = word, y = freq)) + 
                geom_bar(position = "dodge", stat = "identity", fill = graph$color) + 
                coord_flip() +
                labs(title = graph$title, subtitle = graph$subtitle, caption = graph$caption) + t()
        }
            
        save$plot <- plot
        assign(name, save, envir = toprint)
        
        plot(plot)
    }

    name <- as.character(runif(1))
    env = environment()
    env$save <- list()
    env$save$name <- "Most common words"
    class(env$save) <- "save"

    PageGUI("Most commond words", Plot, id = as.character(match.call()[[1]]), envir = envir, 
        color = "lightblue", theme = "theme_gray", title = "Most common words", subtitle = " ", caption = " ",
        limit = 50, parent = parent, notebook = notebook, to = nrow(X$data))
}