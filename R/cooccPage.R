cooccPage <- function(X, parent, notebook, envir) {
    size <- bigram <- word1 <- word2 <- NULL
    
    Plot <- function(graph) {
        if(!is.null(graph$reload)) { 
            plot(env$save$plot)
            return(NULL)
        }
        
        w <- w %>% dplyr::filter(n > graph$limit) %>% igraph::graph_from_data_frame(directed = FALSE)

        igraph::V(w)$size <- igraph::degree(w)
        igraph::V(w)$clu <- as.character(igraph::membership(igraph::cluster_louvain(w)))

        plot <- ggraph::ggraph(graph = w, layout = 'fr') + # nicely
            ggraph::geom_edge_link0(aes(edge_width = n), colour = graph$vcolor, alpha = (if(graph$alpha == 1) 1 else 0.50))  +
            ggraph::geom_node_point(aes(fill = as.numeric(igraph::V(w)$clu), size = size), stroke = 0, shape = 21)+
            ggraph::geom_node_text(aes(label = name), color = graph$tcolor, size = graph$tsize, repel = FALSE) +
            scale_fill_distiller(palette = graph$palette)+
            ggraph::scale_edge_width(range = c(0.2,3))+
            scale_size(range = c(1,6))+
            theme(legend.position = "none") +
            theme(
                plot.background = element_rect(fill = graph$background),
                panel.background = element_rect(fill = graph$background),
                panel.border = element_rect(color = graph$background, fill = NA),
                axis.line = element_line(color = graph$background),
                axis.ticks = element_line(color = graph$background),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())

        env$save$plot <- plot
        assign(name, env$save, envir = toprint)

        plot(plot)
    }

    w <- X$bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>% 
        count(word1, word2, sort = TRUE)

    t <- w %>% select(n)
    
    t.min <- t %>% min
    t.max <- t %>% max

    name <- as.character(runif(1))
    env = environment()
    env$save <- list()
    env$save$name <- "Co-occurrence"
    class(env$save) <- "save"

    PageGUI("Co-occurrence", Plot, id = as.character(match.call()[[1]]), envir = envir, palette = "Dark2", vector_color = "gray70", text_color = "#000000", 
        background = "#ffffff", limit = 10, from = t.min, to = t.max, resolution = 1, text_size = 4,
        parent = parent, notebook = notebook)
}