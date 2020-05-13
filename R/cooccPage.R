cooccPage <- function(X, parent, notebook) {
    Plot <- function(graph) {
        w <- X$bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>% 
        count(word1, word2, sort = TRUE) %>%
        filter(n > graph$limit) %>% graph_from_data_frame(directed = FALSE)

        V(w)$size <- degree(w)
        V(w)$clu <- as.character(membership(cluster_louvain(w)))

        set.seed(0)
        plot <- ggraph(graph = w, layout = 'fr') + # nicely
            geom_edge_link0(aes(edge_width = n), colour = graph$vcolor, alpha = (if(graph$alpha == 1) 1 else 0.50))  +
            geom_node_point(aes(fill = as.numeric(V(w)$clu), size = size), stroke = 0, shape = 21)+
            geom_node_text(aes(filter = size >= 10, label = name), color = graph$tcolor, size = 8, repel = FALSE) +
            geom_node_text(aes(filter = size < 10, label = name), color = graph$tcolor, size = 4, repel = FALSE) +
            scale_fill_distiller(palette = graph$palette)+
            scale_edge_width(range = c(0.2,3))+
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

        plot(plot)
    }

    if(!require(igraph)) return(NULL)
    if(!require(ggraph)) return(NULL)
    if(is.null(X$bigrams)) return(NULL)

    t <- X$bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>% 
        count(word1, word2, sort = TRUE) %>% select(n)
    
    t.min <- t %>% min
    t.max <- t %>% max

    PageGUI("co-occurrence", Plot, palette = "Dark2", vector_color = "gray70", text_color = "#000000", background = "#ffffff", limit = 10, from = t.min, to = t.max, resolution = 1,
        parent = parent, notebook = notebook)
}