cooccPage <- function(X, parent, notebook, envir) {
    Plot <- function(graph) {
        w <- w %>% filter(n > graph$limit) %>% graph_from_data_frame(directed = FALSE)

        V(w)$size <- degree(w)
        V(w)$clu <- as.character(membership(cluster_louvain(w)))

        set.seed(0)
        plot <- ggraph(graph = w, layout = 'fr') + # nicely
            geom_edge_link0(aes(edge_width = n), colour = graph$vcolor, alpha = (if(graph$alpha == 1) 1 else 0.50))  +
            geom_node_point(aes(fill = as.numeric(V(w)$clu), size = size), stroke = 0, shape = 21)+
            geom_node_text(aes(label = name), color = graph$tcolor, size = graph$tsize, repel = FALSE) +
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

        save$plot <<- plot
        assign(name, save, envir = print)

        plot(plot)
    }

    if(is.null(X$bigrams)) {
        tkmessageBox(title = "Error", message = "Error:", icon = "error", detail = "Bigrams it's need.", type = "ok")
        return(NULL)
    }

    w <- X$bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>% 
        count(word1, word2, sort = TRUE)

    t <- w %>% select(n)
    
    t.min <- t %>% min
    t.max <- t %>% max

    name <- as.character(runif(1))
    save <- list()
    save$name <- as.character(match.call()[[1]])
    class(save) <- "save"

    PageGUI("co-occurrence", Plot, id = as.character(match.call()[[1]]), envir = envir, palette = "Dark2", vector_color = "gray70", text_color = "#000000", 
        background = "#ffffff", limit = 10, from = t.min, to = t.max, resolution = 1, text_size = 4,
        parent = parent, notebook = notebook)
}