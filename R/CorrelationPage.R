CorrelationPage <- function(X, parent, notebook) {
    Plot <- function(graph) {
        corx <- X$data %>% correlate() %>% fashion()
        console(cmds = "corx", e = environment())

        cor <- X$data %>% correlate() %>% network_plot(min_cor = graph$limit, repel = TRUE) 
        cor <- cor + theme_white() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                                           axis.title.y = element_blank(), axis.text.y = element_blank())

        if(graph$alpha == 1) {
            cor <- cor + scale_alpha_continuous(range = c(1,1))
        }

        return(cor)
    }

    PageGUI("Correlation", Plot, parent = parent, notebook = notebook, from = 0.1, to = 0.9, resolution = 0.1, limit = 0.5)
}