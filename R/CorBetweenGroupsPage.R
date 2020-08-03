CorBetweenGroupsPage <- function(X, parent, notebook, envir) {
    .data <- NULL
    vars <- colnames(X$data)

    group1 <- tclVar("")
    group2 <- tclVar("")

    window <- tktoplevel(width = 300, height = 175)
    tkwm.minsize(window, "300", "175")
    tkwm.maxsize(window, "300", "175")

    tkwm.title(window, "Correlation between groups")
    frame <- ttkframe(window, padding = c(3,3,12,12))
    tkpack(frame, expand = TRUE, fill = "both")

    label_frame <- ttklabelframe(frame, text = "Options", padding = 10)
    tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)

    tkgrid.columnconfigure(label_frame, 0, weight = 1)
    tkgrid.columnconfigure(label_frame, 1, weight = 10)
    tkgrid.columnconfigure(label_frame, 2, weight = 1)
    tkgrid.columnconfigure(label_frame, 1, weight = 10)

    put_label(label_frame, "Group1: ", 1, 0)
    combo_box1 <- ttkcombobox(label_frame, 
                    values = vars, 
                    textvariable = group1,
                    state = "normal",
                    justify = "left")
    tkgrid(combo_box1, row = 1, column = 1, sticky = "ew", padx = 2)

    put_label(label_frame, "Group2: ", 2, 0)
    combo_box2 <- ttkcombobox(label_frame, 
                    values = vars, 
                    textvariable = group2,
                    state = "normal",
                    justify = "left")
    tkgrid(combo_box2, row = 2, column = 1, sticky = "ew", padx = 2)

    button_frame <- ttkframe(frame)
    cancel_button <- ttkbutton(button_frame, text = "cancel",
        command = function() { 
            tkdestroy(window) 
        })
    ok_button <- ttkbutton(button_frame, text = "ok",
        command = function() { 
            g1 <- tclvalue(group1)
            g2 <- tclvalue(group2)

            if(g1 != "" && g2 != "") {
                Plot <- function(graph) {
                    if(!is.null(graph$reload)) { 
                        plot(env$save$plot)
                        return(NULL)
                    }

                    w <- X$data[1:graph$limit,]
                    plot <- ggplot(w, aes(.data[[g1]], .data[[g2]])) +
                                geom_jitter(alpha = graph$alpha, size = 2.5, width = 0.25, height = 0.25) +
                                geom_text(aes(label = rownames(w)), check_overlap = TRUE, vjust = 1.5) +
                                geom_abline(color = "red") +
                                theme_bw() +
                                labs(x = g1, y = g2) +
                                theme(axis.text.x = element_blank(),
                                      axis.text.y = element_blank())

                    name <- as.character(runif(1))
                    env$save <- list()
                    env$save$name <- "Correlation Between Two Groups"
                    env$save$plot <- plot
                    class(env$save) <- "save"
                    assign(name, env$save, envir = toprint)

                    plot(plot)
                }

                env = environment()

                PageGUI("Correlation between groups", Plot, id = "CorBetweenGroupsPage", limit = 100, envir = envir, parent = parent, notebook = notebook, to = nrow(X$data))
                tkdestroy(window)
            }   
        })
    tkpack(button_frame, fill = "x", padx = 5, pady = 5)
    tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
       fill = "y", side = "left")               
    sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
}