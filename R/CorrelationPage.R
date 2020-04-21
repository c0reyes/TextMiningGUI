CorrelationPage <- function() {
    window <- tktoplevel(width = 350, height = 150)
    tkwm.minsize(window, "350", "150")
    tkwm.maxsize(window, "350", "150")
    tkwm.title(window, "Correlation")

    frame <- ttkframe(window, padding = c(3,3,12,12))
    tkpack(frame, expand = TRUE, fill = "both")

    label_frame <- ttklabelframe(frame, text = "Correlations", padding = 10)
    tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)

    tkgrid.columnconfigure(label_frame, 0, weight = 1)
    tkgrid.columnconfigure(label_frame, 1, weight = 10)
    tkgrid.columnconfigure(label_frame, 2, weight = 1)
    tkgrid.columnconfigure(label_frame, 1, weight = 10)

    put_label(label_frame, "Minimum: ", 1, 0)
    min_cor <- tclVar(0.5)
    mincor <- tkscale(label_frame, from = 0, to = 0.9, variable = min_cor, showvalue = TRUE, resolution = 0.1, orient = "horiz")
    tkgrid(mincor, row = 1, column = 1, sticky = "ew", padx = 2)

    button_frame <- ttkframe(frame)
    cancel_button <- ttkbutton(button_frame, text = "cancel",
        command = function() { 
            tkdestroy(window) 
        })
    ok_button <- ttkbutton(button_frame, text = "ok",
        command = function() { 
            Plot <- function(graph) {
                cor <- tm$data %>% correlate() %>% network_plot(min_cor = tclvalue(min_cor), repel = TRUE) 
                cor <- cor + theme_void()

                if(graph$alpha == 1) {
                    cor <- cor + scale_alpha_continuous(range = c(1,1))
                }

                return(cor)
            }

            console_chunk("print(tm$data %>% correlate() %>% fashion())")
            PageGUI("Correlation", Plot)

            tkdestroy(window)
        })
    tkpack(button_frame, fill = "x", padx = 5, pady = 5)
    tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
       fill = "y", side = "left")               
    sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
}