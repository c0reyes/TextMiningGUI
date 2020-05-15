Save <- function(X, name = "", type = "") {
    window <- tktoplevel(width = 420, height = 420)
    tkwm.minsize(window, "420", "240")
    tkwm.maxsize(window, "420", "240")

    tkwm.title(window, "Save")
    frame <- ttkframe(window, padding = c(3,3,12,12))
    tkpack(frame, expand = TRUE, fill = "both")

    label_frame <- ttklabelframe(frame, text = "Options", padding = 10)
    tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)

    tkgrid.columnconfigure(label_frame, 0, weight = 1)
    tkgrid.columnconfigure(label_frame, 1, weight = 10)
    tkgrid.columnconfigure(label_frame, 2, weight = 1)
    tkgrid.columnconfigure(label_frame, 1, weight = 10)

    name <- tclVar(name)
    put_label(label_frame, "Name: ", 1, 0)
    name_entry <- ttkentry(label_frame, textvariable = name)
    tkgrid(name_entry, row = 1, column = 1, sticky = "ew", padx = 2)

    extension <- tclVar("png")
    put_label(label_frame, "extension: ", 2, 0)
    extension_box <- ttkcombobox(label_frame, 
                    values = c("png","pdf","jpeg","tiff","bmp","svg","tex","ps","eps"), 
                    textvariable = extension,
                    state = "normal",
                    justify = "left")
    tkgrid(extension_box, row = 2, column = 1, sticky = "ew", padx = 2)

    unit <- tclVar("")
    put_label(label_frame, "unit: ", 3, 0)
    unit_box <- ttkcombobox(label_frame,
              values = c("in", "cm", "mm"),
              textvariable = unit,
              state = "normal",
              justify = "left")
    tkgrid(unit_box, row = 3, column = 1, sticky = "ew", padx = 2)

    scale <- tclVar(init = 1)
    put_label(label_frame, "scale: ", 4, 0)
    scale_entry <- ttkentry(label_frame, textvariable = scale)
    tkgrid(scale_entry, row = 4, column = 1, sticky = "ew", padx = 2)

    width <- tclVar("")
    put_label(label_frame, "width: ", 5, 0)
    width_entry <- ttkentry(label_frame, textvariable = width)
    tkgrid(width_entry, row = 5, column = 1, sticky = "ew", padx = 2)

    height <- tclVar("")
    put_label(label_frame, "height: ", 6, 0)
    height_entry <- ttkentry(label_frame, textvariable = height)
    tkgrid(height_entry, row = 6, column = 1, sticky = "ew", padx = 2)

    dpi <- tclVar(init = 96)
    put_label(label_frame, "dpi/ppi: ", 7, 0)
    dpi_entry <- ttkentry(label_frame, textvariable = dpi)
    tkgrid(dpi_entry, row = 7, column = 1, sticky = "ew", padx = 2)

    button_frame <- ttkframe(frame)
    cancel_button <- ttkbutton(button_frame, text = "cancel",
        command = function() { 
            tkdestroy(window) 
        })
    ok_button <- ttkbutton(button_frame, text = "ok",
        command = function() {
            if(name == "") return(NULL)

            units <- if(tclvalue(unit) != "") tclvalue(unit) else c("in", "cm", "mm")
            width <- if(tclvalue(width) != "") as.numeric(tclvalue(width)) else NA
            height <- if(tclvalue(height) != "") as.numeric(tclvalue(height)) else NA
            dpi <- if(tclvalue(dpi) != "") as.numeric(tclvalue(dpi)) else 96
            scale <- if(tclvalue(scale) != "") as.numeric(tclvalue(scale)) else 1
            limitsize <- if(!is.na(width) && !is.na(height)) FALSE else TRUE

            if(type == "plot") {
                if(!tclvalue(extension) %in% c("bmp","jpeg","png","tiff")) {
                    tkmessageBox(title = "Save", message = "Error:", icon = "error", 
                        detail = "This plot not support the extension, try: bmp, jpeg, png or tiff", type = "ok") 
                    return(NULL)
                }

                p <- match.fun(tclvalue(extension))

                units <- if(length(units) == 3) "px" else units
                width <- if(is.na(width)) 620 else width
                height <- if(is.na(height)) 480 else height

                p(filename = paste0(tclvalue(name), ".", tclvalue(extension)), 
                 width = width, height = height,
                 units = units, res = dpi
                 )
                X
                dev.off()
            }else{
                ggsave(paste0(tclvalue(name), ".", tclvalue(extension)), 
                       plot = X, dpi = dpi,
                       units = units, scale = scale,
                       width = width, height = height, 
                       limitsize = limitsize
                       )
            }
            
            tkmessageBox(title = "Save", message = "Plot", detail = "The graph was saved.", type = "ok") 
            tkdestroy(window) 
        })
    tkpack(button_frame, fill = "x", padx = 5, pady = 5)
    tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
       fill = "y", side = "left")               
    sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
}