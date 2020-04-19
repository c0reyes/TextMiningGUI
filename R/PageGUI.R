PageGUI <- function(name, Plot, color = "", theme = "", title = "", 
                    xlab = "", ylab = "", flip = "", palette = "", subtitle = "", caption = "",
                    background = "", text_color = "", vector_color = "", point_color = "", repel = "", limit = 0) {
    themes <- c("theme_gray", "theme_bw", "theme_linedraw", "theme_light", "theme_dark", "theme_minimal", "theme_classic", "theme_void")
    palettes <- c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Paired", "Dark2", "Accent")

    .color <- color
    .theme <- theme
    .title <- title
    .xlab <- xlab
    .ylab <- ylab
    .flip <- flip
    .palette <- palette
    .subtitle <- subtitle
    .caption <- caption
    .background <- background
    .tcolor <- text_color
    .vcolor <- vector_color
    .pcolor <- point_color
    .repel <- repel
    .limit <- limit

    graph <- list()
    graph$color <- .color
    graph$theme <- .theme
    graph$title <- .title
    graph$xlab <- .xlab
    graph$ylab <- .ylab
    graph$flip <- .flip
    graph$palette <- .palette
    graph$subtitle <- .subtitle
    graph$caption <- .caption
    graph$background <- .background
    graph$tcolor <- .tcolor
    graph$vcolor <- .vcolor
    graph$pcolor <- .pcolor
    graph$repel <- .repel
    graph$limit <- .limit
    graph$alpha <- 1
    class(graph) <- "graph"

    console_chunk("tm")
    page <- Page(notebook, name)
    content <- page$content

    sidebar <- ttklabelframe(content, width = 200, text = "Options")
    frame <- tkframe(content)

    tkgrid(sidebar, row = 0, column = 0, sticky = "nsw", padx = 5, pady = 5)
    tkgrid(frame, row = 0, column = 1, sticky = "nsw", padx = 5, pady = 5)
    tkgrid.columnconfigure(content, 0, weight = 1)
    tkgrid.columnconfigure(content, 1, weight = 2)
    tkgrid.rowconfigure(content, 0, weight = 1)

    # Sidebar
    tkgrid.columnconfigure(sidebar, 0, weight = 1)
    tkgrid.rowconfigure(sidebar, 0, weight = 0)
    tkgrid.rowconfigure(sidebar, 1, weight = 0)
    tkgrid.rowconfigure(sidebar, 2, weight = 0)
    tkgrid.rowconfigure(sidebar, 3, weight = 0)
    tkgrid.rowconfigure(sidebar, 4, weight = 0)
    tkgrid.rowconfigure(sidebar, 5, weight = 0)
    tkgrid.rowconfigure(sidebar, 6, weight = 0)
    tkgrid.rowconfigure(sidebar, 7, weight = 0)
    tkgrid.rowconfigure(sidebar, 8, weight = 0)
    tkgrid.rowconfigure(sidebar, 9, weight = 0)
    tkgrid.rowconfigure(sidebar, 10, weight = 0)
    tkgrid.rowconfigure(sidebar, 11, weight = 0)
    tkgrid.rowconfigure(sidebar, 12, weight = 0)
    tkgrid.rowconfigure(sidebar, 13, weight = 0)
    tkgrid.rowconfigure(sidebar, 14, weight = 0)
    tkgrid.rowconfigure(sidebar, 15, weight = 0)
    tkgrid.rowconfigure(sidebar, 16, weight = 0)
    tkgrid.rowconfigure(sidebar, 17, weight = 0)
    tkgrid.rowconfigure(sidebar, 18, weight = 0)
    tkgrid.rowconfigure(sidebar, 19, weight = 0)
    tkgrid.rowconfigure(sidebar, 20, weight = 0)
    tkgrid.rowconfigure(sidebar, 21, weight = 0)
    tkgrid.rowconfigure(sidebar, 22, weight = 0)
    tkgrid.rowconfigure(sidebar, 23, weight = 0)
    tkgrid.rowconfigure(sidebar, 24, weight = 0)
    tkgrid.rowconfigure(sidebar, 25, weight = 0)
    tkgrid.rowconfigure(sidebar, 26, weight = 0)
    tkgrid.rowconfigure(sidebar, 27, weight = 0)
    tkgrid.rowconfigure(sidebar, 28, weight = 0)

    llimit <- tclVar(init = .limit)
    put_label(sidebar, "Limit: ", 1, 1, sticky = "nw")
    limitbar <- tkscale(sidebar, from = 1, to = nrow(tm$data), variable = llimit, 
        showvalue = TRUE, resolution = 1, orient = "horiz", state = "disabled")
    tkgrid(limitbar, row = 2, column = 1, sticky = "ew", padx = 2)

    title <- tclVar()
    put_label(sidebar, "Title: ", 3, 1, sticky = "nw")
    entry1 <- ttkentry(sidebar, textvariable = title, state = "disabled")
    tkgrid(entry1, row = 4, column = 1, sticky = "nw", padx = 2)

    subtitle <- tclVar()
    put_label(sidebar, "Subtitle: ", 5, 1, sticky = "nw")
    subentry <- ttkentry(sidebar, textvariable = subtitle, state = "disabled")
    tkgrid(subentry, row = 6, column = 1, sticky = "nw", padx = 2)

    caption <- tclVar()
    put_label(sidebar, "Caption: ", 7, 1, sticky = "nw")
    capentry <- ttkentry(sidebar, textvariable = caption, state = "disabled")
    tkgrid(capentry, row = 8, column = 1, sticky = "nw", padx = 2)
    
    xlab <- tclVar()
    put_label(sidebar, "xlab: ", 9, 1, sticky = "nw")
    entry2 <- ttkentry(sidebar, textvariable = xlab, state = "disabled")
    tkgrid(entry2, row = 10, column = 1, sticky = "nw", padx = 2)

    ylab <- tclVar()
    put_label(sidebar, "ylab: ", 11, 1, sticky = "nw")
    entry3 <- ttkentry(sidebar, textvariable = ylab, state = "disabled")
    tkgrid(entry3, row = 12, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Theme: ", 13, 1, sticky = "nw")
    themex <- tclVar()
    theme_box <- ttkcombobox(sidebar, 
                        values = themes, 
                        textvariable = themex,
                        state = "normal",
                        justify = "left", state = "disabled")
    tkgrid(theme_box, row = 14, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Palette: ", 15, 1, sticky = "nw")
    palette <- tclVar()
    palette_box <- ttkcombobox(sidebar, 
                        values = palettes, 
                        textvariable = palette,
                        state = "normal",
                        justify = "left", state = "disabled")
    tkgrid(palette_box, row = 16, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Bar: ", 17, 1, sticky = "nw")
    color <- tkcanvas(sidebar, width = 40, height = 16, 
        background = "#000000",
        highlightbackground = "#ababab")
    tkgrid(color, row = 18, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Background: ", 19, 1, sticky = "nw")
    bcolor <- tkcanvas(sidebar, width = 40, height = 16, 
        background = "#000000",
        highlightbackground = "#ababab")
    tkgrid(bcolor, row = 20, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Text: ", 21, 1, sticky = "nw")
    tcolor <- tkcanvas(sidebar, width = 40, height = 16, 
        background = "#000000",
        highlightbackground = "#ababab")
    tkgrid(tcolor, row = 22, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Vector: ", 23, 1, sticky = "nw")
    vcolor <- tkcanvas(sidebar, width = 40, height = 16, 
        background = "#000000",
        highlightbackground = "#ababab")
    tkgrid(vcolor, row = 24, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Point: ", 25, 1, sticky = "nw")
    pcolor <- tkcanvas(sidebar, width = 40, height = 16, 
        background = "#000000",
        highlightbackground = "#ababab")
    tkgrid(pcolor, row = 26, column = 1, sticky = "nw", padx = 2)

    flip <- tclVar(FALSE)
    label_var <- tclVar("Flip graph")
    check_button <- ttkcheckbutton(sidebar, variable = flip, textvariable = label_var, state = "disabled", command = function() {
            .flip <<- if( tclvalue(flip) == "1" ) TRUE else FALSE
            graph$flip <<- .flip
            tkrreplot(eplot, fun = function() {
                plot(Plot(graph))
            }, hscale = hscale, vscale = vscale)
        })
    tkgrid(check_button, row = 27, column = 1, sticky = "nw", padx = 2)

    repel <- tclVar(FALSE)
    label_repel <- tclVar("Repel point text")
    repel_button <- ttkcheckbutton(sidebar, variable = repel, textvariable = label_repel, state = "disabled", command = function() {
            .repel <<- if( tclvalue(repel) == "1" ) TRUE else FALSE
            graph$repel <<- .repel
            tkrreplot(eplot, fun = function() {
                plot(Plot(graph))
            }, hscale = hscale, vscale = vscale)
        })
    tkgrid(repel_button, row = 28, column = 1, sticky = "nw", padx = 2)

    tkbind(color, "<Button-1>", function(W) {
            if(.color != "") {
                color <- tcl("tk_chooseColor", parent = W, title = "Set box color")
                .color <<- tclvalue(color)
                graph$color <<- .color
                if(nchar(.color)) {
                    tkconfigure(W, background = .color) 
                    tkrreplot(eplot, fun = function() {
                            plot(Plot(graph))
                        }, hscale = hscale, vscale = vscale)
                }
            }
        })

    tkbind(bcolor, "<Button-1>", function(W) {
            if(.background != "") {
                bcolor <- tcl("tk_chooseColor", parent = W, title = "Set box color")
                .background <<- tclvalue(bcolor)
                graph$background <<- .background
                if(nchar(.background)) {
                    tkconfigure(W, background = .background) 
                    tkrreplot(eplot, fun = function() {
                            plot(Plot(graph))
                        }, hscale = hscale, vscale = vscale)
                }
            }
        })

    tkbind(tcolor, "<Button-1>", function(W) {
            if(.tcolor != "") {
                tcolor <- tcl("tk_chooseColor", parent = W, title = "Set box color")
                .tcolor <<- tclvalue(tcolor)
                graph$tcolor <<- .tcolor
                if(nchar(.tcolor)) {
                    tkconfigure(W, background = .tcolor) 
                    tkrreplot(eplot, fun = function() {
                            plot(Plot(graph))
                        }, hscale = hscale, vscale = vscale)
                }
            }
        })

    tkbind(vcolor, "<Button-1>", function(W) {
            if(.vcolor != "") {
                vcolor <- tcl("tk_chooseColor", parent = W, title = "Set box color")
                .vcolor <<- tclvalue(vcolor)
                graph$vcolor <<- .vcolor
                if(nchar(.vcolor)) {
                    tkconfigure(W, background = .vcolor) 
                    tkrreplot(eplot, fun = function() {
                            plot(Plot(graph))
                        }, hscale = hscale, vscale = vscale)
                }
            }
        })

    tkbind(pcolor, "<Button-1>", function(W) {
            if(.pcolor != "") {
                pcolor <- tcl("tk_chooseColor", parent = W, title = "Set box color")
                .pcolor <<- tclvalue(pcolor)
                graph$pcolor <<- .pcolor
                if(nchar(.pcolor)) {
                    tkconfigure(W, background = .pcolor) 
                    tkrreplot(eplot, fun = function() {
                            plot(Plot(graph))
                        }, hscale = hscale, vscale = vscale)
                }
            }
        })

    tkbind(theme_box, "<<ComboboxSelected>>", function() {
            .theme <<- tclvalue(themex)
            graph$theme <<- .theme
            tkrreplot(eplot, fun = function() {
                plot(Plot(graph))
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(palette_box, "<<ComboboxSelected>>", function() {
            .palette <<- tclvalue(palette)
            graph$palette <<- .palette
            tkrreplot(eplot, fun = function() {
                plot(Plot(graph))
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(entry1, "<Return>", function() {
            .title <<- tclvalue(title)
            graph$title <<- .title
            tkrreplot(eplot, fun = function() {
                plot(Plot(graph))
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(entry2, "<Return>", function() {
            .xlab <<- tclvalue(xlab)
            graph$xlab <<- .xlab
            tkrreplot(eplot, fun = function() {
                plot(Plot(graph))
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(entry3, "<Return>", function() {
            .ylab <<- tclvalue(ylab)
            graph$ylab <<- .ylab
            tkrreplot(eplot, fun = function() {
                plot(Plot(graph))
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(subentry, "<Return>", function() {
            .subtitle <<- tclvalue(subtitle)
            graph$subtitle <<- .subtitle
            tkrreplot(eplot, fun = function() {
                plot(Plot(graph))
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(capentry, "<Return>", function() {
            .caption <<- tclvalue(caption)
            graph$caption <<- .caption
            tkrreplot(eplot, fun = function() {
                plot(Plot(graph))
            }, hscale = hscale, vscale = vscale)
        })

    if(.limit > 0)
        tkbind(limitbar, "<ButtonRelease-1>", function() {
                .limit <<- as.numeric(tclvalue(llimit))
                graph$limit <<- .limit
                tkrreplot(eplot, fun = function() {
                    plot(Plot(graph))
                }, hscale = hscale, vscale = vscale)
            })

    # Graph
    eplot <- tkrplot(frame, fun = function() {
            plot(Plot(graph))
        }, hscale = hscale, vscale = vscale)
    tkpack(eplot)

    l <- length(as.character(tcl(notebook,"tabs")))
    tcl(notebook, "select", l-1)

    # Save
    tkbind(page$save, "<ButtonRelease-1>", function() {
            graph$alpha <- 0.1
            ggsave(paste0(name, ".png"), plot = Plot(graph))
            tkmessageBox(title = "Save", message = "Plot", detail = "The graph was saved.", type = "ok")
        })

    # Zoom
    tkbind(page$zoom, "<ButtonRelease-1>", function() {
            graph$alpha <- 0.1
            plot(Plot(graph))
        })

    if(.title != "") tcl(entry1, "config", "-state", "normal")
    if(.xlab != "") tcl(entry2, "config", "-state", "normal")
    if(.ylab != "") tcl(entry3, "config", "-state", "normal")
    if(.theme != "") tcl(theme_box, "config", "-state", "normal")
    if(.palette != "") tcl(palette_box, "config", "-state", "normal")
    if(.flip != "") tcl(check_button, "config", "-state", "normal")
    if(.subtitle != "") tcl(subentry, "config", "-state", "normal")
    if(.caption != "") tcl(capentry, "config", "-state", "normal")
    if(.repel != "") tcl(repel_button, "config", "-state", "normal")
    if(.limit > 0) tcl(limitbar, "config", "-state", "normal")

    if(.color != "") tkconfigure(color, background = .color)
    if(.background != "") tkconfigure(bcolor, background = .background)
    if(.tcolor != "") tkconfigure(tcolor, background = .tcolor)
    if(.vcolor != "") tkconfigure(vcolor, background = .vcolor)
    if(.pcolor != "") tkconfigure(pcolor, background = .pcolor)
}