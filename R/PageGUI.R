PageGUI <- function(name, Plot, hscale = 1.5, vscale = 1.5, color = "", theme = "", title = "", xlab = "", ylab = "", flip = FALSE, palette = "") {
    themes <- c("theme_gray", "theme_bw", "theme_linedraw", "theme_light", "theme_dark", "theme_minimal", "theme_classic", "theme_void")
    palettes <- c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Paired", "Dark2", "Accent")

    .color <- color
    .theme <- theme
    .title <- title
    .xlab <- xlab
    .ylab <- ylab
    .flip <- flip
    .palette <- palette

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

    title <- tclVar()
    put_label(sidebar, "Title: ", 1, 1, sticky = "nw")
    entry1 <- ttkentry(sidebar, textvariable = title)
    tkgrid(entry1, row = 2, column = 1, sticky = "nw", padx = 2)

    xlab <- tclVar()
    put_label(sidebar, "xlab: ", 3, 1, sticky = "nw")
    entry2 <- ttkentry(sidebar, textvariable = xlab)
    tkgrid(entry2, row = 4, column = 1, sticky = "nw", padx = 2)

    ylab <- tclVar()
    put_label(sidebar, "ylab: ", 5, 1, sticky = "nw")
    entry3 <- ttkentry(sidebar, textvariable = ylab)
    tkgrid(entry3, row = 6, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Theme: ", 7, 1, sticky = "nw")
    themex <- tclVar()
    theme_box <- ttkcombobox(sidebar, 
                        values = themes, 
                        textvariable = themex,
                        state = "normal",
                        justify = "left")
    tkgrid(theme_box, row = 8, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Palette: ", 9, 1, sticky = "nw")
    palette <- tclVar()
    palette_box <- ttkcombobox(sidebar, 
                        values = palettes, 
                        textvariable = palette,
                        state = "normal",
                        justify = "left")
    tkgrid(palette_box, row = 10, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Barcolor: ", 11, 1, sticky = "nw")
    color <- tkcanvas(sidebar, width = 40, height = 16, 
        background = "#000000",
        highlightbackground = "#ababab")
    tkgrid(color, row = 12, column = 1, sticky = "nw", padx = 2)

    flip <- tclVar(FALSE)
    label_var <- tclVar("Flip graph")
    check_button <- ttkcheckbutton(sidebar, variable = flip, textvariable = label_var, command = function() {
            .flip <<- if( tclvalue(flip) == "1" ) TRUE else FALSE
            tkrreplot(eplot, fun = function() {
                plot(Plot(color = .color, theme = .theme, title = .title, xlab = .xlab, ylab = .ylab, flip = .flip, palette = .palette))
            }, hscale = hscale, vscale = vscale)
        })
    tkgrid(check_button, row = 13, column = 1, sticky = "nw", padx = 2)

    tkbind(color, "<Button-1>", function(W) {
            color <- tcl("tk_chooseColor", parent = W, title = "Set box color")
            .color <<- tclvalue(color)
            if(nchar(.color)) {
                tkconfigure(W, background = .color) 
                tkrreplot(eplot, fun = function() {
                        plot(Plot(color = .color, theme = .theme, title = .title, xlab = .xlab, ylab = .ylab, flip = .flip, palette = .palette))
                    }, hscale = hscale, vscale = vscale)
            }
        })

    tkbind(theme_box, "<<ComboboxSelected>>", function() {
            .theme <<- tclvalue(themex)
            tkrreplot(eplot, fun = function() {
                plot(Plot(color = .color, theme = .theme, title = .title, xlab = .xlab, ylab = .ylab, flip = .flip, palette = .palette))
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(palette_box, "<<ComboboxSelected>>", function() {
            .palette <<- tclvalue(palette)
            tkrreplot(eplot, fun = function() {
                plot(Plot(color = .color, theme = .theme, title = .title, xlab = .xlab, ylab = .ylab, flip = .flip, palette = .palette))
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(entry1, "<Return>", function() {
            .title <<- tclvalue(title)
            tkrreplot(eplot, fun = function() {
                plot(Plot(color = .color, theme = .theme, title = .title, xlab = .xlab, ylab = .ylab, flip = .flip, palette = .palette))
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(entry2, "<Return>", function() {
            .xlab <<- tclvalue(xlab)
            tkrreplot(eplot, fun = function() {
                plot(Plot(color = .color, theme = .theme, title = .title, xlab = .xlab, ylab = .ylab, flip = .flip, palette = .palette))
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(entry3, "<Return>", function() {
            .ylab <<- tclvalue(ylab)
            tkrreplot(eplot, fun = function() {
                plot(Plot(color = .color, theme = .theme, title = .title, xlab = .xlab, ylab = .ylab, flip = .flip, palette = .palette))
            }, hscale = hscale, vscale = vscale)
        })

    # Graph
    eplot <- tkrplot(frame, fun = function() {
            plot(Plot(color = .color, theme = .theme, title = .title, xlab = .xlab, ylab = .ylab, flip = .flip, palette = .palette))
        }, hscale = hscale, vscale = vscale)
    tkpack(eplot)

    l <- length(as.character(tcl(notebook,"tabs")))
    tcl(notebook, "select", l-1)

    # Save
    tkbind(page$save, "<ButtonRelease-1>", function() {
            ggsave(paste0(name, ".png"), plot = Plot(color = .color, theme = .theme, title = .title, xlab = .xlab, ylab = .ylab, flip = .flip, palette = .palette))
            tkmessageBox(title = "Save", message = "Plot", detail = "The graph was saved.", type = "ok")
        })

    # Zoom
    tkbind(page$zoom, "<ButtonRelease-1>", function() {
            plot(Plot(color = .color, theme = .theme, title = .title, xlab = .xlab, ylab = .ylab, flip = .flip, palette = .palette))
        })
}