## Analysis
Explorer <- function() {
    Plot <- function(color = "#000000", theme = "theme_classic", title = "Words by groups", xlab = "Groups", ylab = "Counts") {
        t <- match.fun(theme)
        p <- tm$token %>% ggplot(aes(x = GROUP)) + geom_bar(fill = color) + labs(title = title) + xlab(xlab) + ylab(ylab)
        p <- p + theme(text = element_text(size = 12)) + t()
        return(p)
    }

    themes <- c("theme_gray", "theme_bw", "theme_linedraw", "theme_light", "theme_dark", "theme_minimal", "theme_classic", "theme_void")

    .color <<- "#000000"
    .theme <<- "theme_classic"
    .title <<- "Words by groups"
    .xlab <<- "Groups"
    .ylab <<- "Counts"

    console_chunk("tm")
    content <- Page(notebook, "Explorer")
    
    sidebar <- ttklabelframe(content, width = 200, text = "Options")
    frame <- tkframe(content)

    tkgrid(sidebar, row = 0, column = 0, sticky = "nsw", padx = 5, pady = 5)
    tkgrid(frame, row = 0, column = 1, sticky = "nsw", padx = 5, pady = 5)
    tkgrid.columnconfigure(content, 0, weight = 1)
    tkgrid.columnconfigure(content, 1, weight = 1)
    tkgrid.rowconfigure(content, 0, weight = 1)

    # Sidebar
    tkgrid.columnconfigure(sidebar, 0, weight = 1)
    tkgrid.rowconfigure(sidebar, 0, weight = 0)
    tkgrid.rowconfigure(sidebar, 1, weight = 0)
    tkgrid.rowconfigure(sidebar, 2, weight = 0)
    tkgrid.rowconfigure(sidebar, 3, weight = 0)
    tkgrid.rowconfigure(sidebar, 4, weight = 0)

    put_label(sidebar, "Theme: ", 1, 1, sticky = "nw")
    themex <- tclVar()
    theme_box <- ttkcombobox(sidebar, 
                        values = themes, 
                        textvariable = themex,
                        state = "normal",
                        justify = "left")
    tkgrid(theme_box, row = 2, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Barcolor: ", 3, 1, sticky = "nw")
    color <- tkcanvas(sidebar, width = 40, height = 16, 
        background = "#000000",
        highlightbackground = "#ababab")
    tkgrid(color, row = 4, column = 1, sticky = "nw", padx = 2)

    tkbind(color, "<Button -1>", function(W) {
            color <- tcl("tk_chooseColor", parent = W, title = "Set box color")
            .color <<- tclvalue(color)
            if(nchar(.color)) {
                tkconfigure(W, background = .color) 
                tkrreplot(eplot, fun = function() {
                        plot(Plot(.color, .theme, .title, .xlab, .ylab))
                    }, hscale = 1.5, vscale = 1.5)
            }
        })

    tkbind(theme_box, "<<ComboboxSelected>>", function() {
            .theme <<- tclvalue(themex)
            tkrreplot(eplot, fun = function() {
                plot(Plot(.color, .theme, .title, .xlab, .ylab))
            }, hscale = 1.5, vscale = 1.5)
        })

    # Graph
    eplot <- tkrplot(frame, fun = function() {
            plot(Plot())
        }, hscale = 1.5, vscale = 1.5)
    tkpack(eplot)
    # tm$token %>% ggplot(aes(x = GROUP)) + geom_bar() + labs(title = "Distinct words by groups")

    l <- length(as.character(tcl(notebook,"tabs")))
    tcl(notebook, "select", l-1)
}