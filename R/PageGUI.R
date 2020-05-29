PageGUI <- function(name, Plot, id = "", color = "", theme = "", title = "", type = "", envir = parent.frame(), time = "",
                    xlab = "", ylab = "", flip = "", palette = "", subtitle = "", caption = "", clustert = "", text_size = 0, dim = "",
                    background = "", text_color = "", vector_color = "", point_color = "", repel = "", limit = 0, vector_text = "", point_text = "",
                    vector_size = 0, point_size = 0, parent, notebook, to = 1, from = 10, resolution = 10, distances = "", cluster = 0) {
    
    resize <- function(parent, env) {
        geometry <- unlist(strsplit(unlist(strsplit(tclvalue(tkwm.geometry(parent)),"\\+"))[1],"x"))
        
        assign("hscale", (as.numeric(geometry[1])-240) / 480, envir = env)
        assign("vscale", (as.numeric(geometry[2])-85) / 480 , envir = env)

        if(as.character(tcl("tk", "windowingsystem")) == "win32") { 
            assign("hscale", hscale * 1.25, envir = env)
            assign("vscale", vscale * 1.25, envir = env)
        }

        return(geometry)
    }

    replot <- function() {
        if(id != "") assign(id, graph, envir = envir)
        tkrreplot(eplot, fun = function() {
            Plot(graph)
        }, hscale = hscale, vscale = vscale)
    }
                   
    themes <- c("theme_white","theme_dark2","theme_gray", "theme_bw", "theme_linedraw", "theme_light", "theme_dark", "theme_minimal", "theme_classic", "theme_void")
    #palettes <- c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Paired", "Dark2", "Accent")
    palettes <- rownames(brewer.pal.info)

    env = environment()
    resize(parent, env)
    assign("g", unlist(strsplit(unlist(strsplit(tclvalue(tkwm.geometry(parent)),"\\+"))[1],"x")), envir = env)

    graph <- list()
    graph$color <- color
    graph$theme <- theme
    graph$title <- title
    graph$xlab <- xlab
    graph$ylab <- ylab
    graph$flip <- flip
    graph$palette <- palette
    graph$subtitle <- subtitle
    graph$caption <- caption
    graph$background <- background
    graph$tcolor <- text_color
    graph$vcolor <- vector_color
    graph$pcolor <- point_color
    graph$repel <- repel
    graph$limit <- limit
    graph$alpha <- 1
    graph$vtext <- vector_text
    graph$ptext <- point_text
    graph$vsize <- vector_size
    graph$psize <- point_size
    graph$tsize <- text_size
    graph$cluster <- cluster
    graph$distance <- ""
    graph$clustert <- clustert
    graph$dim <- dim
    graph$time <- time
    class(graph) <- "graph"

    try(if(typeof(get(id, envir = envir)) != "closure") graph <- get(id, envir = envir), silent = TRUE)
    graph$dim <- if(is.null(graph$dim)) dim else graph$dim
    graph$limit <- if(graph$limit > to) to else graph$limit
    
    page <- Page(notebook, name)
    content <- page$content

    bar <- ttklabelframe(content, width = 200, text = "Options")
    frame <- tkframe(content)

    tkgrid(bar, row = 0, column = 0, sticky = "nsw", padx = 5, pady = 5)
    tkgrid(frame, row = 0, column = 1, sticky = "nsw", padx = 5, pady = 5)
    tkgrid.columnconfigure(content, 0, weight = 1)
    tkgrid.columnconfigure(content, 1, weight = 2)
    tkgrid.rowconfigure(content, 0, weight = 1)

    # Sidebar
    sidecanvas <- tkcanvas(bar, width = 180, borderwidth = 0, highlightthickness = 0)
    addScrollbars(bar, sidecanvas, horiz = FALSE)

    sidebar <- ttkframe(sidecanvas, padding = c(0,0,0,0))
    frame_id <- tkcreate(sidecanvas, "window", 0, 0, anchor = "nw", window = sidebar, width = 180)

    tkitemconfigure(sidecanvas, frame_id, width =  as.numeric(tkwinfo("width", sidebar)))
    tkbind(sidebar, "<Configure>", function() {  
            bbox <- tcl(sidecanvas, "bbox", "all")
            tcl(sidecanvas, "config", scrollregion = bbox)
        })
    tkbind(sidecanvas, "<Configure>", function(W) {
            width <- as.numeric(tkwinfo("width", W))
            frame_width <- as.numeric(tkwinfo("width", sidebar))
            if(frame_width < width)
                tkitemconfigure(sidecanvas, frame_id, width = width)
        })

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
    tkgrid.rowconfigure(sidebar, 29, weight = 0)
    tkgrid.rowconfigure(sidebar, 30, weight = 0)
    tkgrid.rowconfigure(sidebar, 31, weight = 0)
    tkgrid.rowconfigure(sidebar, 32, weight = 0)
    tkgrid.rowconfigure(sidebar, 33, weight = 0)
    tkgrid.rowconfigure(sidebar, 34, weight = 0)
    tkgrid.rowconfigure(sidebar, 35, weight = 0)
    tkgrid.rowconfigure(sidebar, 36, weight = 0)
    tkgrid.rowconfigure(sidebar, 37, weight = 0)
    tkgrid.rowconfigure(sidebar, 38, weight = 0)
    tkgrid.rowconfigure(sidebar, 39, weight = 0)
    tkgrid.rowconfigure(sidebar, 40, weight = 0)
    tkgrid.rowconfigure(sidebar, 41, weight = 0)
    tkgrid.rowconfigure(sidebar, 42, weight = 0)
    tkgrid.rowconfigure(sidebar, 43, weight = 0)
    tkgrid.rowconfigure(sidebar, 44, weight = 0)
    tkgrid.rowconfigure(sidebar, 45, weight = 0)

    limit <- tclVar(init = graph$limit)
    #put_label(sidebar, "Limit: ", 1, 1, sticky = "nw")
    limitbar <- tkscale(sidebar, from = from, to = to, variable = limit, 
        showvalue = TRUE, resolution = resolution, orient = "horiz", state = "disabled")
    tkgrid(limitbar, row = 2, column = 1, sticky = "ew", padx = 2)

    title <- tclVar(graph$title)
    put_label(sidebar, "Title: ", 3, 1, sticky = "nw")
    entry1 <- ttkentry(sidebar, textvariable = title, state = "disabled")
    tkgrid(entry1, row = 4, column = 1, sticky = "nw", padx = 2)

    subtitle <- tclVar(graph$subtitle)
    put_label(sidebar, "Subtitle: ", 5, 1, sticky = "nw")
    subentry <- ttkentry(sidebar, textvariable = subtitle, state = "disabled")
    tkgrid(subentry, row = 6, column = 1, sticky = "nw", padx = 2)

    caption <- tclVar(graph$caption)
    put_label(sidebar, "Caption: ", 7, 1, sticky = "nw")
    capentry <- ttkentry(sidebar, textvariable = caption, state = "disabled")
    tkgrid(capentry, row = 8, column = 1, sticky = "nw", padx = 2)
    
    xlab <- tclVar(graph$xlab)
    put_label(sidebar, "xlab: ", 9, 1, sticky = "nw")
    entry2 <- ttkentry(sidebar, textvariable = xlab, state = "disabled")
    tkgrid(entry2, row = 10, column = 1, sticky = "nw", padx = 2)

    ylab <- tclVar(graph$ylab)
    put_label(sidebar, "ylab: ", 11, 1, sticky = "nw")
    entry3 <- ttkentry(sidebar, textvariable = ylab, state = "disabled")
    tkgrid(entry3, row = 12, column = 1, sticky = "nw", padx = 2)

    themex <- tclVar(graph$theme)
    put_label(sidebar, "Theme: ", 13, 1, sticky = "nw")
    theme_box <- ttkcombobox(sidebar, 
                        values = themes, 
                        textvariable = themex,
                        state = "normal",
                        justify = "left", state = "disabled")
    tkgrid(theme_box, row = 14, column = 1, sticky = "nw", padx = 2)

    palette <- tclVar(graph$palette)
    put_label(sidebar, "Palette: ", 15, 1, sticky = "nw")
    palette_box <- ttkcombobox(sidebar, 
                        values = palettes, 
                        textvariable = palette,
                        state = "normal",
                        justify = "left", state = "disabled")
    tkgrid(palette_box, row = 16, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Bar: ", 17, 1, sticky = "nw")
    color <- tkcanvas(sidebar, width = 40, height = 16, 
        background = "#d3d3d3",
        highlightbackground = "#ababab")
    tkgrid(color, row = 18, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Background: ", 19, 1, sticky = "nw")
    bcolor <- tkcanvas(sidebar, width = 40, height = 16, 
        background = "#d3d3d3",
        highlightbackground = "#ababab")
    tkgrid(bcolor, row = 20, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Text: ", 21, 1, sticky = "nw")
    tcolor <- tkcanvas(sidebar, width = 40, height = 16, 
        background = "#d3d3d3",
        highlightbackground = "#ababab")
    tkgrid(tcolor, row = 22, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Vector/Link: ", 23, 1, sticky = "nw")
    vcolor <- tkcanvas(sidebar, width = 40, height = 16, 
        background = "#d3d3d3",
        highlightbackground = "#ababab")
    tkgrid(vcolor, row = 24, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Point: ", 25, 1, sticky = "nw")
    pcolor <- tkcanvas(sidebar, width = 40, height = 16, 
        background = "#d3d3d3",
        highlightbackground = "#ababab")
    tkgrid(pcolor, row = 26, column = 1, sticky = "nw", padx = 2)

    flip <- tclVar(graph$flip)
    ft <- tclVar("Flip graph")
    check_button <- ttkcheckbutton(sidebar, variable = flip, textvariable = ft, state = "disabled", command = function() {
            graph$flip <<- if( tclvalue(flip) == "1" ) TRUE else FALSE
            replot()
        })
    tkgrid(check_button, row = 27, column = 1, sticky = "nw", padx = 2)

    time <- tclVar(graph$time)
    tm <- tclVar("Time series")
    time_button <- ttkcheckbutton(sidebar, variable = time, textvariable = tm, state = "disabled", command = function() {
            graph$time <<- if( tclvalue(time) == "1" ) TRUE else FALSE
            replot()
        })
    tkgrid(time_button, row = 28, column = 1, sticky = "nw", padx = 2)

    repel <- tclVar(graph$repel)
    rt <- tclVar("Repel point text")
    repel_button <- ttkcheckbutton(sidebar, variable = repel, textvariable = rt, state = "disabled", command = function() {
            graph$repel <<- if( tclvalue(repel) == "1" ) TRUE else FALSE
            replot()
        })
    tkgrid(repel_button, row = 29, column = 1, sticky = "nw", padx = 2)

    vtext <- tclVar(graph$vtext)
    tt <- tclVar("Show vector text")
    text_button <- ttkcheckbutton(sidebar, variable = vtext, textvariable = tt, state = "disabled", command = function() {
            graph$vtext <<- if( tclvalue(vtext) == "1" ) TRUE else FALSE
            replot()
        })
    tkgrid(text_button, row = 30, column = 1, sticky = "nw", padx = 2)

    ptext <- tclVar(graph$ptext)
    vt <- tclVar("Show point text")
    vector_button <- ttkcheckbutton(sidebar, variable = ptext, textvariable = vt, state = "disabled", command = function() {
            graph$ptext <<- if( tclvalue(ptext) == "1" ) TRUE else FALSE
            replot()
        })
    tkgrid(vector_button, row = 31, column = 1, sticky = "nw", padx = 2)

    vector_size <- tclVar(init = graph$vsize)
    put_label(sidebar, "Vector size: ", 32, 1, sticky = "nw")
    vectorsize <- tkscale(sidebar, from = 1, to = 5, variable = vector_size, 
        showvalue = TRUE, resolution = 0.5, orient = "horiz", state = "disabled")
    tkgrid(vectorsize, row = 33, column = 1, sticky = "ew", padx = 2)

    point_size <- tclVar(init = graph$psize)
    put_label(sidebar, "Point size: ", 34, 1, sticky = "nw")
    pointsize <- tkscale(sidebar, from = 1, to = 5, variable = point_size, 
        showvalue = TRUE, resolution = 0.5, orient = "horiz", state = "disabled")
    tkgrid(pointsize, row = 35, column = 1, sticky = "ew", padx = 2)

    text_size <- tclVar(init = graph$tsize)
    put_label(sidebar, "Text size: ", 36, 1, sticky = "nw")
    textsize <- tkscale(sidebar, from = 1, to = 48, variable = text_size, 
        showvalue = TRUE, resolution = 1, orient = "horiz", state = "disabled")
    tkgrid(textsize, row = 37, column = 1, sticky = "ew", padx = 2)

    distance <- tclVar(graph$distance)
    put_label(sidebar, "Projection: ", 38, 1, sticky = "nw")
    distance_box <- ttkcombobox(sidebar, 
                        values = distances, 
                        textvariable = distance,
                        state = "normal",
                        justify = "left", state = "disabled")
    tkgrid(distance_box, row = 39, column = 1, sticky = "nw", padx = 2)

    dim <- tclVar(graph$dim)
    put_label(sidebar, "Contributions: ", 40, 1, sticky = "nw")
    radio_box <- ttkcombobox(sidebar, 
                        values = c("all", "dim1", "dim2"), 
                        textvariable = dim,
                        state = "normal",
                        justify = "left", state = "disabled")
    tkgrid(radio_box, row = 41, column = 1, sticky = "nw", padx = 2)

    cluster <- tclVar(graph$cluster)
    put_label(sidebar, "Cluster: ", 42, 1, sticky = "nw")
    cluster_box <- tkscale(sidebar, from = 1, to = 8, variable = cluster, 
        showvalue = TRUE, resolution = 1, orient = "horiz", state = "disabled")
    tkgrid(cluster_box, row = 43, column = 1, sticky = "ew", padx = 2)

    cluster_type <- tclVar(graph$clustert)
    put_label(sidebar, "Cluster type: ", 44, 1, sticky = "nw")
    clustert_box <- ttkcombobox(sidebar, 
                        values = c("rectangle", "phylogram", "cladogram", "unrooted", "fan", "radial", "cladogram"), 
                        textvariable = cluster_type,
                        state = "normal",
                        justify = "left", state = "disabled")
    tkgrid(clustert_box, row = 45, column = 1, sticky = "nw", padx = 2)

    tkbind(color, "<Button-1>", function(W) {
            if(graph$color != "") {
                color <- tcl("tk_chooseColor", parent = W, title = "Set box color")
                graph$color <<- tclvalue(color)
                if(nchar(tclvalue(color))) {
                    tkconfigure(W, background = tclvalue(color)) 
                    replot()
                }
            }
        })

    tkbind(bcolor, "<Button-1>", function(W) {
            if(graph$background != "") {
                bcolor <- tcl("tk_chooseColor", parent = W, title = "Set box color")
                graph$background <<- tclvalue(bcolor)
                if(nchar(tclvalue(bcolor))) {
                    tkconfigure(W, background = tclvalue(bcolor)) 
                    replot()
                }
            }
        })

    tkbind(tcolor, "<Button-1>", function(W) {
            if(graph$tcolor != "") {
                tcolor <- tcl("tk_chooseColor", parent = W, title = "Set box color")
                graph$tcolor <<- tclvalue(tcolor)
                if(nchar(tclvalue(tcolor))) {
                    tkconfigure(W, background = tclvalue(tcolor)) 
                    replot()
                }
            }
        })

    tkbind(vcolor, "<Button-1>", function(W) {
            if(graph$vcolor != "") {
                vcolor <- tcl("tk_chooseColor", parent = W, title = "Set box color")
                graph$vcolor <<- tclvalue(vcolor)
                if(nchar(tclvalue(vcolor))) {
                    tkconfigure(W, background = tclvalue(vcolor)) 
                    replot()
                }
            }
        })

    tkbind(pcolor, "<Button-1>", function(W) {
            if(graph$pcolor != "") {
                pcolor <- tcl("tk_chooseColor", parent = W, title = "Set box color")
                graph$pcolor <<- tclvalue(pcolor)
                if(nchar(tclvalue(pcolor))) {
                    tkconfigure(W, background = tclvalue(pcolor)) 
                    replot()
                }
            }
        })

    tkbind(theme_box, "<<ComboboxSelected>>", function() {
            graph$theme <<- tclvalue(themex)
            replot()
        })

    tkbind(palette_box, "<<ComboboxSelected>>", function() {
            graph$palette <<- tclvalue(palette)
            replot()
        })

    tkbind(distance_box, "<<ComboboxSelected>>", function() {
            graph$distance <<- tclvalue(distance)
            replot()
        })

    tkbind(radio_box, "<<ComboboxSelected>>", function() {
            graph$dim <<- tclvalue(dim)
            replot()
        })

    if(as.numeric(graph$cluster) > 0)
        tkbind(cluster_box, "<ButtonRelease-1>", function() {
                graph$cluster <<- as.numeric(tclvalue(cluster))
                replot()
            })

    tkbind(clustert_box, "<<ComboboxSelected>>", function() {
            graph$clustert <<- tclvalue(cluster_type)
            replot()
        })

    tkbind(entry1, "<Return>", function() {
            graph$title <<- tclvalue(title) 
            replot()
        })

    tkbind(entry2, "<Return>", function() {
            graph$xlab <<- tclvalue(xlab)
            replot()
        })

    tkbind(entry3, "<Return>", function() {
            graph$ylab <<- tclvalue(ylab)
            replot()
        })

    tkbind(subentry, "<Return>", function() {
            graph$subtitle <<- tclvalue(subtitle)
            replot()
        })

    tkbind(capentry, "<Return>", function() {
            graph$caption <<- tclvalue(caption)
            replot()
        })

    if(as.numeric(graph$limit) > 0)
        tkbind(limitbar, "<ButtonRelease-1>", function() {
                graph$limit <<- as.numeric(tclvalue(limit))
                replot()
            })

    if(as.numeric(graph$vsize) > 0)
        tkbind(vectorsize, "<ButtonRelease-1>", function() {
                graph$vsize <<- as.numeric(tclvalue(vector_size))
                replot()
            })

    if(as.numeric(graph$psize) > 0)
        tkbind(pointsize, "<ButtonRelease-1>", function() {
                graph$psize <<- as.numeric(tclvalue(point_size))
                replot()
            })

    if(as.numeric(graph$tsize) > 0)
        tkbind(textsize, "<ButtonRelease-1>", function() {
                graph$tsize <<- as.numeric(tclvalue(text_size))
                replot()
            })

    # Graph
    eplot <- tkrplot(frame, fun = function() {
            Plot(graph)
        }, hscale = hscale, vscale = vscale)
    tkpack(eplot)

    if(id != "") assign(id, graph, envir = envir)

    l <- length(as.character(tcl(notebook,"tabs")))
    tcl(notebook, "select", l-1)

    # Save
    tkbind(page$save, "<ButtonRelease-1>", function() {
            graph$alpha <<- 0.1
            Save(X = Plot(graph), name = name, type = type)
            graph$alpha <<- 1
        })

    # Zoom
    tkbind(page$zoom, "<ButtonRelease-1>", function() {
            graph$alpha <<- 0.1
            Plot(graph)
            graph$alpha <<- 1
        })
 
    tkbind(content, "<Configure>", function() {
            geometry <- resize(parent, env)
            if(paste0(geometry, collapse = "") != paste0(get("g", envir = env), collapse = "")) {
                assign("g", geometry, envir = env)
                tkrreplot(eplot, fun = function() {
                    graph$reload <- TRUE
                    Plot(graph)
                }, hscale = hscale, vscale = vscale)
            }
        })

    if(graph$title != "") tcl(entry1, "config", "-state", "normal")
    if(graph$xlab != "") tcl(entry2, "config", "-state", "normal")
    if(graph$ylab != "") tcl(entry3, "config", "-state", "normal")
    if(graph$theme != "") tcl(theme_box, "config", "-state", "normal")
    if(graph$palette != "") tcl(palette_box, "config", "-state", "normal")
    if(graph$flip != "") tcl(check_button, "config", "-state", "normal")
    if(graph$subtitle != "") tcl(subentry, "config", "-state", "normal")
    if(graph$caption != "") tcl(capentry, "config", "-state", "normal")
    if(graph$repel != "") tcl(repel_button, "config", "-state", "normal")
    if(graph$limit > 0) tcl(limitbar, "config", "-state", "normal")
    if(graph$ptext != "") tcl(text_button, "config", "-state", "normal")
    if(graph$vtext != "") tcl(vector_button, "config", "-state", "normal")
    if(graph$vsize > 0) tcl(vectorsize, "config", "-state", "normal")
    if(graph$psize > 0) tcl(pointsize, "config", "-state", "normal")
    if(graph$tsize > 0) tcl(textsize, "config", "-state", "normal")
    if(graph$dim != "") tcl(radio_box, "config", "-state", "normal")
    if(graph$time != "") tcl(time_button, "config", "-state", "normal")

    if(graph$color != "") tkconfigure(color, background = graph$color )
    if(graph$background  != "") tkconfigure(bcolor, background = graph$background)
    if(graph$tcolor != "") tkconfigure(tcolor, background = graph$tcolor)
    if(graph$vcolor != "") tkconfigure(vcolor, background = graph$vcolor)
    if(graph$pcolor != "") tkconfigure(pcolor, background = graph$pcolor)

    if(graph$distance != "") tcl(distance_box, "config", "-state", "normal")
    if(graph$cluster > 0) tcl(cluster_box, "config", "-state", "normal")
    if(graph$clustert != "" && require(ape)) tcl(clustert_box, "config", "-state", "normal")
}