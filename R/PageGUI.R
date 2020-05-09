PageGUI <- function(name, Plot, color = "", theme = "", title = "", type = "",
                    xlab = "", ylab = "", flip = "", palette = "", subtitle = "", caption = "",
                    background = "", text_color = "", vector_color = "", point_color = "", repel = "", limit = 0, vector_text = "", point_text = "",
                    vector_size = 0, point_size = 0, parent, notebook, to = 1, from = 10, resolution = 10, distances = "", cluster = 0, map = "") {
    
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

    draw <- function() {
        if(type == "hclust") 
            plot(Plot(graph), sub = "", xlab = "", main = graph$title)
        else
            plot(Plot(graph))
    }
                        
    themes <- c("theme_white","theme_gray", "theme_bw", "theme_linedraw", "theme_light", "theme_dark", "theme_minimal", "theme_classic", "theme_void")
    palettes <- c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Paired", "Dark2", "Accent")

    env = environment()
    resize(parent, env)
    assign("g", unlist(strsplit(unlist(strsplit(tclvalue(tkwm.geometry(parent)),"\\+"))[1],"x")), env)

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
    graph$cluster <- cluster
    graph$distance <- ""
    graph$map <- map
    class(graph) <- "graph"

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

    limit <- tclVar(init = limit)
    put_label(sidebar, "Limit: ", 1, 1, sticky = "nw")
    limitbar <- tkscale(sidebar, from = from, to = to, variable = limit, 
        showvalue = TRUE, resolution = resolution, orient = "horiz", state = "disabled")
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

    put_label(sidebar, "Vector: ", 23, 1, sticky = "nw")
    vcolor <- tkcanvas(sidebar, width = 40, height = 16, 
        background = "#d3d3d3",
        highlightbackground = "#ababab")
    tkgrid(vcolor, row = 24, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Point: ", 25, 1, sticky = "nw")
    pcolor <- tkcanvas(sidebar, width = 40, height = 16, 
        background = "#d3d3d3",
        highlightbackground = "#ababab")
    tkgrid(pcolor, row = 26, column = 1, sticky = "nw", padx = 2)

    flip <- tclVar(FALSE)
    ft <- tclVar("Flip graph")
    check_button <- ttkcheckbutton(sidebar, variable = flip, textvariable = ft, state = "disabled", command = function() {
            graph$flip <<- if( tclvalue(flip) == "1" ) TRUE else FALSE
            tkrreplot(eplot, fun = function() {
                draw()
            }, hscale = hscale, vscale = vscale)
        })
    tkgrid(check_button, row = 27, column = 1, sticky = "nw", padx = 2)

    repel <- tclVar(FALSE)
    rt <- tclVar("Repel point text")
    repel_button <- ttkcheckbutton(sidebar, variable = repel, textvariable = rt, state = "disabled", command = function() {
            graph$repel <<- if( tclvalue(repel) == "1" ) TRUE else FALSE
            tkrreplot(eplot, fun = function() {
                draw()
            }, hscale = hscale, vscale = vscale)
        })
    tkgrid(repel_button, row = 28, column = 1, sticky = "nw", padx = 2)

    vtext <- tclVar(FALSE)
    tt <- tclVar("Show vector text")
    text_button <- ttkcheckbutton(sidebar, variable = vtext, textvariable = tt, state = "disabled", command = function() {
            graph$vtext <<- if( tclvalue(vtext) == "1" ) TRUE else FALSE
            tkrreplot(eplot, fun = function() {
                draw()
            }, hscale = hscale, vscale = vscale)
        })
    tkgrid(text_button, row = 29, column = 1, sticky = "nw", padx = 2)

    ptext <- tclVar(FALSE)
    vt <- tclVar("Show point text")
    vector_button <- ttkcheckbutton(sidebar, variable = ptext, textvariable = vt, state = "disabled", command = function() {
            graph$ptext <<- if( tclvalue(ptext) == "1" ) TRUE else FALSE
            tkrreplot(eplot, fun = function() {
                draw()
            }, hscale = hscale, vscale = vscale)
        })
    tkgrid(vector_button, row = 30, column = 1, sticky = "nw", padx = 2)

    vector_size <- tclVar(init = vector_size)
    put_label(sidebar, "Vector size: ", 31, 1, sticky = "nw")
    vectorsize <- tkscale(sidebar, from = 1, to = 5, variable = vector_size, 
        showvalue = TRUE, resolution = 0.5, orient = "horiz", state = "disabled")
    tkgrid(vectorsize, row = 32, column = 1, sticky = "ew", padx = 2)

    point_size <- tclVar(init = point_size)
    put_label(sidebar, "Point size: ", 33, 1, sticky = "nw")
    pointsize <- tkscale(sidebar, from = 1, to = 5, variable = point_size, 
        showvalue = TRUE, resolution = 0.5, orient = "horiz", state = "disabled")
    tkgrid(pointsize, row = 34, column = 1, sticky = "ew", padx = 2)

    put_label(sidebar, "Projection: ", 35, 1, sticky = "nw")
    distance <- tclVar()
    distance_box <- ttkcombobox(sidebar, 
                        values = distances, 
                        textvariable = distance,
                        state = "normal",
                        justify = "left", state = "disabled")
    tkgrid(distance_box, row = 36, column = 1, sticky = "nw", padx = 2)

    put_label(sidebar, "Cluster: ", 37, 1, sticky = "nw")
    cluster <- tclVar()
    cluster_box <- tkscale(sidebar, from = 1, to = 10, variable = cluster, 
        showvalue = TRUE, resolution = 1, orient = "horiz", state = "disabled")
    tkgrid(cluster_box, row = 38, column = 1, sticky = "ew", padx = 2)

    put_label(sidebar, "Map: ", 39, 1, sticky = "nw")
    map <- tclVar()
    map_box <- ttkcombobox(sidebar, 
                        values = c("symbiplot", "rowprincipal", "colprincipal"), 
                        textvariable = map,
                        state = "normal",
                        justify = "left", state = "disabled")
    tkgrid(map_box, row = 40, column = 1, sticky = "nw", padx = 2)

    tkbind(color, "<Button-1>", function(W) {
            if(graph$color != "") {
                color <- tcl("tk_chooseColor", parent = W, title = "Set box color")
                graph$color <<- tclvalue(color)
                if(nchar(tclvalue(color))) {
                    tkconfigure(W, background = tclvalue(color)) 
                    tkrreplot(eplot, fun = function() {
                            draw()
                        }, hscale = hscale, vscale = vscale)
                }
            }
        })

    tkbind(bcolor, "<Button-1>", function(W) {
            if(graph$background != "") {
                bcolor <- tcl("tk_chooseColor", parent = W, title = "Set box color")
                graph$background <<- tclvalue(bcolor)
                if(nchar(tclvalue(bcolor))) {
                    tkconfigure(W, background = tclvalue(bcolor)) 
                    tkrreplot(eplot, fun = function() {
                            draw()
                        }, hscale = hscale, vscale = vscale)
                }
            }
        })

    tkbind(tcolor, "<Button-1>", function(W) {
            if(graph$tcolor != "") {
                tcolor <- tcl("tk_chooseColor", parent = W, title = "Set box color")
                graph$tcolor <<- tclvalue(tcolor)
                if(nchar(tclvalue(tcolor))) {
                    tkconfigure(W, background = tclvalue(tcolor)) 
                    tkrreplot(eplot, fun = function() {
                            draw()
                        }, hscale = hscale, vscale = vscale)
                }
            }
        })

    tkbind(vcolor, "<Button-1>", function(W) {
            if(graph$vcolor != "") {
                vcolor <- tcl("tk_chooseColor", parent = W, title = "Set box color")
                graph$vcolor <<- tclvalue(vcolor)
                if(nchar(tclvalue(vcolor))) {
                    tkconfigure(W, background = tclvalue(vcolor)) 
                    tkrreplot(eplot, fun = function() {
                            draw()
                        }, hscale = hscale, vscale = vscale)
                }
            }
        })

    tkbind(pcolor, "<Button-1>", function(W) {
            if(graph$pcolor != "") {
                pcolor <- tcl("tk_chooseColor", parent = W, title = "Set box color")
                graph$pcolor <<- tclvalue(pcolor)
                if(nchar(tclvalue(pcolor))) {
                    tkconfigure(W, background = tclvalue(pcolor)) 
                    tkrreplot(eplot, fun = function() {
                            draw()
                        }, hscale = hscale, vscale = vscale)
                }
            }
        })

    tkbind(theme_box, "<<ComboboxSelected>>", function() {
            graph$theme <<- tclvalue(themex)
            tkrreplot(eplot, fun = function() {
                draw()
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(palette_box, "<<ComboboxSelected>>", function() {
            graph$palette <<- tclvalue(palette)
            tkrreplot(eplot, fun = function() {
                draw()
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(distance_box, "<<ComboboxSelected>>", function() {
            graph$distance <<- tclvalue(distance)
            tkrreplot(eplot, fun = function() {
                draw()
            }, hscale = hscale, vscale = vscale)
        })

    if(as.numeric(graph$cluster) > 0)
        tkbind(cluster_box, "<ButtonRelease-1>", function() {
                graph$cluster <<- as.numeric(tclvalue(cluster))
                tkrreplot(eplot, fun = function() {
                    draw()
                }, hscale = hscale, vscale = vscale)
            })

    tkbind(map_box, "<<ComboboxSelected>>", function() {
            graph$map <<- tclvalue(map)
            tkrreplot(eplot, fun = function() {
                draw()
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(entry1, "<Return>", function() {
            graph$title <<- tclvalue(title) 
            tkrreplot(eplot, fun = function() {
                draw()
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(entry2, "<Return>", function() {
            graph$xlab <<- tclvalue(xlab)
            tkrreplot(eplot, fun = function() {
                draw()
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(entry3, "<Return>", function() {
            graph$ylab <<- tclvalue(ylab)
            tkrreplot(eplot, fun = function() {
                draw()
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(subentry, "<Return>", function() {
            graph$subtitle <<- tclvalue(subtitle)
            tkrreplot(eplot, fun = function() {
                draw()
            }, hscale = hscale, vscale = vscale)
        })

    tkbind(capentry, "<Return>", function() {
            graph$caption <<- tclvalue(caption)
            tkrreplot(eplot, fun = function() {
                draw()
            }, hscale = hscale, vscale = vscale)
        })

    if(as.numeric(graph$limit) > 0)
        tkbind(limitbar, "<ButtonRelease-1>", function() {
                graph$limit <<- as.numeric(tclvalue(limit))
                tkrreplot(eplot, fun = function() {
                    draw()
                }, hscale = hscale, vscale = vscale)
            })

    if(as.numeric(graph$vsize) > 0)
        tkbind(vectorsize, "<ButtonRelease-1>", function() {
                graph$vsize <<- as.numeric(tclvalue(vector_size))
                tkrreplot(eplot, fun = function() {
                    draw()
                }, hscale = hscale, vscale = vscale)
            })

    if(as.numeric(graph$psize) > 0)
        tkbind(pointsize, "<ButtonRelease-1>", function() {
                graph$psize <<- as.numeric(tclvalue(point_size))
                tkrreplot(eplot, fun = function() {
                    draw()
                }, hscale = hscale, vscale = vscale)
            })

    # Graph
    eplot <- tkrplot(frame, fun = function() {
            draw()
        }, hscale = hscale, vscale = vscale)
    tkpack(eplot)

    l <- length(as.character(tcl(notebook,"tabs")))
    tcl(notebook, "select", l-1)

    # Save
    tkbind(page$save, "<ButtonRelease-1>", function() {
            if(type == "hclust") {
                png(file = paste0(name, ".png"), width = 1920, height = 1080)
                plot(Plot(graph), sub = "", xlab = "", main = graph$title)
                dev.off()
            }else{
                graph$alpha <<- 0.1
                ggsave(paste0(name, ".png"), plot = Plot(graph))
                graph$alpha <<- 1
            }
            tkmessageBox(title = "Save", message = "Plot", detail = "The graph was saved.", type = "ok")
        })

    # Zoom
    tkbind(page$zoom, "<ButtonRelease-1>", function() {
            graph$alpha <<- 0.1
            draw()
            graph$alpha <<- 1
        })
 
    tkbind(content, "<Configure>", function() {
            geometry <- resize(parent, env)
            if(geometry != get("g", envir = env)) {
                assign("g", geometry, envir = env)
                tkrreplot(eplot, fun = function() {
                    draw()
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

    if(graph$color != "") tkconfigure(color, background = graph$color )
    if(graph$background  != "") tkconfigure(bcolor, background = graph$background)
    if(graph$tcolor != "") tkconfigure(tcolor, background = graph$tcolor)
    if(graph$vcolor != "") tkconfigure(vcolor, background = graph$vcolor)
    if(graph$pcolor != "") tkconfigure(pcolor, background = graph$pcolor)

    if(distances != "") tcl(distance_box, "config", "-state", "normal")
    if(graph$cluster > 0) tcl(cluster_box, "config", "-state", "normal")
    if(graph$map != "") tcl(map_box, "config", "-state", "normal")
}