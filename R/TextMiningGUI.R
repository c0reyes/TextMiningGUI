TextMiningGUI <- function() {
    # Scrollbars
    addScrollbars <<- function(parent, widget) {
        xscr <- ttkscrollbar(parent, orient = "horizontal",
                           command = function(...) tkxview(widget, ...))
        yscr <- ttkscrollbar(parent, orient = "vertical",
                           command = function(...) tkyview(widget, ...))
        tkconfigure(widget,
                  xscrollcommand = function(...) tkset(xscr,...),
                  yscrollcommand = function(...) tkset(yscr,...))
        tkgrid(widget, row = 0, column = 0, sticky = "news")
        tkgrid(yscr, row = 0, column = 1, sticky = "ns")
        tkgrid(xscr, row = 1, column = 0, sticky = "ew")
        tkgrid.columnconfigure(parent, 0, weight = 1)
        tkgrid.rowconfigure(parent, 0, weight = 1)
    }

    # Labels
    put_label <<- function(parent, text, row, column, sticky = "e") {
        label <- ttklabel(parent, text = text)
        tkgrid(label, row = row, column = column, sticky = sticky)
    }

    # Console
    console <- function() {
        if(!exists("windowc") || is.null(windowc)) {
            windowc <<- tktoplevel()
            tkwm.minsize(windowc, "600", "200")
            tkwm.title(windowc, "Console")
            tkwm.protocol(windowc, "WM_DELETE_WINDOW", function() {
                    tkdestroy(windowc)
                    windowc <<- NULL
                    txt <<- NULL
                })
            frame <- ttkframe(windowc, padding = c(3,3,12,12))
            frame_2 <- ttkframe(windowc)
            tkpack(frame, expand = TRUE, fill = "both")
            tkpack(frame_2, side = "right")

            txt <<- tktext(frame, width = 80, height = 24, wrap = "none", font = "courier")
            addScrollbars(frame, txt)

            tktag.configure(txt, "commandTag", foreground = "blue", font = "courier 12 italic")
            tktag.configure(txt, "outputTag", font = "courier 12")
            tktag.configure(txt, "errorTag", foreground = "red", font = "courier 12 bold")

            button_frame <- ttkframe(frame_2)
            close_button <- ttkbutton(button_frame, text = "close",
                command = function() { 
                    tkdestroy(windowc) 
                    windowc <<- NULL
                    txt <<- NULL
                })
            tkpack(button_frame, fill = "x", padx = 5, pady = 5)             
            tkpack(close_button, side = "right", padx = 0)             
        }
    }

    console_chunk <<- function(cmds) {
        if(exists("txt") && !is.null(txt)) {
            tkinsert(txt, "end", "\n")
            tkinsert(txt, "end", "\n")

            cmd_chunks <- try(parse(text = cmds), silent = TRUE)

            for(cmd in cmd_chunks) {
                cutoff <- 0.75 * getOption("width")
                dcmd <- deparse(cmd, width.cutoff = cutoff)
                command <- paste(getOption("prompt"),
                            paste(dcmd, collapse = paste("\n", 
                            getOption("continue"), sep = "")),
                            sep = "", collapse = "")
                tkinsert(txt, "end", command, "commandTag")
                tkinsert(txt, "end","\n")
                
                output <- capture.output(eval(cmd, envir = .GlobalEnv))
                output <- iconv(output, to = "ASCII//TRANSLIT")
                output <- paste(output, collapse = "\n")
                tkinsert(txt, "end", output, "outputTag")
                tkinsert(txt, "end","\n")
            }
        }
    }

    # Configure
    Configure <- function() {             
        window <- tktoplevel(width = 350, height = 200)
        tkwm.minsize(window, "350", "200")
        tkwm.maxsize(window, "350", "200")
        tkwm.title(window, "Configure")

        frame <- ttkframe(window, padding = c(3,3,12,12))
        tkpack(frame, expand = TRUE, fill = "both")

        label_frame <- ttklabelframe(frame, text = "Configure", padding = 10)
        tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)

        tkgrid.columnconfigure(label_frame, 0, weight = 1)
        tkgrid.columnconfigure(label_frame, 1, weight = 10)
        tkgrid.columnconfigure(label_frame, 2, weight = 1)
        tkgrid.columnconfigure(label_frame, 1, weight = 10)

        put_label(label_frame, "hscale: ", 1, 0)
        thscale <- tclVar(init = hscale)
        hscalef <- tkscale(label_frame, from = 1, to = 3, variable = thscale, showvalue = TRUE, resolution = 0.05, orient = "horiz")
        tkgrid(hscalef, row = 1, column = 1, sticky = "ew", padx = 2)

        put_label(label_frame, "vscale:  ", 2, 0)
        tvscale <- tclVar(init = vscale)
        vscalef <- tkscale(label_frame, from = 1, to = 3, variable = tvscale, showvalue = TRUE, resolution = 0.05, orient = "horiz")
        tkgrid(vscalef, row = 2, column = 1, sticky = "ew", padx = 2)
        
        button_frame <- ttkframe(frame)
        
        ok_button <- ttkbutton(button_frame, text = "ok",
            command = function() { 
                hscale <<- as.numeric(tclvalue(thscale))
                vscale <<- as.numeric(tclvalue(tvscale))
                tkdestroy(window)
            })
        tkpack(button_frame, fill = "x", padx = 5, pady = 5)
        tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
           fill = "y", side = "left")               
        sapply(list(ok_button), tkpack, side = "left", padx = 6)
    }

    # About
    About <- function() {             
        window <- tktoplevel(width = 600, height = 400)
        tkwm.minsize(window, "600", "400")
        tkwm.maxsize(window, "600", "400")
        tkwm.title(window, "About")

        frame <- tkframe(window, bg = "white")
        tkpack(frame, expand = TRUE, fill = "both")

        tcl("image", "create", "photo", "imageID1", file = system.file("logos/TextMiningGUI.png", package = "TextMiningGUI"))
        img1 <- ttklabel(frame, image = "imageID1", compound = "image")
        tkconfigure(img1, background = "white")
        tkpack(img1)

        text1 <- ttklabel(frame, text = "Version: 0.1")
        tkconfigure(text1, background = "white")
        text2 <- ttklabel(frame, text = "License: GPL")
        tkconfigure(text2, background = "white")
        text3 <- ttklabel(frame, text = "Developer: Conrado Reyes\n\n")
        tkconfigure(text3, background = "white")
        tkpack(text1, text2, text3)

        tcl("image", "create", "photo", "imageID2", file = system.file("logos/usal.png", package = "TextMiningGUI"))
        img2 <- ttklabel(frame, image = "imageID2", compound = "image")
        tkconfigure(img2, background = "white")
        tkpack(img2)
    }

    # Converter
    Converter <- function() {             
        vars <- colnames(DATA)
        types <- c("factor", "date")

        var <- tclVar("")
        type <- tclVar("")
        value <- tclVar("")

        window <- tktoplevel(width = 300, height = 175)
        tkwm.minsize(window, "300", "175")
        tkwm.maxsize(window, "300", "175")

        tkwm.title(window, "Converter")
        frame <- ttkframe(window, padding = c(3,3,12,12))
        tkpack(frame, expand = TRUE, fill = "both")

        label_frame <- ttklabelframe(frame, text = "Options", padding = 10)
        tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)

        tkgrid.columnconfigure(label_frame, 0, weight = 1)
        tkgrid.columnconfigure(label_frame, 1, weight = 10)
        tkgrid.columnconfigure(label_frame, 2, weight = 1)
        tkgrid.columnconfigure(label_frame, 1, weight = 10)

        put_label(label_frame, "Variable: ", 1, 0)
        combo_box1 <- ttkcombobox(label_frame, 
                        values = vars, 
                        textvariable = var,
                        state = "normal",
                        justify = "left")
        tkgrid(combo_box1, row = 1, column = 1, sticky = "ew", padx = 2)

        put_label(label_frame, "Type: ", 2, 0)
        combo_box2 <- ttkcombobox(label_frame, 
                        values = types, 
                        textvariable = type,
                        state = "normal",
                        justify = "left")
        tkgrid(combo_box2, row = 2, column = 1, sticky = "ew", padx = 2)

        put_label(label_frame, "Values or format: ", 3, 0)
        options <- ttkentry(label_frame, width = 15, textvariable = value)
        tkgrid(options, row = 3, column = 1, sticky = "ew", padx = 2)

        button_frame <- ttkframe(frame)
        cancel_button <- ttkbutton(button_frame, text = "cancel",
            command = function() { 
                tkdestroy(window) 
            })
        ok_button <- ttkbutton(button_frame, text = "ok",
            command = function() { 
                v <- tclvalue(var)
                t <- tclvalue(type)
                vv <- tclvalue(value)

                if(v != "" && t == "factor") {
                    if(vv != "") {
                        labels <- unlist(strsplit(vv, ","))
                        DATA[[v]] <<- factor(DATA[[v]], labels = labels)
                    }else{
                        DATA[[v]] <<- factor(DATA[[v]])
                    }
                }else if(v != "" && t == "date" && vv != "") {
                    DATA[[v]] <<- as.Date(DATA[[v]], vv)
                }

                RefreshTableFrame()
                dataFrameTable(tableFrame, DATA)
                tkdestroy(window)
            })
        tkpack(button_frame, fill = "x", padx = 5, pady = 5)
        tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
           fill = "y", side = "left")               
        sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
    }

    # Transformation
    Transformation <- function() {    
        languages <- c("spanish")         
        names <- colnames(DATA)
        group <- tclVar("")
        text <- tclVar("")
        lang <- tclVar("")

        window <- tktoplevel(width = 300, height = 175)
        tkwm.minsize(window, "300", "175")
        tkwm.maxsize(window, "300", "175")

        tkwm.title(window, "Transformation")
        frame <- ttkframe(window, padding = c(3,3,12,12))
        tkpack(frame, expand = TRUE, fill = "both")

        label_frame <- ttklabelframe(frame, text = "Options", padding = 10)
        tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)

        tkgrid.columnconfigure(label_frame, 0, weight = 1)
        tkgrid.columnconfigure(label_frame, 1, weight = 10)
        tkgrid.columnconfigure(label_frame, 2, weight = 1)
        tkgrid.columnconfigure(label_frame, 1, weight = 10)

        put_label(label_frame, "Group: ", 1, 0)
        combo_box1 <- ttkcombobox(label_frame, 
                        values = names, 
                        textvariable = group,
                        state = "normal",
                        justify = "left")
        tkgrid(combo_box1, row = 1, column = 1, sticky = "ew", padx = 2)

        put_label(label_frame, "Text: ", 2, 0)
        combo_box2 <- ttkcombobox(label_frame, 
                        values = names, 
                        textvariable = text,
                        state = "normal",
                        justify = "left")
        tkgrid(combo_box2, row = 2, column = 1, sticky = "ew", padx = 2)

        put_label(label_frame, "Language: ", 3, 0)
        combo_box3 <- ttkcombobox(label_frame, 
                        values = languages, 
                        textvariable = lang,
                        state = "normal",
                        justify = "left")
        tkgrid(combo_box3, row = 3, column = 1, sticky = "ew", padx = 2)

        button_frame <- ttkframe(frame)
        cancel_button <- ttkbutton(button_frame, text = "cancel",
            command = function() { 
                tkdestroy(window) 
            })
        ok_button <- ttkbutton(button_frame, text = "ok",
            command = function() { 
                g <- tclvalue(group)
                t <- tclvalue(text)
                l <- tclvalue(lang)

                TMP <- DATA %>% distinct() %>% select(g, t) 
                colnames(TMP) <- c("GROUP","TEXT")

                RefreshTableFrame()
                tm <<- DataTM(TMP, l)
                dataFrameTable(tableFrame, tm$data)
                tkdestroy(window) 
                tkentryconfigure(menu_bar, 3, state = "normal")
                tkentryconfigure(data_menu, 4, state = "normal")
            })
        tkpack(button_frame, fill = "x", padx = 5, pady = 5)
        tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
           fill = "y", side = "left")               
        sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
    }

    # Fill Table
    fillTable <- function(treeview, DF) {
        children <- as.character(tcl(treeview, "children", ""))
        for(i in children) 
            tcl(treeview, "delete", i)                 
        shade <- c("none", "gray")
        strs <- unlist(lapply(DF, is.character)) 
        for(i in seq_len(nrow(DF))) {
            DF[i,] <- lapply(DF[i,], iconv, to = "ASCII//TRANSLIT")
            tcl(treeview, "insert", "", "end", tag = shade[i %% 2], 
                text = "",  
                values = unlist(DF[i,]))               
        }
        tktag.configure(treeview, "gray", background = "gray95")
    }

    # Table
    dataFrameTable <- function(frame, DF) {
        ID <- rownames(DF)
        COL <- colnames(DF)
        count <- nrow(DF)

        DF <- cbind(ID, DF)
        colnames(DF) <- c("-", COL)

        console_chunk("dim(DATA)")
        console_chunk("str(DATA)")

        l <- if(nrow(DF) > 100) 100 else nrow(DF)
        DF <- DF[1:l,]

        .toCharacter <- function(x,width,...) UseMethod(".toCharacter")
        .toCharacter.default <- function(x,width,...) as.character(x)
        .toCharacter.integer <- function(x,width,...) {
            if(missing(width)) width <- max(nchar(as.character(x))) + 2  
            format(x, justify = "right", width = width)
        }
        .toCharacter.numeric <- function(x,width,...) {
            if(missing(width)) width <- max(nchar(as.character(x))) + 2
            format(x,trim = FALSE, width = width, justify = "right")
        }
        .toCharacter.factor <- function(x,width,...) {
            if(missing(width)) width <- max(nchar(as.character(x))) + 2
            .toCharacter(as.character(x),width,...)
        }
        .toCharacter.logical <- function(x,width,...) {
            if(missing(width)) width <- 7
            format(as.character(x), justify = "centre", width = width)
        }
        .toCharacter.data.frame <- function(x,width =  10, ...) {
            nms <- dimnames(x)
            DF <- as.data.frame(lapply(x,function(i) .toCharacter(i, width = width)),
                          stringsAsFactors = FALSE)
            dimnames(DF) <- nms
            return(DF)
        }

        # filter
        frame_0 <- ttkframe(frame)
        tkpack(frame_0, fill = "x")
        label <- ttklabel(frame_0, text = "Filter: ")
        tkpack(label, side = "left")
        filter_var <- tclVar("")
        filter_entry <- ttkentry(frame_0, textvariable = filter_var)
        tkpack(filter_entry, side = "left")

        # make tree
        frame_1 <- ttkframe(frame)
        tkpack(frame_1, expand = TRUE, fill = "both")
        treeview <- ttktreeview(frame_1, columns = 1:ncol(DF), 
                      displaycolumns = 1:(ncol(DF)), 
                      show = "headings",     
                      selectmode = "browse") 

        # count
        frame_2 <- ttkframe(frame)
        tkpack(frame_2, fill = "x")
        count <- paste0(" | Total rows: ", count)
        label_count <- ttklabel(frame_0, text = count)
        tkpack(label_count, side = "left")

        # scrollbars tree
        addScrollbars(frame_1, treeview)

        # configure
        nms <- names(DF)
        for(i in 1:ncol(DF)) {
            tcl(treeview, "heading", i, text = nms[i])
            tcl(treeview, "column", i, width = 10, stretch = TRUE, anchor = "w")
        }

        # fill
        DF <- data.frame(lapply(DF, as.character), stringsAsFactors = FALSE)
        fillTable(treeview, DF)

        # filter bind
        cur_ind <- 1:nrow(DF)
        tkbind(filter_entry, "<KeyRelease>", function(W, K) {
            val <- tclvalue(tkget(W))
            poss_vals <- apply(DF, 1, function(...) 
                        paste(..., collapse = " "))
            ind <- grep(val, poss_vals)
            if(length(ind) == 0) ind <- 1:nrow(DF)
                fillTable(treeview, DF[ind,])
        })
    }

    # Examples
    ReadExample <- function() {                                                                                                                                                                                                                         
        df_names <- c("chistes")
        var <- tclVar(" ")

        window <- tktoplevel(width = 300, height = 150)
        tkwm.minsize(window, "300", "150")
        tkwm.maxsize(window, "300", "150")

        tkwm.title(window, "Data Options")
        frame <- ttkframe(window, padding = c(3,3,12,12))
        tkpack(frame, expand = TRUE, fill = "both")

        label_frame <- ttklabelframe(frame, text = "Options", padding = 10)
        tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)

        tkgrid.columnconfigure(label_frame, 0, weight = 1)
        tkgrid.columnconfigure(label_frame, 1, weight = 10)
        tkgrid.columnconfigure(label_frame, 2, weight = 1)
        tkgrid.columnconfigure(label_frame, 1, weight = 10)

        put_label(label_frame, "Data: ", 1, 0)
        combo_box <- ttkcombobox(label_frame, 
                        values = df_names, 
                        textvariable = var,
                        state = "normal",
                        justify = "left")
        tkgrid(combo_box, row = 1, column = 1, sticky = "ew", padx = 2)

        tkbind(combo_box, "<<ComboboxSelected>>", function() {
            if(is.matrix(get(tclvalue(var)))) {
                DATA <<- as.data.frame(get(tclvalue(var)))
            }else{
                DATA <<- get(tclvalue(var))
            }
        })

        button_frame <- ttkframe(frame)
        cancel_button <- ttkbutton(button_frame, text = "cancel",
            command = function() { 
                tkdestroy(window) 
            })
        ok_button <- ttkbutton(button_frame, text = "ok",
            command = function() { 
                dataFrameTable(tableFrame, DATA)
                tkdestroy(window) 
                tkentryconfigure(menu_bar, 2, state = "normal")
                tkentryconfigure(menu_bar, 3, state = "disabled")
                tkentryconfigure(data_menu, 4, state = "disabled")
            })
        tkpack(button_frame, fill = "x", padx = 5, pady = 5)
        tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
           fill = "y", side = "left")               
        sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
    }

    # Read Data
    ReadData <- function(file_name) {                                                                                                                                                                                                                         
        load(file_name)
        df_names <- ls(pattern = "[^file_name]")
        var <- tclVar(" ")

        window <- tktoplevel(width = 300, height = 150)
        tkwm.minsize(window, "300", "150")
        tkwm.maxsize(window, "300", "150")

        tkwm.title(window, "Data Options")
        frame <- ttkframe(window, padding = c(3,3,12,12))
        tkpack(frame, expand = TRUE, fill = "both")

        label_frame <- ttklabelframe(frame, text = "Options", padding = 10)
        tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)

        tkgrid.columnconfigure(label_frame, 0, weight = 1)
        tkgrid.columnconfigure(label_frame, 1, weight = 10)
        tkgrid.columnconfigure(label_frame, 2, weight = 1)
        tkgrid.columnconfigure(label_frame, 1, weight = 10)

        put_label(label_frame, "Data: ", 1, 0)
        combo_box <- ttkcombobox(label_frame, 
                        values = df_names, 
                        textvariable = var,
                        state = "normal",
                        justify = "left")
        tkgrid(combo_box, row = 1, column = 1, sticky = "ew", padx = 2)

        tkbind(combo_box, "<<ComboboxSelected>>", function() {
            if(is.matrix(get(tclvalue(var)))) {
                DATA <<- as.data.frame(get(tclvalue(var)))
            }else{
                DATA <<- get(tclvalue(var))
            }
        })

        button_frame <- ttkframe(frame)
        cancel_button <- ttkbutton(button_frame, text = "cancel",
            command = function() { 
                tkdestroy(window) 
            })
        ok_button <- ttkbutton(button_frame, text = "ok",
            command = function() { 
                dataFrameTable(tableFrame, DATA)
                tkdestroy(window) 
                tkentryconfigure(menu_bar, 2, state = "normal")
                tkentryconfigure(menu_bar, 3, state = "disabled")
                tkentryconfigure(data_menu, 4, state = "disabled")
            })
        tkpack(button_frame, fill = "x", padx = 5, pady = 5)
        tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
           fill = "y", side = "left")               
        sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
    }

    # Read Excel
    ReadExcel <- function(file_name) {                                                                                                                                                                                                                              
        DATA <<- read_excel(file_name)                                                                                                                                                                                            
        dataFrameTable(tableFrame, DATA)
        tkentryconfigure(menu_bar, 2, state = "normal")
        tkentryconfigure(menu_bar, 3, state = "disabled")
        tkentryconfigure(data_menu, 4, state = "disabled")
    }

    # Read Table
    ReadTable <- function(file_name) {
        encoding <- "UTF-8"
        header <- tclVar(TRUE) 
        sep <- tclVar(",")  
        comment.char <- tclVar("")
        other.char <- tclVar("")

        window <- tktoplevel(width = 350, height = 200)
        tkwm.minsize(window, "350", "200")
        tkwm.maxsize(window, "350", "200")

        tkwm.title(window, "Table Options")
        frame <- ttkframe(window, padding = c(3,3,12,12))
        tkpack(frame, expand = TRUE, fill = "both")

        label_frame <- ttklabelframe(frame, text = "Options", padding = 10)
        tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)

        tkgrid.columnconfigure(label_frame, 0, weight = 1)
        tkgrid.columnconfigure(label_frame, 1, weight = 10)
        tkgrid.columnconfigure(label_frame, 2, weight = 1)
        tkgrid.columnconfigure(label_frame, 1, weight = 10)

        put_label(label_frame, "Header: ", 1, 0)
        header_check <- ttkcheckbutton(label_frame, variable = header)
        tkgrid(header_check, row = 1, column = 1, sticky = "ew", padx = 2)

        put_label(label_frame, "Spacer: ", 2, 0)
        rb_frame <- ttkframe(label_frame)
        sapply(c(",","|","other:"), function(i) {
            radio_button <- ttkradiobutton(rb_frame, variable = sep, text = i, value = i)
            tkpack(radio_button, side = "left")
        })
        tkgrid(rb_frame, row = 2, column = 1, sticky = "ew", padx = 2)

        other <- ttkentry(label_frame, width = 15, textvariable = other.char)
        tkgrid(other, row = 2, column = 2, sticky = "ew", padx = 2)

        put_label(label_frame, "Comments: ", 3, 0)
        entry <- ttkentry(label_frame, textvariable = comment.char)
        tkgrid(entry, row = 3, column = 1, sticky = "ew", padx = 2)

        button_frame <- ttkframe(frame)
        cancel_button <- ttkbutton(button_frame, text = "cancel",
            command = function() { 
                tkdestroy(window) 
            })
        ok_button <- ttkbutton(button_frame, text = "ok",
            command = function() { 
                h <- if( tclvalue(header) == "1" ) TRUE else FALSE
                s <- if( tclvalue(sep) == "other:") tclvalue(other.char) else tclvalue(sep)
                DATA <<- read.table(file_name, 
                    encoding = encoding, 
                    header = h, 
                    sep = s, 
                    fill = TRUE, 
                    quote = "", 
                    stringsAsFactors = FALSE, 
                    comment.char = tclvalue(comment.char))

                dataFrameTable(tableFrame, DATA)
                tkdestroy(window)
                tkentryconfigure(menu_bar, 2, state = "normal")
                tkentryconfigure(menu_bar, 3, state = "disabled")
                tkentryconfigure(data_menu, 4, state = "disabled")
            })
        tkpack(button_frame, fill = "x", padx = 5, pady = 5)
        tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
           fill = "y", side = "left")               
        sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
    }

    RefreshTableFrame <- function() {
        if(exists("tableFrame")) {        
            tkdestroy(tableFrame)
        }
        tableFrame <<- ttkframe(frame, padding = c(3,3,3,12))
        tkpack(tableFrame, expand = TRUE, fill = "both")
    }

    Page <<- function(parent, name) {
        iconx <- tclVar()
        icons <- tclVar()
        iconl <- tclVar()
        tcl("image", "create", "photo", iconx, file = system.file("icons/x.png", package = "TextMiningGUI"))
        tcl("image", "create", "photo", icons, file = system.file("icons/save.png", package = "TextMiningGUI"))
        tcl("image", "create", "photo", iconl, file = system.file("icons/lupa.png", package = "TextMiningGUI"))

        frame <- ttkframe(parent, padding = c(3,3,3,12))
        tkadd(parent, frame, sticky = "nswe", text = name, compound = "right")
        
        tool_bar <- ttkframe(frame, padding = 2)
        content <- ttkframe(frame, padding = 2)
        
        tkgrid(tool_bar, row = 0, column = 0, sticky = "we")
        tkgrid(content, row = 1, column = 0, sticky  =  "news")
        tkgrid.rowconfigure(frame, 0, weight = 0)
        tkgrid.rowconfigure(frame, 1, weight = 1)
        tkgrid.columnconfigure(frame, 0, weight = 1)

        img3 <- ttklabel(tool_bar, image = iconl)
        img1 <- ttklabel(tool_bar, image = icons)
        img2 <- ttklabel(tool_bar, image = iconx)
        tkpack(img2, img1, img3, side = "right")

        tkbind(img2, "<ButtonRelease-1>", function() { tcl(parent, "forget", frame)})

        page <- list()
        page$content <- content
        page$save <- img1
        page$zoom <- img3
        class(page) <- "Page"

        return(page)
    }

    # Scale
    hscale <<- 1.5
    vscale <<- 1.5

    # Main Window
    window <- tktoplevel(width = 600, height = 400)
    tkwm.minsize(window, "600", "400")
    tkwm.title(window, "TextMiningGUI")
    tkwm.protocol(window, "WM_DELETE_WINDOW", function() {
            tkdestroy(window)
        })

    if(as.character(tcl("tk", "windowingsystem")) == "win32" || Sys.info()["sysname"] == "Windows") {
        tkwm.iconbitmap(window, system.file("icons/textmining.ico", package = "TextMiningGUI"))
    }else{
        tcl('wm', 'iconphoto', window, tcl('image', 'create', 'photo', '-file', system.file("icons/textmining.png", package = "TextMiningGUI")))
    }

    # Menu
    menu_bar <- tkmenu(window)

    tkconfigure(window, menu = menu_bar)
    file_menu <- tkmenu(menu_bar, tearoff = "0")
    tkadd(menu_bar, "cascade", label = "File", menu = file_menu)

    data_menu <- tkmenu(menu_bar, tearoff = "0")
    tkadd(menu_bar, "cascade", label = "Data", menu = data_menu, state = "disabled")

    analysis_menu <- tkmenu(menu_bar, tearoff = "0")
    tkadd(menu_bar, "cascade", label = "Analysis", menu = analysis_menu, state = "disabled")

    help_menu <- tkmenu(menu_bar, tearoff = "0")
    tkadd(menu_bar, "cascade", label = "Help", menu = help_menu)

    # File
    tkadd(file_menu,"command", label = "Text file...",
        command =  function() {
            file_name <- tkgetOpenFile(filetypes=
                        "{{Text files} {.txt}} {{CSV files} {.csv}} {{All files} *}")
            if(file.exists(file_name <- as.character(file_name))) {
                RefreshTableFrame()
                ReadTable(file_name)
            }
        })

    tkadd(file_menu,"command", label = "Excel file...",
        command =  function() {
            file_name <- tkgetOpenFile(filetypes=
                        "{{Excel files} {.xls}} {{Excel files} {.xlsx}} {{All files} *}")
            if(file.exists(file_name <- as.character(file_name))) {
                RefreshTableFrame()
                ReadExcel(file_name)
            }
        })

    tkadd(file_menu,"command", label = "Data file...",
        command =  function() {
            file_name <- tkgetOpenFile(filetypes=
                        "{{Rdata files} {.RData}} {{Rdata files} {.Rdata}} {{Rdata files} {.rdata}} {{Rdata files} {.Rda}} {{Rdata files} {.rda}} {{All files} *}")
            if(file.exists(file_name <- as.character(file_name))) {
                RefreshTableFrame()
                ReadData(file_name)
            }
        })

    tkadd(file_menu,"command", label = "Data Examples...",
        command =  function() {
            RefreshTableFrame()
            ReadExample()
        })

    tkadd(file_menu, "command", label = "Set working directory...",
        command = function() {
            dir_name <- tkchooseDirectory()
            if(nchar(dir_name <- as.character(dir_name)))
                setwd(dir_name)
        })

    tkadd(file_menu, "separator")

    tkadd(file_menu, "command", label = "Quit",
        command = function() {
            tkdestroy(window)
        })

    # Data
    tkadd(data_menu, "command", label = "Converter", command = Converter)

    tkadd(data_menu, "command", label = "Transformation", command = Transformation)

    tkadd(data_menu, "separator")

    tkadd(data_menu, "command", label = "View Raw Text", 
        command = function() {
            RefreshTableFrame()
            dataFrameTable(tableFrame, DATA)
        })

    tkadd(data_menu, "command", label = "View Words", state = "disabled",
        command = function() {
            RefreshTableFrame()
            dataFrameTable(tableFrame, tm$data)
        })

    tkadd(data_menu, "separator")

    tkadd(data_menu, "command", label = "Console",
        command = function() {
            console()
        })

    # Analysis
    tkadd(analysis_menu, "command", label = "BallonPlot", command = BallonPlot)

    tkadd(analysis_menu, "command", label = "WorldCounter", command = Explorer)

    tkadd(analysis_menu, "command", label = "WorldCloud", command = WorldCloud)

    tkadd(analysis_menu, "separator")

    ca_menu <- tkmenu(analysis_menu, tearoff = "0")
    tkadd(analysis_menu, "cascade", label = "Correspondence Analysis", menu = ca_menu)

    tkadd(ca_menu, "command", label = "CA & Scree plot", command = CaTM)
    tkadd(ca_menu, "command", label = "CA - Biplot", command = CaBiplot)
    tkadd(ca_menu, "command", label = "CA - Quality of representarion of rows", command = QualityRow)
    tkadd(ca_menu, "command", label = "CA - Quality of representarion of cols", command = QualityCol)

    # Help
    tkadd(help_menu, "command", label = "Configure", command = Configure)

    tkadd(help_menu, "command", label = "About", command = About)

    # Notebook
    notebook <<- ttknotebook(window)
    tkpack(notebook, expand = TRUE, fill = "both")

    # Table
    frame <- ttkframe(notebook, padding = c(3,3,3,12))
    tkadd(notebook, frame, sticky = "nswe", text = "Data", compound = "right")

    # Image
    tcl("image", "create", "photo", "imageID", file = system.file("logos/TextMiningGUI.png", package = "TextMiningGUI"))
    tableFrame <<- ttkframe(frame, padding = c(3,3,3,12))
    tkpack(tableFrame, expand = TRUE, fill = "both")
    img <- ttklabel(tableFrame, image = "imageID", compound = "image", anchor = "center")
    tkpack(img)
}

#scrollable-frame
#example-padding
#validation