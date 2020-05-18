TextMiningGUI <- function() {
    if(exists("x", envir = .BaseNamespaceEnv) && !is.null(x)) stop("You have one session running...") 
        assign("x", 1, envir = .BaseNamespaceEnv)
    
    env <- new.env()
    tb <- new.env()

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
        text3 <- ttklabel(frame, text = "Created by Conrado Reyes\n\n")
        tkconfigure(text3, background = "white")
        tkpack(text1, text2, text3)

        tcl("image", "create", "photo", "imageID2", file = system.file("logos/usal.png", package = "TextMiningGUI"))
        img2 <- ttklabel(frame, image = "imageID2", compound = "image")
        tkconfigure(img2, background = "white")
        tkpack(img2)
    }

    # Converter
    Converter <- function() {            
        vars <- colnames(env$DATA)
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
                        env$DATA[[v]] <- factor(env$DATA[[v]], labels = labels)
                        assign("DATA", env$DATA, envir = env)
                    }else{
                        env$DATA[[v]] <- factor(env$DATA[[v]])
                        assign("DATA", env$DATA, envir = env)
                    }
                }else if(v != "" && t == "date" && vv != "") {
                    env$DATA[[v]] <- as.Date(env$DATA[[v]], vv)
                    assign("DATA", env$DATA, envir = env)
                }

                dataFrameTable(env$DATA)
                tkdestroy(window)
            })
        tkpack(button_frame, fill = "x", padx = 5, pady = 5)
        tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
           fill = "y", side = "left")               
        sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
    }

    # Transformation
    group <- tclVar("")
    text <- tclVar("")
    lang <- tclVar("")
    steam <- tclVar(TRUE)
    normalize <- tclVar("chara-value")
    sparse <- tclVar(init = 0.99)

    Transformation <- function() {  
        languages <- c("danish","dutch","english","finnish","french","german","hungarian","italian","norwegian","portuguese","russian","spanish","swedish")         
        names <- colnames(env$DATA)      

        window <- tktoplevel(width = 420, height = 240)
        tkwm.minsize(window, "420", "240")
        tkwm.maxsize(window, "420", "240")

        tkwm.title(window, "Create Lexical Table")
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
        
        put_label(label_frame, "Stemming: ", 4, 0)
        steam_check <- ttkcheckbutton(label_frame, variable = steam)
        tkgrid(steam_check, row = 4, column = 1, sticky = "ew", padx = 2)

        put_label(label_frame, "Normalize: ", 5, 0)
        radio <- ttkframe(label_frame)
        sapply(c("chara-value","tf-idf","media","none"), function(i) {
            normalize_button <- ttkradiobutton(radio, variable = normalize, text = i, value = i)
            tkpack(normalize_button, side = "left")
        })
        tkgrid(radio, row = 5, column = 1, sticky = "ew", padx = 2)

        put_label(label_frame, "Remove Sparse: ", 6, 0, sticky = "es")
        removebar <- tkscale(label_frame, from = 0.5, to = 1, variable = sparse, 
                             showvalue = TRUE, resolution = 0.01, orient = "horiz")
        tkgrid(removebar, row = 6, column = 1, sticky = "ew", padx = 2)
        
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
                sparse <- as.numeric(tclvalue(sparse))
                steam <- if( tclvalue(steam) == "1" ) TRUE else FALSE
                normalize <- tclvalue(normalize)

                if(g != "" && t != "" && l != "") {
                    TMP <- env$DATA %>% distinct() %>% select(g, t) 
                    colnames(TMP) <- c("GROUP","TEXT")

                    assign("tm", DataTM(TMP, language = l, steam = steam, sparse = sparse, normalize = normalize), envir = env)
                    dataFrameTable(env$tm$data)
                    tkdestroy(window) 
                    tkentryconfigure(menu_bar, 3, state = "normal")
                    tkentryconfigure(data_menu, 4, state = "normal")

                    if(ncol(env$tm$data) < 3) {
                        tkmessageBox(title = "Warning", message = "Warning:", icon = "warning", 
                                     detail = "Some procedure cannot implement, verify your data.", 
                                     type = "ok")
                    }
                }
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
    dataFrameTable <- function(DF) {
        frame <- RefreshTableFrame()

        ID <- rownames(DF)
        COL <- colnames(DF)
        count <- nrow(DF)

        DF <- cbind(ID, DF)
        colnames(DF) <- c("-", COL)

        tcl(notebook, "select", "0")
        console(cmds = "dim(DATA)", envir = env)
        console(cmds = "str(DATA)", envir = env)
        console(cmds = "tm", envir = env)

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
        df_names <- c("chistes", "jockes")
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
            rm(list = ls(envir = env), envir = env)
            if(is.matrix(get(tclvalue(var)))) {
                assign("DATA", as.data.frame(get(tclvalue(var))), envir = env)
            }else{
                assign("DATA", get(tclvalue(var)), envir = env)
            }
        })

        button_frame <- ttkframe(frame)
        cancel_button <- ttkbutton(button_frame, text = "cancel",
            command = function() { 
                tkdestroy(window) 
            })
        ok_button <- ttkbutton(button_frame, text = "ok",
            command = function() { 
                dataFrameTable(env$DATA)
                tkdestroy(window) 
                disableMenu()
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
            rm(list = ls(envir = env), envir = env)
            if(is.matrix(get(tclvalue(var)))) {
                assign("DATA", as.data.frame(get(tclvalue(var))), envir = env)
            }else{
                assign("DATA", get(tclvalue(var)), envir = env)
            }
        })

        button_frame <- ttkframe(frame)
        cancel_button <- ttkbutton(button_frame, text = "cancel",
            command = function() { 
                tkdestroy(window) 
            })
        ok_button <- ttkbutton(button_frame, text = "ok",
            command = function() { 
                dataFrameTable(env$DATA)
                tkdestroy(window) 
                disableMenu()
            })
        tkpack(button_frame, fill = "x", padx = 5, pady = 5)
        tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
           fill = "y", side = "left")               
        sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
    }

    # Read Excel
    ReadExcel <- function(file_name) {      
        rm(list = ls(envir = env), envir = env)                                                                                                                                                                                                                        
        assign("DATA", read_excel(file_name), envir = env)                                                                                                                                                                                       
        dataFrameTable(env$DATA)
        disableMenu()
    }

    # Read Json
    ReadJson <- function(file_name) {
        rm(list = ls(envir = env), envir = env)
        assign("DATA", fromJSON(file_name), envir = env)                                                                                                                                                                                    
        dataFrameTable(env$DATA)
        disableMenu()
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
                rm(list = ls(envir = env), envir = env)
                DATA <- read.table(file_name, 
                    encoding = encoding, 
                    header = h, 
                    sep = s, 
                    fill = TRUE, 
                    quote = "", 
                    stringsAsFactors = FALSE, 
                    comment.char = tclvalue(comment.char))
                assign("DATA", DATA, envir = env)
                dataFrameTable(env$DATA)
                tkdestroy(window)
                disableMenu()
            })
        tkpack(button_frame, fill = "x", padx = 5, pady = 5)
        tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
           fill = "y", side = "left")               
        sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
    }

    # Read Project
    ReadProject <- function(file_name) {
        rm(list = ls(envir = env), envir = env)
        load(file = file_name, envir = env)
        dataFrameTable(env$DATA)
        tkentryconfigure(menu_bar, 2, state = "normal")
        tkentryconfigure(menu_bar, 3, state = "normal")
        tkentryconfigure(data_menu, 4, state = "normal")
        
        group <<- tclVar("")
        text <<- tclVar("")
        lang <<- env$tm$lang
        steam <<- env$tm$steam 
        normalize <<- env$tm$normalize
        sparse <<- env$tm$sparse
    }

    disableMenu <- function() {
        tkentryconfigure(menu_bar, 2, state = "normal")
        tkentryconfigure(menu_bar, 3, state = "disabled")
        tkentryconfigure(data_menu, 4, state = "disabled")

        group <<- tclVar("")
        text <<- tclVar("")
        lang <<- tclVar("")
        steam <<- tclVar(TRUE)
        normalize <<- tclVar("chara-value")
        sparse <<- tclVar(init = 0.99)
    }

    RefreshTableFrame <- function() { 
        if(exists("tableFrame", envir = tb)) tkdestroy(tb$tableFrame)
        assign("tableFrame", ttkframe(frame, padding = c(3,3,3,12)), envir = tb)
        tkpack(tb$tableFrame, expand = TRUE, fill = "both")
        return(tb$tableFrame)
    }

    # Main Window
    window <- tktoplevel(width = 800, height = 600)
    tkwm.minsize(window, "800", "600")
    tkwm.maxsize(window, tclvalue(tkwinfo("screenwidth",".")), tclvalue(tkwinfo("screenheight",".")))
    tkwm.title(window, "TextMiningGUI")
    tkwm.protocol(window, "WM_DELETE_WINDOW", function() {
            assign("x", NULL, envir = .BaseNamespaceEnv)
            tkdestroy(window)
        })

    if(as.character(tcl("tk", "windowingsystem")) == "win32" || Sys.info()["sysname"] == "Windows") {
        tkwm.iconbitmap(window, system.file("icons/textmining.ico", package = "TextMiningGUI"))
        tkwm.state(window, "zoomed")
    }else{
        tcl("wm", "iconphoto", window, tcl("image", "create", "photo", "-file", system.file("icons/textmining.png", package = "TextMiningGUI")))
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
    tkadd(file_menu, "command", label = "Text file...",
        command =  function() {
            file_name <- tkgetOpenFile(filetypes=
                        "{{Text files} {.txt}} {{CSV files} {.csv}} {{All files} *}")
            if(file.exists(file_name <- as.character(file_name))) {
                ReadTable(file_name)
            }
        })

    tkadd(file_menu, "command", label = "Excel file...", state = "disabled",
        command =  function() {
            file_name <- tkgetOpenFile(filetypes=
                        "{{Excel files} {.xls}} {{Excel files} {.xlsx}} {{All files} *}")
            if(file.exists(file_name <- as.character(file_name))) {
                ReadExcel(file_name)
            }
        })

    tkadd(file_menu, "command", label = "Data file...",
        command =  function() {
            file_name <- tkgetOpenFile(filetypes=
                        "{{Rdata files} {.RData}} {{Rdata files} {.Rdata}} {{Rdata files} {.rdata}} {{Rdata files} {.Rda}} {{Rdata files} {.rda}} {{All files} *}")
            if(file.exists(file_name <- as.character(file_name))) {
                ReadData(file_name)
            }
        })

    tkadd(file_menu, "command", label = "JSON file...", state = "disabled",
        command =  function() {
            file_name <- tkgetOpenFile(filetypes=
                        "{{JSON files} {.json}} {{All files} *}")
            if(file.exists(file_name <- as.character(file_name))) {
                ReadJson(file_name)
            }
        })

    tkadd(file_menu, "command", label = "Data Examples...",
        command =  function() {
            ReadExample()
        })

    tkadd(file_menu, "separator")

    tkadd(file_menu, "command", label = "Set working directory...",
        command = function() {
            dir_name <- tkchooseDirectory()
            if(nchar(dir_name <- as.character(dir_name)))
                setwd(dir_name)
        })

    tkadd(file_menu, "separator")

    tkadd(file_menu, "command", label = "Save project...",
        command = function() {
            if(file.exists("project.RData")) file.remove("project.RData")
            save(list = ls(envir = env), file = "project.RData", envir = env)
        })

    tkadd(file_menu, "command", label = "Open project...",
        command =  function() {
            file_name <- tkgetOpenFile(filetypes=
                        "{{Rdata files} {.RData}} {{Rdata files} {.Rdata}} {{Rdata files} {.rdata}} {{Rdata files} {.Rda}} {{Rdata files} {.rda}} {{All files} *}")
            if(file.exists(file_name <- as.character(file_name))) {
                ReadProject(file_name)
            }
        })

    tkadd(file_menu, "separator")

    tkadd(file_menu, "command", label = "Quit",
        command = function() {
            assign("x", NULL, envir = .BaseNamespaceEnv)
            tkdestroy(window)
        })

    # Data
    tkadd(data_menu, "command", label = "Converter", command = Converter)

    tkadd(data_menu, "command", label = "Create Lexical Table", command = Transformation)

    tkadd(data_menu, "separator")

    tkadd(data_menu, "command", label = "View Data", 
        command = function() {
            dataFrameTable(env$DATA)
        })

    tkadd(data_menu, "command", label = "View Lexical Table", state = "disabled",
        command = function() {
            dataFrameTable(env$tm$data)
        })

    tkadd(data_menu, "separator")

    tkadd(data_menu, "command", label = "Console", command = function() { console(start = TRUE) })

    # Analysis
    tkadd(analysis_menu, "command", label = "Statistics", command = function() {
            console(start = TRUE)
            console(cmds = "tm", envir = env)
        })

    tkadd(analysis_menu, "command", label = "Words Most Used", 
        command = function() BalloonPlotPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "Word Counter", 
        command = function() ExplorerPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "Word Cloud", 
        command = function() WordCloudPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "co-occurrence", 
        command = function() cooccPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "separator")

    tkadd(analysis_menu, "command", label = "Cluster",
        command = function() ClusterPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "Correlation", 
        command = function() CorrelationPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "Correlation Between Groups", 
        command = function() CorBetweenGroupsPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "Correspondence Analysis", 
        command = function() CaPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "HJ-Biplot", 
        command = function() HJBiplotPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "Emotions & Sentiments", 
        command = function() EmotionsPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    # Help
    tkadd(help_menu, "command", label = "About", command = About)

    # Notebook
    notebook <- ttknotebook(window)
    tkpack(notebook, expand = TRUE, fill = "both")

    # Table
    frame <- ttkframe(notebook, padding = c(3,3,3,12))
    tkadd(notebook, frame, sticky = "nswe", text = "Data", compound = "right")

    # Image
    tcl("image", "create", "photo", "imageID", file = system.file("logos/TextMiningGUI.png", package = "TextMiningGUI"))
    assign("tableFrame", ttkframe(frame, padding = c(3,3,3,12)), envir = tb)
    tkpack(tb$tableFrame, expand = TRUE, fill = "both")
    img <- ttklabel(tb$tableFrame, image = "imageID", compound = "image", anchor = "center")
    tkpack(img)

    if(require(readxl)) tkentryconfigure(file_menu, 1, state = "normal")
    if(require(jsonlite)) tkentryconfigure(file_menu, 3, state = "normal")
}