toprint <- new.env()

#' Graphic interface for text analysis
#'
#' @param seed The seed of internal function.
TextMiningGUI <- function(seed = 0) {    
    TextMiningGUIEnv <- globalenv()

    if(exists("x", envir = TextMiningGUIEnv) && !is.null(get("x", envir = TextMiningGUIEnv)))
        stop("You have one session running...")  
    
    assign("x", 1, envir = TextMiningGUIEnv)

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

        text1 <- ttklabel(frame, text = "Version: 0.3")
        tkconfigure(text1, background = "white")
        text2 <- ttklabel(frame, text = "License: GPL (>= 2)")
        tkconfigure(text2, background = "white")
        tkpack(text1, text2)

        tcl("image", "create", "photo", "imageID2", file = system.file("logos/usal.png", package = "TextMiningGUI"))
        img2 <- ttklabel(frame, image = "imageID2", compound = "image")
        tkconfigure(img2, background = "white")
        tkpack(img2)
    }

    # Slice
    Slice <- function(X, name = "", type = "") {
        window <- tktoplevel(width = 470, height = 260)
        tkwm.minsize(window, "470", "260")
        tkwm.maxsize(window, "470", "260")
    
        tkwm.title(window, "Slice Groups")
        frame <- ttkframe(window, padding = c(3,3,12,12))
        tkpack(frame, expand = TRUE, fill = "both")
    
        label_frame <- ttklabelframe(frame, text = "Options", padding = 10)
        tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
    
        tkgrid.columnconfigure(label_frame, 0, weight = 1)
        tkgrid.columnconfigure(label_frame, 1, weight = 10)
        tkgrid.columnconfigure(label_frame, 2, weight = 1)
        tkgrid.columnconfigure(label_frame, 1, weight = 10)

        frame1 <- ttkframe(label_frame)
        treeview1 <- ttktreeview(frame1, columns = 1, show = "headings", height = 5)
        addScrollbars(frame1, treeview1)
        tcl(treeview1, "heading", 1, text = "Variable", anchor = "center")
        tkgrid(frame1, row = 1, column = 0, sticky = "ew", padx = 2)

        frame2 <- ttkframe(label_frame)
        treeview2 <- ttktreeview(frame2, columns = 1, show = "headings", height = 5) 
        addScrollbars(frame2, treeview2)
        tcl(treeview2, "heading", 1, text = "Slice", anchor = "center")
        tkgrid(frame2, row = 1, column = 1, sticky = "ew", padx = 2)
    
        if(!exists("data_", envir = env)) assign("data_", env$tm$data, envir = env)
        if(!exists("freq_", envir = env)) assign("freq_", env$tm$freq, envir = env)

        V <- colnames(env$data_)
        env$X <- colnames(env$tm$data)

        for(i in seq_along(V))
            tkinsert(treeview1, "", "end", values = as.tclObj(V[i]))
    
        for(i in seq_along(env$X))
            tkinsert(treeview2, "", "end", values = as.tclObj(env$X[i]))

        button_frame <- ttkframe(frame)
        cancel_button <- ttkbutton(button_frame, text = "cancel",
                                   command = function() { 
                                        tkdestroy(window) 
                                   })
        ok_button <- ttkbutton(button_frame, text = "ok",
                               command = function() {
                                    if(length(env$X) >= 3) {
                                        env$tm$data <- env$data_[,env$X]
                                        env$tm$freq <- env$freq_[env$freq_$GROUP %in% env$X,]
                                        dataFrameTable(env$tm$data)
                                        tkdestroy(window) 
                                    }
                               })
        tkpack(button_frame, fill = "x", padx = 5, pady = 5)
        tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
               fill = "y", side = "left")               
        sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
    
        tkbind(treeview1, "<Button-1>", function(W, x, y) {
            id <- as.character(tcl(W, "identify", "row", x, y))
            value <- as.character(tcl(W, "item", id, "-values"))
            if(value %in% env$X) return(NULL)
            env$X <- c(env$X, value)
            tkinsert(treeview2, "", "end", values = as.tclObj(c(value)))
        })  
    
        tkbind(treeview2, "<Button-1>", function(W, x, y) {
            id <- as.character(tcl(W, "identify", "row", x, y))
            value <- as.character(tcl(W, "item", id, "-values"))
            env$X <- env$X[env$X != value]
            tcl(W, "delete", id)
        }) 
    }

    # Converter
    Converter <- function() {            
        vars <- colnames(env$DATA)

        var <- tclVar("")
        type <- tclVar("")
        value <- tclVar("")
        breaks <- tclVar("")
        name <- tclVar("")

        window <- tktoplevel(width = 420, height = 200)
        tkwm.minsize(window, "420", "200")
        tkwm.maxsize(window, "420", "200")

        tkwm.title(window, "Converter Columns")
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
                        values = c("factor", "date", "range"), 
                        textvariable = type,
                        state = "normal",
                        justify = "left")
        tkgrid(combo_box2, row = 2, column = 1, sticky = "ew", padx = 2)

        put_label(label_frame, "Values or format: ", 3, 0)
        options <- ttkentry(label_frame, width = 15, textvariable = value)
        tkgrid(options, row = 3, column = 1, sticky = "ew", padx = 2)

        put_label(label_frame, "Breaks: ", 4, 0)
        options <- ttkentry(label_frame, width = 15, textvariable = breaks)
        tkgrid(options, row = 4, column = 1, sticky = "ew", padx = 2)

        put_label(label_frame, "New name: ", 5, 0)
        options <- ttkentry(label_frame, width = 15, textvariable = name)
        tkgrid(options, row = 5, column = 1, sticky = "ew", padx = 2)

        button_frame <- ttkframe(frame)
        cancel_button <- ttkbutton(button_frame, text = "cancel",
            command = function() { 
                tkdestroy(window) 
            })
        ok_button <- ttkbutton(button_frame, text = "ok",
            command = function() { 
                var <- tclvalue(var)
                type <- tclvalue(type)
                value <- tclvalue(value)
                breaks <- tclvalue(breaks)
                name <- tclvalue(name)

                labels <- if(value != "") trimws(unlist(strsplit(value, ","))) else ""

                col <- tryCatch({ 
                        if(var != "" && type == "factor") {
                            col <- factor(env$DATA[[var]], labels = labels)
                        }else if(var != "" && type == "date" && value != "") {
                            col <- as.Date(env$DATA[[var]], value)
                        }else if(var != "" && type == "range" && breaks != "") {
                            breaks <- as.numeric(unlist(strsplit(breaks, ",")))

                            if(NA %in% as.numeric(env$DATA[[var]])) return(NULL)
                            if(NA %in% breaks) return(NULL)

                            minvalue <- min(env$DATA[[var]])
                            if(min(breaks) > minvalue)
                                breaks <- c(minvalue, breaks)

                            maxvalue <- max(env$DATA[[var]])
                            if(max(breaks) < maxvalue) 
                                breaks <- c(breaks, maxvalue)

                            col <- cut(env$DATA[[var]], breaks = breaks, labels = labels, right = TRUE)
                        }
                        col
                    }, error = function(cond) {
                            tkmessageBox(title = "Error", message = "Error:", icon = "error", detail = "Some error ocurred.", type = "ok")
                            message(cond)
                        })

                if(name == "") env$DATA[[var]] <- col
                else env$DATA[[name]] <- col

                assign("DATA", env$DATA, envir = env)
                dataFrameTable(env$DATA)
                tkdestroy(window)
            })
        tkpack(button_frame, fill = "x", padx = 5, pady = 5)
        tkpack(ttklabel(button_frame, text = " "), expand = TRUE,
           fill = "y", side = "left")               
        sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
    }

    # Transformation
    env$group <- ""
    env$text <- ""
    env$time <- ""
    env$lang <- ""
    env$steam <- FALSE
    env$steamcomp <- FALSE
    env$stopwords <- TRUE
    env$otherstopwords <- ""
    env$normalize <- "chara-value"
    env$sparse <- 0.998
    env$bigrams <- FALSE

    Transform <- function() { 
        languages <- c("danish","dutch","english","finnish","french","german","hungarian","italian","norwegian","portuguese","russian","spanish","swedish")         
        names <- colnames(env$DATA)      

        group <- tclVar(env$group) 
        text <- tclVar(env$text)
        lang <- tclVar(env$lang)
        time <- tclVar(env$time)
        sparse <- tclVar(init = env$sparse) 
        steam <- tclVar(env$steam)
        steamcomp <- tclVar(env$steamcomp)
        stopwords <- tclVar(env$stopwords)
        otherstopwords <- tclVar(paste(env$otherstopwords, collapse = ","))
        normalize <- tclVar(env$normalize) 
        bigrams <- tclVar(env$bigrams)

        window <- tktoplevel(width = 420, height = 350)
        tkwm.minsize(window, "420", "350")
        tkwm.maxsize(window, "420", "350")

        tkwm.title(window, "Transform")
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
                        values = c("", names), 
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

        put_label(label_frame, "Time: ", 3, 0)
        combo_box3 <- ttkcombobox(label_frame, 
                        values = c("", names), 
                        textvariable = time,
                        state = "normal",
                        justify = "left")
        tkgrid(combo_box3, row = 3, column = 1, sticky = "ew", padx = 2)

        put_label(label_frame, "Language: ", 4, 0)
        combo_box3 <- ttkcombobox(label_frame, 
                        values = languages, 
                        textvariable = lang,
                        state = "normal",
                        justify = "left")
        tkgrid(combo_box3, row = 4, column = 1, sticky = "ew", padx = 2)
        
        put_label(label_frame, "Stopwords: ", 5, 0)
        stop_check <- ttkcheckbutton(label_frame, variable = stopwords)
        tkgrid(stop_check, row = 5, column = 1, sticky = "ew", padx = 2)

        put_label(label_frame, "Other stopwords: ", 6, 0)
        otherstopwords_entry <- ttkentry(label_frame, textvariable = otherstopwords)
        tkgrid(otherstopwords_entry, row = 6, column = 1, sticky = "ew", padx = 2)

        tkbind(otherstopwords_entry, "<Double-1>", function() {
                file_name <- tkgetOpenFile(filetypes = "{{Text files} {.txt}}")
                if(file.exists(file_name <- as.character(file_name))) {
                    words <- iconv(paste(unlist(scan(file_name, what = list(name = character()))), collapse = ","), to = "ASCII//TRANSLIT")
                    otherstopwords <- tclVar(words)
                    tcl(otherstopwords_entry, "configure", "-textvariable", otherstopwords)
                }
            })

        put_label(label_frame, "Stemming: ", 7, 0)
        steam_check <- ttkcheckbutton(label_frame, variable = steam)
        tkgrid(steam_check, row = 7, column = 1, sticky = "ew", padx = 2)

        put_label(label_frame, "Completion: ", 8, 0)
        steam_comp <- ttkcheckbutton(label_frame, variable = steamcomp)
        tkgrid(steam_comp, row = 8, column = 1, sticky = "ew", padx = 2)

        tkbind(steam_check, "<ButtonRelease-1>", function() {
                if(tclvalue(steam) != "1") tcl(steam_comp, "configure", "-state", "normal")
                else tcl(steam_comp, "configure", "-state", "disabled")
            })

        tkbind(steam_comp, "<ButtonRelease-1>", function() {
                if(tclvalue(steamcomp) != "1") 
                    tkmessageBox(title = "Warning", message = "Warning:", icon = "warning", 
                                     detail = "This can take few minutes and use a lot of cpu.", 
                                     type = "ok")
            })

        put_label(label_frame, "Bigrams: ", 9, 0)
        bigrams_check <- ttkcheckbutton(label_frame, variable = bigrams)
        tkgrid(bigrams_check, row = 9, column = 1, sticky = "ew", padx = 2)

        put_label(label_frame, "Normalize: ", 10, 0)
        radio <- ttkframe(label_frame)
        sapply(c("chara-value","tf-idf","media","none"), function(i) {
            normalize_button <- ttkradiobutton(radio, variable = normalize, text = i, value = i)
            tkpack(normalize_button, side = "left")
        })
        tkgrid(radio, row = 10, column = 1, sticky = "ew", padx = 2)

        put_label(label_frame, "Remove Sparse: ", 11, 0, sticky = "es")
        removebar <- tkscale(label_frame, from = 0.499, to = 1, variable = sparse, 
                             showvalue = TRUE, resolution = 0.001, orient = "horiz")
        tkgrid(removebar, row = 11, column = 1, sticky = "ew", padx = 2)
        
        button_frame <- ttkframe(frame)
        cancel_button <- ttkbutton(button_frame, text = "cancel",
            command = function() { 
                tkdestroy(window) 
            })
        ok_button <- ttkbutton(button_frame, text = "ok",
            command = function() { 
                env$group <- tclvalue(group)
                env$text <- tclvalue(text)
                env$lang <- tclvalue(lang)
                env$time <- tclvalue(time)
                env$sparse <- as.numeric(tclvalue(sparse))
                env$steam <- if( tclvalue(steam) == "1" ) TRUE else FALSE
                env$steamcomp <- if( tclvalue(steamcomp) == "1" ) TRUE else FALSE
                env$stopwords <- if( tclvalue(stopwords) == "1" ) TRUE else FALSE
                env$otherstopwords <- tclvalue(otherstopwords)
                env$normalize <- tclvalue(normalize)
                env$bigrams <- if( tclvalue(bigrams) == "1" ) TRUE else FALSE

                if(env$text != "" && env$lang != "") {
                    if(env$time == "")
                        TMP <- env$DATA %>% select(env$text)
                    else {
                        if(class(env$DATA[[env$time]]) == "Date")
                            TMP <- env$DATA %>% select(env$time, env$text)
                        else
                            env$time <- ""
                    }

                    if(env$group == "") {
                        TMP$GROUP <- "dummy"
                        normalize <- if(normalize == "tf-idf") "tf-idf" else "none"
                    } else
                        TMP$GROUP <- env$DATA[[env$group]]

                    colnames(TMP) <- if(env$time == "") c("TEXT", "GROUP") else c("TIME", "TEXT", "GROUP")

                    assign("tm", DataTM(TMP, language = env$lang, steam = env$steam, sparse = env$sparse, normalize = env$normalize, ngrams = env$bigrams, steamcomp = env$steamcomp, stopwords = env$stopwords, otherstopwords = env$otherstopwords), envir = env)
                    env$tm$seed <- seed
                    dataFrameTable(env$tm$data)
                    tkdestroy(window) 
                    
                    tkentryconfigure(menu_bar, 3, state = "normal")
                    tkentryconfigure(data_menu, 2, state = "normal")
                    tkentryconfigure(data_menu, 4, state = "normal")
                    tkentryconfigure(data_menu, 5, state = "normal")
                    tkentryconfigure(data_menu, 6, state = "normal")
                    tkentryconfigure(file_menu, 8, state = "normal")
                    tkentryconfigure(file_menu, 11, state = "normal")

                    if(ncol(env$tm$data) < 3) {
                        tkmessageBox(title = "Warning", message = "Warning:", icon = "warning", 
                                     detail = "Some analysis cannot be implemented.", 
                                     type = "ok")

                        tkentryconfigure(analysis_menu, 7, state = "disabled")
                        tkentryconfigure(analysis_menu, 8, state = "disabled")
                        tkentryconfigure(analysis_menu, 9, state = "disabled")
                        tkentryconfigure(analysis_menu, 10, state = "disabled")
                    }else{
                        tkentryconfigure(analysis_menu, 8, state = "normal")
                        tkentryconfigure(analysis_menu, 10, state = "normal")
                        if(requireNamespace("corrr", quietly = TRUE)) tkentryconfigure(analysis_menu, 7, state = "normal")
                        if(requireNamespace("ca", quietly = TRUE)) tkentryconfigure(analysis_menu, 9, state = "normal")
                    }

                    if(env$bigrams) tkentryconfigure(analysis_menu, 4, state = "normal")
                    else tkentryconfigure(analysis_menu, 4, state = "disabled")
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
        assign("DATA", readxl::read_excel(file_name), envir = env)                                                                                                                                                                                       
        dataFrameTable(env$DATA)
        disableMenu()
    }

    # Read Json
    ReadJson <- function(file_name) {
        rm(list = ls(envir = env), envir = env)
        assign("DATA", jsonlite::fromJSON(file_name), envir = env)                                                                                                                                                                                    
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

        window <- tktoplevel(width = 360, height = 200)
        tkwm.minsize(window, "360", "200")
        tkwm.maxsize(window, "360", "200")

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
        tkentryconfigure(data_menu, 2, state = "normal")
        tkentryconfigure(data_menu, 4, state = "normal")
        tkentryconfigure(data_menu, 5, state = "normal")
        tkentryconfigure(data_menu, 6, state = "normal")
        tkentryconfigure(file_menu, 8, state = "normal")
        tkentryconfigure(file_menu, 11, state = "normal")

        if(ncol(env$tm$data) < 3) {
            tkentryconfigure(analysis_menu, 7, state = "disabled")
            tkentryconfigure(analysis_menu, 8, state = "disabled")
            tkentryconfigure(analysis_menu, 9, state = "disabled")
            tkentryconfigure(analysis_menu, 10, state = "disabled")
        }else{
            tkentryconfigure(analysis_menu, 8, state = "normal")
            tkentryconfigure(analysis_menu, 10, state = "normal")
            if(requireNamespace("corrr", quietly = TRUE)) tkentryconfigure(analysis_menu, 7, state = "normal")
            if(requireNamespace("ca", quietly = TRUE)) tkentryconfigure(analysis_menu, 9, state = "normal")
        }

        env$group <- ""
        env$text <- ""
        env$time <- ""
        env$steamcomp <- env$tm$steamcomp
        env$stopwords <- env$tm$stopwords
        env$otherstopwords <- env$tm$otherstopwords
        env$lang <- env$tm$lang
        env$steam <- env$tm$steam
        env$normalize <- env$tm$normalize
        env$sparse <- env$tm$sparse
        env$bigrams <- env$tm$ngrams
    }

    disableMenu <- function() {
        tkentryconfigure(menu_bar, 2, state = "normal")
        tkentryconfigure(menu_bar, 3, state = "disabled")
        tkentryconfigure(data_menu, 2, state = "disabled")
        tkentryconfigure(data_menu, 4, state = "disabled")
        tkentryconfigure(data_menu, 5, state = "disabled")
        tkentryconfigure(data_menu, 6, state = "disabled")
        tkentryconfigure(file_menu, 8, state = "disabled")
        tkentryconfigure(file_menu, 11, state = "disabled")

        env$group <- ""
        env$text <- ""
        env$lang <- ""
        env$time <- ""
        env$steam <- FALSE
        env$steamcomp <- FALSE
        env$stopwords <- TRUE
        env$otherstopwords <- ""
        env$normalize <- "chara-value"
        env$sparse <- 0.998
        env$bigrams <- FALSE

        rm(list = ls(envir = toprint), envir = toprint)
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
            assign("x", NULL, envir = TextMiningGUIEnv)
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

    tkadd(file_menu, "command", label = "Save project...", state = "disabled",
        command = function() {
            if(file.exists("project.RData")) file.remove("project.RData")
            save(list = ls(envir = env), file = "project.RData", envir = env)
            tkmessageBox(title = "Save", message = "Save:", detail = "Project saved.", type = "ok")
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

    tkadd(file_menu, "command", label = "Export to pdf...", state = "disabled",
        command = function() {
            Export(env$tm)
        })

    tkadd(file_menu, "separator")

    tkadd(file_menu, "command", label = "Quit",
        command = function() {
            assign("x", NULL, envir = TextMiningGUIEnv)
            tkdestroy(window)
        })

    # Data
    tkadd(data_menu, "command", label = "Converter Columns", command = Converter)

    tkadd(data_menu, "command", label = "Transform", command = Transform)

    tkadd(data_menu, "command", label = "Slice Groups", command = Slice, state = "disabled")

    tkadd(data_menu, "separator")

    tkadd(data_menu, "command", label = "View Data", 
        command = function() {
            dataFrameTable(env$DATA)
        })

    tkadd(data_menu, "command", label = "View Lexical Table", state = "disabled",
        command = function() {
            dataFrameTable(env$tm$data)
        })

    tkadd(data_menu, "command", label = "View Clean Data", state = "disabled",
        command = function() {
            dataFrameTable(env$tm$df)
        })

    tkadd(data_menu, "separator")

    tkadd(data_menu, "command", label = "Console", command = function() { console(start = TRUE) })

    # Analysis
    tkadd(analysis_menu, "command", label = "Statistics", command = function() {
            console(start = TRUE)
            console(cmds = "tm", envir = env)
        })
 
    tkadd(analysis_menu, "command", label = "Most common words",
        command = function() CommonPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "Word Group", 
        command = function() GroupPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "Word Cloud", 
        command = function() WordCloudPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "Co-occurrence", state = "disabled",
        command = function() cooccPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "separator")

    tkadd(analysis_menu, "command", label = "Cluster",
        command = function() ClusterPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "Correlation", state = "disabled", 
        command = function() CorrelationPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "Correlation Between Two Groups", 
        command = function() CorBetweenGroupsPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "AFC", state = "disabled", 
        command = function() CaPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "HJ-Biplot", 
        command = function() HJBiplotPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "Emotions & Sentiments", 
        command = function() EmotionsPage(X = env$tm, parent = window, notebook = notebook, envir = env))

    tkadd(analysis_menu, "command", label = "Topic Models", state = "disabled",
        command = function() TopicModelsPage(X = env$tm, parent = window, notebook = notebook, envir = env))

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

    if(requireNamespace("readxl", quietly = TRUE)) tkentryconfigure(file_menu, 1, state = "normal")
    if(requireNamespace("jsonlite", quietly = TRUE)) tkentryconfigure(file_menu, 3, state = "normal")
    if(requireNamespace("topicmodels", quietly = TRUE)) tkentryconfigure(analysis_menu, 13, state = "normal")
    if(requireNamespace("corrr", quietly = TRUE)) tkentryconfigure(analysis_menu, 7, state = "normal")
    if(requireNamespace("ca", quietly = TRUE)) tkentryconfigure(analysis_menu, 9, state = "normal")
    if(requireNamespace("igraph", quietly = TRUE) && requireNamespace("ggraph", quietly = TRUE)) tkentryconfigure(analysis_menu, 4, state = "normal")
    
    TRUE
}