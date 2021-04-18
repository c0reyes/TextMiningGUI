console <- function(start = FALSE, cmds = "", envir = parent.frame()) { 
    windowc <- txt <- NULL   
    consoleEnv <- globalenv()
    
    if(exists("windowc", envir = consoleEnv)) windowc <- get("windowc", envir = consoleEnv)
    if(exists("txt", envir = consoleEnv)) txt <- get("txt", envir = consoleEnv)

    if(start == TRUE) {
        if(!exists("windowc", envir = consoleEnv) || is.null(windowc)) {
            windowc <- tktoplevel()
            tkwm.minsize(windowc, "600", "200")
            tkwm.title(windowc, "Console")
            tkwm.protocol(windowc, "WM_DELETE_WINDOW", function() {
                    tkdestroy(windowc)
                    assign("windowc", NULL, envir = consoleEnv)
                    assign("txt", NULL, envir = consoleEnv)
                })
            frame <- ttkframe(windowc, padding = c(3,3,12,12))
            frame_2 <- ttkframe(windowc)
            tkpack(frame, expand = TRUE, fill = "both")
            tkpack(frame_2, side = "right")
            txt <- tktext(frame, width = 80, height = 24, wrap = "none", font = "courier")
            addScrollbars(frame, txt)
            tktag.configure(txt, "commandTag", foreground = "blue", font = "courier 12 italic")
            tktag.configure(txt, "outputTag", font = "courier 12")
            tktag.configure(txt, "errorTag", foreground = "red", font = "courier 12 bold")
            button_frame <- ttkframe(frame_2)
            close_button <- ttkbutton(button_frame, text = "close",
                command = function() { 
                    tkdestroy(windowc) 
                    assign("windowc", NULL, envir = consoleEnv)
                    assign("txt", NULL, envir = consoleEnv)
                })
            tkpack(button_frame, fill = "x", padx = 5, pady = 5)             
            tkpack(close_button, side = "right", padx = 0)  

            assign("windowc", windowc, envir = consoleEnv)   
            assign("txt", txt, envir = consoleEnv)        
        }
    }else{
        if(!is.null(txt)) {
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

                output <- try(capture.output(eval(cmd, envir = envir)))
                output <- iconv(output, to = "ASCII//TRANSLIT")
                output <- paste(output, collapse = "\n")
                tkinsert(txt, "end", output, "outputTag")
                tkinsert(txt, "end","\n")
                tcl(txt ,"see","end")
            }
        }
    }
}