addScrollbars <- function(parent, widget, horiz = TRUE) {
    if(horiz)
        xscr <- ttkscrollbar(parent, orient = "horizontal",
                       command = function(...) tkxview(widget, ...))
    
    yscr <- ttkscrollbar(parent, orient = "vertical",
                       command = function(...) tkyview(widget, ...))
    if(horiz)
        tkconfigure(widget,
              xscrollcommand = function(...) tkset(xscr,...),
              yscrollcommand = function(...) tkset(yscr,...))
    else
        tkconfigure(widget,
              yscrollcommand = function(...) tkset(yscr,...))
    
    tkgrid(widget, row = 0, column = 0, sticky = "news")
    tkgrid(yscr, row = 0, column = 1, sticky = "ns")
    if(horiz)
        tkgrid(xscr, row = 1, column = 0, sticky = "ew")
    tkgrid.columnconfigure(parent, 0, weight = 1)
    tkgrid.rowconfigure(parent, 0, weight = 1)
}