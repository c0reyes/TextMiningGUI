Page <- function(parent, name) {
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