.onAttach <- function(libname, pkgname) {
    msg <- c(
        "",
        "Text Mining GUI Interface",
        "",
        "For help: ?TextMiningGUI",
        ""
    )

    if(!requireNamespace("tkrplot", quietly = TRUE)) {
        msg <- c(msg, "** Install optional package \"tkrplot\" for improve the interface","")
    }

    packageStartupMessage(paste(strwrap(msg), collapse = "\n"))
}