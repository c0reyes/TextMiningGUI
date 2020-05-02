put_label <- function(parent, text, row, column, sticky = "e") {
    label <- ttklabel(parent, text = text)
    tkgrid(label, row = row, column = column, sticky = sticky)
}