tmPalette <- function(name, n) {
    colorRampPalette(brewer.pal(brewer.pal.info[name,]$maxcolors, name))(n)
}