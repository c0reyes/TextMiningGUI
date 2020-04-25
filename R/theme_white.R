theme_white <- function (base_size = 11, base_family = "") {
    theme_light() %+replace% 
    theme(
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "white", fill = NA),
        axis.line = element_line(color = "white"),
        axis.ticks = element_line(color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
}