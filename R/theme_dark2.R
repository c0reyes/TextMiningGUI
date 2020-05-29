theme_dark2 <- function (base_size = 11, base_family = "") {
    theme_light() %+replace% 
    theme(
        line = element_line(colour = "#ffffff", size = 0.5, 
                      linetype = 1, lineend = "butt"), 
        rect = element_rect(fill = "#ffffff", colour = "#ffffff",
                      size = 0.5, linetype = 1),
        text = element_text(family = base_family, face = "plain",
                      colour = "#ffffff", size = base_size,
                      lineheight = 0.9,  hjust = 0.5,
                      vjust = 0.5, angle = 0, 
                      margin = margin(), debug = FALSE), 
        plot.background = element_rect(fill = "#666666"),
        panel.background = element_rect(fill = "#666666"),
        panel.border = element_rect(color = "#666666", fill = NA),
        axis.line = element_line(color = "#666666"),
        axis.ticks = element_line(color = "#666666"),
        axis.text = element_text(size = rel(0.8), colour = "#ffffff"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "#666666")
      )
}