library(ggplot2)
theme_set(theme_bw(base_family = "Century Gothic"))

line.color = "black"

theme_update(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    strip.background = element_rect(fill = "transparent", colour = NA),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 13, margin=margin(-2, 0, 10, 0)),
    text = element_text(size = 14),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, color = "black"),
    axis.text.x = element_text(margin = margin(t = 5, b = 8)),
    axis.text.y = element_text(margin = margin(l = 0, r = 5)),
    legend.text = element_text(size = 14),
    legend.position="top",
    # plot.caption = element_text(hjust = 0, face= "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.line = element_line(colour = line.color, linewidth = 0.5),
    axis.ticks.y = element_line(colour = line.color, linewidth = 0.5),
    axis.ticks.x = element_line(colour = line.color, linewidth = 0.5),
    axis.ticks.length = unit(5, "points"),
    strip.text.x = element_text(size = 14)
)

knitr::opts_chunk$set(
    echo = FALSE, warning = FALSE, message = FALSE,
    fig.width = 1000/96, fig.height = 5,
    dpi = 96*3, dev.args = list(bg = "transparent")
)

# knitr::opts_knit$set(base.dir = output.folder)

COLORS = c("#c3423f", "#86cae7", "#b5c969")

if (exists("output.file")) {
    knitr::opts_chunk$set(fig.path = glue("{output.file}_files/figure-markdown_strict/"))
    rm(output.file)
}

options(OutDec= ",")

rm(line.color)
