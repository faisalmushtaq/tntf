# theme.R -- publication-quality ggplot styling for TNF -----------------------
#
# The house style borrows from ggprism and the BBC's bbplot: bold left-aligned
# titles, no chart junk, generous whitespace, thick prism-style axes. If the
# optional ggprism package is installed it is used as the base; otherwise an
# equivalent hand-rolled theme is applied, so the project has no hard
# dependency on packages outside the standard repositories.

#' TNF colour palette
#' @export
tnf_colors <- list(
  bibs = "#F2A900",       # fluorescent training-bib orange
  nonbibs = "#1F5FA8",    # away-day blue
  draw = "#8C8C8C",
  ink = "#1A1A1A",
  accent = "#C7361F",
  grid = "#E6E6E6",
  good = "#2E8B57",
  bad = "#C7361F"
)

#' Named team colour scale values
#' @export
team_palette <- c("Bibs" = "#F2A900", "Non-Bibs" = "#1F5FA8", "Draw" = "#8C8C8C")

#' The TNF house ggplot theme
#'
#' @param base_size base font size
#' @export
theme_tnf <- function(base_size = 13) {
  base <- if (requireNamespace("ggprism", quietly = TRUE)) {
    ggprism::theme_prism(base_size = base_size)
  } else {
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        axis.line = ggplot2::element_line(colour = tnf_colors$ink, linewidth = 0.8),
        axis.ticks = ggplot2::element_line(colour = tnf_colors$ink, linewidth = 0.8),
        axis.text = ggplot2::element_text(colour = tnf_colors$ink, face = "bold"),
        axis.title = ggplot2::element_text(face = "bold")
      )
  }
  base +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size * 1.35,
                                         hjust = 0),
      plot.subtitle = ggplot2::element_text(colour = "#4D4D4D", hjust = 0,
                                            margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(colour = "#8C8C8C", hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = tnf_colors$grid,
                                                 linewidth = 0.4),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(15, 15, 12, 15)
    )
}

#' Team fill/colour scales
#' @export
scale_fill_tnf <- function(...) ggplot2::scale_fill_manual(values = team_palette, ...)
#' @rdname scale_fill_tnf
#' @export
scale_color_tnf <- function(...) ggplot2::scale_color_manual(values = team_palette, ...)

#' Save a figure to figures/ at report quality
#' @export
save_figure <- function(plot, name, width = 9, height = 6, dir = "figures") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(file.path(dir, paste0(name, ".png")), plot,
                  width = width, height = height, dpi = 300, bg = "white")
  invisible(plot)
}
