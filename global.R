# global.R -- shared setup for the TNF Shiny app ------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(lubridate)
  library(DT)
  library(plotly)
  library(tidygraph)
  library(ggraph)
})

for (f in list.files("R", full.names = TRUE)) source(f)

RAW_PATH <- "data-raw/team_sheets_raw.txt"
ALIAS_PATH <- "data-raw/player_aliases.csv"

# Build (or load) the database once at startup; the app keeps its own
# reactive copy that upload/alias edits refresh.
initial_db <- if (file.exists("data/tnf_database.rds")) {
  load_database("data")
} else {
  db <- build_database(RAW_PATH, ALIAS_PATH)
  save_database(db, "data")
  db
}

tnf_theme <- bs_theme(
  version = 5,
  preset = "flatly",
  primary = "#1F5FA8",
  secondary = "#F2A900",
  base_font = font_collection("Segoe UI", "Helvetica Neue", "sans-serif"),
  heading_font = font_collection("Segoe UI", "Helvetica Neue", "sans-serif")
)

#' Interactive plotly version of the player network
plotly_network <- function(db, min_together = 3) {
  g <- build_player_graph(db, min_together) |>
    activate(nodes) |>
    filter(!node_is_isolated())
  ig <- as.igraph(g)
  set.seed(42)
  xy <- igraph::layout_with_fr(ig, weights = igraph::E(ig)$weight)
  nodes <- g |> activate(nodes) |> as_tibble() |>
    mutate(x = xy[, 1], y = xy[, 2])
  edges <- g |> activate(edges) |> as_tibble() |>
    mutate(x0 = nodes$x[from], y0 = nodes$y[from],
           x1 = nodes$x[to], y1 = nodes$y[to],
           col = scales::col_numeric(
             c("#C7361F", "#D9D9D9", "#2E8B57"), c(0, 1))(win_rate))

  p <- plot_ly()
  for (i in seq_len(nrow(edges))) {
    p <- add_segments(p, x = edges$x0[i], xend = edges$x1[i],
                      y = edges$y0[i], yend = edges$y1[i],
                      line = list(color = edges$col[i],
                                  width = 0.8 + edges$weight[i] / 2),
                      opacity = 0.6, showlegend = FALSE, hoverinfo = "none")
  }
  p |>
    add_markers(data = nodes, x = ~x, y = ~y,
                size = ~appearances, sizes = c(12, 42),
                color = ~community, colors = "viridis",
                text = ~paste0("<b>", name, "</b><br>",
                               appearances, " appearances<br>",
                               "win rate ", fmt_pct(win_pct), "<br>",
                               "community (friend cluster) ", community),
                hoverinfo = "text", showlegend = FALSE) |>
    add_text(data = nodes, x = ~x, y = ~y,
             text = ~sub(" ", "\n", name),
             textposition = "top center",
             textfont = list(size = 9, color = "#1A1A1A"),
             showlegend = FALSE, hoverinfo = "none") |>
    layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
           plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
           autosize = TRUE, dragmode = "pan",
           margin = list(l = 10, r = 10, t = 10, b = 10)) |>
    config(responsive = TRUE, displayModeBar = FALSE, scrollZoom = TRUE)
}
