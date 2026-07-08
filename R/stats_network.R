# stats_network.R -- the TNF social network -----------------------------------

#' Build the player co-appearance graph
#'
#' Nodes are players (guests excluded by default); edges connect players who
#' have shared a team, weighted by matches together and carrying the pair's
#' win rate. Node-level centrality measures and Louvain communities are
#' attached.
#'
#' @param db database list
#' @param min_together drop edges lighter than this (default 1)
#' @param include_guests keep guest players in the graph
#' @return a tidygraph tbl_graph
#' @export
build_player_graph <- function(db, min_together = 1, include_guests = FALSE) {
  ps <- pair_stats(db, min_together)
  keep <- db$players |>
    dplyr::filter(include_guests | !is_guest) |>
    dplyr::pull(player_name)
  edges <- ps |>
    dplyr::filter(player_a %in% keep, player_b %in% keep) |>
    dplyr::transmute(from = player_a, to = player_b,
                     weight = together, win_rate = win_pct)
  apps <- player_stats(db) |>
    dplyr::select(player_name, appearances, win_pct)

  g <- tidygraph::tbl_graph(
    nodes = tibble::tibble(name = keep) |>
      dplyr::left_join(apps, by = c(name = "player_name")),
    edges = edges, directed = FALSE
  )
  g |>
    tidygraph::activate(nodes) |>
    dplyr::mutate(
      degree = tidygraph::centrality_degree(),
      strength = tidygraph::centrality_degree(weights = weight),
      betweenness = tidygraph::centrality_betweenness(weights = 1 / weight),
      closeness = tidygraph::centrality_closeness(weights = 1 / weight),
      pagerank = tidygraph::centrality_pagerank(weights = weight),
      eigen = tidygraph::centrality_eigen(weights = weight),
      community = as.factor(tidygraph::group_louvain(weights = weight))
    )
}

#' Node-level network metrics as a plain tibble, ranked by PageRank
#' @export
network_metrics <- function(db, ...) {
  build_player_graph(db, ...) |>
    tidygraph::activate(nodes) |>
    tibble::as_tibble() |>
    dplyr::arrange(dplyr::desc(pagerank))
}
