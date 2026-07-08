# plots.R -- the TNF figure library --------------------------------------------
#
# Every function returns a ggplot object styled with theme_tnf(). Figures are
# written to figures/ by scripts/build_all.R via save_figure().

#' Attendance at every match night
#' @export
plot_attendance_over_time <- function(db) {
  d <- attendance_series(db)
  ggplot2::ggplot(d, ggplot2::aes(date, attendance)) +
    ggplot2::geom_hline(yintercept = mean(d$attendance), linetype = "dashed",
                        colour = tnf_colors$draw) +
    ggplot2::geom_line(colour = tnf_colors$nonbibs, linewidth = 1) +
    ggplot2::geom_point(colour = tnf_colors$nonbibs, size = 3) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::labs(title = "Who's turning up on Tuesdays?",
                  subtitle = paste0("Players per match night; dashed line = season average (",
                                    round(mean(d$attendance), 1), ")"),
                  x = NULL, y = "Players") +
    theme_tnf()
}

#' Match results as a diverging margin chart
#' @export
plot_results_timeline <- function(db) {
  d <- db$matches |>
    dplyr::filter(!is.na(result)) |>
    dplyr::mutate(signed_margin = ifelse(result == "Non-Bibs", -margin, margin))
  ggplot2::ggplot(d, ggplot2::aes(date, signed_margin, fill = result)) +
    ggplot2::geom_col(width = 4.5) +
    ggplot2::geom_hline(yintercept = 0, colour = tnf_colors$ink) +
    ggplot2::geom_point(data = d |> dplyr::filter(result == "Draw"),
                        colour = tnf_colors$draw, size = 3.2,
                        show.legend = FALSE) +
    scale_fill_tnf() +
    ggplot2::labs(title = "Every result, at a glance",
                  subtitle = "Bars above the line are Bibs wins, below are Non-Bibs wins; bar height = winning margin",
                  x = NULL, y = "Winning margin (goals)") +
    theme_tnf()
}

#' Overall team record bar chart
#' @export
plot_team_wins <- function(db) {
  d <- db$matches |>
    dplyr::filter(!is.na(result)) |>
    dplyr::count(result) |>
    dplyr::mutate(result = factor(result, c("Bibs", "Draw", "Non-Bibs")))
  ggplot2::ggplot(d, ggplot2::aes(result, n, fill = result)) +
    ggplot2::geom_col(width = 0.65, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = n), vjust = -0.4, fontface = "bold",
                       size = 5) +
    scale_fill_tnf() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .12))) +
    ggplot2::labs(title = "Who wins Tuesday nights?",
                  subtitle = "Match outcomes across the whole record",
                  x = NULL, y = "Matches") +
    theme_tnf()
}

#' Violin + jitter of goals scored per match by each side
#' @export
plot_goals_violin <- function(db) {
  d <- db$teams |> dplyr::filter(!is.na(goals_for))
  ggplot2::ggplot(d, ggplot2::aes(team, goals_for, fill = team)) +
    ggplot2::geom_violin(alpha = 0.6, colour = NA, show.legend = FALSE) +
    ggplot2::geom_jitter(width = 0.08, size = 2, alpha = 0.7,
                         show.legend = FALSE,
                         ggplot2::aes(colour = team)) +
    scale_fill_tnf() + scale_color_tnf() +
    ggplot2::labs(title = "Scoring distributions",
                  subtitle = "Goals scored per match by each side; every dot is one match",
                  x = NULL, y = "Goals in a match") +
    theme_tnf()
}

#' Ridgeline of goals per match by month
#' @export
plot_goals_ridgeline <- function(db) {
  d <- db$matches |>
    dplyr::filter(!is.na(total_goals)) |>
    dplyr::mutate(month = factor(format(date, "%b %Y"),
                                 levels = unique(format(sort(date), "%b %Y"))))
  ggplot2::ggplot(d, ggplot2::aes(total_goals, month, fill = ggplot2::after_stat(x))) +
    ggridges::geom_density_ridges_gradient(scale = 1.4, rel_min_height = 0.01,
                                           colour = "white") +
    ggplot2::scale_fill_viridis_c(option = "C", guide = "none") +
    ggplot2::labs(title = "Total goals per night, month by month",
                  subtitle = "Distribution of combined scorelines in each month",
                  x = "Total goals in a match", y = NULL) +
    theme_tnf()
}

#' Lollipop chart of appearances per player
#' @export
plot_appearances_lollipop <- function(db, top = 25) {
  d <- player_stats(db) |>
    dplyr::filter(!is_guest) |>
    dplyr::slice_max(appearances, n = top) |>
    dplyr::mutate(player_name = forcats::fct_reorder(player_name, appearances))
  ggplot2::ggplot(d, ggplot2::aes(appearances, player_name)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = appearances,
                                       yend = player_name),
                          colour = tnf_colors$grid, linewidth = 1.2) +
    ggplot2::geom_point(colour = tnf_colors$bibs, size = 4) +
    ggplot2::geom_text(ggplot2::aes(label = appearances), hjust = -0.9,
                       size = 3.2, fontface = "bold") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .08))) +
    ggplot2::labs(title = "The ever-presents",
                  subtitle = "Total appearances per player",
                  x = "Appearances", y = NULL) +
    theme_tnf() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Win percentage with Wilson confidence intervals
#' @export
plot_win_pct_ci <- function(db, min_apps = 5) {
  d <- player_stats(db) |>
    dplyr::filter(appearances >= min_apps, !is_guest) |>
    dplyr::mutate(player_name = forcats::fct_reorder(player_name, win_pct))
  ggplot2::ggplot(d, ggplot2::aes(win_pct, player_name)) +
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed",
                        colour = tnf_colors$draw) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper),
                            height = 0.25, colour = tnf_colors$nonbibs) +
    ggplot2::geom_point(ggplot2::aes(size = appearances),
                        colour = tnf_colors$accent) +
    ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::scale_size_continuous(range = c(2.5, 5), guide = "none") +
    ggplot2::labs(title = "Win rates, with honesty bars",
                  subtitle = paste0("Players with ", min_apps,
                                    "+ appearances; whiskers are 95% Wilson intervals - wide bars mean small samples"),
                  x = "Win percentage", y = NULL) +
    theme_tnf() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Diverging goal-difference bars
#' @export
plot_goal_diff <- function(db, min_apps = 5) {
  d <- player_stats(db) |>
    dplyr::filter(appearances >= min_apps, !is_guest) |>
    dplyr::mutate(player_name = forcats::fct_reorder(player_name, goal_diff),
                  sign = ifelse(goal_diff >= 0, "positive", "negative"))
  ggplot2::ggplot(d, ggplot2::aes(goal_diff, player_name, fill = sign)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_vline(xintercept = 0, colour = tnf_colors$ink) +
    ggplot2::scale_fill_manual(values = c(positive = tnf_colors$good,
                                          negative = tnf_colors$bad)) +
    ggplot2::labs(title = "Lucky charms and bad omens",
                  subtitle = paste0("Team goal difference while each player (",
                                    min_apps, "+ apps) is on the pitch"),
                  x = "Goal difference", y = NULL) +
    theme_tnf() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Co-occurrence heatmap: how often each pair share a team
#' @export
plot_co_occurrence <- function(db, min_apps = 5) {
  regulars <- player_stats(db) |>
    dplyr::filter(appearances >= min_apps, !is_guest) |>
    dplyr::pull(player_name)
  m <- co_occurrence_matrix(db)
  m <- m[rownames(m) %in% regulars, colnames(m) %in% regulars]
  d <- as.data.frame.table(m, responseName = "together") |>
    dplyr::rename(a = Var1, b = Var2)
  ggplot2::ggplot(d, ggplot2::aes(a, b, fill = together)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(together > 0, together, "")),
                       size = 2.6) +
    ggplot2::scale_fill_gradient(low = "#FFF7E6", high = tnf_colors$bibs) +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = "Who plays with whom",
                  subtitle = "Matches on the same team, all regular pairs",
                  x = NULL, y = NULL, fill = "Together") +
    theme_tnf() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   panel.grid = ggplot2::element_blank(),
                   legend.position = "right",
                   legend.title = ggplot2::element_text(face = "bold"))
}

#' Head-to-head win-rate heatmap
#' @export
plot_h2h_heatmap <- function(db, min_apps = 8, min_meetings = 4) {
  regulars <- player_stats(db) |>
    dplyr::filter(appearances >= min_apps, !is_guest) |>
    dplyr::pull(player_name)
  d <- head_to_head(db, min_meetings) |>
    dplyr::filter(player %in% regulars, opponent %in% regulars)
  ggplot2::ggplot(d, ggplot2::aes(opponent, player, fill = win_pct)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = fmt_pct(win_pct)), size = 2.6) +
    ggplot2::scale_fill_gradient2(low = tnf_colors$nonbibs, mid = "white",
                                  high = tnf_colors$bibs, midpoint = 0.5,
                                  labels = scales::percent) +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = "Who beats whom",
                  subtitle = paste0("Row player's win rate when facing the column player (",
                                    min_meetings, "+ meetings)"),
                  x = "… against", y = "Win rate of …",
                  fill = "Win rate") +
    theme_tnf() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   panel.grid = ggplot2::element_blank(),
                   legend.position = "right",
                   legend.title = ggplot2::element_text(face = "bold"))
}

#' The TNF network diagram
#' @export
plot_network <- function(db, min_together = 3) {
  g <- build_player_graph(db, min_together) |>
    tidygraph::activate(nodes) |>
    dplyr::filter(!tidygraph::node_is_isolated()) |>
    dplyr::mutate(community = as.factor(tidygraph::group_louvain(weights = weight)))
  set.seed(42)
  ggraph::ggraph(g, layout = "fr", weights = weight) +
    ggraph::geom_edge_link(ggplot2::aes(width = weight, colour = win_rate),
                           alpha = 0.75) +
    ggraph::geom_node_point(ggplot2::aes(size = appearances, fill = community),
                            shape = 21, colour = "white", stroke = 1.2) +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE,
                           fontface = "bold", size = 3.2) +
    ggraph::scale_edge_width(range = c(0.3, 2.6), guide = "none") +
    ggraph::scale_edge_colour_gradient2(low = tnf_colors$bad, mid = "#D9D9D9",
                                        high = tnf_colors$good, midpoint = 0.5,
                                        labels = scales::percent,
                                        name = "Pair win rate") +
    ggplot2::scale_size_continuous(range = c(3, 10), name = "Appearances") +
    ggplot2::scale_fill_viridis_d(option = "G", begin = 0.25, end = 0.95,
                                  name = "Community") +
    ggplot2::labs(title = "The Tuesday night network",
                  subtitle = paste0("Players linked by ", min_together,
                                    "+ matches together; line thickness = matches together, colour = win rate as a pair"),
                  caption = "Communities found with the Louvain algorithm") +
    ggraph::theme_graph(base_family = "sans") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 17),
                   legend.position = "right")
}

#' PageRank / centrality lollipop
#' @export
plot_centrality <- function(db, metric = "pagerank", top = 15) {
  d <- network_metrics(db) |>
    dplyr::slice_max(.data[[metric]], n = top) |>
    dplyr::mutate(name = forcats::fct_reorder(name, .data[[metric]]))
  ggplot2::ggplot(d, ggplot2::aes(.data[[metric]], name)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data[[metric]], yend = name),
                          colour = tnf_colors$grid, linewidth = 1.2) +
    ggplot2::geom_point(colour = tnf_colors$nonbibs, size = 4) +
    ggplot2::labs(title = "The social hubs of TNF",
                  subtitle = paste0("Player influence by ", metric,
                                    " in the co-appearance network"),
                  x = metric, y = NULL) +
    theme_tnf() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Rolling win percentage lines
#' @export
plot_rolling_winpct <- function(db, k = 5) {
  d <- rolling_win_pct(db, k)
  ggplot2::ggplot(d, ggplot2::aes(date, rolling, colour = team)) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed",
                        colour = tnf_colors$draw) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 2.4) +
    scale_color_tnf() +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::labs(title = "Momentum swings",
                  subtitle = paste0("Rolling win percentage over the last ", k,
                                    " matches for each side"),
                  x = NULL, y = paste0(k, "-match rolling win %")) +
    theme_tnf()
}

#' Bibs' cumulative share of points
#' @export
plot_bib_dominance <- function(db) {
  d <- bib_dominance(db)
  ggplot2::ggplot(d, ggplot2::aes(date, dominance)) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed",
                        colour = tnf_colors$draw) +
    ggplot2::geom_line(colour = tnf_colors$bibs, linewidth = 1.3) +
    ggplot2::geom_point(colour = tnf_colors$bibs, size = 2.6) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::labs(title = "Bib dominance over time",
                  subtitle = "Bibs' cumulative share of available points (win = 1, draw = ½); above the dashed line means Bibs lead the season",
                  x = NULL, y = "Cumulative share of points") +
    theme_tnf()
}

#' Cumulative appearance curves with labelled leaders
#' @export
plot_cumulative_appearances <- function(db, label_top = 8) {
  d <- cumulative_appearances(db)
  leaders <- d |>
    dplyr::group_by(player_name) |>
    dplyr::summarise(total = max(cumulative), last_date = max(date),
                     .groups = "drop") |>
    dplyr::slice_max(total, n = label_top, with_ties = FALSE)
  ggplot2::ggplot(d, ggplot2::aes(date, cumulative, group = player_name)) +
    ggplot2::geom_step(colour = "grey75", linewidth = 0.5) +
    ggplot2::geom_step(data = d |> dplyr::filter(player_name %in% leaders$player_name),
                       ggplot2::aes(colour = player_name), linewidth = 1.1) +
    ggrepel::geom_text_repel(
      data = d |> dplyr::semi_join(leaders, by = "player_name") |>
        dplyr::group_by(player_name) |> dplyr::slice_max(date, n = 1),
      ggplot2::aes(label = player_name, colour = player_name),
      fontface = "bold", size = 3.2, direction = "y", hjust = 0, nudge_x = 4,
      segment.colour = NA, show.legend = FALSE) +
    ggplot2::scale_colour_viridis_d(option = "H", guide = "none") +
    ggplot2::scale_x_date(expand = ggplot2::expansion(mult = c(0.02, 0.16))) +
    ggplot2::labs(title = "The race for most appearances",
                  subtitle = "Cumulative appearances per player; the busiest players are labelled",
                  x = NULL, y = "Cumulative appearances") +
    theme_tnf()
}

#' Calendar heatmap of attendance
#' @export
plot_calendar_heatmap <- function(db) {
  d <- db$matches |>
    dplyr::mutate(month = lubridate::floor_date(date, "month"),
                  week = (lubridate::mday(date) - 1) %/% 7 + 1,
                  month_lab = factor(format(month, "%b %Y"),
                                     levels = unique(format(sort(month), "%b %Y"))))
  ggplot2::ggplot(d, ggplot2::aes(week, forcats::fct_rev(month_lab),
                                  fill = attendance)) +
    ggplot2::geom_tile(colour = "white", linewidth = 1.5) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(format(date, "%d"), "\n",
                                                   attendance)),
                       size = 2.8, fontface = "bold", colour = "grey20",
                       lineheight = 0.9) +
    ggplot2::scale_fill_gradient(low = "#FFF3D6", high = tnf_colors$bibs) +
    ggplot2::coord_fixed(0.6) +
    ggplot2::labs(title = "The Tuesday calendar",
                  subtitle = "Every match night: date (top) and players who turned up (bottom); darker = busier",
                  x = "Week of month", y = NULL, fill = "Players") +
    theme_tnf() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "right",
                   legend.title = ggplot2::element_text(face = "bold"))
}

#' Bump chart of monthly appearance ranks
#' @export
plot_monthly_bump <- function(db, top = 8) {
  d <- player_appearances_by_month(db)
  keep <- player_stats(db) |>
    dplyr::filter(!is_guest) |>
    dplyr::slice_max(appearances, n = top, with_ties = FALSE) |>
    dplyr::pull(player_name)
  d <- d |>
    dplyr::group_by(month) |>
    dplyr::mutate(rank = rank(-appearances, ties.method = "first")) |>
    dplyr::ungroup() |>
    dplyr::filter(player_name %in% keep)
  ggplot2::ggplot(d, ggplot2::aes(month, rank, colour = player_name)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 3) +
    ggrepel::geom_text_repel(
      data = d |> dplyr::group_by(player_name) |> dplyr::slice_max(month, n = 1),
      ggplot2::aes(label = player_name), fontface = "bold", size = 3,
      direction = "y", hjust = 0, nudge_x = 12, segment.colour = NA,
      show.legend = FALSE) +
    ggplot2::scale_y_reverse(breaks = 1:20) +
    ggplot2::scale_colour_viridis_d(option = "H", guide = "none") +
    ggplot2::scale_x_date(expand = ggplot2::expansion(mult = c(0.03, 0.2))) +
    ggplot2::labs(title = "Monthly attendance pecking order",
                  subtitle = paste0("Rank of appearances per month for the ",
                                    top, " busiest players (1 = most matches that month)"),
                  x = NULL, y = "Rank") +
    theme_tnf()
}

#' Player timeline: every appearance coloured by result
#' @export
plot_player_timeline <- function(db) {
  d <- player_match_results(db) |>
    dplyr::filter(!is.na(outcome)) |>
    dplyr::mutate(player_name = forcats::fct_reorder(player_name, date,
                                                     .fun = min, .desc = TRUE),
                  outcome = factor(outcome, c("W", "D", "L")))
  ggplot2::ggplot(d, ggplot2::aes(date, player_name, colour = outcome)) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::scale_colour_manual(values = c(W = tnf_colors$good,
                                            D = tnf_colors$draw,
                                            L = tnf_colors$bad),
                                 labels = c("Win", "Draw", "Loss")) +
    ggplot2::labs(title = "Every player's Tuesday story",
                  subtitle = "One dot per appearance, coloured by result; players ordered by debut date",
                  x = NULL, y = NULL) +
    theme_tnf() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 8))
}

#' Alluvial: how each side's matches flow into results
#' @export
plot_results_alluvial <- function(db) {
  d <- db$teams |>
    dplyr::filter(!is.na(outcome)) |>
    dplyr::mutate(outcome = factor(dplyr::recode(outcome, W = "Win", D = "Draw",
                                                 L = "Loss"),
                                   c("Win", "Draw", "Loss"))) |>
    dplyr::count(team, outcome)
  ggplot2::ggplot(d, ggplot2::aes(y = n, axis1 = team, axis2 = outcome)) +
    ggalluvial::geom_alluvium(ggplot2::aes(fill = team), width = 1/6,
                              alpha = 0.85) +
    ggalluvial::geom_stratum(width = 1/6, fill = "grey95", colour = "grey40") +
    ggplot2::geom_text(stat = ggalluvial::StatStratum,
                       ggplot2::aes(label = ggplot2::after_stat(stratum)),
                       fontface = "bold", size = 3.4) +
    scale_fill_tnf() +
    ggplot2::scale_x_discrete(limits = c("Side", "Result"),
                              expand = c(0.08, 0.08)) +
    ggplot2::labs(title = "From bib colour to full-time result",
                  subtitle = "How each side's 21 matches split into wins, draws and losses",
                  y = "Matches") +
    theme_tnf() +
    ggplot2::theme(legend.position = "none")
}

#' Top partnerships bar chart with CI
#' @export
plot_partnerships <- function(db, min_together = 5, top = 12) {
  d <- pair_stats(db, min_together) |>
    dplyr::slice_max(win_pct, n = top, with_ties = FALSE) |>
    dplyr::mutate(pair = forcats::fct_reorder(paste(player_a, "+", player_b),
                                              win_pct))
  ggplot2::ggplot(d, ggplot2::aes(win_pct, pair)) +
    ggplot2::geom_col(fill = tnf_colors$bibs, width = 0.7) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper),
                            height = 0.2, colour = tnf_colors$ink) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(wins, "W/", together)),
                       hjust = -0.15, size = 3, fontface = "bold") +
    ggplot2::scale_x_continuous(labels = scales::percent,
                                limits = c(0, 1.05),
                                expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(title = "Dream teams",
                  subtitle = paste0("Best win rates among pairs with ", min_together,
                                    "+ matches together (whiskers: 95% Wilson CI)"),
                  x = "Win percentage together", y = NULL) +
    theme_tnf() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Longest personal win streaks
#' @export
plot_streaks <- function(db, top = 12) {
  d <- player_stats(db) |>
    dplyr::filter(!is_guest) |>
    dplyr::slice_max(longest_win_streak, n = top, with_ties = FALSE) |>
    dplyr::mutate(player_name = forcats::fct_reorder(player_name,
                                                     longest_win_streak))
  ggplot2::ggplot(d, ggplot2::aes(longest_win_streak, player_name)) +
    ggplot2::geom_col(fill = tnf_colors$good, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = longest_win_streak), hjust = -0.4,
                       fontface = "bold") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .1)),
                                breaks = scales::breaks_width(1)) +
    ggplot2::labs(title = "Hot streaks",
                  subtitle = "Longest run of consecutive wins per player",
                  x = "Consecutive wins", y = NULL) +
    theme_tnf() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' All report figures as a named list (used by build_all and the app)
#' @export
all_figures <- function(db) {
  list(
    attendance_over_time = plot_attendance_over_time(db),
    results_timeline = plot_results_timeline(db),
    team_wins = plot_team_wins(db),
    goals_violin = plot_goals_violin(db),
    goals_ridgeline = plot_goals_ridgeline(db),
    appearances_lollipop = plot_appearances_lollipop(db),
    win_pct_ci = plot_win_pct_ci(db),
    goal_diff = plot_goal_diff(db),
    co_occurrence = plot_co_occurrence(db),
    h2h_heatmap = plot_h2h_heatmap(db),
    network = plot_network(db),
    centrality = plot_centrality(db),
    rolling_winpct = plot_rolling_winpct(db),
    bib_dominance = plot_bib_dominance(db),
    cumulative_appearances = plot_cumulative_appearances(db),
    calendar_heatmap = plot_calendar_heatmap(db),
    monthly_bump = plot_monthly_bump(db),
    player_timeline = plot_player_timeline(db),
    results_alluvial = plot_results_alluvial(db),
    partnerships = plot_partnerships(db),
    streaks = plot_streaks(db)
  )
}
