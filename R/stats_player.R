# stats_player.R -- per-player statistics -------------------------------------

#' Appearance-level results for every player
#'
#' One row per player per match with that player's team, result and goals,
#' ordered by date. Everything else in this file builds on it.
#' @export
player_match_results <- function(db) {
  db$appearances |>
    dplyr::inner_join(db$teams, by = c("match_id", "date", "team")) |>
    dplyr::inner_join(db$players, by = "player_id") |>
    dplyr::arrange(date) |>
    dplyr::select(player_id, player_name, is_guest, match_id, date, team,
                  goals_for, goals_against, outcome)
}

#' The big player statistics table
#'
#' Appearances, W/D/L, win % with a Wilson confidence interval, goals for and
#' against while on the pitch, goal difference, streaks, attendance
#' consistency (share of matches attended since first appearance) and
#' first/latest appearance. `small_sample` flags records built on fewer than
#' `small_n` matches.
#'
#' @param db database list
#' @param small_n threshold below which win % should be taken with a pinch of
#'   salt (default 8)
#' @export
player_stats <- function(db, small_n = 8) {
  pmr <- player_match_results(db)
  match_dates <- sort(unique(db$matches$date))

  base <- pmr |>
    dplyr::filter(!is.na(outcome)) |>
    dplyr::group_by(player_id, player_name, is_guest) |>
    dplyr::summarise(
      appearances = dplyr::n(),
      wins = sum(outcome == "W"),
      draws = sum(outcome == "D"),
      losses = sum(outcome == "L"),
      win_pct = wins / appearances,
      goals_for = sum(goals_for),
      goals_against = sum(goals_against),
      goal_diff = goals_for - goals_against,
      avg_goals_for = mean(goals_for),
      avg_goals_against = mean(goals_against),
      longest_win_streak = longest_run(outcome, "W"),
      longest_losing_streak = longest_run(outcome, "L"),
      current_streak = paste0(current_run(outcome)$length,
                              current_run(outcome)$value),
      first_appearance = min(date),
      last_appearance = max(date),
      bibs_apps = sum(team == "Bibs"),
      nonbibs_apps = sum(team == "Non-Bibs"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      wilson_ci(wins, appearances),
      matches_available = purrr::map_int(
        first_appearance, ~ sum(match_dates >= .x)),
      attendance_consistency = appearances / matches_available,
      small_sample = appearances < small_n
    ) |>
    dplyr::arrange(dplyr::desc(appearances), dplyr::desc(win_pct))
  base
}

#' Appearances per player per calendar month
#' @export
player_appearances_by_month <- function(db) {
  player_match_results(db) |>
    dplyr::mutate(month = lubridate::floor_date(date, "month")) |>
    dplyr::count(player_id, player_name, month, name = "appearances")
}

#' Rank players on every numeric metric in player_stats()
#'
#' Returns a long table: one row per player per metric with the player's rank
#' (1 = best). Metrics where lower is better (losses, goals against) are
#' ranked accordingly.
#' @param min_apps minimum appearances to be ranked (default 5)
#' @export
player_rankings <- function(db, min_apps = 5) {
  ps <- player_stats(db) |>
    dplyr::filter(appearances >= min_apps, !is_guest)
  lower_better <- c("losses", "goals_against", "avg_goals_against",
                    "longest_losing_streak")
  ps |>
    dplyr::select(player_name, appearances, wins, losses, draws, win_pct,
                  goals_for, goals_against, goal_diff, avg_goals_for,
                  avg_goals_against, longest_win_streak,
                  longest_losing_streak, attendance_consistency) |>
    tidyr::pivot_longer(-player_name, names_to = "metric", values_to = "value") |>
    dplyr::group_by(metric) |>
    dplyr::mutate(rank = ifelse(metric %in% lower_better,
                                rank(value, ties.method = "min"),
                                rank(-value, ties.method = "min"))) |>
    dplyr::ungroup() |>
    dplyr::arrange(metric, rank)
}

#' A single player's profile as a named list (used by the Shiny app)
#' @export
player_profile <- function(db, player_name) {
  ps <- player_stats(db) |> dplyr::filter(player_name == !!player_name)
  pmr <- player_match_results(db) |> dplyr::filter(player_name == !!player_name)
  list(stats = ps, matches = pmr)
}
