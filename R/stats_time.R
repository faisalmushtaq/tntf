# stats_time.R -- how Tuesday nights change over time -------------------------

#' Attendance per match night
#' @export
attendance_series <- function(db) {
  db$matches |>
    dplyr::arrange(date) |>
    dplyr::select(date, attendance, total_goals, result)
}

#' Rolling win percentage for each side
#' @param k window size in matches (default 5)
#' @export
rolling_win_pct <- function(db, k = 5) {
  db$teams |>
    dplyr::filter(!is.na(outcome)) |>
    dplyr::arrange(date) |>
    dplyr::group_by(team) |>
    dplyr::mutate(
      match_no = dplyr::row_number(),
      win = as.numeric(outcome == "W"),
      rolling = purrr::map_dbl(match_no, function(i) {
        mean(win[max(1, i - k + 1):i])
      })
    ) |>
    dplyr::ungroup() |>
    dplyr::select(date, team, match_no, rolling)
}

#' Cumulative appearances per player over the season
#' @export
cumulative_appearances <- function(db) {
  player_match_results(db) |>
    dplyr::arrange(date) |>
    dplyr::group_by(player_name) |>
    dplyr::mutate(cumulative = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::select(player_name, date, cumulative)
}

#' Debutants (players never seen before) per month, and the newcomer rate
#' @export
newcomer_rate <- function(db) {
  debuts <- player_match_results(db) |>
    dplyr::group_by(player_name) |>
    dplyr::summarise(debut = min(date), .groups = "drop") |>
    dplyr::mutate(month = lubridate::floor_date(debut, "month")) |>
    dplyr::count(month, name = "debutants")
  monthly_players <- player_match_results(db) |>
    dplyr::mutate(month = lubridate::floor_date(date, "month")) |>
    dplyr::group_by(month) |>
    dplyr::summarise(players = dplyr::n_distinct(player_name), .groups = "drop")
  monthly_players |>
    dplyr::left_join(debuts, by = "month") |>
    dplyr::mutate(debutants = tidyr::replace_na(debutants, 0L),
                  newcomer_rate = debutants / players)
}

#' Month-to-month player retention
#'
#' Of the players seen in month m, what share also appeared in month m+1?
#' @export
player_retention <- function(db) {
  by_month <- player_match_results(db) |>
    dplyr::mutate(month = lubridate::floor_date(date, "month")) |>
    dplyr::distinct(month, player_name) |>
    dplyr::group_by(month) |>
    dplyr::summarise(squad = list(player_name), .groups = "drop") |>
    dplyr::arrange(month)
  if (nrow(by_month) < 2) return(tibble::tibble())
  tibble::tibble(
    month = by_month$month[-nrow(by_month)],
    next_month = by_month$month[-1],
    players = lengths(by_month$squad[-nrow(by_month)]),
    retained = purrr::map2_int(by_month$squad[-nrow(by_month)],
                               by_month$squad[-1],
                               ~ length(intersect(.x, .y))),
    retention = retained / players
  )
}

#' Bibs' share of available points over time (cumulative)
#'
#' Win = 1, draw = 0.5. A value above 0.5 means Bibs have had the better of
#' the season so far.
#' @export
bib_dominance <- function(db) {
  db$matches |>
    dplyr::filter(!is.na(result)) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      bibs_points = dplyr::case_when(result == "Bibs" ~ 1,
                                     result == "Draw" ~ 0.5,
                                     TRUE ~ 0),
      dominance = cumsum(bibs_points) / dplyr::row_number()
    ) |>
    dplyr::select(date, dominance)
}
