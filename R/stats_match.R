# stats_match.R -- match-level statistics -------------------------------------

#' Headline numbers for the whole record
#' @param db database list from build_database()
#' @return one-row tibble of key figures
#' @export
match_summary <- function(db) {
  m <- db$matches
  tibble::tibble(
    total_matches = nrow(m),
    total_players = sum(!db$players$is_guest),
    total_goals = sum(m$total_goals, na.rm = TRUE),
    goals_per_match = mean(m$total_goals, na.rm = TRUE),
    mean_attendance = mean(m$attendance),
    bibs_wins = sum(m$result == "Bibs", na.rm = TRUE),
    nonbibs_wins = sum(m$result == "Non-Bibs", na.rm = TRUE),
    draws = sum(m$result == "Draw", na.rm = TRUE),
    first_match = min(m$date),
    last_match = max(m$date),
    biggest_margin = max(m$margin, na.rm = TRUE),
    highest_scoring = max(m$total_goals, na.rm = TRUE),
    red_cards = sum(db$events$event_type == "red card", na.rm = TRUE)
  )
}

#' Win/draw/loss record for each side with Wilson confidence intervals
#' @export
team_record <- function(db) {
  db$teams |>
    dplyr::filter(!is.na(outcome)) |>
    dplyr::group_by(team) |>
    dplyr::summarise(
      played = dplyr::n(),
      wins = sum(outcome == "W"),
      draws = sum(outcome == "D"),
      losses = sum(outcome == "L"),
      goals_for = sum(goals_for),
      goals_against = sum(goals_against),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      goal_diff = goals_for - goals_against,
      win_pct = wins / played,
      wilson_ci(wins, played)
    )
}

#' Longest winning streak for each side
#' @return tibble with team, streak length and the dates it spanned
#' @export
team_streaks <- function(db) {
  db$teams |>
    dplyr::filter(!is.na(outcome)) |>
    dplyr::arrange(date) |>
    dplyr::group_by(team) |>
    dplyr::summarise(
      longest_win_streak = longest_run(outcome, "W"),
      longest_unbeaten = longest_run(outcome != "L", TRUE),
      longest_losing_streak = longest_run(outcome, "L"),
      current = paste0(current_run(outcome)$length, current_run(outcome)$value),
      .groups = "drop"
    )
}

#' The biggest victories on record
#' @param n number of matches to return
#' @export
biggest_victories <- function(db, n = 5) {
  db$matches |>
    dplyr::filter(!is.na(result), result != "Draw") |>
    dplyr::arrange(dplyr::desc(margin), dplyr::desc(total_goals)) |>
    dplyr::mutate(score = paste0(bibs_goals, "-", nonbibs_goals)) |>
    dplyr::select(date, score, winner = result, margin, attendance) |>
    utils::head(n)
}

#' Distribution of goals scored by a team in a match
#' @export
scoring_distribution <- function(db) {
  db$teams |>
    dplyr::filter(!is.na(goals_for)) |>
    dplyr::count(team, goals_for, name = "matches")
}

#' Full match results in presentation order
#' @export
match_results_table <- function(db) {
  db$matches |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      score = paste0(bibs_goals, " - ", nonbibs_goals),
      winner = dplyr::coalesce(result, "unknown")
    ) |>
    dplyr::select(date, score, winner, margin, attendance, note)
}
