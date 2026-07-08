# stats_h2h.R -- head-to-head: who beats whom when they are on opposite sides -

#' All opposing player pairs, one row per ordered pair per match
#'
#' Ordered: `player` vs `opponent`, with `player`'s result. Every meeting
#' therefore appears twice (once from each player's point of view), which is
#' what you want for "when X plays against Y" questions.
#' @export
opposition_match_results <- function(db) {
  pmr <- player_match_results(db)
  pmr |>
    dplyr::inner_join(pmr, by = c("match_id", "date"),
                      suffix = c("", "_opp"),
                      relationship = "many-to-many") |>
    dplyr::filter(team != team_opp) |>
    dplyr::select(match_id, date, player = player_name,
                  opponent = player_name_opp, outcome, goals_for, goals_against)
}

#' Head-to-head summary for every ordered player pair
#'
#' @param db database list
#' @param min_meetings drop pairs who met fewer times than this
#' @export
head_to_head <- function(db, min_meetings = 1) {
  opposition_match_results(db) |>
    dplyr::filter(!is.na(outcome)) |>
    dplyr::group_by(player, opponent) |>
    dplyr::summarise(
      meetings = dplyr::n(),
      wins = sum(outcome == "W"),
      draws = sum(outcome == "D"),
      losses = sum(outcome == "L"),
      win_pct = wins / meetings,
      goal_diff = sum(goals_for) - sum(goals_against),
      .groups = "drop"
    ) |>
    dplyr::mutate(wilson_ci(wins, meetings)) |>
    dplyr::filter(meetings >= min_meetings) |>
    dplyr::arrange(dplyr::desc(win_pct), dplyr::desc(meetings))
}

#' Plain-English head-to-head sentences ("When A plays against B ...")
#'
#' @param min_meetings only report rivalries with at least this many meetings
#' @param top how many sentences to return
#' @export
h2h_sentences <- function(db, min_meetings = 5, top = 10) {
  head_to_head(db, min_meetings) |>
    dplyr::filter(player < opponent | win_pct > 0.5) |>
    dplyr::arrange(dplyr::desc(win_pct * meetings)) |>
    utils::head(top) |>
    dplyr::mutate(sentence = paste0(
      "When ", player, " plays against ", opponent, ", ", player,
      "'s team wins ", fmt_pct(win_pct), " of matches (",
      wins, "W-", draws, "D-", losses, "L from ", meetings, " meetings)."
    )) |>
    dplyr::pull(sentence)
}
