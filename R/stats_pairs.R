# stats_pairs.R -- pairwise teammate analyses ---------------------------------

#' All same-team player pairs, one row per pair per match
#'
#' Pairs are stored alphabetically (player_a < player_b) so each pair has a
#' single identity.
#' @export
pair_match_results <- function(db) {
  pmr <- player_match_results(db)
  pmr |>
    dplyr::inner_join(pmr, by = c("match_id", "date", "team", "goals_for",
                                  "goals_against", "outcome"),
                      suffix = c("_a", "_b"),
                      relationship = "many-to-many") |>
    dplyr::filter(player_name_a < player_name_b) |>
    dplyr::select(match_id, date, team, outcome, goals_for, goals_against,
                  player_a = player_name_a, player_b = player_name_b)
}

#' Partnership statistics for every teammate pair
#'
#' @param db database list
#' @param min_together drop pairs with fewer shared matches than this
#' @export
pair_stats <- function(db, min_together = 1) {
  pair_match_results(db) |>
    dplyr::filter(!is.na(outcome)) |>
    dplyr::group_by(player_a, player_b) |>
    dplyr::summarise(
      together = dplyr::n(),
      wins = sum(outcome == "W"),
      draws = sum(outcome == "D"),
      losses = sum(outcome == "L"),
      win_pct = wins / together,
      loss_pct = losses / together,
      goal_diff = sum(goals_for) - sum(goals_against),
      .groups = "drop"
    ) |>
    dplyr::mutate(wilson_ci(wins, together)) |>
    dplyr::filter(together >= min_together) |>
    dplyr::arrange(dplyr::desc(together))
}

#' Symmetric co-occurrence matrix (times each pair shared a team)
#'
#' @return a named integer matrix, players in alphabetical order
#' @export
co_occurrence_matrix <- function(db) {
  players <- sort(db$players$player_name[!db$players$is_guest])
  m <- matrix(0L, length(players), length(players),
              dimnames = list(players, players))
  pr <- pair_match_results(db) |>
    dplyr::count(player_a, player_b)
  keep <- pr$player_a %in% players & pr$player_b %in% players
  pr <- pr[keep, ]
  m[cbind(pr$player_a, pr$player_b)] <- pr$n
  m[cbind(pr$player_b, pr$player_a)] <- pr$n
  m
}
