# stats_chemistry.R -- combinations, line-ups and squad overlap ---------------

#' Statistics for every k-player same-team combination
#'
#' Enumerates all trios/quartets/quintets that ever shared a team and their
#' collective record. With squads of 7-9 this stays small enough to brute
#' force.
#'
#' @param db database list
#' @param size combination size (3 = trio, 4 = quartet, 5 = quintet)
#' @param min_together only keep combos with at least this many joint matches
#' @export
combo_stats <- function(db, size = 3, min_together = 3) {
  pmr <- player_match_results(db) |> dplyr::filter(!is.na(outcome))
  combos <- pmr |>
    dplyr::group_by(match_id, team, outcome) |>
    dplyr::group_map(function(g, key) {
      nms <- sort(g$player_name)
      if (length(nms) < size) return(NULL)
      cmb <- utils::combn(nms, size)
      tibble::tibble(
        combo = apply(cmb, 2, paste, collapse = " + "),
        outcome = key$outcome
      )
    }) |>
    dplyr::bind_rows()
  combos |>
    dplyr::group_by(combo) |>
    dplyr::summarise(
      together = dplyr::n(),
      wins = sum(outcome == "W"),
      draws = sum(outcome == "D"),
      losses = sum(outcome == "L"),
      win_pct = wins / together,
      .groups = "drop"
    ) |>
    dplyr::filter(together >= min_together) |>
    dplyr::arrange(dplyr::desc(win_pct), dplyr::desc(together))
}

#' Full line-ups and how often each exact XI (or V) recurs
#' @export
lineup_counts <- function(db) {
  db$appearances |>
    dplyr::inner_join(db$players, by = "player_id") |>
    dplyr::group_by(match_id, date, team) |>
    dplyr::summarise(lineup = paste(sort(player_name), collapse = ", "),
                     n_players = dplyr::n(), .groups = "drop") |>
    dplyr::count(team, lineup, n_players, name = "times_fielded") |>
    dplyr::arrange(dplyr::desc(times_fielded))
}

#' Line-up stability: Jaccard overlap between consecutive match squads
#'
#' Computed for the whole night (all attendees) and for each side separately.
#' 1 = identical squads, 0 = complete turnover.
#' @export
lineup_stability <- function(db) {
  night_sets <- db$appearances |>
    dplyr::inner_join(db$players, by = "player_id") |>
    dplyr::group_by(date) |>
    dplyr::summarise(squad = list(unique(player_name)), .groups = "drop") |>
    dplyr::arrange(date)
  if (nrow(night_sets) < 2) return(tibble::tibble())
  tibble::tibble(
    from = night_sets$date[-nrow(night_sets)],
    to = night_sets$date[-1],
    overlap = purrr::map2_int(night_sets$squad[-nrow(night_sets)],
                              night_sets$squad[-1],
                              ~ length(intersect(.x, .y))),
    jaccard = purrr::map2_dbl(night_sets$squad[-nrow(night_sets)],
                              night_sets$squad[-1],
                              ~ length(intersect(.x, .y)) /
                                length(union(.x, .y)))
  )
}

#' Most successful partnerships, filtered for a sensible sample size
#' @export
best_partnerships <- function(db, min_together = 5, top = 10) {
  pair_stats(db, min_together) |>
    dplyr::arrange(dplyr::desc(win_pct), dplyr::desc(together)) |>
    utils::head(top)
}

#' Partnerships that almost never lose together
#' @export
rarely_lose_together <- function(db, min_together = 5, top = 10) {
  pair_stats(db, min_together) |>
    dplyr::arrange(loss_pct, dplyr::desc(together)) |>
    utils::head(top)
}
