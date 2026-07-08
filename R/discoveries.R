# discoveries.R -- automatically written headline facts -----------------------

#' Generate plain-English notable facts from the database
#'
#' Each discovery is a sentence a club WhatsApp group would enjoy, with the
#' supporting numbers included. Facts based on small samples say so.
#'
#' @param db database list
#' @param min_apps minimum appearances for player-level claims
#' @param min_together minimum joint matches for partnership claims
#' @return character vector of sentences
#' @export
generate_discoveries <- function(db, min_apps = 8, min_together = 5) {
  out <- character()
  say <- function(...) out <<- c(out, paste0(...))

  ms <- match_summary(db)
  tr <- team_record(db)
  ps <- player_stats(db) |> dplyr::filter(!is_guest)
  ps_qual <- ps |> dplyr::filter(appearances >= min_apps)

  # club-level ---------------------------------------------------------------
  bibs <- tr[tr$team == "Bibs", ]
  say("Across ", ms$total_matches, " matches, ", ms$total_players,
      " different players have taken the field, scoring ", ms$total_goals,
      " goals (", round(ms$goals_per_match, 1), " per match).")
  say("Bibs have won ", fmt_pct(bibs$win_pct), " of matches (",
      bibs$wins, "W-", bibs$draws, "D-", bibs$losses,
      "L; 95% CI ", fmt_pct(bibs$lower), "-", fmt_pct(bibs$upper), ").")

  big <- biggest_victories(db, 1)
  say("The biggest victory was ", big$score, " to the ", big$winner,
      " on ", format(big$date, "%d %B %Y"), ".")

  wild <- db$matches |> dplyr::slice_max(total_goals, n = 1)
  say("The wildest scoreline was ", wild$bibs_goals, "-", wild$nonbibs_goals,
      " on ", format(wild$date, "%d %B %Y"), " - ", wild$total_goals,
      " goals in one night.")

  # players --------------------------------------------------------------------
  top_app <- ps |> dplyr::slice_max(appearances, n = 1, with_ties = TRUE)
  nm <- top_app$player_name
  joined <- if (length(nm) > 2) {
    paste0(paste(nm[-length(nm)], collapse = ", "), " and ", nm[length(nm)])
  } else paste(nm, collapse = " and ")
  say(joined, if (length(nm) > 1) " share" else " holds",
      " the record for most appearances (",
      top_app$appearances[1], " of ", ms$total_matches, " matches).")

  if (nrow(ps_qual) > 0) {
    best <- ps_qual |> dplyr::slice_max(win_pct, n = 1, with_ties = FALSE)
    say(best$player_name, " has the best win percentage among players with at least ",
        min_apps, " appearances: ", fmt_pct(best$win_pct), " (",
        best$wins, "W-", best$draws, "D-", best$losses, "L; 95% CI ",
        fmt_pct(best$lower), "-", fmt_pct(best$upper), ").")

    worst <- ps_qual |> dplyr::slice_min(win_pct, n = 1, with_ties = FALSE)
    say(worst$player_name, " has endured the toughest season of the regulars: a ",
        fmt_pct(worst$win_pct), " win rate from ", worst$appearances,
        " appearances.")

    gd <- ps_qual |> dplyr::slice_max(goal_diff, n = 1, with_ties = FALSE)
    say(gd$player_name, "'s teams outscore opponents by ",
        gd$goal_diff, " goals across ", gd$appearances,
        " appearances - the best goal difference in the club.")

    streak <- ps |> dplyr::slice_max(longest_win_streak, n = 1, with_ties = FALSE)
    say(streak$player_name, " put together the longest personal winning streak: ",
        streak$longest_win_streak, " matches in a row.")

    loyal <- ps_qual |> dplyr::slice_max(attendance_consistency, n = 1,
                                         with_ties = FALSE)
    say(loyal$player_name, " is the most reliable attendee, playing ",
        fmt_pct(loyal$attendance_consistency),
        " of matches since their first appearance.")
  }

  # partnerships ---------------------------------------------------------------
  bp <- best_partnerships(db, min_together, 1)
  if (nrow(bp) > 0) {
    say(bp$player_a, " and ", bp$player_b,
        " are the most successful partnership (min ", min_together,
        " matches together): ", fmt_pct(bp$win_pct), " wins from ",
        bp$together, " matches side by side.")
  }
  most_together <- pair_stats(db) |> dplyr::slice_max(together, n = 1,
                                                      with_ties = FALSE)
  say(most_together$player_a, " and ", most_together$player_b,
      " are practically inseparable - teammates in ",
      most_together$together, " matches, more than any other pair.")

  # head-to-head ---------------------------------------------------------------
  h2h <- head_to_head(db, min_meetings = min_together) |>
    dplyr::filter(meetings >= min_together) |>
    dplyr::slice_max(win_pct, n = 1, with_ties = FALSE)
  if (nrow(h2h) > 0) {
    say("The most one-sided rivalry: when ", h2h$player, " plays against ",
        h2h$opponent, ", ", h2h$player, "'s team wins ",
        fmt_pct(h2h$win_pct), " of the ", h2h$meetings, " meetings.")
  }

  # network --------------------------------------------------------------------
  nm <- network_metrics(db)
  say(nm$name[1], " is the network's most influential player by PageRank - ",
      "the closest thing Tuesday nights have to a social hub.")

  # events ---------------------------------------------------------------------
  reds <- db$events |> dplyr::filter(event_type == "red card")
  if (nrow(reds) > 0) {
    who <- db$players$player_name[match(reds$player_id, db$players$player_id)]
    say("Disciplinary record: ", nrow(reds), " red card(s) - ",
        paste(who, collapse = " and "), ", both on ",
        format(reds$date[1], "%d %B %Y"),
        " (the night recorded simply as 'the fight').")
  }

  out
}
