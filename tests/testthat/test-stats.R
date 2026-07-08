# Tests for the statistics layer, run against the real historical database ----

db <- build_database(test_path("../../data-raw/team_sheets_raw.txt"),
                     test_path("../../data-raw/player_aliases.csv"))

test_that("database tables are internally consistent", {
  expect_equal(nrow(db$matches), 21)
  expect_equal(nrow(db$teams), 2 * nrow(db$matches))
  # every appearance points at a real match and player
  expect_true(all(db$appearances$match_id %in% db$matches$match_id))
  expect_true(all(db$appearances$player_id %in% db$players$player_id))
  # attendance equals appearances per match
  att <- db$appearances |> dplyr::count(match_id)
  m <- db$matches |> dplyr::select(match_id, attendance)
  expect_equal(dplyr::arrange(att, match_id)$n,
               dplyr::arrange(m, match_id)$attendance)
})

test_that("team records mirror each other", {
  tr <- team_record(db)
  expect_equal(tr$wins[tr$team == "Bibs"], tr$losses[tr$team == "Non-Bibs"])
  expect_equal(tr$goals_for[tr$team == "Bibs"],
               tr$goals_against[tr$team == "Non-Bibs"])
  expect_equal(sum(tr$draws), 2 * sum(db$matches$result == "Draw"))
})

test_that("player stats add up", {
  ps <- player_stats(db)
  expect_true(all(ps$wins + ps$draws + ps$losses == ps$appearances))
  expect_true(all(ps$win_pct >= ps$lower & ps$win_pct <= ps$upper))
  expect_true(all(ps$attendance_consistency <= 1))
  # total appearances across players equals appearance rows
  expect_equal(sum(ps$appearances), nrow(db$appearances))
})

test_that("wilson intervals behave", {
  ci <- wilson_ci(5, 10)
  expect_true(ci$lower > 0.19 && ci$lower < 0.3)
  expect_true(ci$upper > 0.7 && ci$upper < 0.81)
  wide <- wilson_ci(1, 2); narrow <- wilson_ci(50, 100)
  expect_true((wide$upper - wide$lower) > (narrow$upper - narrow$lower))
})

test_that("streak helpers work", {
  x <- c("W", "W", "L", "W", "W", "W", "D")
  expect_equal(longest_run(x, "W"), 3)
  expect_equal(longest_run(x, "L"), 1)
  expect_equal(current_run(x)$value, "D")
  expect_equal(current_run(x)$length, 1)
  expect_equal(longest_run(character(0), "W"), 0L)
})

test_that("pair stats are symmetric and bounded", {
  pr <- pair_stats(db)
  expect_true(all(pr$player_a < pr$player_b))
  expect_true(all(pr$together >= pr$wins + pr$draws + pr$losses - 1e-9))
  m <- co_occurrence_matrix(db)
  expect_true(isSymmetric(m))
  expect_true(all(diag(m) == 0))
})

test_that("head-to-head views agree from both sides", {
  h <- head_to_head(db)
  ab <- h |> dplyr::filter(player == "Faisal", opponent == "Suki")
  ba <- h |> dplyr::filter(player == "Suki", opponent == "Faisal")
  expect_equal(ab$meetings, ba$meetings)
  expect_equal(ab$wins, ba$losses)
})

test_that("events captured the fight", {
  reds <- db$events |> dplyr::filter(event_type == "red card")
  expect_equal(nrow(reds), 2)
  expect_setequal(reds$player_id, c("umar_zaffar", "suki"))
})

test_that("discoveries generate non-empty plain sentences", {
  d <- generate_discoveries(db)
  expect_gt(length(d), 8)
  expect_true(any(grepl("Bibs have won", d)))
})

test_that("network metrics exist for every non-guest player", {
  nm <- network_metrics(db)
  expect_equal(nrow(nm), sum(!db$players$is_guest))
  expect_true(all(c("degree", "betweenness", "closeness", "pagerank",
                    "community") %in% names(nm)))
})
