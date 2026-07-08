# Tests for the raw team sheet parser -----------------------------------------

test_that("dates parse in all the formats the sheets use", {
  expect_equal(parse_sheet_date("30", "12", "25"), as.Date("2025-12-30"))
  expect_equal(parse_sheet_date("6", "1", "26"), as.Date("2026-01-06"))
  expect_equal(parse_sheet_date("13", "01", "2026"), as.Date("2026-01-13"))
  expect_true(is.na(parse_sheet_date("31", "2", "26")))
})

test_that("team labels standardise to Bibs / Non-Bibs", {
  expect_equal(standardise_team("Bibs"), "Bibs")
  expect_equal(standardise_team("No Bibs"), "Non-Bibs")
  expect_equal(standardise_team("non bibs"), "Non-Bibs")
  expect_equal(standardise_team("Non-bibs"), "Non-Bibs")
  expect_equal(standardise_team("BIBS"), "Bibs")
})

test_that("player lines survive bullets, dots and event notes", {
  expect_equal(parse_player_line("\t•\tShergal Rodaina.")$name, "Shergal Rodaina")
  expect_equal(parse_player_line("- \tFaisal")$name, "Faisal")
  expect_equal(parse_player_line("* Tom Clapham")$name, "Tom Clapham")
  pl <- parse_player_line("•\tUmar Zaffar (red card)")
  expect_equal(pl$name, "Umar Zaffar")
  expect_equal(pl$events, "red card")
  # the U+2060 word-joiner bullets from 7/7/26
  expect_equal(parse_player_line("•⁠  ⁠Vadim")$name, "Vadim")
})

test_that("a small messy sheet parses end to end", {
  txt <- "1/1/26- test note\n\n## Bibs (5)n\n- Alpha\n•\tBeta (red card)\n\nno bibs (3):\nGamma\nDelta\n"
  p <- parse_team_sheets(txt)
  expect_equal(nrow(p$matches_raw), 1)
  expect_equal(p$matches_raw$bibs_goals, 5L)
  expect_equal(p$matches_raw$nonbibs_goals, 3L)
  expect_equal(p$matches_raw$note, "test note")
  expect_equal(nrow(p$appearances_raw), 4)
  expect_equal(p$events_raw$event, "red card")
})

test_that("missing scores are flagged, not fatal", {
  txt <- "1/1/26\n\nBibs\n- A\n\nNon-Bibs (2)\n- B\n"
  p <- parse_team_sheets(txt)
  expect_equal(nrow(p$matches_raw), 1)
  expect_true(is.na(p$matches_raw$bibs_goals))
  expect_true(any(grepl("Missing score", p$issues$message)))
})

test_that("the full historical file parses to 21 matches", {
  p <- read_team_sheets(test_path("../../data-raw/team_sheets_raw.txt"))
  expect_equal(nrow(p$matches_raw), 21)
  expect_true(all(!is.na(p$matches_raw$bibs_goals)))
  expect_true(all(!is.na(p$matches_raw$nonbibs_goals)))
  expect_equal(sum(p$events_raw$event == "red card"), 2)
})
