# Tests for alias resolution ---------------------------------------------------

make_aliases <- function() {
  tibble::tibble(
    alias = c("Matt", "Tom", "Tom", "Shergal"),
    canonical = c("Matthew Eastwood", "Tom Clapham", "Tom Exon",
                  "Shergal Rodaina"),
    ambiguous = c(FALSE, TRUE, TRUE, FALSE),
    note = NA_character_
  ) |>
    dplyr::mutate(alias_key = normalise_name_key(alias))
}

test_that("plain aliases and unknown names resolve", {
  app <- tibble::tibble(
    date = as.Date("2026-01-01"), team = "Bibs",
    raw_name = c("Matt", "Shergal", "Brand New Player")
  )
  res <- resolve_names(app, make_aliases())
  expect_equal(res$appearances$canonical,
               c("Matthew Eastwood", "Shergal Rodaina", "Brand New Player"))
  expect_equal(res$appearances$resolution,
               c("alias", "alias", "new-player"))
})

test_that("two ambiguous Toms resolve by elimination", {
  app <- tibble::tibble(
    date = as.Date("2026-01-27"), team = "Non-Bibs",
    raw_name = c("Tom", "Tom", "Shergal")
  )
  res <- resolve_names(app, make_aliases())
  toms <- res$appearances |> dplyr::filter(raw_name == "Tom")
  expect_setequal(toms$canonical, c("Tom Clapham", "Tom Exon"))
  expect_true(all(toms$resolution == "ambiguous-auto"))
})

test_that("one ambiguous Tom resolves when the other Tom is already named", {
  app <- tibble::tibble(
    date = as.Date("2026-02-03"),
    team = c("Bibs", "Non-Bibs"),
    raw_name = c("Tom Clapham", "Tom")
  )
  aliases <- make_aliases() |>
    dplyr::bind_rows(tibble::tibble(alias = "Tom Clapham",
                                    canonical = "Tom Clapham",
                                    ambiguous = FALSE, note = NA,
                                    alias_key = "tom clapham"))
  res <- resolve_names(app, aliases)
  lone_tom <- res$appearances |> dplyr::filter(raw_name == "Tom")
  expect_equal(lone_tom$canonical, "Tom Exon")
})

test_that("an unresolvable ambiguous name is flagged, not guessed", {
  app <- tibble::tibble(
    date = as.Date("2026-03-03"), team = "Bibs", raw_name = "Tom"
  )
  res <- resolve_names(app, make_aliases())
  expect_true(is.na(res$appearances$canonical))
  expect_true(any(res$issues$severity == "error"))
})

test_that("duplicate canonical players within a match are deduplicated", {
  app <- tibble::tibble(
    date = as.Date("2026-04-04"), team = "Bibs",
    raw_name = c("Matt", "Matthew Eastwood")
  )
  aliases <- make_aliases() |>
    dplyr::bind_rows(tibble::tibble(alias = "Matthew Eastwood",
                                    canonical = "Matthew Eastwood",
                                    ambiguous = FALSE, note = NA,
                                    alias_key = "matthew eastwood"))
  res <- resolve_names(app, aliases)
  expect_equal(sum(res$appearances$canonical == "Matthew Eastwood",
                   na.rm = TRUE), 1)
  expect_true(any(grepl("duplicates dropped", res$issues$message)))
})

test_that("name keys normalise smart quotes and case", {
  expect_equal(normalise_name_key("Lee’s mate"), "lee's mate")
  expect_equal(normalise_name_key("  SHERGAL   Rodaina. "), "shergal rodaina")
})
