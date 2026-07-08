# utils.R -- small shared helpers used across the TNF package -----------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

#' Strip invisible unicode, smart quotes and stray bullets from a line of text
#'
#' Team sheets are pasted from phones and contain word-joiners (U+2060),
#' non-breaking spaces, smart apostrophes and a zoo of bullet characters.
#' This normalises a line to plain ASCII-ish text without losing content.
#'
#' @param x character vector
#' @return character vector of the same length
clean_text_line <- function(x) {
  x <- stringr::str_replace_all(x, "[⁠​‌‍﻿]", "")
  x <- stringr::str_replace_all(x, "[   \t]", " ")
  x <- stringr::str_replace_all(x, "[‘’ʼ]", "'")
  x <- stringr::str_replace_all(x, "[“”]", '"')
  x <- stringr::str_replace_all(x, "[–—]", "-")
  stringr::str_squish(x)
}

#' Normalise a player name for alias matching
#'
#' Lower-cases, squishes whitespace and removes trailing punctuation so that
#' "Shergal Rodaina." and "shergal  rodaina" match the same alias key.
#'
#' @param x character vector of raw names
#' @return character vector of normalised keys
normalise_name_key <- function(x) {
  x <- clean_text_line(x)
  x <- stringr::str_remove_all(x, "[.,;:]+$")
  stringr::str_to_lower(stringr::str_squish(x))
}

#' Turn a canonical player name into a stable snake_case id
#'
#' @param x character vector of canonical names
#' @return character vector of ids, e.g. "tom_clapham"
make_player_id <- function(x) {
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "_")
  stringr::str_remove_all(x, "^_|_$")
}

#' Wilson score confidence interval for a binomial proportion
#'
#' Preferred over the normal approximation for the small samples typical of
#' five-a-side records. Returns the interval for wins / n.
#'
#' @param wins number of successes
#' @param n number of trials
#' @param conf confidence level (default 0.95)
#' @return tibble with columns lower and upper (proportions in 0-1)
wilson_ci <- function(wins, n, conf = 0.95) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- ifelse(n > 0, wins / n, NA_real_)
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half <- (z / denom) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  tibble::tibble(
    lower = ifelse(n > 0, pmax(0, centre - half), NA_real_),
    upper = ifelse(n > 0, pmin(1, centre + half), NA_real_)
  )
}

#' Longest run of a given value in a sequence
#'
#' @param x vector (e.g. c("W","W","L"))
#' @param value the value whose longest run is wanted
#' @return integer length of the longest run (0 if never present)
longest_run <- function(x, value) {
  if (length(x) == 0) return(0L)
  r <- rle(x == value)
  runs <- r$lengths[r$values]
  if (length(runs) == 0) 0L else max(runs)
}

#' Current streak at the end of a sequence
#'
#' @param x vector of results ordered oldest to newest
#' @return list(value = last value, length = run length), or NULLs if empty
current_run <- function(x) {
  if (length(x) == 0) return(list(value = NA_character_, length = 0L))
  r <- rle(x)
  list(value = r$values[length(r$values)], length = r$lengths[length(r$lengths)])
}

#' Format a proportion as a percentage string
#' @param p proportion in 0-1
#' @param digits decimal places
fmt_pct <- function(p, digits = 0) {
  ifelse(is.na(p), "-", paste0(formatC(100 * p, format = "f", digits = digits), "%"))
}

#' Categorise a match night's format from its attendance
#'
#' Tuesday nights are only ever 5-, 7- or 8-a-side, so the players-per-team
#' count (attendance / 2) is snapped to the nearest of those. Odd numbers
#' round down (15 players = 7-a-side with a spare).
#'
#' @param attendance number of players on the night
#' @return factor like "7-a-side" with levels 5 < 7 < 8
match_format <- function(attendance) {
  sides <- c(5, 7, 8)
  side <- vapply(attendance, function(a) {
    if (is.na(a)) return(NA_real_)
    sides[which.min(abs(sides - a / 2))]
  }, numeric(1))
  factor(paste0(side, "-a-side"),
         levels = paste0(sides, "-a-side"))
}
