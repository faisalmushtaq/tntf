# parse.R -- robust parser for messy Tuesday Night Football team sheets -------
#
# The raw sheets are pasted from WhatsApp/Notes and are wildly inconsistent:
# bullets of every flavour, tabs, smart quotes, "Bibs"/"No Bibs"/"Non-bibs",
# dates in d/m/yy or dd/mm/yy, scores only as the number in brackets, stray
# "Line Ups" headings, trailing full stops, per-player notes like "(red card)"
# and match notes appended to the date line ("9/6/26- the fight").
#
# parse_team_sheets() turns all of that into three tidy staging tables plus a
# list of parse issues that a human can review in the Shiny app.

# Regexes ---------------------------------------------------------------------

.date_rx <- "^(\\d{1,2})\\s*[/.-]\\s*(\\d{1,2})\\s*[/.-]\\s*(\\d{2,4})\\b(.*)$"
# team header: optional markdown/bullets, then bibs / no bibs / non bibs /
# non-bibs / nonbibs, optional "(<goals>)", tolerate trailing junk (":" , "n")
.team_rx <- "(?i)^[-#*• ]*((?:no[nt]?[ -]*)?bibs)\\s*\\(\\s*(\\d+)?\\s*\\)?\\s*\\)?[:.]?.*$"
.team_word_rx <- "(?i)^[-#*• ]*((?:no[nt]?[ -]*)?bibs)\\b[:.]?\\s*$"
.noise_rx <- "(?i)^(line\\s*-?\\s*ups?|teams?|team\\s*sheets?|results?)\\s*[:]?$"
.bullet_rx <- "^[•·●▪‣⁃*\\-–—]+\\s*"

#' Standardise a matched team label to "Bibs" or "Non-Bibs"
#' @param label raw label such as "no bibs", "Non-bibs", "BIBS"
#' @return "Bibs" or "Non-Bibs"
standardise_team <- function(label) {
  ifelse(grepl("^no", tolower(stringr::str_squish(label))), "Non-Bibs", "Bibs")
}

#' Parse a d/m/y date fragment, assuming 20xx for two-digit years
#' @return Date or NA
parse_sheet_date <- function(day, month, year) {
  year <- as.integer(year)
  year <- ifelse(year < 100, 2000L + year, year)
  out <- suppressWarnings(lubridate::make_date(year, as.integer(month), as.integer(day)))
  out
}

#' Split one player line into a clean name and any parenthetical events
#'
#' "Umar Zaffar (red card)" -> name "Umar Zaffar", events "red card".
#' Trailing punctuation and every bullet style are stripped.
#'
#' @param line one raw line
#' @return list(name = character(1), events = character())
parse_player_line <- function(line) {
  x <- clean_text_line(line)
  x <- stringr::str_remove(x, .bullet_rx)
  events <- stringr::str_match_all(x, "\\(([^)]*)\\)")[[1]][, 2]
  events <- stringr::str_squish(events)
  events <- events[events != "" & !grepl("^\\d+$", events)]
  x <- stringr::str_remove_all(x, "\\([^)]*\\)")
  x <- stringr::str_remove_all(x, "[.,;:]+$")
  list(name = stringr::str_squish(x), events = events)
}

#' Parse raw team sheet text into tidy staging tables
#'
#' @param text a single string or character vector of lines
#' @return list with:
#'   * matches_raw: one row per match (date, note, goals per team, source lines)
#'   * appearances_raw: one row per raw name per match-team
#'   * events_raw: one row per parenthetical event next to a player
#'   * issues: tibble of things a human should review
#' @export
parse_team_sheets <- function(text) {
  lines <- if (length(text) == 1) stringr::str_split(text, "\r?\n")[[1]] else text
  lines <- clean_text_line(lines)

  matches <- list(); appearances <- list(); events <- list(); issues <- list()
  add_issue <- function(date, severity, message) {
    issues[[length(issues) + 1]] <<- tibble::tibble(
      date = as.Date(date), severity = severity, message = message
    )
  }

  cur_date <- as.Date(NA); cur_note <- NA_character_
  cur_team <- NA_character_; match_open <- FALSE
  goals <- c(Bibs = NA_integer_, `Non-Bibs` = NA_integer_)

  flush_match <- function() {
    if (!match_open) return(invisible())
    matches[[length(matches) + 1]] <<- tibble::tibble(
      date = cur_date,
      note = cur_note,
      bibs_goals = unname(goals["Bibs"]),
      nonbibs_goals = unname(goals["Non-Bibs"])
    )
    if (is.na(goals["Bibs"]) || is.na(goals["Non-Bibs"])) {
      add_issue(cur_date, "warning", "Missing score for one or both teams")
    }
  }

  for (raw in lines) {
    line <- stringr::str_squish(raw)
    if (line == "" || grepl(.noise_rx, line)) next
    if (grepl("^#", line) && !grepl(.team_rx, line, perl = TRUE)) next  # markdown titles

    dm <- stringr::str_match(line, .date_rx)
    if (!is.na(dm[1, 1])) {
      flush_match()
      cur_date <- parse_sheet_date(dm[1, 2], dm[1, 3], dm[1, 4])
      cur_note <- stringr::str_squish(stringr::str_remove(dm[1, 5], "^[-: ]+"))
      cur_note <- ifelse(cur_note == "", NA_character_, cur_note)
      cur_team <- NA_character_
      goals <- c(Bibs = NA_integer_, `Non-Bibs` = NA_integer_)
      match_open <- TRUE
      if (is.na(cur_date)) add_issue(NA, "error", paste0("Unparseable date line: '", line, "'"))
      next
    }

    tm <- stringr::str_match(line, .team_rx)
    if (is.na(tm[1, 1])) tm <- stringr::str_match(line, .team_word_rx)
    if (!is.na(tm[1, 1])) {
      if (!match_open) {
        add_issue(NA, "error", paste0("Team header before any date: '", line, "'"))
        next
      }
      cur_team <- standardise_team(tm[1, 2])
      g <- if (ncol(tm) >= 3) suppressWarnings(as.integer(tm[1, 3])) else NA_integer_
      if (!is.na(g)) {
        goals[cur_team] <- g
      } else {
        add_issue(cur_date, "warning",
                  paste0("No score found in team header '", line, "'"))
      }
      next
    }

    # anything else inside a match with an active team is a player line
    if (match_open && !is.na(cur_team)) {
      pl <- parse_player_line(line)
      if (pl$name == "") next
      appearances[[length(appearances) + 1]] <- tibble::tibble(
        date = cur_date, team = cur_team, raw_name = pl$name
      )
      if (length(pl$events) > 0) {
        events[[length(events) + 1]] <- tibble::tibble(
          date = cur_date, team = cur_team, raw_name = pl$name,
          event = pl$events
        )
      }
    } else if (match_open) {
      add_issue(cur_date, "warning",
                paste0("Line before any team header ignored: '", line, "'"))
    }
  }
  flush_match()

  matches_raw <- dplyr::bind_rows(matches)
  appearances_raw <- dplyr::bind_rows(appearances)
  events_raw <- if (length(events) > 0) dplyr::bind_rows(events) else
    tibble::tibble(date = as.Date(character()), team = character(),
                   raw_name = character(), event = character())
  issues <- if (length(issues) > 0) dplyr::bind_rows(issues) else
    tibble::tibble(date = as.Date(character()), severity = character(),
                   message = character())

  # duplicate raw names within a match-team are almost always paste errors,
  # except for deliberately ambiguous first names handled by the alias stage
  if (nrow(appearances_raw) > 0) {
    dups <- appearances_raw |>
      dplyr::count(date, team, key = normalise_name_key(raw_name)) |>
      dplyr::filter(n > 1)
    for (i in seq_len(nrow(dups))) {
      issues <- dplyr::bind_rows(issues, tibble::tibble(
        date = dups$date[i], severity = "info",
        message = paste0("'", dups$key[i], "' listed ", dups$n[i],
                         "x for ", dups$team[i],
                         " - left to the alias resolver")
      ))
    }
  }

  list(matches_raw = matches_raw, appearances_raw = appearances_raw,
       events_raw = events_raw, issues = issues)
}

#' Read and parse a team sheet file
#' @param path path to a raw text file
#' @export
read_team_sheets <- function(path) {
  parse_team_sheets(readr::read_file(path))
}
