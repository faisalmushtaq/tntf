# database.R -- build, save and load the tidy relational data model -----------
#
# Tables (all tidy, one observation per row):
#   matches        one row per match night
#   teams          one row per team per match (two per match)
#   players        one row per canonical player
#   appearances    one row per player per match
#   events         one row per notable event (red cards, match notes)
#   player_aliases the lookup table used for name resolution
#   parse_issues   everything the pipeline wants a human to look at

#' Build the full tidy database from a raw team sheet file
#'
#' @param raw_path path to raw team sheet text
#' @param aliases_path path to the alias csv
#' @param extra_text optional additional raw sheet text (e.g. an upload)
#'   appended after the file contents
#' @return named list of tibbles
#' @export
build_database <- function(raw_path = "data-raw/team_sheets_raw.txt",
                           aliases_path = "data-raw/player_aliases.csv",
                           extra_text = NULL) {
  txt <- readr::read_file(raw_path)
  if (!is.null(extra_text) && nzchar(extra_text)) {
    txt <- paste(txt, extra_text, sep = "\n\n")
  }
  parsed <- parse_team_sheets(txt)
  aliases <- load_aliases(aliases_path)
  build_database_from_parsed(parsed, aliases)
}

#' Build the tidy database from already-parsed staging tables
#'
#' Split out from build_database() so the Shiny app can preview an upload
#' before committing it.
#'
#' @param parsed output of parse_team_sheets()
#' @param aliases output of load_aliases()
#' @return named list of tibbles
#' @export
build_database_from_parsed <- function(parsed, aliases) {
  res <- resolve_names(parsed$appearances_raw, aliases)
  app <- res$appearances |> dplyr::filter(!is.na(canonical))

  # matches -------------------------------------------------------------------
  matches <- parsed$matches_raw |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      match_id = make.unique(paste0("M", format(date, "%Y%m%d")), sep = "_"),
      result = dplyr::case_when(
        is.na(bibs_goals) | is.na(nonbibs_goals) ~ NA_character_,
        bibs_goals > nonbibs_goals ~ "Bibs",
        bibs_goals < nonbibs_goals ~ "Non-Bibs",
        TRUE ~ "Draw"
      ),
      total_goals = bibs_goals + nonbibs_goals,
      margin = abs(bibs_goals - nonbibs_goals)
    )

  attendance <- app |>
    dplyr::count(date, name = "attendance")
  matches <- matches |>
    dplyr::left_join(attendance, by = "date") |>
    dplyr::mutate(attendance = tidyr::replace_na(attendance, 0L)) |>
    dplyr::select(match_id, date, note, bibs_goals, nonbibs_goals,
                  result, total_goals, margin, attendance)

  # teams ---------------------------------------------------------------------
  team_sizes <- app |> dplyr::count(date, team, name = "team_size")
  teams <- matches |>
    tidyr::pivot_longer(c(bibs_goals, nonbibs_goals),
                        names_to = "team", values_to = "goals_for") |>
    dplyr::mutate(team = ifelse(team == "bibs_goals", "Bibs", "Non-Bibs")) |>
    dplyr::group_by(match_id) |>
    dplyr::mutate(goals_against = rev(goals_for)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      outcome = dplyr::case_when(
        is.na(goals_for) | is.na(goals_against) ~ NA_character_,
        goals_for > goals_against ~ "W",
        goals_for < goals_against ~ "L",
        TRUE ~ "D"
      )
    ) |>
    dplyr::left_join(team_sizes, by = c("date", "team")) |>
    dplyr::select(match_id, date, team, goals_for, goals_against,
                  outcome, team_size)

  # players -------------------------------------------------------------------
  players <- app |>
    dplyr::distinct(canonical) |>
    dplyr::arrange(canonical) |>
    dplyr::transmute(
      player_id = make_player_id(canonical),
      player_name = canonical,
      is_guest = canonical %in% guest_players()
    )

  # appearances ---------------------------------------------------------------
  appearances <- app |>
    dplyr::left_join(matches |> dplyr::select(match_id, date), by = "date") |>
    dplyr::transmute(
      match_id, date, team,
      player_id = make_player_id(canonical),
      raw_name, resolution
    )

  # events --------------------------------------------------------------------
  player_events <- parsed$events_raw |>
    dplyr::mutate(key = normalise_name_key(raw_name)) |>
    dplyr::left_join(
      res$appearances |> dplyr::mutate(key = normalise_name_key(raw_name)) |>
        dplyr::select(date, key, canonical) |> dplyr::distinct(),
      by = c("date", "key")
    ) |>
    dplyr::left_join(matches |> dplyr::select(match_id, date), by = "date") |>
    dplyr::transmute(
      match_id, date,
      player_id = ifelse(is.na(canonical), NA_character_, make_player_id(canonical)),
      event_type = stringr::str_to_lower(event),
      detail = paste0(raw_name, " (", team, ")")
    )
  match_notes <- matches |>
    dplyr::filter(!is.na(note)) |>
    dplyr::transmute(match_id, date, player_id = NA_character_,
                     event_type = "match note", detail = note)
  events <- dplyr::bind_rows(player_events, match_notes) |>
    dplyr::arrange(date)

  parse_issues <- dplyr::bind_rows(parsed$issues, res$issues) |>
    dplyr::arrange(date)

  list(
    matches = matches,
    teams = teams,
    players = players,
    appearances = appearances,
    events = events,
    player_aliases = load_aliases_plain(aliases),
    parse_issues = parse_issues
  )
}

# strip the derived alias_key column before storing
load_aliases_plain <- function(aliases) {
  aliases |> dplyr::select(alias, canonical, ambiguous, note)
}

#' Save every table in the database to data/ as both rds and csv
#' @export
save_database <- function(db, dir = "data") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(db, file.path(dir, "tnf_database.rds"))
  purrr::iwalk(db, function(tbl, name) {
    readr::write_csv(tbl, file.path(dir, paste0(name, ".csv")), na = "")
  })
  invisible(db)
}

#' Load the saved database
#' @export
load_database <- function(dir = "data") {
  readRDS(file.path(dir, "tnf_database.rds"))
}
