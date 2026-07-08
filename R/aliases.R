# aliases.R -- map messy raw names onto canonical players ---------------------
#
# The alias table (data-raw/player_aliases.csv) has one row per alias with the
# canonical name it maps to. An alias may be marked ambiguous=TRUE and appear
# on several rows (e.g. "Tom" -> Tom Clapham AND Tom Exon); the resolver then
# uses match context to decide, and records what it assumed.

#' Load the player alias lookup table
#' @param path csv with columns alias, canonical, ambiguous, note
#' @export
load_aliases <- function(path = "data-raw/player_aliases.csv") {
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      alias_key = normalise_name_key(alias),
      ambiguous = as.logical(ambiguous)
    )
}

#' Resolve raw appearance names to canonical player names
#'
#' Resolution order for each raw name:
#'   1. exact match against the set of canonical names already in the aliases
#'      table (or seen elsewhere) -- unknown full names simply become new
#'      canonical players;
#'   2. unambiguous alias -> its canonical name;
#'   3. ambiguous alias -> decided from context within the match:
#'      candidates already named elsewhere in the same match are excluded,
#'      then if the alias occurs exactly as many times as there are remaining
#'      candidates each occurrence takes one candidate (in alias-table order);
#'      a single remaining candidate resolves directly; anything else is left
#'      unresolved and flagged.
#'
#' @param appearances_raw staging table from parse_team_sheets()
#' @param aliases alias table from load_aliases()
#' @return list(appearances = tibble with canonical + resolution columns,
#'              issues = tibble of alias problems and assumptions)
#' @export
resolve_names <- function(appearances_raw, aliases) {
  issues <- list()
  add_issue <- function(date, severity, message) {
    issues[[length(issues) + 1]] <<- tibble::tibble(
      date = as.Date(date), severity = severity, message = message
    )
  }

  plain <- aliases |> dplyr::filter(!ambiguous)
  ambi  <- aliases |> dplyr::filter(ambiguous)

  app <- appearances_raw |>
    dplyr::mutate(
      key = normalise_name_key(raw_name),
      canonical = NA_character_,
      resolution = NA_character_
    )

  # pass 1: unambiguous aliases (covers identity rows such as full names)
  hit <- match(app$key, plain$alias_key)
  found <- !is.na(hit)
  app$canonical[found] <- plain$canonical[hit[found]]
  app$resolution[found] <- ifelse(
    normalise_name_key(app$canonical[found]) == app$key[found], "exact", "alias")

  # pass 2: unknown names that are not ambiguous aliases become new players,
  # keeping their cleaned raw spelling as the canonical name
  is_ambi <- app$key %in% ambi$alias_key
  new_players <- !found & !is_ambi
  app$canonical[new_players] <- stringr::str_to_title(app$raw_name[new_players]) |>
    stringr::str_replace_all("\\bMc(\\w)", function(m) m) # keep simple title case
  # preserve original capitalisation where it already contains uppercase
  keep <- new_players & grepl("[A-Z]", app$raw_name)
  app$canonical[keep] <- app$raw_name[keep]
  app$resolution[new_players] <- "new-player"

  # pass 3: ambiguous aliases, match by match
  if (any(is_ambi)) {
    for (d in unique(app$date[is_ambi])) {
      in_match <- app$date == d
      present <- unique(stats::na.omit(app$canonical[in_match]))
      for (k in unique(app$key[is_ambi & in_match])) {
        rows <- which(in_match & app$key == k & is.na(app$canonical))
        cands <- ambi$canonical[ambi$alias_key == k]
        cands <- setdiff(cands, present)
        if (length(cands) == length(rows) && length(cands) > 0) {
          app$canonical[rows] <- cands
          app$resolution[rows] <- "ambiguous-auto"
          add_issue(d, "info", paste0(
            "Ambiguous name '", app$raw_name[rows[1]], "' x", length(rows),
            " resolved by elimination to: ", paste(cands, collapse = ", ")))
        } else if (length(cands) == 1) {
          app$canonical[rows] <- cands
          app$resolution[rows] <- "ambiguous-auto"
          add_issue(d, "info", paste0(
            "Ambiguous name '", app$raw_name[rows[1]],
            "' resolved by elimination to ", cands))
        } else {
          app$resolution[rows] <- "unresolved"
          add_issue(d, "error", paste0(
            "Could not resolve ambiguous name '", app$raw_name[rows[1]],
            "' (candidates: ",
            paste(ambi$canonical[ambi$alias_key == k], collapse = ", "), ")"))
        }
      }
    }
  }

  # true duplicates after resolution (same canonical player twice in a match)
  dup <- app |>
    dplyr::filter(!is.na(canonical)) |>
    dplyr::count(date, canonical) |>
    dplyr::filter(n > 1)
  for (i in seq_len(nrow(dup))) {
    add_issue(dup$date[i], "warning", paste0(
      dup$canonical[i], " appears ", dup$n[i],
      " times on ", format(dup$date[i]), " - duplicates dropped"))
  }
  app <- dplyr::bind_rows(
    app |> dplyr::filter(!is.na(canonical)) |>
      dplyr::distinct(date, canonical, .keep_all = TRUE),
    app |> dplyr::filter(is.na(canonical))
  ) |> dplyr::arrange(date)

  issues <- if (length(issues) > 0) dplyr::bind_rows(issues) else
    tibble::tibble(date = as.Date(character()), severity = character(),
                   message = character())
  list(appearances = app, issues = issues)
}

#' Known guest players (not part of the regular squad)
#' @export
guest_players <- function() c("Lee's Mate")
