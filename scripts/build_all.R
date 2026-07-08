#!/usr/bin/env Rscript
# build_all.R -- one-shot pipeline: parse -> database -> figures -> report ----
#
# Usage: Rscript scripts/build_all.R [--no-report]

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidygraph)
  library(ggraph)
})

args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(".")
for (f in list.files(file.path(root, "R"), full.names = TRUE)) source(f)

message("1/4 Building database from raw team sheets ...")
db <- build_database("data-raw/team_sheets_raw.txt", "data-raw/player_aliases.csv")
save_database(db, "data")
message("    ", nrow(db$matches), " matches, ", nrow(db$players), " players, ",
        nrow(db$appearances), " appearances")
if (nrow(db$parse_issues) > 0) {
  message("    ", nrow(db$parse_issues), " parse note(s) written to data/parse_issues.csv")
}

message("2/4 Rendering figures to figures/ ...")
figs <- all_figures(db)
sizes <- list(co_occurrence = c(11, 10), h2h_heatmap = c(10, 9),
              network = c(11, 8.5), player_timeline = c(9, 8))
for (nm in names(figs)) {
  sz <- sizes[[nm]] %||% c(9, 6)
  save_figure(figs[[nm]], nm, width = sz[1], height = sz[2])
}
message("    ", length(figs), " figures saved")

message("3/4 Writing discoveries ...")
writeLines(generate_discoveries(db), "data/discoveries.txt")

if (!("--no-report" %in% args)) {
  message("4/4 Rendering PDF report ...")
  rmarkdown::render("reports/tnf_report.Rmd", output_dir = "reports",
                    quiet = TRUE)
  message("    reports/tnf_report.pdf")
} else {
  message("4/4 Skipping report (--no-report)")
}
message("Done.")
