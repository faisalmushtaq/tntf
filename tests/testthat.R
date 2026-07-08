# Test runner: Rscript tests/testthat.R
suppressPackageStartupMessages({
  library(testthat)
  library(tidyverse)
  library(tidygraph)
})
for (f in list.files("R", full.names = TRUE)) source(f)
test_dir("tests/testthat", reporter = "summary")
