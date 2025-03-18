## Load package(s) required for R functions

pkgs <- c("dplyr")

invisible(lapply(pkgs, library, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
