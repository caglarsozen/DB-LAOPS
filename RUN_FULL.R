# Run from the repository root. Uses frozen inputs by default.
source("DB_LAOPS_v90_MAJOR_REVISION.R")
result <- db_main("full", root = getwd(), download = FALSE, install = TRUE)
