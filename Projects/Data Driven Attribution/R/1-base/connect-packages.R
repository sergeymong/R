read_packages <- function(package) {
  suppressMessages(suppressWarnings(library(package, character.only = T)))
}

packages <- 
  c(
    "data.table",
    "RClickhouse",
    "dplyr",
    "lubridate",
    "DBI",
    "tidyr",
    "zoo",
    "magrittr",
    "ChannelAttribution",
    "fst",
    "fasttime",
    "stringr",
    "glue",
    "ggplot2"
  )

invisible(lapply(packages, read_packages))
options(warn=-1, scipen = 999)
rm(read_packages, packages)
