# Install personal selected packages

#' Install personal selected packages
#'
#' @return NULL
install_pkgs <- function() {
#' Install personal selected packages on CRAN
#'
#' @return NULL
install_pkgs_cran <- function() {
  cran_pkgs <-
    c(
      "devtools",
      "dplyr",
      "fs",
      "ggalt",
      "ggraph",
      "ggplot2",
      "hrbrthemes",
      "igraph",
      "maps",
      "roxygen2",
      "rvest",
      "stringr",
      "testthat",
      "tidyverse"
    )
  uninstalled <- cran_pkgs[!(cran_pkgs %in% installed.packages())]
  if (length(uninstalled)) {
    install.packages(uninstalled)
    message("Success!")
  } else {
    message("All CRAN packages installed!")
  }
}


#' Install personal selected packages on GitHub
#'
#' @return NULL
install_pkgs_github <- function() {
  gb_pkgs <-
    c()
  uninstalled <- gb_pkgs[!(gb_pkgs %in% installed.packages())]
  if (length(uninstalled)) {
    devtools::install_github(uninstalled)
    message("Success!")
  } else {
    message("All GitHub packages installed!")
  }
}
