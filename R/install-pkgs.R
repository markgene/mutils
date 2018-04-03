# Install personal selected packages

#' Install personal selected packages
#'
#' @return NULL
#' @export
install_pkgs <- function() {
  install_pkgs_cran()
  install_pkgs_github()
}


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
      "purrr",
      "roxygen2",
      "rvest",
      "stringr",
      "testthat",
      "tidyverse"
    )
  uninstalled <- cran_pkgs[!(cran_pkgs %in% installed.packages())]
  if (length(uninstalled)) {
    install.packages(uninstalled, repo = "https://cloud.r-project.org")
    message("Success!")
  } else {
    message("All CRAN packages have been installed!")
  }
}


#' Install personal selected packages on GitHub
#'
#' @return NULL
install_pkgs_github <- function() {
  gb_pkgs <-
    c("thomasp85/ggraph",
      "hrbrmstr/hrbrthemes")
  uninstalled <- gb_pkgs[!(gb_pkgs %in% installed.packages())]
  if (length(uninstalled)) {
    devtools::install_github(uninstalled)
    message("Success!")
  } else {
    message("All GitHub packages have been installed!")
  }
}
