# Install personal selected packages

#' Install personal selected packages
#'
#' @return NULL
install_pkgs <- function() {
  cran_pkgs <-
    c(
      "devtools",
      "dplyr",
      "ggplot2",
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

