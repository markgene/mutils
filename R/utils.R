# Helper functions

#' Pipe
#'
#' Put description here
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs specify what lhs and rhs are
#' @noRd
NULL


#' Load an object from Rda file
#'
#' @param rda A character scalar of Rda file path
#' @param name A character scalar of object name
#' @return An object
#' @export
load_object <- function(rda, name) {
  env <- new.env()
  if (file.exists(rda)) {
    load(rda, envir = env)
  } else {
    stop(paste("Rda file", rda, "does not exist"))
  }
  if (name %in% names(env))
    env[[name]]
  else
    stop("Object ", name, " does not exist.")
}


#' Split a file path into a vector of folder names
#'
#' @details
#' The function is adopted from \href{https://stackoverflow.com/questions/29214932/split-a-file-path-into-folder-names-vector}{a discussion on StackOverflow}.
#'
#' @param x A character scalar of file path.
#' @return A character vector.
#' @export
split_path <- function(x) {
  if (dirname(x)==x)
    x
  else
    c(basename(x),split_path(dirname(x)))
}
