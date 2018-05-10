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
