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


#' Generate significance stars from p-values
#'
#' Generate significance stars (e.g. '***', '**', '*', 'ns') from p-values.
#' Inspired by \code{gtools::stars.pval()} with extra arguments and flexibility
#' and different default.
#'
#' Mapping from p-value ranges to symbols: \describe{ \item{0 - 0.001}{'***'}
#' \item{0.001 - 0.01}{'**'} \item{0.01 - 0.05}{'*'}
#' \item{0.05 - 1.0}{'ns'} }
#'
#' @param p.value numeric vector of p-values
#' @param ... any parameters passed to \code{\link[stats]{symnum}}.
#' @return A character vector containing the same number of elements as
#' \code{p-value}, with an attribute "legend" providing the conversion pattern.
#' @author Mark Chen \email{mjchen.gene@gmail.com}
#' @seealso \code{\link[stats]{symnum}}
#' @keywords misc
#' @examples
#'
#' p.val <- c(0.0004, 0.0015, 0.013, 0.044, 0.067, 0.24)
#' stars_pval(p.val)
#' @importFrom stats symnum
#' @export
stars_pval <-
  function(p.value,
           cutpoints = c(0, 0.001, 0.01, 0.05, 1),
           symbols = c("***", "**", "*", "ns"),
           corr = FALSE,
           na = FALSE,
           ...) {
    unclass(symnum(
      p.value,
      corr = corr,
      na = na,
      cutpoints = cutpoints,
      symbols = symbols
    ))
  }
