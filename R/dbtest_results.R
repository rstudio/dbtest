
#' Test if the object is a dbtest_results object
#'
#' This function returns FALSE if the object is not a
#' dbtest_results object, and TRUE if it is.
#'
#' @param x An object
#'
#' @rdname is_dbtest_results
#'
#' @export
is.dbtest_results <- function(x){
  "dbtest_results" %in% class(x)
}

#' @rdname is_dbtest_results
#' @export
is_dbtest_results <- function(x){
  "dbtest_results" %in% class(x)
}

#' Coerce object to a dbtest_results object
#'
#' `as_dbtest_results` will convert a list to a dbtest_results
#' object, provided that it meets the necessary requirements
#'
#' @param x A list.  Should have a "results" object of class
#' `testthat_results` and a "connection" object of class `character`
#' @param ... Other arguments passed on to individual methods
#'
#' @rdname as_dbtest_results
#' @export
as_dbtest_results <- function(x, ...) {
  UseMethod("as_dbtest_results")
}

#' @rdname as_dbtest_results
#' @export
as.dbtest_results <- function(x, ...) {
  UseMethod("as_dbtest_results")
}

#' @export
as_dbtest_results.list <- function(x, ...) {
  stopifnot(
    all(c("connection", "results") %in% names(x))
    , "testthat_results" %in% class(x[["results"]])
    , "character" %in% class(x[["connection"]])
  )
  class(x) <- c("dbtest_results", class(x))
  return(x)
}

#' @export
as_dbtest_results.dbtest_results <- function(x, ...) {
  x
}
