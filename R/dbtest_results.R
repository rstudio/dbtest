
#' @export
is.dbtest_results <- function(x){
  "dbtest_results" %in% class(x)
}

#' @export
is_dbtest_results <- function(x){
  "dbtest_results" %in% class(x)
}

#' @export
as_dbtest_results <- function(x, ...) {
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
