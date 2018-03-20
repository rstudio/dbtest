#' @rdname utils
#' @title Utility Functions
#'
#' @description Utility functions useful for package default behavior
#'
#' @param file The file to reference at the default location
#'
#' @export
default_tests_path <- function(file = "tests.yml"){
  system.file("extdata", "tests", file, package = "dbtest")
}

#' @rdname utils
#' @export
default_config_path <- function(file = "config.yml"){
  system.file("extdata", "connections", file, package = "dbtest")
}
