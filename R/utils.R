#' @rdname utils
#' @title Utility Functions
#'
#' @description Utility functions useful for package default behavior.
#' `default_test_path` returns the path to tests from the `dbtest` package.
#' `default_config_path` returns the path to connection config from the `dbtest`
#' package.  `all_tests` returns the path to all .yml or .yaml files in a given
#' directory.  The default is to return all tests in the `dbtest` package
#'
#' @param file optional The file to reference at the default location
#' @param dir optional The directory for which to show all_tests
#'
#' @export
default_test_path <- function(file = "simple-tests.yml"){
  system.file("extdata", "tests", fs::path_file(file), package = "dbtest")
}

#' @rdname utils
#' @export
all_tests <- function(dir = system.file("extdata", "tests", package = "dbtest")) {
  fs::dir_ls(dir) %>% purrr::keep(tolower(fs::path_ext(.)) %in% c("yml","yaml"))
}

#' @rdname utils
#' @export
default_config_path <- function(file = "config.yml"){
  system.file("extdata", "connections", file, package = "dbtest")
}
