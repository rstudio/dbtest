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
  system.file("extdata", "tests", path_file(file), package = "dbtest")
}

#' @rdname utils
#' @export
all_tests <- function(dir = system.file("extdata", "tests", package = "dbtest")) {
  dir_ls(dir) %>% purrr::keep(tolower(path_ext(.)) %in% c("yml","yaml"))
}

#' @rdname utils
#' @export
default_config_path <- function(file = "config.yml"){
  system.file("extdata", "connections", file, package = "dbtest")
}

#' @title Write Tests
#'
#' @description A utility to make writing tests a bit easier
#' and more reproducible
#'
#' @param file The file the tests should be written to
#' @param header The header to name the test section.  Preferably short
#' @param expression The expression to use for writing tests
#'
#' @export
write_test <- function(file, header, expression, overwrite = FALSE, comparison = .x > sample(1:10,1)) {
  existing <- if (fs::file_exists(file) && !overwrite) read_yaml(file) else list()

  #compare <- enquo(comparison)
  #print(compare)
  # need to figure out rlang semantics

  new <- list(
    setNames(
      list(
        list(
          "mutate" = expression
          , "filter" = expression
          , "summarize" = paste0("sum(",expression,", na.rm = TRUE)")
          , "group_by" = expression
          , "arrange" = expression
          )
        )
      , header
    )
  )

  write_yaml(
    c(existing, new)
    ,file
    )
}
