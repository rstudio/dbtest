#' @rdname utils
#' @title Utility Functions
#'
#' @description Utility functions useful for package default behavior.
#' `pkg_test` returns the path to tests from the `dbtest` package.
#' `pkg_config` returns the path to connection config from the `dbtest`
#' package.  `all_tests` returns the path to all .yml or .yaml files in a given
#' directory.  The default is to return all tests in the `dbtest` package
#'
#' @param file optional The file to reference at the default location
#' @param dir optional The directory for which to show all_tests
#'
#' @export
pkg_test <- function(file = "simple-tests.yml"){
  system.file("extdata", "tests", path_file(file), package = "dbtest")
}

#' @rdname utils
#' @export
all_tests <- function(dir = system.file("extdata", "tests", package = "dbtest")) {
  dir_ls(dir) %>% keep(tolower(path_ext(.)) %in% c("yml","yaml"))
}

#' @rdname utils
#' @export
pkg_config <- function(file = "config.yml"){
  system.file("extdata", "connections", file, package = "dbtest")
}

#' @title Write Tests
#'
#' @description A utility to make writing tests a bit easier
#' and more reproducible
#'
#' @param file The file the tests should be written to
#' @param header The header to name the test section.  Preferably short
#' @param expr The expression to use for writing tests
#' @param overwrite optional Whether to overwrite the existing file or
#' append.  Defaults to FALSE
#' @param comparison optional The type of comparison to use for filter
#' when creating the test.  Currently ignored
#'
#' @export
write_test <- function(
  file
  , header
  , expr
  , overwrite = FALSE
  , comparison = .x > sample(1:10,1)
  ) {
  existing <- if (file_exists(file) && !overwrite) read_yaml(file) else list()

  #compare <- enquo(comparison)
  #print(compare)
  # need to figure out rlang semantics

  new <- list(
    setNames(
      list(
        list(
          "mutate" = expr
          , "filter" = expr
          #, "summarize" = paste0("sum(",expression,", na.rm = TRUE)")
          , "summarize" = paste0("n_distinct(",expr,")")
          , "group_by" = expr
          , "arrange" = expr
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
