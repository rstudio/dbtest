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
pkg_test <- function(file = "simple-tests.yml") {
  system.file("extdata", "tests", path_file(file), package = "dbtest")
}

#' @rdname utils
#' @export
all_tests <- function(dir = system.file("extdata", "tests", package = "dbtest")) {
  dir_ls(dir) %>% keep(tolower(path_ext(.)) %in% c("yml", "yaml"))
}

#' @rdname utils
#' @export
pkg_config <- function(file = "config.yml") {
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
                       , comparison = " > 10") {
  existing <- if (file_exists(file) && !overwrite) read_yaml(file) else list()

  # compare <- enquo(comparison)
  # print(compare)
  # need to figure out rlang semantics

  new <- list(
    setNames(
      list(
        list(
          "mutate" = expr
          , "filter" = paste(expr, comparison)
          # , "summarize" = paste0("sum(",expression,", na.rm = TRUE)")
          , "summarize" = paste0("n_distinct(", expr, ")")
          , "group_by" = expr
          , "arrange" = expr
        )
      )
      , header
    )
  )

  write_yaml(
    c(existing, new)
    , file
  )
}




new_test_data <- function(numrow = 10, seed=NULL) {
  set_seed(seed)
  tibble(
    fld_factor = new_factor_col(numrow)

    , fld_datetime = paste(
      new_date_col(numrow)
      , new_time_col(numrow)
    )
    , fld_date = new_date_col(numrow)
    , fld_time = new_time_col(numrow)

    , fld_binary = sample(c(1, 0), numrow, TRUE)

    , fld_integer = new_integer_col(numrow)
    , fld_integer_alt = new_integer_col(numrow)

    , fld_double = runif(numrow, -10000, 10000)
    , fld_double_alt = new_double_col(numrow)

    , fld_logical = new_logical_col(numrow)
    , fld_logical_alt = new_logical_col(numrow)

    , fld_character = new_character_col(numrow = numrow)
    , fld_character_symbol = new_character_col(
      numrow
      , strsplit("~!@#$%^&*(){}|[]\\;'./,?><-_==+", "")[[1]]
    )
    , fld_character_lead_trail_whitespace = paste0(
      new_character_col(numrow, " ", 7)
      , new_character_col(numrow, maxlength = 10)
      , new_character_col(numrow, " ", 7)
    )

    , fld_datetime_utc = paste(
      new_date_col(numrow)
      , new_time_col(numrow)
      , "UTC"
    )
  ) %>%
    lapply(na_portion) %>%
    as_tibble()
}

new_integer_col <- function(numrow = 10, magnitude=10000) {
  as.integer(sample(-magnitude:magnitude, numrow, TRUE))
}

new_double_col <- function(numrow = 10, magnitude = 10000) {
  runif(numrow, -magnitude, magnitude)
}

new_date_col <- function(numrow = 10) {
  paste(
    sprintf(sample(1:12, numrow, TRUE), fmt = "%02d")
    , sprintf(sample(1:28, numrow, TRUE), fmt = "%02d")
    , sample(1990:2020, numrow, TRUE)
    ,
    sep = "/"
  )
}

new_time_col <- function(numrow = 10) {
  paste(sprintf(sample(1:24, numrow, TRUE), fmt = "%02d")
    , sprintf(sample(0:60, numrow, TRUE), fmt = "%02d")
    , sprintf(sample(0:60, numrow, TRUE), fmt = "%02d")
    ,
    sep = ":"
  )
}

new_character <- function(numrow = 10
                          , charset = c(LETTERS, tolower(LETTERS))
                          , maxlength = 20) {
  paste(
    sample(
      charset
      , sample(1:maxlength, 1)
      , TRUE
    )
    ,
    collapse = ""
  )
}

new_factor_col <- function(
                           numrow = 10
                           , charset = c(LETTERS, tolower(LETTERS))
                           , maxlength = 20) {
  raw_chr <- new_character_col(numrow = numrow, charset = charset, maxlength = maxlength)
  return(factor(raw_chr, levels = raw_chr))
}

new_character_col <- function(numrow = 10
                              , charset = c(LETTERS, tolower(LETTERS))
                              , maxlength = 20) {
  as.character(lapply(1:numrow
    , new_character
    ,
    charset = charset
    , maxlength = maxlength
  ))
}

new_logical_col <- function(numrow = 10) {
  sample(c(TRUE, FALSE), numrow, TRUE)
}


na_portion <- function(input, minpct = 0.1, maxpct = 0.4) {
  sel <- sample(1:length(input)
    ,
    size = sample(pmax(3, length(input) * minpct):(length(input) * maxpct), 1)
    , replace = FALSE
  )
  input[sel] <- NA
  return(input)
}


set_seed <- function(seed=NULL) {
  if (!missing(seed)) {
    set.seed(seed)
  }
  invisible()
}
