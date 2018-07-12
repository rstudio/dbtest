#' @importFrom bit64 is.integer64
#' @importFrom dbplyr remote_con
#' @importFrom fs dir_ls
#' @importFrom fs file_exists
#' @importFrom fs path_ext
#' @importFrom fs path_ext_remove
#' @importFrom fs path_file
#' @importFrom odbc odbc
#' @importFrom odbc odbcListDataSources
#' @importFrom purrr flatten
#' @importFrom purrr keep
#' @importFrom purrr map
#' @importFrom purrr map_df
#' @importFrom purrr map2
#' @importFrom purrr reduce
#' @importFrom purrr set_names
#' @importFrom rlang parse_expr
#' @importFrom rprojroot find_rstudio_root_file
#' @importFrom stats runif
#' @importFrom testthat context
#' @importFrom testthat expect_equal
#' @importFrom testthat ListReporter
#' @importFrom testthat MultiReporter
#' @importFrom testthat MinimalReporter
#' @importFrom testthat skip
#' @importFrom testthat test_that
#' @importFrom testthat with_reporter
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom yaml read_yaml
#' @importFrom yaml write_yaml
#' @import htmltools
#' @import ggplot2
#' @import DBI
#' @import utils
#' @import pracma
#' @import dplyr
#' @keywords internal
"_PACKAGE"
NULL
utils::globalVariables(
  c(
    "."
    , ".x"
    , "alt"
    , "Failed"
    , "Passed"
    , "connection"
    , "database"
    , "error"
    , "fail"
    , "failed"
    , "filler"
    , "justtest"
    , "justverb"
    , "label"
    , "pass"
    , "pct"
    , "res"
    , "result"
    , "results.context"
    , "results.error"
    , "results.failed"
    , "results.file"
    , "results.test"
    , "score"
    , "setNames"
    , "spread"
    , "test"
    , "testdata"
    , "testfile"
    , "total"
  )
)
