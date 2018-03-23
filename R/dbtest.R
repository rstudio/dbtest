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
#' @importFrom rlang parse_expr
#' @importFrom rprojroot find_rstudio_root_file
#' @importFrom testthat context
#' @importFrom testthat expect_equal
#' @importFrom testthat ListReporter
#' @importFrom testthat MultiReporter
#' @importFrom testthat MinimalReporter
#' @importFrom testthat test_that
#' @importFrom testthat with_reporter
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
    , "Failed"
    , "Passed"
    , "connection"
    , "database"
    , "error"
    , "failed"
    , "filler"
    , "justtest"
    , "res"
    , "result"
    , "results.context"
    , "results.error"
    , "results.failed"
    , "results.test"
    , "setNames"
    , "spread"
    , "test"
    , "testdata"
  )
)
