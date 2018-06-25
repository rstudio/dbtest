#' @title Test Databases
#'
#' @description A wrapper around `test_sigle_database` that iterates over multiple datasources
#' and executes the testing suite on each.  Output is organized in such a way as to
#' give nice, consolidated results.
#'
#' @param datasources optional Defaults to using a SQLite database.  Pass "dsn" to use
#' all DSNs available on the system.  Use "config" or a path to a "config.yml" file to
#' use connection parameters in a YAML file.  Connection parameters will be passed to
#' `dbConnect` as-is
#' @param tests optional  A character vector of yaml tests to execute.
#' References `dbtest` test suite by default
#'
#' @return Returns a list of lists containing the respective datasource labels and testthat output
#'
#' @seealso test_single_database
#'
#' @examples
#' # test all dsns with dbtest suite -----------------------
#' \dontrun{
#' test_databases(datasources = "dsn")
#' }
#' # test sqlite with custom suite -------------------------
#' \dontrun{
#' test_databases(tests = "./path/to/my.yml")
#' }
#' # test connection yaml file with dbtest suite -----------
#' \dontrun{
#' test_databases(datasources = "./path/to/conn.yml")
#' }
#'
#' @export
test_databases <- function(datasources = NULL, tests = pkg_test()) {
  UseMethod("test_databases", datasources)
}

#' @export
test_databases.list <- function(datasources = NULL, tests = pkg_test()) {
  message("LIST")
  lapply(datasources, test_databases)
}

#' @export
test_databases.character <- function(datasources = NULL, tests = pkg_test()) {
  message("CHARACTER")

  config_check <- tolower(path_ext(datasources)) %in% c("yml","yaml")
  config_files <- datasources[config_check]
  non_config_files <- datasources[!config_check]

  # goal is a single list of connection objects...
  config_output <- list()
  non_config_output <- list()

  ## handle config files
  if (length(config_files) > 0) {
    config_check <- file_exists(config_files);
    config_files_exist <- config_files[config_check]
    config_files_nonexist <- config_files[!config_check]
    if (length(config_files_nonexist) > 0) {
      warning(
        "The following config files do not exist and will be removed: "
        ,paste(paste0("'",config_files_nonexist, "'"), collapse=", ")
        )
    }

    # connect to DBs
    config_output <- config_files_exist %>%
      map(~ {
        suppressWarnings(cfg <- config::get(file = .x))
        names(cfg) %>% map(~{
          curr <- flatten(cfg[.x])
          con <- do.call(DBI::dbConnect, args = curr)
          test_output <- test_single_database(datasource = con, label = .x, tests = tests)
          DBI::dbDisconnect(con)
          test_output
        })
      })

  }

  ## handle non-config files (i.e. DSNs)
  if (length(non_config_files) > 0) {
    all_dsns <- odbc::odbcListDataSources()
    if ("dsn" %in% non_config_files) {
      # add all DSNS
      non_config_dsns <- all_dsns$name
    } else {
      # check that DSNs exist
      dsn_check <- non_config_files %in% all_dsns$name
      warning(
        "The following DSNs were not found and will be removed: "
        , paste(paste0("'",non_config_files[!dsn_check], "'"), collapse=", ")
        )
      non_config_dsns <- non_config_files[dsn_check]
    }

    # connect to DBs
    if (length(non_config_dsns) > 0) {
      # connect to DSNs safely
      non_config_output <- non_config_dsns %>%
        map(
          ~ {
            con <- DBI::dbConnect(odbc::odbc(), .x)
            test_output <- test_single_database(datasource = con, label = .x, tests = tests)
            DBI::dbDisconnect(con)
            test_output
          }
      )
    }
  }

  return(c(unlist(config_output, recursive = FALSE), non_config_output))
}

#' @export
test_databases.DBIConnection <- function(datasources = NULL, tests = pkg_test()) {
  message("DBI")
  test_single_database(datasource = datasources, tests = pkg_test(), label = class(datasources)[[1]])
}

#' @export
test_databases.tbl_sql <- function(datasources = NULL, tests = pkg_test()) {
  message("TBL_SQL")
  test_single_database(datasource = datasources, tests = pkg_test(), label = datasources[["ops"]][["x"]])
}


#' @title Test Single Database
#'
#' @description Run a single datasource through the testing suite.  Typically, this
#' object would be a connection or a `tbl_sql`
#'
#' @param datasource The datasource to test against.  Either a DBI connection or a tbl_sql
#' @param tests optional A character vector of yaml tests to execute.
#' References `dbtest` test suite by default
#' @param label optional The label to give the test.  If not provided, one will be generated
#'
#' @return A list object with the label and testthat results
#'
#' @examples
#'
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' res <- test_single_database(con, pkg_test("simple-tests.yml"))
#' DBI::dbDisconnect(con)
#'
#' @seealso test_databases
#' @export
test_single_database <- function(datasource, tests = pkg_test(), label = NULL) {
  reporter <- MultiReporter$new(
    reporters = list(
      MinimalReporter$new()
      , ListReporter$new()
    )
  )

  r <- with_reporter(
    reporter, {
      tests %>% map(
        ~ {
          # get ListReporter, if any
          lr <- reporter$reporters[
            as.logical(
              reporter$reporters %>%
                lapply(function(x) {
                  "ListReporter" %in% class(x)
                })
            )
          ]
          # set test filename
          if (length(lr) > 0) {
            lr[[1]]$start_file(path_ext_remove(path_file(.x)))
          }

          testthat_database(
            datasource = datasource,
            tests = .x
          )
        }
      )
    }
  )

  if (is.null(label) & isS4(datasource)) {
    label <- class(datasource)[1]
  }
  if (is.null(label) & "tbl_sql" %in% class(datasource)) {
    label <- class(remote_con(datasource))[1]
  }


  df <- structure(
    r$reporters[[2]]$results$as_list(),
    class = "testthat_results"
  )

  list(
    connection = label,
    results = df
  ) %>%
    as_dbtest_results()
}

testthat_database <- function(datasource, tests = pkg_test()) {

  # Load test scripts from YAML format
  if (class(tests) == "character") tests <- read_yaml(tests)

  if (class(tests) != "list") error("Tests need to be in YAML format")

  # Address test data
  if (isS4(datasource) && inherits(datasource, "DBIConnection")) {
    remote_df <- build_remote_tbl(datasource, testdata)

    local_df <- testdata
  }
  if ("tbl_sql" %in% class(datasource)) {
    remote_df <- head(datasource, 1000)
    local_df <- collect(remote_df)
  }
  stopifnot(
    inherits(remote_df, "tbl_sql")
  )

  # Create a testing function that lives inside the new testthat env
  run_test <- function(verb, vector_expression) {
    f <- parse_expr(vector_expression)

    if (verb %in% c("summarise", "summarize")) manip <- . %>% summarise(!!f) %>% pull()
    if (verb == "mutate") manip <- . %>% mutate(!!f) %>% pull()
    if (verb == "arrange") {
      manip <- . %>%
        mutate(new_col = !!f) %>%
        arrange(!!f) %>%
        collect() %>%
        pull("new_col")
    }
    if (verb == "filter") manip <- . %>% filter(!!f) %>% pull()
    if (verb == "group_by") manip <- . %>% group_by(!!f) %>% summarise() %>% pull() %>% sort()

    test_that(paste0(verb, ": ", vector_expression), {
      invisible({
        expect_equal(
          manip(local_df),
          manip(remote_df)
        )
      })
    })
  }

  # dplyr test orchestrator
  tests %>%
    map(~ {
      curr_test <- .x
      context(names(curr_test))
      curr_test %>%
        flatten() %>%
        map2(
          names(.)
          , ~ run_test(.y, .x)
        )
    })
}
