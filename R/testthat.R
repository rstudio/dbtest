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
#' @param tests optional The tests to execute.  References `dbtest` test suite by default
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
test_databases <- function(datasources = NULL,
                           tests = pkg_test()) {
  if (is.null(datasources))
    datasources <- ""

  if (datasources == "dsn") {
    odbcListDataSources()$name %>%
      map( ~ {
        con <- dbConnect(odbc(), dsn = .x)
        test_single_database(con, tests = tests)
        dbDisconnect(con)
      })
  } else if (datasources == "config" |
             datasources == "" |
             (all(tolower(path_ext(datasources)) %in% c("yml","yaml")) &&
             all(file_exists(datasources)))
             ) {

    if (datasources == "") {
      file_path = pkg_config()
    } else if (
      all(tolower(path_ext(datasources)) %in% c("yml", "yaml"))
      && all(file_exists(datasources))
      ) {
      file_path = datasources
    } else {
      file_path = NULL
    }

    if (length(file_path) > 1)
      stop(sprintf("Multiple datasources are not currently supported"))

    # Suppress warnings until config issue is resolved
    # https://github.com/rstudio/config/issues/12
    suppressWarnings(cons <- config::get(file = file_path))

    names(cons) %>%
      map( ~ {
        curr <- flatten(cons[.x])
        con <- do.call(dbConnect, args = curr)
        test_output <- test_single_database(con, label = .x, tests = tests)
        dbDisconnect(con)
        test_output
      })

  } else {
    stop(
      paste0(
        "Unrecognized value for `datasources`: '%s'"
        ,
        ", please use either an existing config YAML file, 'config', 'dsn' or NULL"
      ) %>%
        sprintf(datasources)
    )
  }

}

#' @title Test Single Database
#'
#' @description Run a single datasource through the testing suite.  Typically, this
#' object would be a connection or a `tbl_sql`
#'
#' @param datasource The datasource to test against.  Either a DBI connection or a tbl_sql
#' @param tests optional The tests to execute.  References `dbtest` test suite by default
#' @param label optional The label to give the test.  If not provided, one will be generated
#'
#' @return A list object with the label and testthat results
#'
#' @examples
#'
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' res <- test_single_database(con, "simple-tests.yml")
#' DBI::dbDisconnect(con)
#'
#' @seealso test_databases
#' @export
test_single_database <- function(datasource, tests = pkg_test(), label = NULL) {

  reporter <- MultiReporter$new(
    reporters = list(MinimalReporter$new()
                     , ListReporter$new()
                     )
  )

  r <- with_reporter(
    reporter, {
      testthat_database(
        datasource = datasource,
        tests = tests
      )
    }
    )

  if(is.null(label) & isS4(datasource))
    label <- class(datasource)[1]
  if(is.null(label) & "tbl_sql" %in% class(datasource))
    label <- class(remote_con(datasource))[1]


  df <- structure(
    r$reporters[[2]]$results$as_list(),
    class = "testthat_results"
  )

  list(
    connection = label,
    results = df
  )
}

testthat_database <- function(datasource, tests = pkg_test()) {

  # Load test scripts from YAML format
  if (class(tests) == "character") tests <- read_yaml(tests)

  if (class(tests) != "list") error("Tests need to be in YAML format")

  # Address test data
  if(isS4(datasource)){
    remote_df <- copy_to(datasource, testdata
                         , name=tolower(
                           paste(
                             sample(LETTERS, size=20, replace=TRUE)
                             ,collapse='')
                           )
                         )

    local_df  <- testdata
  }
  if("tbl_sql" %in% class(datasource)){
    remote_df <- head(datasource, 1000)
    local_df  <- collect(remote_df)
  }

  # Create a testing function that lives inside the new testthat env
  run_test <- function(verb, vector_expression) {
    f <- parse_expr(vector_expression)

    if (verb %in% c("summarise","summarize")) manip <- . %>% summarise(!! f) %>% pull()
    if (verb == "mutate") manip <- . %>% mutate(!! f) %>% pull()
    if (verb == "arrange") manip <- . %>% arrange(!! f) %>% pull()
    if (verb == "filter") manip <- . %>% filter(!! f) %>% pull()
    if (verb == "group_by") manip <- . %>% group_by(!! f) %>% arrange(!! f) %>% summarise() %>% pull()

    test_that(paste0(verb,": ",vector_expression), {
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
    map(~{
      curr_test <- .x
      context(names(curr_test))
      curr_test %>%
        flatten() %>%
        map2(names(.)
             , ~run_test(.y, .x))
    })
}
