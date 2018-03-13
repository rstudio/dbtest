#' @export
test_databases <- function(datasources = NULL,
                           tests = "default") {
  if (is.null(datasources))
    datasources <- ""

  if (datasources == "dsn") {
    odbc::odbcListDataSources()$name %>%
      map( ~ {
        con <- DBI::dbConnect(odbc::odbc(), dsn = .x)
        test_single_database(con, tests = tests)
        dbDisconnect(con)
      })
  } else if (datasources == "config" |
             datasources == "" |
             (all(tolower(fs::path_ext(datasources)) %in% c("yml","yaml")) &&
             all(fs::file_exists(datasources)))
             ) {

    if (datasources == "") {
      file_path = default_config_path()
    } else if (
      all(tolower(fs::path_ext(datasources)) %in% c("yml", "yaml"))
      && all(fs::file_exists(datasources))
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
        curr <- purrr::flatten(cons[.x])
        con <- do.call(DBI::dbConnect, args = curr)
        tests <- test_single_database(con, label = .x)
        dbDisconnect(con)
        tests
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

rm_decoys <- function(x) {
  x[!names(x) %in% c("Table","TableSchema")]
}

#' @export
test_single_database <- function(datasource, label = NULL, tests = "default") {

  reporter <- testthat::MultiReporter$new(
    reporters = list(MinimalReporter$new(), ListReporter$new())
  )

  r <- with_reporter(
    reporter,
    testthat_database(
      datasource = datasource,
      tests = tests
      )
    )

  if(is.null(label) & isS4(datasource))
    label <- class(datasource)[1]
  if(is.null(label) & "tbl_sql" %in% class(datasource))
    label <- class(dbplyr::remote_con(datasource))[1]


  df <- structure(
    r$reporters[[2]]$results$as_list(),
    class = "testthat_results"
  )

  list(
    connection = label,
    results = df
  )
}

testthat_database <- function(datasource, label = NULL, tests = "default") {

  # Load test scripts from YAML format
  if (tests == "default") {
    tests <- yaml::read_yaml(default_tests_path())
  } else {
    if (class(tests) == "character") tests <- yaml::read_yaml(tests)
  }
  if (class(tests) != "list") error("Tests need to be in YAML format")

  # Address test data
  if(isS4(datasource)){
    remote_df <- copy_to(datasource, testdata)
    local_df  <- testdata
  }
  if("tbl_sql" %in% class(datasource)){
    remote_df <- head(datasource, 1000)
    local_df  <- collect(remote_df)
  }

  # Create a testing function that lives inside the new testthat env
  run_test <- function(verb, vector_expression) {
    f <- parse_expr(vector_expression)

    if (verb == "summarise") manip <- . %>% summarise(!! f) %>% pull()
    if (verb == "mutate") manip <- . %>% mutate(!! f) %>% pull()
    if (verb == "arrange") manip <- . %>% arrange(!! f) %>% pull()
    if (verb == "filter") manip <- . %>% filter(!! f) %>% pull()
    if (verb == "group_by") manip <- . %>% group_by(!! f) %>% summarise() %>% pull()

    test_that(paste0(vector_expression), {
      invisible({
        expect_equal(
          manip(local_df),
          manip(remote_df)
        )
      })
    })
  }

  # dplyr test orchestrator
  verbs <- c(
    "mutate",
    "filter",
    "summarise",
    "group_by",
    "arrange"
  )
  verbs %>%
    map(~{
      verb <- .x
      context(verb)
      tests %>%
        purrr::flatten() %>%
        map(~.x[verb]) %>%
        purrr::flatten() %>%
        map(~run_test(verb, .x))
    })
}
