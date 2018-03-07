#' @export
test_single_database <- function(datasource, label = NULL, tests = "default") {

  reporter <- MultiReporter$new(
    reporters = list(ProgressReporter$new(), ListReporter$new())
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
    tests <- read_yaml(default_tests_path())
  } else {
    if (class(tests) == "character") tests <- read_yaml(tests)
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
        flatten() %>%
        map(~.x[verb]) %>%
        flatten() %>%
        map(~run_test(verb, .x))
    })
}
