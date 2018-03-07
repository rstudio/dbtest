#' @export
test_database <- function(tests = "default") {
  reporter <- MultiReporter$new(
    reporters = list(ProgressReporter$new(), ListReporter$new())
  )
  r <- with_reporter(reporter, testthat_database("default"))

  df <- structure(
    r$reporters[[2]]$results$as_list(),
    class = "testthat_results"
  )
  df <- as.data.frame(df)
  df[, 2:length(df)]
}

testthat_database <- function(tests = "default") {
  if (tests == "default") {
    tests <- read_yaml(default_tests_path())
  } else {
    if (class(tests) == "character") tests <- read_yaml(tests)
  }

  if (class(tests) != "list") error("Tests need to be in YAML format")

  local_df <- mtcars %>%
    mutate(
      x = mpg,
      y = wt
    )

  remote_df <- mtcars %>%
    mutate(
      x = mpg,
      y = wt
    )

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
