context("plot_tests")

test_that("works with test_single_database", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_single_database(
    con
    , pkg_test("simple-tests.yml")
  )
  dbDisconnect(con)

  gg <- plot_tests(output)
  expect_s3_class(gg, c("gg", "ggplot"))
})

test_that("works witih test_database", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_database(
    pkg_config("config.yml")
    , pkg_test("simple-tests.yml")
  )
  dbDisconnect(con)

  gg <- plot_tests(output)
  expect_s3_class(gg, c("gg", "ggplot"))
})

test_that("works with multiple test_database", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_database(
    pkg_config("multiple.yml")
    , pkg_test("simple-tests.yml")
  )
  dbDisconnect(con)

  gg <- plot_tests(output)
  expect_s3_class(gg, c("gg", "ggplot"))
})
