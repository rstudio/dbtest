context("plot_tests")

test_that("works with test_database and DBI connection", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_database(
    con
    , pkg_test("simple-tests.yml")
  )
  dbDisconnect(con)

  gg <- plot_tests(output)[[1]]
  expect_s3_class(gg, c("gg", "ggplot"))
})

test_that("works with test_database", {
  output <- test_database(
    pkg_config("config.yml")
    , pkg_test("simple-tests.yml")
  )

  gg <- plot_tests(output)[[1]]
  expect_s3_class(gg, c("gg", "ggplot"))
})

test_that("works with multiple test_database", {
  output <- test_database(
    pkg_config("multiple.yml")
    , pkg_test("simple-tests.yml")
  )

  gg <- plot_tests(output)[[1]]
  expect_s3_class(gg, c("gg", "ggplot"))
})

test_that("works with multiple test files", {
  skip("TODO: write test")
})

context("plot_summary")

test_that("works with test_database", {
  skip("TODO: write test")
})

test_that("works with multiple test files", {
  skip("TODO: write test")
})

context("print_interactive")

test_that("works in a non-interactive session", {
  skip("TODO: write test")
})

