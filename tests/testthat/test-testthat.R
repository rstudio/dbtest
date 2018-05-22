context("test_single_database")

test_that("works with a connection object", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_single_database(
    con
    , pkg_test("simple-tests.yml")
  )
  dbDisconnect(con)

  expect_s3_class(output, "dbtest_results")
})

test_that("works with tbl_sql object", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  tdat <- copy_to(con, testdata, "test-single-database")
  output <- test_single_database(
    tdat
    , pkg_test("simple-tests.yml")
  )
  dbDisconnect(con)

  expect_s3_class(output, "dbtest_results")
})

test_that("works with multiple test files", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_single_database(
    con
    , c(
      pkg_test("simple-tests.yml")
      , pkg_test("simple-tests-alt.yml")
    )
  )
  dbDisconnect(con)

  expect_s3_class(output, "dbtest_results")
  expect_equal(
    output$results %>%
      as.data.frame() %>%
      .$file %>%
      unique()
    , c("simple-tests", "simple-tests-alt")
  )
})

context("test_databases")

test_that("works with a yaml file", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_databases(
    pkg_config("config.yml")
    , pkg_test("simple-tests.yml")
  )
  dbDisconnect(con)

  expect_s3_class(output[[1]], "dbtest_results")
})

test_that("works with multiple connections", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_databases(
    pkg_config("multiple.yml")
    , pkg_test("simple-tests.yml")
  )
  dbDisconnect(con)

  expect_s3_class(output[[1]], "dbtest_results")
  expect_s3_class(output[[2]], "dbtest_results")
  expect_equal(length(output), 2)
  expect_equal(
    lapply(output, length) %>% as.double()
    , c(2, 2)
  )
})

test_that("works with multiple test files", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_databases(
    pkg_config("config.yml")
    , c(
      pkg_test("simple-tests.yml")
      , pkg_test("simple-tests-alt.yml")
    )
  )
  dbDisconnect(con)

  expect_s3_class(output[[1]], "dbtest_results")
  expect_equal(
    output[[1]]$results %>%
      as.data.frame() %>%
      .$file %>%
      unique()
    , c("simple-tests", "simple-tests-alt")
  )
})
