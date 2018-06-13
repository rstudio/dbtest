context("test_single_database")

test_that("works with a connection object", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_single_database(
    con
    , pkg_test("simple-tests.yml")
  )
  dbDisconnect(con)

  expect_s3_class(output$results, "testthat_results")
  expect_equal(length(output), 2)
})

test_that("works with tbl_sql object", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  tdat <- copy_to(con, testdata, "test-single-database")
  output <- test_single_database(
    tdat
    , pkg_test("simple-tests.yml")
  )
  dbDisconnect(con)

  expect_s3_class(output$results, "testthat_results")
  expect_equal(length(output), 2)
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

  expect_s3_class(output$results, "testthat_results")
  expect_equal(length(output), 2)
  expect_equal(
    output$results %>%
      as.data.frame() %>%
      .$file %>%
      unique()
    , c("simple-tests", "simple-tests-alt")
  )
})

test_that("works on successive tests to same connection", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")

  set.seed(1234)
  output <- test_single_database(
    con
    , pkg_test("simple-tests.yml")
  )

  set.seed(1234)
  output2 <- tryCatch({test_single_database(
    con
    , pkg_test("simple-tests.yml")
  )}, error = function(x){stop(x)})
  dbDisconnect(con)

  # expect results - TODO - class object
  expect_s3_class(output$results, "testthat_results")
  expect_equal(length(output), 2)

  expect_s3_class(output2$results, "testthat_results")
  expect_equal(length(output2), 2)
})

context("test_databases")

test_that("works with a yaml file", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_databases(
    pkg_config("config.yml")
    , pkg_test("simple-tests.yml")
  )
  dbDisconnect(con)

  expect_s3_class(output[[1]]$results, "testthat_results")
  expect_equal(length(output[[1]]), 2)
})

test_that("works with multiple connections", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_databases(
    pkg_config("multiple.yml")
    , pkg_test("simple-tests.yml")
  )
  dbDisconnect(con)

  expect_s3_class(output[[1]]$results, "testthat_results")
  expect_s3_class(output[[2]]$results, "testthat_results")
  expect_equal(length(output), 2)
  expect_equal(
    lapply(output, length) %>% as.double()
    , c(2, 2)
  )
})

test_that("works with multiple yaml files", {
  skip("Need to write test")
})

test_that("throws out non-existent config files", {
  skip("Need to write test")
})

test_that("works with a list of DBI connections", {
  skip("Need to write test")
})

test_that("works with a list of tbl_sql objects", {
  skip("Need to write test")
})

test_that("works with a DSN", {
  skip("Need to write test - is this even possible? System dependent")
})

test_that("works with a list of DSNs", {
  skip("Need to write test - is this even possible? System dependent")
})

test_that("throws out non-existent DSNs", {
  skip("Need to write test")
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

  expect_s3_class(output[[1]]$results, "testthat_results")
  expect_equal(length(output[[1]]), 2)
  expect_equal(
    output[[1]]$results %>%
      as.data.frame() %>%
      .$file %>%
      unique()
    , c("simple-tests", "simple-tests-alt")
  )
})

