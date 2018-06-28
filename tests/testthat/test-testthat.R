context("test_database")

test_that("works with a connection object", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_database(
    con
    , pkg_test("simple-tests-alt.yml")
  )
  dbDisconnect(con)

  expect_s3_class(output, "dbtest_results")
  expect_equal(
    output$results %>% as.data.frame() %>%
      distinct(file) %>% pull()
    , "simple-tests-alt"
  )
})

test_that("works with tbl_sql object", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  tdat <- copy_to(con, testdata, "test-database")
  output <- test_database(
    tdat
    , pkg_test("simple-tests-alt.yml")
  )
  dbDisconnect(con)

  expect_s3_class(output, "dbtest_results")
  expect_equal(
    output$results %>% as.data.frame() %>%
      distinct(file) %>% pull()
    , "simple-tests-alt"
  )
})

test_that("works with multiple test files", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_database(
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

test_that("works on successive tests to same connection", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")

  set.seed(1234)
  output <- test_database(
    con
    , pkg_test("simple-tests.yml")
  )

  set.seed(1234)
  output2 <- tryCatch({test_database(
    con
    , pkg_test("simple-tests.yml")
  )}, error = function(x){stop(x)})
  dbDisconnect(con)

  expect_s3_class(output, "dbtest_results")

  expect_s3_class(output, "dbtest_results")
})

test_that("works with a yaml file", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_database(
    pkg_config("config.yml")
    , pkg_test("simple-tests.yml")
  )
  dbDisconnect(con)

  expect_s3_class(output[[1]], "dbtest_results")
})

test_that("works with multiple connections in a yaml file", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_database(
    pkg_config("multiple.yml")
    , pkg_test("simple-tests.yml")
  )
  dbDisconnect(con)

  lapply(output, expect_s3_class, class = "dbtest_results")
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
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  con2 <- dbConnect(RSQLite::SQLite(), ":memory:")
  output <- test_database(
    list(con, con2)
    , pkg_test("simple-tests-alt.yml")
  )
  lapply(output, expect_s3_class, class = "dbtest_results")
  lapply(output, function(x){
    expect_equal(
      x$results %>%
        as.data.frame() %>%
        distinct(file) %>%
        pull()
      , "simple-tests-alt"
    )
  })
})

test_that("works with a list of tbl_sql objects", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  tdat <- copy_to(con, testdata, "test-list-tbl-sql")
  tdat2 <- copy_to(con, testdata, "test-list-tbl-sql2")
  output <- test_database(
    list(tdat, tdat2)
    , pkg_test("simple-tests-alt.yml")
  )
  dbDisconnect(con)

  lapply(output, expect_s3_class, class = "dbtest_results")
  lapply(output, function(x){
    expect_equal(
      x$results %>%
        as.data.frame() %>%
        distinct(file) %>%
        pull()
      , "simple-tests-alt"
    )
  })
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
  output <- test_database(
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

test_that("works with different integer types", {
  conn_path <- rprojroot::find_testthat_root_file("conn.yml")
  if (!fs::file_exists(conn_path)) {
    skip("requires a postgres database")
  }
  raw_conn <- yaml::read_yaml(conn_path)$default
  if (!"pg" %in% names(raw_conn)) {
    skip("requires a postgres database")
  }

  tmp_file <- fs::file_temp("integer-test", ext=".yml")
  write_test(file = tmp_file
             , header = "integer-conversion"
             , expr = "fld_integer"
             , overwrite = TRUE
             )


  pg <- raw_conn$pg
  con <- do.call(DBI::dbConnect, pg)
  output <- suppressMessages(test_single_database(con, tmp_file))

  expect_equal(
    as.data.frame(output)[3,"results.failed"]
    , 0
    , info = "Test should pass if integer64 compares to integer"
    )
})

