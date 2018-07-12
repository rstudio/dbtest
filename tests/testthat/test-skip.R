context("skip")

test_that("handles specific test", {
  skip_file <- fs::file_temp("skip", ext = "yml")
  skip_text <- "skip subtract mutate"
  write_skip(
    file = skip_file
    , text = skip_text
    , db = "sqlite"
    , skip_file = "simple-tests"
    , context = "subtract"
    , test = "mutate"
  )

  test_output <- test_database(
    datasource = pkg_config("multiple.yml")
    , tests = pkg_test("simple-tests.yml")
    , skip = skip_file
  )

  test_detail <- get_dbtest_detail(test_output)

  expect_equal(
    test_detail[
     test_detail$test == "mutate: fld_double - fld_integer"
     , c("sqlite")] %>%
     pull()
    , skip_text)
})

test_that("wildcard match database", {
  skip("TODO: write test")
})

test_that("wildcard match file", {
  skip("TODO: write test")
})

test_that("wildcard match context", {
  skip("TODO: write test")
})

test_that("wildcard match test", {
  skip("TODO: write test")
})

test_that("wildcard match all does nothing", {
  skip("TODO: write test")
})

test_that("handles multiple skip files", {
  skip_file <- fs::file_temp("skip", ext = "yml")
  skip_file_alt <- fs::file_temp("skip", ext = "yml")

  skip_text <- "skip subtract mutate"
  skip_text_alt <- "skip add filter"
  write_skip(
    file = skip_file
    , text = skip_text
    , db = "sqlite"
    , skip_file = "simple-tests"
    , context = "subtract"
    , test = "mutate"
  )
  write_skip(
    file = skip_file_alt
    , text = skip_text_alt
    , db = "sqlite"
    , skip_file = "simple-tests"
    , context = "add"
    , test = "filter"
  )

  test_output <- test_database(
    datasource = pkg_config("multiple.yml")
    , tests = pkg_test("simple-tests.yml")
    , skip = c(skip_file, skip_file_alt)
  )

  test_detail <- get_dbtest_detail(test_output)

  expect_equal(
    test_detail[
     test_detail$test == "mutate: fld_double - fld_integer"
     , c("sqlite")] %>%
     pull()
    , skip_text)
  expect_equal(
    test_detail[
      test_detail$test == "filter: fld_double + fld_integer < 10"
      , c("sqlite") ] %>%
      pull()
    , skip_text_alt
  )
})
