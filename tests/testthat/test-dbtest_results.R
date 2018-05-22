context("is_dbtest_results")

test_that("false for bad list input", {
  empty <- list()
  just_names <- list(results=list(), connection=character())
  res <- list(); class(res) <- "testthat_results"
  wrong_class_con <- list(results=res, connection=list())

  expect_false(is_dbtest_results(empty))
  expect_false(is_dbtest_results(just_names))
  expect_false(is_dbtest_results(wrong_class_con))
})

test_that("true for valid input", {
  res <- list()
  class(res) <- "testthat_results"
  obj <- as_dbtest_results(list(connection="test", results=res))

  expect_true(is_dbtest_results(obj))
})

context("as_dbtest_results")

test_that("identity on dbtest_results object", {
  obj <- list(results=list(), connection=character())
  class(obj) <- c("dbtest_results")
  expect_identical(obj, as_dbtest_results(obj))
  expect_s3_class(as_dbtest_results(obj), "dbtest_results")
})

test_that("converts a list object", {
  # one liner for object creation?
  res <- list()
  class(res) <- "testthat_results"
  obj <- list(connection="test", results=res)

  expect_false(is_dbtest_results(obj))
  expect_s3_class(as_dbtest_results(obj), "dbtest_results")
})

test_that("fails for a bad list input", {
  empty <- list()
  just_names <- list(results=list(), connection=character())
  res <- list(); class(res) <- "testthat_results"
  wrong_class_con <- list(results=res, connection=list())


  expect_error(as_dbtest_results(empty))
  expect_error(as_dbtest_results(just_names))
  expect_error(as_dbtest_results(wrong_class_con))
})

