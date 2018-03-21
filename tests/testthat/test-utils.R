context("default_test_path")

test_that("returns yaml files", {
  expect_is(default_test_path(), "character")
  expect_match(default_test_path(),"*.yml")
})

test_that("returns existing files", {
  expect_true(all(fs::file_exists(default_test_path())))
})

context("all_tests")

test_that("returns expected types", {
  expect_is(all_tests(), "character")
  expect_match(all_tests(), "*.yml")
})

test_that("returns multiple files", {
  expect_gt(length(all_tests()), 1)
})

test_that("returns existing files",  {
  expect_true(all(fs::file_exists(all_tests())))
})

context("default_config_path")

test_that("returns yaml files", {
  expect_is(default_config_path(), "character")
  expect_match(default_config_path(), "*.yml")
})

test_that("returns existing files", {
  expect_true(all(fs::file_exists(default_config_path())))
})
