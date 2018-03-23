context("pkg_test")

test_that("returns yaml files", {
  expect_is(pkg_test(), "character")
  expect_match(pkg_test(),"*.yml")
})

test_that("returns existing files", {
  expect_true(all(fs::file_exists(pkg_test())))
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

context("pkg_config")

test_that("returns yaml files", {
  expect_is(pkg_config(), "character")
  expect_match(pkg_config(), "*.yml")
})

test_that("returns existing files", {
  expect_true(all(fs::file_exists(pkg_config())))
})
