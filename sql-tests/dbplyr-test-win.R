context("base_win")

expect_window_equivalent <- function(expr,
                                     db = db_test_table,
                                     local = test_table){

  expr <- enquo(expr)
  manip <- . %>% arrange(!!expr) %>% mutate(value = !!expr) %>% pull()
  expect_equal(manip(db), manip(local))
}

test_that("row_number()",{
  expect_window_equivalent(row_number(fld_double))
})

test_that("min_rank()",{
  expect_window_equivalent(min_rank(fld_double))
})

test_that("rank()",{
  expect_window_equivalent(rank(fld_double))
})

test_that("dense_rank()",{
  expect_window_equivalent(dense_rank(fld_double))
})

test_that("percent_rank()",{
  expect_window_equivalent(percent_rank(fld_double))
})

test_that("cume_dist()",{
  expect_window_equivalent(cume_dist(fld_double))
})

test_that("ntile()",{
  expect_window_equivalent(ntile(fld_double, 2))
})

test_that("first()",{
  expect_window_equivalent(first(fld_integer))
})

test_that("last()",{
  expect_window_equivalent(last(fld_integer))
})

test_that("nth()",{
  expect_window_equivalent(nth(fld_integer, 5))
})

test_that("lead()",{
  expect_window_equivalent(lead(fld_integer))
})

test_that("lag()",{
  expect_window_equivalent(lag(fld_integer))
})

test_that("cummean()",{
  expect_window_equivalent(cummean(fld_double))
})

test_that("cumsum()",{
  expect_window_equivalent(cumsum(fld_double))
})

test_that("cummin()",{
  expect_window_equivalent(cummin(fld_double))
})

test_that("sd() win",{
  expect_window_equivalent(value = sd(fld_double))
})

test_that("cummax()",{
  expect_window_equivalent(cummax(fld_double))
})
