context("base_agg")

expect_summarise_equivalent <- function(expr,
                                        db = db_test_table,
                                        local = test_table){

  expr <- enquo(expr)
  manip <- . %>% summarise(value = !!expr) %>% pull()
  expect_equal(manip(db), manip(local))
}

test_that("n()", {
  expect_summarise_equivalent(n())
})

test_that("mean()", {
  expect_summarise_equivalent(mean(fld_double))
})

test_that("var()", {
  expect_summarise_equivalent(var(fld_double))
})

test_that("sd() agg",{
  expect_summarise_equivalent(sd(fld_double))
})

test_that("sum()",{
  expect_summarise_equivalent(sum(fld_double))
})

test_that("min()",{
  expect_summarise_equivalent(min(fld_double))
})

test_that("max()",{
  expect_summarise_equivalent(max(fld_double))
})

test_that("n_distinct()",{
  expect_summarise_equivalent(n_distinct(fld_double))
})


