library(dplyr)
library(testthat)

table_name <- dbplyr:::random_table_name()

test_table <- test_data()

context("basic")

test_that("copy_to()",{
  expect_silent({
    dplyr::copy_to(con, test_table, table_name, temporary = FALSE)
  })
})

db_test_table <- dplyr::tbl(con, table_name)

expect_mutate_equivalent <- function(expr,
                                     db = db_test_table,
                                     local = test_table) {
  expr <- enquo(expr)
  manip <- . %>% mutate(value = !!expr) %>% pull()
  expect_equal(manip(db), manip(local))
}

expect_summarise_equivalent <- function(expr,
                                        db = db_test_table,
                                        local = test_table){

  expr <- enquo(expr)
  manip <- . %>% summarise(value = !!expr) %>% pull()
  expect_equal(manip(db), manip(local))
}

expect_window_equivalent <- function(expr,
                                        db = db_test_table,
                                        local = test_table){

  expr <- enquo(expr)
  manip <- . %>% arrange(!!expr) %>% mutate(value = !!expr) %>% pull()
  expect_equal(manip(db), manip(local))
}



# base_agg tests --------------------------------

context("base_agg")



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




# base_win tests --------------------------------
context("base_win")

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


# base_scalar tests -----------------------------
context("base_scalar")

test_that("sd() scalar", {
  expect_mutate_equivalent(sd(fld_double))
})

test_that("abs()",{
  expect_mutate_equivalent(value = abs(fld_double))
})

test_that("acos()",{
  expect_mutate_equivalent(value = acos(fld_binary))
})


test_that("acosh()",{
  expect_mutate_equivalent(value = acosh(fld_double))
})

test_that("asin()",{
  expect_mutate_equivalent(value = asin(fld_binary))
})


test_that("asinh()",{
  expect_mutate_equivalent(value = asinh(fld_double))
})



test_that("atan()",{
  expect_mutate_equivalent(value = atan(fld_double))
})


test_that("atan2()",{
  expect_mutate_equivalent(value = atan2(fld_double, 1))
})

test_that("atanh()",{
  expect_mutate_equivalent(value = atanh(fld_binary))
})


test_that("pracma::ceil()",{
  expect_mutate_equivalent(value = ceil(fld_double))
})

test_that("ceiling()",{
  expect_mutate_equivalent(value = ceiling(fld_double))
})


test_that("cos()",{
  expect_mutate_equivalent(value = cos(fld_binary))
})

test_that("cosh()",{
  expect_mutate_equivalent(value = cosh(fld_binary))
})

test_that("pracma::cot()",{
  expect_mutate_equivalent(value = cot(fld_double))
})

test_that("pracma::coth()",{
  expect_mutate_equivalent(value = coth(fld_double))
})



test_that("exp()",{
  expect_mutate_equivalent(value = exp(fld_double))
})


test_that("floor()",{
  expect_mutate_equivalent(value = floor(fld_double))
})

test_that("log()",{
  expect_mutate_equivalent(value = log(fld_double))
})

test_that("log10()",{
  expect_mutate_equivalent(value = log10(fld_double))
})

test_that("round()",{
  expect_mutate_equivalent(value = round(fld_double))
})

test_that("sign()",{
  expect_mutate_equivalent(value = sign(fld_double))
})

test_that("sin()",{
  expect_mutate_equivalent(value = sin(fld_double))
})

test_that("sinh()",{
  expect_mutate_equivalent(value = sinh(fld_double))
})

test_that("sqrt()",{
  expect_mutate_equivalent(value = sqrt(fld_double))
})

test_that("tan()",{
  expect_mutate_equivalent(value = tan(fld_double))
})


test_that("tanh()",{
  expect_mutate_equivalent(value = tanh(fld_double))
})

test_that("tolower()",{
  expect_mutate_equivalent(value = tolower(fld_character))
})

test_that("toupper()",{
  expect_mutate_equivalent(value = toupper(fld_character))
})


test_that("trimws()",{
  expect_mutate_equivalent(value = trimws(fld_character))
})

test_that("nchar()",{
  expect_mutate_equivalent(value = nchar(fld_character))
})

test_that("substr()",{
  expect_mutate_equivalent(value = substr(fld_character, 1, 1))
})

test_that("ifelse()",{
  expect_mutate_equivalent(value = ifelse(fld_binary == 1, "Yes", "No"))
})

# sql() - Not sure what to do with this function.

test_that("desc()",{
  expect_mutate_equivalent(value = fld_character)
})

test_that("is.null()",{
  expect_mutate_equivalent(value = is.null(fld_character))
})

test_that("is.na()",{
  expect_mutate_equivalent(value = is.na(fld_character))
})

test_that("coalesce()",{
  expect_mutate_equivalent(value = coalesce(fld_character, fld_character))
})

test_that("as.numeric()",{
  expect_mutate_equivalent(value = as.numeric(fld_logical))
})

test_that("as.double()",{
  expect_mutate_equivalent(value = as.double(fld_logical))
})

test_that("as.integer()",{
  expect_mutate_equivalent(value = as.integer(fld_double))
})


test_that("between()",{
  expect_mutate_equivalent(value = fld_double)
})

test_that("pmin()",{
  expect_mutate_equivalent(value = pmin(fld_binary, fld_integer, fld_double))
})

test_that("pmax()",{
  expect_mutate_equivalent(value = pmax(fld_binary, fld_integer, fld_double))
})

test_that("db_drop_table()",{
  expect_silent({
    dplyr::db_drop_table(con, table_name)
  })
})



