context("base_scalar")

expect_mutate_equivalent <- function(expr,
                                     db = db_test_table,
                                     local = test_table) {
  expr <- rlang::enquo(expr)
  manip <- . %>% dplyr::mutate(value = !!expr) %>% dplyr::pull()
  expect_equal(manip(db), manip(local))
}

test_that("sd() scalar", {
  expect_mutate_equivalent(sd(fld_double))
})

test_that("abs()",{
  expect_mutate_equivalent(abs(fld_double))
})

test_that("acos()",{
  expect_mutate_equivalent(acos(fld_binary))
})

test_that("acosh()",{
  expect_mutate_equivalent(acosh(fld_double))
})

test_that("asin()",{
  expect_mutate_equivalent(asin(fld_binary))
})


test_that("asinh()",{
  expect_mutate_equivalent(asinh(fld_double))
})

test_that("atan()",{
  expect_mutate_equivalent(atan(fld_double))
})

test_that("atan2()",{
  expect_mutate_equivalent(atan2(fld_double, 1))
})

test_that("atanh()",{
  expect_mutate_equivalent(atanh(fld_binary))
})

test_that("pracma::ceil()",{
  expect_mutate_equivalent(ceil(fld_double))
})

test_that("ceiling()",{
  expect_mutate_equivalent(ceiling(fld_double))
})

test_that("cos()",{
  expect_mutate_equivalent(cos(fld_binary))
})

test_that("cosh()",{
  expect_mutate_equivalent(cosh(fld_binary))
})

test_that("pracma::cot()",{
  expect_mutate_equivalent(cot(fld_double))
})

test_that("pracma::coth()",{
  expect_mutate_equivalent(coth(fld_double))
})

test_that("exp()",{
  expect_mutate_equivalent(exp(fld_double))
})

test_that("floor()",{
  expect_mutate_equivalent(floor(fld_double))
})

test_that("log()",{
  expect_mutate_equivalent(log(fld_double))
})

test_that("log10()",{
  expect_mutate_equivalent(log10(fld_double))
})

test_that("round()",{
  expect_mutate_equivalent(round(fld_double))
})

test_that("sign()",{
  expect_mutate_equivalent(sign(fld_double))
})

test_that("sin()",{
  expect_mutate_equivalent(sin(fld_double))
})

test_that("sinh()",{
  expect_mutate_equivalent(sinh(fld_double))
})

test_that("sqrt()",{
  expect_mutate_equivalent(sqrt(fld_double))
})

test_that("tan()",{
  expect_mutate_equivalent(tan(fld_double))
})

test_that("tanh()",{
  expect_mutate_equivalent(tanh(fld_double))
})

test_that("tolower()",{
  expect_mutate_equivalent(tolower(fld_character))
})

test_that("toupper()",{
  expect_mutate_equivalent(toupper(fld_character))
})


test_that("trimws()",{
  expect_mutate_equivalent(trimws(fld_character))
})

test_that("nchar()",{
  expect_mutate_equivalent(nchar(fld_character))
})

test_that("substr()",{
  expect_mutate_equivalent(substr(fld_character, 1, 1))
})

test_that("ifelse()",{
  expect_mutate_equivalent(ifelse(fld_binary == 1, "Yes", "No"))
})

test_that("desc()",{
  expect_mutate_equivalent(fld_character)
})

test_that("is.null()",{
  expect_mutate_equivalent(is.null(fld_character))
})

test_that("is.na()",{
  expect_mutate_equivalent(is.na(fld_character))
})

test_that("coalesce()",{
  expect_mutate_equivalent(coalesce(fld_character, fld_character))
})

test_that("as.numeric()",{
  expect_mutate_equivalent(as.numeric(fld_logical))
})

test_that("as.double()",{
  expect_mutate_equivalent(as.double(fld_logical))
})

test_that("as.integer()",{
  expect_mutate_equivalent(as.integer(fld_double))
})

test_that("between()",{
  expect_mutate_equivalent(fld_double)
})

test_that("pmin()",{
  expect_mutate_equivalent(pmin(fld_binary, fld_integer, fld_double))
})

test_that("pmax()",{
  expect_mutate_equivalent(pmax(fld_binary, fld_integer, fld_double))
})




