context("base_scalar")

expect_mutate_equivalent <- function(expr,
                                     db = db_test_table,
                                     local = test_table) {
  expr <- enquo(expr)
  manip <- . %>% mutate(value = !!expr) %>% pull()
  expect_equal(manip(db), manip(local))
}

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




