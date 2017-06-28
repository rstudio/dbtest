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

test_that("cummax()",{
  expect_window_equivalent(cummax(fld_double))
})


# base_scalar tests -----------------------------
context("base_scalar")

test_that("sd() scalar", {
  expect_mutate_equivalent(sd(fld_double))
})

test_that("abs()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = abs(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = abs(fld_double)) %>%
        dplyr::select(value)
    })
  )
})

test_that("acos()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = acos(fld_binary)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = acos(fld_binary)) %>%
        dplyr::select(value)
    })
  )
})


test_that("acosh()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = acosh(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = acosh(fld_double)) %>%
        dplyr::select(value)
    })
  )
})

test_that("asin()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = asin(fld_binary)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = asin(fld_binary)) %>%
        dplyr::select(value)
    })
  )
})


test_that("asinh()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = asinh(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = asinh(fld_double)) %>%
        dplyr::select(value)
    })
  )
})



test_that("atan()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = atan(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = atan(fld_double)) %>%
        dplyr::select(value)
    })
  )
})



test_that("atan2()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = atan2(fld_double, 1)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = atan2(fld_double, 1)) %>%
        dplyr::select(value)
    })
  )
})


test_that("atanh()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = atanh(fld_binary)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = atanh(fld_binary)) %>%
        dplyr::select(value)
    })
  )
})


test_that("pracma::ceil()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = ceil(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect() %>%
        dplyr::mutate(value = as.integer(value))
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = pracma::ceil(fld_double)) %>%
        dplyr::select(value)
    })
  )
})

test_that("ceiling()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = ceiling(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect() %>%
        dplyr::mutate(value = as.integer(value))
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = ceiling(fld_double)) %>%
        dplyr::select(value)
    })
  )
})



test_that("cos()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = cos(fld_binary)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = cos(fld_binary)) %>%
        dplyr::select(value)
    })
  )
})

test_that("cosh()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = cosh(fld_binary)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = cosh(fld_binary)) %>%
        dplyr::select(value)
    })
  )
})

test_that("pracma::cot()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = cot(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = pracma::cot(fld_double)) %>%
        dplyr::select(value)
    })
  )
})


test_that("pracma::coth()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = coth(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = pracma::coth(fld_double)) %>%
        dplyr::select(value)
    })
  )
})



test_that("exp()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = exp(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = exp(fld_double)) %>%
        dplyr::select(value)
    })
  )
})


test_that("floor()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = floor(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect() %>%
        dplyr::mutate(value = as.integer(value))
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = floor(fld_double)) %>%
        dplyr::select(value)
    })
  )
})

test_that("log()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = log(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = log(fld_double)) %>%
        dplyr::select(value)
    })
  )
})

test_that("log10()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = log10(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = log10(fld_double)) %>%
        dplyr::select(value)
    })
  )
})

test_that("round()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = round(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = round(fld_double)) %>%
        dplyr::select(value)
    })
  )
})

test_that("sign()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = sign(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = sign(fld_double)) %>%
        dplyr::select(value)
    })
  )
})

test_that("sin()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = sin(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = sin(fld_double)) %>%
        dplyr::select(value)
    })
  )
})


test_that("sinh()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = sinh(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = sinh(fld_double)) %>%
        dplyr::select(value)
    })
  )
})

test_that("sqrt()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = sqrt(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = sqrt(fld_double)) %>%
        dplyr::select(value)
    })
  )
})


test_that("tan()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = tan(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = tan(fld_double)) %>%
        dplyr::select(value)
    })
  )
})


test_that("tanh()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = tanh(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = tanh(fld_double)) %>%
        dplyr::select(value)
    })
  )
})

test_that("tolower()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = tolower(fld_character)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = tolower(fld_character)) %>%
        dplyr::select(value)
    })
  )
})

test_that("toupper()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = toupper(fld_character)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = toupper(fld_character)) %>%
        dplyr::select(value)
    })
  )
})


test_that("trimws()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = trimws(fld_character)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = trimws(fld_character)) %>%
        dplyr::select(value)
    })
  )
})

test_that("nchar()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = nchar(fld_character)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = nchar(fld_character)) %>%
        dplyr::select(value)
    })
  )
})


test_that("substr()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = substr(fld_character, 1, 1)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = substr(fld_character, 1, 1)) %>%
        dplyr::select(value)
    })
  )
})


test_that("ifelse()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = ifelse(fld_binary == 1, "Yes", "No")) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = ifelse(fld_binary == 1, "Yes", "No")) %>%
        dplyr::select(value)
    })
  )
})

# sql() - Not sure what to do with this function.

test_that("desc()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = fld_character) %>%
        dplyr::select(value) %>%
        dplyr::arrange(desc(value)) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = fld_character) %>%
        dplyr::select(value) %>%
        dplyr::arrange(desc(value))
    })
  )
})


test_that("is.null()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = is.null(fld_character)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = is.null(fld_character)) %>%
        dplyr::select(value)
    })
  )
})


test_that("is.na()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = is.na(fld_character)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = is.na(fld_character)) %>%
        dplyr::select(value)
    })
  )
})

test_that("coalesce()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = coalesce(fld_character, fld_character)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = coalesce(fld_character, fld_character)) %>%
        dplyr::select(value)
    })
  )
})


test_that("as.numeric()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = as.numeric(fld_logical)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = as.numeric(fld_logical)) %>%
        dplyr::select(value)
    })
  )
})

test_that("as.double()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = as.double(fld_logical)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = as.double(fld_logical)) %>%
        dplyr::select(value)
    })
  )
})

test_that("as.integer()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = as.integer(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = as.integer(fld_double)) %>%
        dplyr::select(value)
    })
  )
})

# c() - Not sure what to do with this function.


test_that("between()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = fld_double) %>%
        dplyr::filter(between(value, 1, 5)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = fld_double) %>%
        dplyr::filter(between(value, 1, 5)) %>%
        dplyr::select(value)
    })
  )
})

test_that("pmin()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = pmin(fld_binary, fld_integer, fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = pmin(fld_binary, fld_integer, fld_double)) %>%
        dplyr::select(value)
    })
  )
})

test_that("pmax()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = pmax(fld_binary, fld_integer, fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = pmax(fld_binary, fld_integer, fld_double)) %>%
        dplyr::select(value)
    })
  )
})

test_that("sd() win",{
  expect_equal(
    as.list({
      (db_test_table %>%
         dplyr::group_by(fld_factor) %>%
         dplyr::mutate(value = sd(fld_double)) %>%
         dplyr::select(fld_factor, value) %>%
         dplyr::arrange(value) %>%
         dplyr::collect())[,2]
    }),
    as.list({
      (test_table %>%
         dplyr::group_by(fld_factor) %>%
         dplyr::mutate(value = sd(fld_double)) %>%
         dplyr::arrange(value) %>%
         dplyr::select(value))[,2]
    })
  )
})

test_that("db_drop_table()",{
  expect_silent({
    dplyr::db_drop_table(con, table_name)
  })
})



