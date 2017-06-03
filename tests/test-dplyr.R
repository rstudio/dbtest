
table_name <- dbplyr:::random_table_name()

test_table <- test_data()

context("basic")

test_that("copy_to()",{
  expect_silent({
    dplyr::copy_to(con, test_table, table_name, temporary = FALSE)
  })
})

db_test_table <- dplyr::tbl(con, table_name)

# base_agg tests --------------------------------

context("base_agg")

test_that("n()",{
  expect_equal(
    as.numeric({
      db_test_table %>%
        dplyr::summarise(value = n()) %>%
        dplyr::collect() %>%
        dplyr::mutate(value = as.integer(value))
    }),
    as.numeric({
      test_table %>%
        dplyr::summarise(value = n())
    })
  )
})


test_that("mean()",{
  expect_equal(
    as.numeric({
      db_test_table %>%
        dplyr::summarise(value = mean(fld_double)) %>%
        dplyr::collect()
    }),
    as.numeric({
      test_table %>%

        dplyr::summarise(value = mean(fld_double))
    })
  )
})

test_that("var()",{
  expect_equal(
    as.numeric({
      db_test_table %>%
        dplyr::summarise(value = var(fld_double)) %>%
        dplyr::collect()
    }),
    as.numeric({
      test_table %>%
        dplyr::summarise(value = var(fld_double))
    })
  )
})

test_that("sum()",{
  expect_equal(
    as.numeric({
      db_test_table %>%
        dplyr::summarise(value = sum(fld_double)) %>%
        dplyr::collect()
    }),
    as.numeric({
      test_table %>% dplyr::summarise(value = sum(fld_double))
    })
  )
})

test_that("min()",{
  expect_equal(
    as.numeric({
      db_test_table %>%
        dplyr::summarise(value = min(fld_double)) %>%
        dplyr::collect()
    }),
    as.numeric({
      test_table %>%
        dplyr::summarise(value = min(fld_double))
    })
  )
})


test_that("max()",{
  expect_equal(
    as.numeric({
      db_test_table %>%
        dplyr::summarise(value = max(fld_double)) %>%
        dplyr::collect()
    }),
    as.numeric({
      test_table %>%
        dplyr::summarise(value = max(fld_double))
    })
  )
})

test_that("n_distinct()",{
  expect_equal(
    as.numeric({
      db_test_table %>%
        dplyr::summarise(value = n_distinct(fld_factor)) %>%
        dplyr::collect() %>%
        dplyr::mutate(value = as.integer(value))
    }),
    as.numeric({
      test_table %>%
        dplyr::summarise(value = n_distinct(fld_factor))
    })
  )
})


# base_win tests --------------------------------
context("base_win")

test_that("row_number()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = row_number(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::arrange(value) %>%
        dplyr::collect()
      }) ,
    as.list({
      test_table %>%
        dplyr::mutate(value = row_number(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::arrange(value)
    })
  )
})

test_that("min_rank()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = min_rank(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }) ,
    as.list({
      test_table %>%
        dplyr::mutate(value = min_rank(fld_double)) %>%
        dplyr::select(value)
    })
    )
})

test_that("rank()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = rank(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }) ,
    as.list({
      test_table %>%
        dplyr::mutate(value = rank(fld_double)) %>%
        dplyr::select(value)
    })
  )
})


test_that("dense_rank()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = dense_rank(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }) ,
    as.list({
      test_table %>%
        dplyr::mutate(value = dense_rank(fld_double)) %>%
        dplyr::select(value)
    })
  )
})


test_that("percent_rank()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = percent_rank(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }) ,
    as.list({
      test_table %>%
        dplyr::mutate(value = percent_rank(fld_double)) %>%
        dplyr::select(value)
    })
  )
})


test_that("cume_dist()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = cume_dist(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }) ,
    as.list({
      test_table %>%
        dplyr::mutate(value = cume_dist(fld_double)) %>%
        dplyr::select(value)
    })
  )
})

test_that("ntile()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = ntile(fld_double, 2)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }) ,
    as.list({
      test_table %>%
        dplyr::mutate(value = ntile(fld_double, 2)) %>%
        dplyr::select(value)
    })
  )
})

test_that("first()",{
  expect_equal(
    as.character({
      db_test_table %>%
        dplyr::summarise(value = first(fld_character)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }) ,
    as.character({
      test_table %>%
        dplyr::summarise(value = first(fld_character)) %>%
        dplyr::select(value)
    })
  )
})


test_that("last()",{
  expect_equal(
    as.character({
      db_test_table %>%
        dplyr::summarise(value = last(fld_character)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }) ,
    as.character({
      test_table %>%
        dplyr::summarise(value = last(fld_character)) %>%
        dplyr::select(value)
    })
  )
})

test_that("nth()",{
  expect_equal(
    as.character({
      db_test_table %>%
        dplyr::summarise(value = nth(fld_character, 5)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }) ,
    as.character({
      test_table %>%
        dplyr::summarise(value = nth(fld_character, 5)) %>%
        dplyr::select(value)
    })
  )
})

test_that("lead()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = lead(fld_character, 1)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = lead(fld_character, 1)) %>%
        dplyr::select(value)
    })
  )
})

test_that("lag()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = lag(fld_character, 1)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = lag(fld_character, 1)) %>%
        dplyr::select(value)
    })
  )
})


test_that("cummean()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = cummean(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = cummean(fld_double)) %>%
        dplyr::select(value)
    })
  )
})

test_that("cumsum()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = cumsum(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = cumsum(fld_double)) %>%
        dplyr::select(value)
    })
  )
})

test_that("cummin()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = cummin(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = cummin(fld_double)) %>%
        dplyr::select(value)
    })
  )
})

test_that("cummax()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = cummax(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = cummax(fld_double)) %>%
        dplyr::select(value)
    })
  )
})


# base_scalar tests -----------------------------
context("base_scalar")

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
        dplyr::mutate(value = cei(fld_double)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
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
        dplyr::mutate(value = ceiling(fld_binary)) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = ceiling(fld_binary)) %>%
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
        dplyr::collect()
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

test_that("is.na()",{
  expect_equal(
    as.list({
      db_test_table %>%
        dplyr::mutate(value = na_if(fld_character, "X")) %>%
        dplyr::select(value) %>%
        dplyr::collect()
    }),
    as.list({
      test_table %>%
        dplyr::mutate(value = na_if(fld_character, "X")) %>%
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


test_that("db_drop_table()",{
  expect_silent({
    dplyr::db_drop_table(con, table_name)
  })
})



