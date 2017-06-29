context("cleanup")

test_that("db_drop_table()",{
  expect_silent({
    dplyr::db_drop_table(con, table_name)
  })
})



