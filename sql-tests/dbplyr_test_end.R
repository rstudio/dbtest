context("cleanup")

test_that("db_drop_table()",{
  expect_silent({
    db_drop_table(con, table_name)
  })
})



