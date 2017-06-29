context("initialize")

table_name <- dbplyr:::random_table_name()

test_table <- test_data()

test_that("copy_to()",{
  expect_silent({
    copy_to(con, test_table, table_name, temporary = FALSE)
  })
})

db_test_table <- tbl(con, table_name)




