context("initialize")

test_data <- function(){
  data <- dbtest::testdata
  data$fld_double <- as.double(data$fld_double)
  data$fld_character <- as.character(data$fld_character)
  data
}

table_name <<- dbplyr:::random_table_name()

test_table <<- test_data()

test_that("copy_to()",{
  expect_silent({
    dplyr::copy_to(con, test_table, table_name, temporary = FALSE)
  })
})

db_test_table <<- dplyr::tbl(con, table_name)




