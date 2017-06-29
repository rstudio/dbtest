context("basic")

test_that(" test_database return results",{
  expect_equal(class(test_database()),
               "data.frame")
})
