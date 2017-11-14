test_equal <- function(expr,
                       test = "summarise",
                       db = db_test_table,
                       local = test_table){

  f <- rlang::parse_quosure(expr)

  # Add new tests here, match test name to the yml file
  if(test == "summarise") manip <- . %>%  summarise(x = !!f) %>%  pull()
  if(test == "mutate") manip <- . %>% mutate(x = !!f) %>%  pull()
  if(test == "window") manip <- . %>% arrange(!!f) %>% mutate(value = !!f) %>% pull()
  if(test == "filter") manip <- . %>% filter(!!f) %>% pull()

  # Using context to allow grouping by test
  context(test)
  # Wrapping test_that() to create a single entry per entry in the yml file
  test_that(expr,{
    expect_equal(manip(db), manip(local))
  })
}

run_test <- function(test){
  test_config %>%
    pluck(test) %>%
    unlist() %>%
    map(~test_equal(.x, test = test))
}

c("summarise", "mutate", "window", "filter") %>%
  walk(~run_test(.x))
