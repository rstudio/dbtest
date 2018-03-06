#' @export
default_tests_path <- function(file = "tests.yml"){
  system.file("extdata", file, package = "dbtest")
}

