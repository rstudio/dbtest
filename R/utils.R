#' @export
default_tests_path <- function(file = "tests.yml"){
  system.file("extdata", file, package = "dbtest")
}

#' @export
default_config_path <- function(file = "config.yml"){
  system.file("extdata", file, package = "dbtest")
}
