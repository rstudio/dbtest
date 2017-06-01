
#' @export
test_data <- function(){
  data <- dbtest::testdata
  data$fld_datetime <- as.POSIXct(strptime(data$fld_datetime, "%m/%d/%y %H:%M"))
  data$fld_date <- as.POSIXct(strptime(data$fld_date, "%m/%d/%y"))
  data$fld_time <- as.POSIXct(strptime(data$fld_time, "%H:%M:%S"))
  data$fld_double <- as.double(data$fld_double)
  data$fld_character <- as.character(data$fld_character)
  data$fld_logical <- as.logical(data$fld_logical)
  data
}



#' @export
run_script <- function(connection_name){

  db <- config::get(connection_name)

  con <<- DBI::dbConnect(
    odbc::odbc(),
    Driver = db$Driver,
    Server = db$Server,
    Database = db$Database,
    UID  = db$UID,
    PWD = db$PWD,
    Port = db$Port)

  results <- testthat::test_dir("tests")

  DBI::dbDisconnect(con)

  return(results)
}

#' @import dplyr
#' @import purrr
#' @export
test_database <- function(databases, configuration =  NULL){

  original_configuration <- Sys.getenv("R_CONFIG_ACTIVE")
  if(is.null(configuration)==FALSE) Sys.setenv(R_CONFIG_ACTIVE = configuration)


  all_results <- databases %>%
    map(function(.x)run_script(.x))



  text_results <- 1:length(databases) %>%
    map(function(x){
      1:length(all_results[[x]]) %>%
        map(function(y)all_results[[x]][[y]]$results[[1]]$message)
    })

  text_call<- 1:length(databases) %>%
    map(function(x){
      1:length(all_results[[x]]) %>%
        map(function(y)all_results[[x]][[y]]$results[[1]]$call[[1]])
    })

  new_results <- all_results %>%
    map(as.data.frame)



  new_results <- 1:length(databases) %>%
    map(function(.x)mutate(new_results[[.x]],
                           database = databases[.x],
                           result = as.character(text_results[[.x]]),
                           call = as.character(text_call[[.x]])
    )
    ) %>%
    bind_rows() %>%
    mutate(res = ifelse(nb == 1 & failed == 0, "Passed" , "Failed")) %>%
    arrange(desc(database))

  Sys.setenv(R_CONFIG_ACTIVE = original_configuration)

  new_results
}



