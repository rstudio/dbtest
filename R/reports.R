parse_results <- function(databases, all_results) {
  text_results <- 1:length(databases) %>%
    map(function(x) {
      1:length(all_results[[x]]) %>%
        map(function(y) all_results[[x]][[y]]$results[[1]]$message)
    })

  text_call <- 1:length(databases) %>%
    map(function(x) {
      1:length(all_results[[x]]) %>%
        map(function(y) all_results[[x]][[y]]$results[[1]]$call[[1]])
    })

  new_results <- all_results %>%
    map(as.data.frame)

  new_results <- 1:length(databases) %>%
    map(function(.x) mutate(new_results[[.x]],
        database = databases[.x],
        result = as.character(text_results[[.x]]),
        call = as.character(text_call[[.x]])
      )) %>%
    bind_rows() %>%
    mutate(res = ifelse(failed == 0 & error == FALSE, "Passed", "Failed")) %>%
    arrange(desc(database))
}

html_report <- function(
                        results_variable,
                        filename = "dplyr_test_results.html",
                        title = "dplyr SQL translation tests",
                        show_when_complete = TRUE) {
  html_result <- list(
    tags$h1(title),
    1:nrow(results_variable) %>%
      map(function(x) {
        print_result(results_variable[x, ], x)
      })
  )

  save_html(html_result, filename)

  if (show_when_complete) utils::browseURL(filename)
}

print_result <- function(record, id) {
  list(
    tags$h3(tags$strong(paste0(id, " - ", record$database, " - Test: ", record$test))),
    tags$table(
      tags$tr(
        tags$td(tags$strong("")),
        tags$td(tags$strong("File:")),
        tags$td(record$file),
        tags$td(tags$strong("Result:"),
          if (record$failed == 0) {
            paste0("Passed", if (record$error) " had error(s)")
          } else {
            "Failed"
          },
          colspan = 2
        ),
        tags$td(tags$strong("Runtime (In Secs.):  "), round(record$real, digits = 2), colspan = 2)
      ),
      tags$tr(tags$td(tags$p(""))),

      tags$tr(
        tags$td(tags$p("")),
        tags$td(tags$strong("Test Message:"), colspan = 5)
      ),
      tags$tr(
        tags$td(tags$p(""), colspan = 2),
        tags$td(record$result, colspan = 4)
      ),
      tags$tr(tags$td(tags$p(""))),

      tags$tr(
        tags$td(tags$p("")),
        tags$td(tags$strong("Call:"), colspan = 5)
      ),
      tags$tr(
        tags$td(tags$p(""), colspan = 2),
        tags$td(record$call, colspan = 4)
      )
    ),
    tags$br(),
    tags$hr(),
    tags$br()
  )
}


coverage <- function(results) {
  coverage <- results %>%
    group_by(database, res) %>%
    tally() %>%
    spread(res, n) %>%
    mutate(coverage = round(Passed / (Passed + Failed), digits = 2))

  coverage
}


#' Plot Tests
#'
#' Plot the output from `test_single_database` or `test_database` as a ggplot2 object
#' for easy visualization of test success or failure across databases.
#'
#' @param results Output from `test_single_database` or `test_database`
#'
#' @return ggplot2 object / graph
#'
#' @examples
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' tbl_data <- dplyr::copy_to(con, testdata)
#' res <- test_database(tbl_data, pkg_test("simple-tests.yml"))
#' plot_tests(res)
#' DBI::dbDisconnect(con)
#'
#' @export
plot_tests <- function(results) {
  if (is.list(results) & all(as.logical(lapply(results, is_dbtest_results))) ) {
    prep_results <- results %>%
      map_df(~ as.data.frame(.x))
  } else if (is_dbtest_results(results)) {
    prep_results <- results %>% as.data.frame()
  } else {
    stop("Invalid input: did not find a `dbtest_results` object or a list of `dbtest_results` objects")
  }
  dataset <- prep_results %>%
    mutate(
      result = ifelse(results.failed == 1 | results.error, "Failed", "Passed"),
      test = paste0(results.test, "\n", results.context),
      filler = ""
    ) %>%
    select(connection, test, result, filler, justtest = results.test, context = results.context)

  ggplot(dataset) +
    geom_tile(aes(x = filler, y = justtest, fill = result), color = "black") +
    scale_fill_discrete(limits = c("Failed", "Passed")) +
    facet_grid(context ~ connection, scales = "free") +
    labs(x = "", y = "")
}
