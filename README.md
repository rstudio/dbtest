dbtest
================

[![Build Status](https://travis-ci.org/rstudio/dbtest.svg?branch=master)](https://travis-ci.org/rstudio/dbtest)

Overview
========

The package uses **testthat** to automate dbplyr SQL translation by running the tests against live database connections.

Install
=======

``` r
devtools::install_github("rstudio/dbtest")
```

Setup
=====

The `config` package is used to secure the credentials and to allow to easily point the tests to different servers. The file needs to be named `config.yml`.

Here is a sample of the structure:

``` bash
default:
  mssql:
    Driver: "MS SQL Driver"
    server: "localhost"
    Schema: "default"
    UID: "rstudio"
    PWD: "rstudio"
    port: 10000
```

Usage
=====

Use the `test_database` command to run the tests. The arguments are as follows:

-   **databases** - A vector of one or more databases that will be tested. The value should match the name should match the section inside the `config.yml` file.

-   **configuration** - Defaults to `default`. It refers to the main configuration selected that is in the `config.yml` file.

Using the sample yml file from above, we would use `test_database("mssql")` to run the tests against that database.

Passing a character vector to the **databases** argument allows us to run the same tests over multiple databases, for example: `test_database(c("mssql","oracle","hive"))`, will run the tests over the connections named "mssql", "oracle" and "hive" in a serial manner.

Use a variable to retain the results. The `test_database` function returns a `data.frame` with the results.

``` r
library(dbtest)
results <- test_database()
```

    ## ...EE....E.EEEEEEEEEEEEEEEEEEEEEEEE.......FFE......EEEEEEEEEEEEEEEEE.

`test_database` uses the 'minimal' reporting from `testthat`. The result of each test will display as a single character:

-   Period means that the test passed with no errors
-   **E** means that the test passed but with errors
-   **F** means that the test failed

This feature is useful if you wish to monitor the test while it runs. The real output of the test is a `data.frame` that is returned after all of the tests are complete. The resulting `data.frame` has the following variables:

-   **file** - Name of the R script - In GitHub the scripts are found in the */inst/sql-tests* folder
-   **context**
-   **test** - The test names coincide with the function it tests, the name is usually a single word long.
-   **nb**
-   **failed**
-   **skipped**
-   **error**
-   **warning**
-   **user**
-   **system**
-   **real** - Time elapsed for the test
-   **database**
-   **result** - Contains the actual error returned
-   **call** - Contains the code used in the test
-   **res** - If the test either fails or has errors then the value will be **Failed**, otherwise it will be **Passed**

Analysis
========

Percentage passed
-----------------

A simple calculation of the percentage of the tests that passed helps to have a single number we can work on improving. Seeing the percentage climb after improvements to the translations are made is very motivating. Additionally, if multiple databases are being run at the same time, it's a great comparative number that provides context by giving us an idea who well our translation works on one database but not another.

Here is the code I usually run:

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:testthat':
    ## 
    ##     matches

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
coverage <- results %>%
  group_by(database, res) %>%
  tally %>%
  spread(res, n) %>%
  mutate(coverage = round(Passed / (Passed + Failed), digits = 2))

coverage
```

    ## # A tibble: 1 x 4
    ## # Groups:   database [1]
    ##   database Failed Passed coverage
    ##      <chr>  <int>  <int>    <dbl>
    ## 1              47     22     0.32

ggplot2
-------

A quick visualization so I can see at-a-glance what failed and passed.

``` r
library(ggplot2) 

ggplot(data = results) +
  geom_raster(aes(x = database, y = test, fill = res))
```

![](README_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)

HTML Report
-----------

The package has a quick-n-easy report that takes the output from the `test_databases` output and creates an html based report. The flexible layout of the report allows the user to have an easier look at the results of each test.

``` r
html_report(results)
```

The code above will create a report of all the tests. Most time we just want to see what failed and why, here is what I normally use:

``` r
library(htmltools)
html_report(results %>%
              filter(res == "Failed"),
            filename = "dbtest-exceptions.html")
```

In the report a test result will look like this:

    14 - impala - Test: row_number()

    File:       Result: Failed  Runtime (In Secs.): 0.09

    Test Message:

    manip(db) not equal to manip(local). Attributes: < Modes: list, NULL > Attributes: < Lengths: 1, 0 > 
    Attributes: < names for target but not for current > Attributes: < current is not list-like > target 
    is integer64, current is numeric

    Call:

    expect_window_equivalent(row_number(fld_double))
