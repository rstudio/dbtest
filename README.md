dbtest
================

It uses `testthat` to run a battery of tests against a given database connection. The idea is to find out what translations from `dplyr` are and what are not supported by a given database. It also useful to test new translations that close the gap of what it is supported by `dplyr`.

Install
=======

``` r
devtools::install_github("edgararuiz/dbtest")
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

-   **databases** - A vector of one or more databases that will be tested. The value should match the name should match the section inside the `config.yml` file. In the exampel above, we would use `mssql`

-   **configuration** - Defaults to `default`. It refers to the main configuration selected that is in the `config.yml` file.

Use a variable to retain the results. The `test_database` function returns a `data.frame` with the results.

``` r
library(dbtest)
results <- test_database("mssql")
```

The resulting `data.frame` has the following variables:

-   **file** - Name of the R script
-   **context**
-   **test** - It has the test's name. At this time, the names are kept to the single function name that was the focus of the test
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

The package has a quick-n-easy report that takes the output from the `test_databases` output and creates an html based report.

``` r
html_report(results)
```

Another way I found was to just run a quick visualization

``` r
library(ggplot2) 

ggplot(data = results) +
  geom_raster(aes(x = database, y = test, fill = res))
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)
