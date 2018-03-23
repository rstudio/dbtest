#' Test Data
#'
#' A simple dataset that presents varied data types to facilitate
#' testing
#'
#' @format A data frame with 10 rows and 9 columns:
#' \describe{
#'   \item{fld_factor}{A field with values in "a","b","c"}
#'   \item{fld_datetime}{A field with datetime values}
#'   \item{fld_date}{A field with date values}
#'   \item{fld_time}{A field with time values}
#'   \item{fld_binary}{A field with 1s or 0s}
#'   \item{fld_integer}{A field with integer values}
#'   \item{fld_double}{A field with double precision values}
#'   \item{fld_character}{A field with character values}
#'   \item{fld_logical}{A field with logical values}
#' }
"testdata"

#' Test Data Large
#'
#' A test dataset with slightly more data.  At present this is just
#' a variation of good ol' iris with renamed fields
#'
#' @format A data frame with 150 rows and 5 columns:
#' \describe{
#'   \item{fld_double}{A field with double precision values}
#'   \item{fld_integer}{A field with integer values}
#'   \item{fld_character}{A field with character values}
#'   \item{fld_binary}{A field with binary (1 or 0) values}
#'   \item{fld_factor}{A field with factor values}
#' }
"testdata_large"
