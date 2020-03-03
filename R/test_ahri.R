#' @title test_ahri_dta
#' 
#' @description  Tests to see if you are using the correct default .dta datasets. The
#' tests will generate summary values from each of the datasets and check if they match
#' expected values defined in the test. 
#' 
#' @param How to display results, can be "minimal", "summary", "progress". 
#' 
#' @return NULL
#'
#' @export 

test_ahri_dta <- function(report='progress') {
  check_getFiles()
  testthat::test_package('ahri', filter='ahri_dta', reporter=report)
}

#' @title test_ahri_func
#' 
#' @description  Tests to see if your version of the \code{ahri} functions will produce
#' the correct results.  The tests will generate summary values of several datasets and check if they match
#' expected values defined in the test. 
#' 
#' @param How to display results, can be "minimal", "summary", "progress". 
#' 
#' @return NULL
#'
#' @export 

test_ahri_func <- function(report='progress') {
  check_getFiles()
  testthat::test_package('ahri', filter='ahri_funcs', reporter=report)
}

