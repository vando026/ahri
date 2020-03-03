#' @title test_ahri_dta
#' 
#' @description  Tests to see if you are using the correct default .dta datasets. The
#' tests will generate summary values from each of the datasets and check if they match
#' expected values defined in the test. 
#' 
#' @param year The release year that you want to perform the tests on. Currently, test years include 2018 only.
#' @param How to display results, can be "minimal", "summary", "progress". 
#' 
#' @return NULL
#'
#' @export 

test_ahri_dta <- function(year="2018", report='progress') {
  check_getFiles()
  message(sprintf('Running tests on %s data release...', year))
  testthat::test_package('ahri', filter=paste0('ahri_dta_', year), reporter=report)
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

test_ahri_func <- function(year="2018", report='progress') {
  check_getFiles()
  message(sprintf('Running tests on %s data release...', year))
  testthat::test_package('ahri', filter=paste0('ahri_funcs_', year), reporter=report)
}

