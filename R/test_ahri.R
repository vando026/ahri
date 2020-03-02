#' @title test_ahri_dta
#' 
#' @description  Tests to see if you are using the correct default .dta datasets. The
#' tests will generate summary values from each of the datasets and check if they match
#' expected values defined in the test. 
#' @param path Path to .dta datasets, calls \code{\link{setFiles}} by default.  
#' 
#' @return NULL
#'
#' @export 

test_ahri_dta <- function() {
  ahri_error()
  testthat::test_package('ahri', filter='ahri_dta', reporter='progress')
}

