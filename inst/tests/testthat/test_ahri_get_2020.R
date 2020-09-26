## Description: Test functions
## Project: ahri
## Author: AV / Created: 26Sep2020
# testthat::test_file('inst/tests/testthat/test_ahri_get_2020.R')

context("Test RTDates")
testDates <- function(dat=NULL) {
  testNeg <- filter(dat, is.finite(obs_start) & is.finite(late_neg))
  if(any(with(testNeg, obs_start > late_neg))) 
      stop("Some obs_start > late_neg") 
  testPos <- filter(dat, is.finite(early_pos) & is.finite(late_neg))
  if(any(with(testPos, late_neg >= early_pos))) 
      stop("Some late_neg >= early_pos") 
}

rtdat <- getRTData(dat=getHIV())
rtdat$obs_start[rtdat$IIntID==17]  <- as.Date("2020-01-01", origin="1970-01-01")
test_that("Test RTDates: wrong date", {
  expect_error(testDates(rtdat))
})

context("Test splitEpisodes")
Args <- setArgs(Years=c(2005:2018), Age=list(All=c(15,54)),
  imputeMethod=imputeMidPoint)
hiv <- getHIV()
rtdat <- getRTData(hiv)
dat <- Args$imputeMethod(rtdat)
edat <- splitAtSeroDate(dat) 
edat2 <- splitAtEarlyPos(dat)
test_that("Check splitAtSeroDate", {
  expect_equal(nrow(edat),  188160)
  expect_equal(sum(as.numeric(edat$obs_end)), 2895975962)
})
test_that("Check splitAtEarlyPos", {
  expect_equal(nrow(edat2), 196555)
  expect_equal(sum(as.numeric(edat2$obs_end)), 3030384325)
})
