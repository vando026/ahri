## Description: Test read code and dta
## Project: ahri
## Author: AV / Created: 23 September 2020
# testthat::test_file('inst/tests/testthat/test_ahri_dta_2020.R')

context("Test getBSData")
bdat <- getBSData()
test_that("Check readBSData N", {
  expect_equal(length(unique(bdat$BSIntID)), 31978)
  expect_equal(sum(bdat$PIPSA %in% "Southern PIPSA", na.rm=TRUE), 18581)
})


context("Test readHIVData")
hdat1 <- readHIVData(dropTasP=FALSE, write_rda=FALSE)
hdat2 <- dropTasPData(hdat1)
test_that("Check getHIVData N", {
  expect_equal(length(unique(hdat1$IIntID)), 67273)
})
test_that("Check getHIVData N (dropTasP)", {
  expect_equal(length(unique(hdat2$IIntID)), 55256)
})


context("Test readEpisodes")
edat <- readEpisodes(dropTasP=FALSE, write_rda=FALSE, nrow=2e5)
test_that("Check readEpisodes N", {
  expect_equal(length(unique(edat$IIntID)), 4743) 
  expect_equal(length(unique(edat$BSIntID)), 1511) 
  expect_equal(length(edat$Female[edat$Female==0]), 97568) 
})

context("Test readMGH readWGH")
wgh0 <- readHealthData(Female=1, write_rda=FALSE)
mgh0 <- readHealthData(Female=0, write_rda=FALSE)
test_that("Check WGH/MGH N and Age", {
  expect_equal(length(unique(mgh0$IIntID)), 40640)
  expect_equal(length(unique(wgh0$IIntID)), 53487)
  expect_equal(sum(mgh0$Age), 5203098)
  expect_equal(sum(wgh0$Age), 9191667)
})
