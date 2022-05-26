## Description: Test read code and dta
## Project: ahri
## Author: AV / Created: 23 September 2020
# testthat::test_file('inst/tests/testthat/test_ahri_dta_2020.R')

if (exists("getFiles", env = .GlobalEnv))
  getFiles_User <- getFiles

assign("getFiles", 
  setFiles(system.file("data", package = "ahri"),
    hivfile = "RD05-99_ACDIS_HIV_Sample.dta",
    bsifile = "RD01-03_ACDIS_BS_Sample.dta",
    epifile = "SurveillanceEpisodes_Sample.dta",
    wghfile = "RD03-99_ACDIS_WGH_Sample.dta",
    mghfile = "RD04-99_ACDIS_MGH_Sample.dta"), 
  env = globalenv())

context("Test getBSData")
bdat <- getBSData()
testthat::test_that("Check readBSData N", {
  testthat::expect_equal(nrow(bdat), 62)
  testthat::expect_equal(length(unique(bdat$BSIntID)), 62)
  testthat::expect_equal(sum(bdat$KmToNearestClinic), 83.55)
  testthat::expect_equal(sum(bdat$PIPSA %in% "Southern PIPSA", na.rm=TRUE), 51)
  testthat::expect_equal(ncol(bdat), 18)
})

context("Test readHIVData")
hdat1 <- readHIVData(dropTasP=FALSE, write_rda=FALSE)
testthat::test_that("Check getHIVData N", {
  testthat::expect_equal(length(unique(hdat1$IIntID)), 5)
  testthat::expect_equal(nrow(hdat1), 35)
  testthat::expect_equal(ncol(hdat1), 9)
  testthat::expect_equal(sum(hdat1$Female), 31)
  testthat::expect_equal(sum(hdat1$Age), 1374)
})

context("Test readHIVData (dropTasP")
hdat2 <- readHIVData(dropTasP=TRUE, write_rda=FALSE)
testthat::test_that("Check getHIVData N", {
  testthat::expect_equal(length(unique(hdat2$IIntID)), 5)
  testthat::expect_equal(nrow(hdat2), 32)
  testthat::expect_equal(ncol(hdat2), 10)
  testthat::expect_equal(sum(hdat2$Female), 28)
  testthat::expect_equal(sum(hdat2$Age), 1280)
})


context("Test readEpisodes")
edat <- readEpisodes(dropTasP=FALSE, write_rda=FALSE)
testthat::test_that("Check readEpisodes N", {
  testthat::expect_equal(length(unique(edat$IIntID)), 2) 
  testthat::expect_equal(length(unique(edat$BSIntID)), 2) 
  testthat::expect_equal(length(edat$Female[edat$Female==0]), 44) 
  testthat::expect_equal(nrow(edat), 51) 
  testthat::expect_equal(ncol(edat), 16) 
  testthat::expect_equal(sum(edat$Age), 1684) 
})


context("Test readMGH & readWGH")
testthat::test_that("Check WGH/MGH N and Age", {
  wgh0 <- readWGH(write_rda=FALSE)
  mgh0 <- readMGH(write_rda=FALSE)
  testthat::expect_equal(length(unique(mgh0$IIntID)), 13)
  testthat::expect_equal(length(unique(wgh0$IIntID)), 11)
  testthat::expect_equal(sum(mgh0$Age), 1725)
  testthat::expect_equal(nrow(mgh0), 51)
  testthat::expect_equal(ncol(mgh0), 110)
  testthat::expect_equal(nrow(wgh0), 51)
  testthat::expect_equal(ncol(wgh0), 126)
  testthat::expect_equal(sum(wgh0$Age), 2856)
})

rm(list = c("getFiles"), envir = .GlobalEnv)
if (exists("getFiles_User"))
  getFiles <<- getFiles_User
