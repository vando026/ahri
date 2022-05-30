## Description: Test read code and dta
## Project: ahri
## Author: AV / Created: 23 September 2020
# testthat::test_file('inst/tests/testthat/test_ahri_dta_2020.R')

if (exists("getFiles", env = .GlobalEnv))
  getFiles_User <- getFiles

assign("getFiles", 
  setFiles(system.file("exdata", package = "ahri"),
    hivfile = "RD05-99_ACDIS_HIV_Sample.dta",
    bsifile = "RD01-03_ACDIS_BS_Sample.dta",
    epifile = "SurveillanceEpisodes_Sample.dta",
    wghfile = "RD03-99_ACDIS_WGH_Sample.dta",
    mghfile = "RD04-99_ACDIS_MGH_Sample.dta"), 
  env = globalenv())

context("Test getBSData")
bdat <- getBSData()
testthat::test_that("Check readBSData N", {
  testthat::expect_equal(nrow(bdat), 7)
  testthat::expect_equal(length(unique(bdat$BSIntID)), 7)
  testthat::expect_equal(sum(bdat$PIPSA %in% "Southern PIPSA", na.rm=TRUE), 6)
  testthat::expect_equal(ncol(bdat), 18)
})

context("Test readHIVData")
hdat1 <- readHIVData(dropTasP=FALSE, write_rda=FALSE)
testthat::test_that("Check getHIVData N", {
  testthat::expect_equal(length(unique(hdat1$IIntID)), 5)
  testthat::expect_equal(nrow(hdat1), 14)
  testthat::expect_equal(ncol(hdat1), 9)
  testthat::expect_equal(length(unique(hdat1$IIntID[hdat1$Female ==1])), 3)
  testthat::expect_equal(sum(hdat1$Age), 427)
})

context("Test readHIVData (dropTasP")
hdat2 <- readHIVData(dropTasP=TRUE, write_rda=FALSE)
testthat::test_that("Check getHIVData N", {
  testthat::expect_equal(sort(unique(hdat2$BSIntID)), c(6496, 9305, 15588, 17843))
  testthat::expect_equal(ncol(hdat2), 10)
  testthat::expect_equal(length(unique(hdat2$IIntID[hdat2$Female ==1])), 3)
  testthat::expect_equal(sum(hdat2$Age), 343)
})


context("Test readEpisodes")
edat <- readEpisodes(dropTasP=FALSE, write_rda=FALSE)
testthat::test_that("Check readEpisodes N", {
  testthat::expect_equal(length(unique(edat$IIntID)), 5) 
  testthat::expect_equal(length(unique(edat$BSIntID)), 7) 
  testthat::expect_equal(length(unique(edat$IIntID[edat$Female==1])), 3) 
  testthat::expect_equal(ncol(edat), 16) 
  testthat::expect_equal(sort(unique(edat$BSIntID)), c(616, 3455, 6496, 9305, 15588, 16563, 17843))
})

context("Test readEpisodes (dropTasP)")
edat <- readEpisodes(dropTasP=TRUE, write_rda=FALSE)
testthat::test_that("Check readEpisodes N", {
  testthat::expect_equal(length(unique(edat$IIntID)), 5) 
  testthat::expect_equal(length(unique(edat$BSIntID)), 6) 
  testthat::expect_equal(length(unique(edat$IIntID[edat$Female==1])), 3) 
  testthat::expect_equal(ncol(edat), 17) 
  testthat::expect_equal(sort(unique(edat$BSIntID)), c(3455, 6496, 9305, 15588, 16563, 17843))
})

context("Test readMGH & readWGH")
testthat::test_that("Check WGH/MGH N and Age", {
  wgh0 <- readWGH(write_rda=FALSE)
  mgh0 <- readMGH(write_rda=FALSE)
  testthat::expect_equal(sort(unique(mgh0$IIntID)), c(1356, 1436))
  testthat::expect_equal(sort(unique(wgh0$IIntID)), c(740, 795, 800))
  testthat::expect_equal(nrow(mgh0), 10)
  testthat::expect_equal(ncol(mgh0), 110)
  testthat::expect_equal(sum(mgh0$Age), 200)
  testthat::expect_equal(nrow(wgh0), 25)
  testthat::expect_equal(ncol(wgh0), 126)
  testthat::expect_equal(sum(wgh0$Age), 902)
})

rm(list = c("getFiles"), envir = .GlobalEnv)
if (exists("getFiles_User"))
  getFiles <<- getFiles_User
