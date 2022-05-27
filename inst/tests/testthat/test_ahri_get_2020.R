## Description: Test functions
## Project: ahri
## Author: AV / Created: 26Sep2020
# testthat::test_file('inst/tests/testthat/test_ahri_get_2020.R')

if (exists("getFiles", env = .GlobalEnv))
  getFiles_User <- getFiles

assign("getFiles", 
  setFiles(system.file("data", package = "ahri"),
    hivfile = "RD05-99_ACDIS_HIV_Sample.dta",
    bsifile = "RD01-03_ACDIS_BS_Sample.dta",
    epifile = "SurveillanceEpisodes_Sample.dta",
    wghfile = "RD03-99_ACDIS_WGH_Sample.dta",
    mghfile = "RD04-99_ACDIS_MGH_Sample.dta"), 
  env = .GlobalEnv)

hiv0 <- readHIVData(dropTasP = TRUE, write_rda = FALSE)
rtdat <- getRTData(hiv0)
rtdat$early_pos[rtdat$IIntID == 2820] <- as.Date("2019-02-10")
rtdat$sero_date[rtdat$IIntID == 2820] <- 1
rtdat$early_pos[rtdat$IIntID == 2830] <- as.Date("2017-05-01")
rtdat$sero_date[rtdat$IIntID == 2830] <- 1

context("Test splitEpisodes")
Args <- setArgs(Years = c(2005:2018), Age = list(All = c(15,54)),
  imputeMethod = imputeMidPoint)
mdat <- Args$imputeMethod(rtdat)
test_that("Check imputeMidPoint", {
  expect_equal(as.character(mdat$sero_date[mdat$IIntID == 2820]),  "2017-04-20" )
  expect_equal(as.character(mdat$sero_date[mdat$IIntID == 2830]),  "2016-05-30" )
})


edat <- splitAtSeroDate(dat) 
test_that("Check splitAtSeroDate", {
  expect_equal(nrow(edat),  40)
  expect_equal(edat1$sero_date[edat1$IIntID == 2820], as.Date())
  # expect_equal(sum(as.numeric(edat$obs_end)), 2895975962)
})
edat2 <- splitAtEarlyPos(dat)
test_that("Check splitAtEarlyPos", {
  expect_equal(nrow(edat2), 196555)
  expect_equal(sum(as.numeric(edat2$obs_end)), 3030384325)
})

rm(list = c("getFiles"), envir = .GlobalEnv)
if (exists("getFiles_User"))
  getFiles <<- getFiles_User

testDates <- function(dat=NULL) {
  testNeg <- filter(dat, is.finite(obs_start) & is.finite(late_neg))
  if(any(with(testNeg, obs_start > late_neg))) 
      stop("Some obs_start > late_neg") 
  testPos <- filter(dat, is.finite(early_pos) & is.finite(late_neg))
  if(any(with(testPos, late_neg >= early_pos))) 
      stop("Some late_neg >= early_pos") 
}

context("Test RTDates")
rtdat <- getRTData(dat = hiv0)
rtdat$obs_start[rtdat$IIntID==2820]  <- as.Date("2020-01-01", origin="1970-01-01")
test_that("Test RTDates: wrong date", {
  expect_error(testDates(rtdat))
})

