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
hdat2 <- readHIVData(dropTasP=TRUE, write_rda=FALSE)
test_that("Check getHIVData N", {
  expect_equal(length(unique(hdat1$IIntID)), 67273)
  expect_equal(length(unique(hdat2$IIntID)), 55256)
})

context("Test readHIVData (internal)")
hiv <- haven::read_dta(getFiles()$hivfile) %>% 
  dplyr::select(IIntID=IIntId, BSIntID=ResidencyBSIntId, VisitDate, 
    HIVResult, Female=Sex, Age=AgeAtVisit)
test_that("Check N", {
  expect_equal(length(unique(hiv$IIntID)), 104228)
})

# hiv <- haven::zap_labels(hiv)
hiv <- dplyr::filter(hiv, Female %in% c(1,2))
test_that("Check Sex", {
  expect_equal(length(unique(hiv$IIntID)), 104228)
})

hiv <- dplyr::mutate(hiv,
  IIntID = as.integer(IIntID),
  BSIntID = as.integer(BSIntID),
  Female=as.integer(ifelse(Female==2, 1, 0)))
test_that("Check readHIV Sex", {
  expect_equal(length(unique(hiv$IIntID)), 104228)
})

hiv <- dplyr::arrange(hiv, IIntID, VisitDate)
hiv <- dplyr::filter(hiv, HIVResult %in% c(0,1))
test_that("Check readHIV Result", {
  expect_equal(length(unique(hiv$IIntID)), 67330)
})

hiv <- dplyr::filter(hiv, Age %in% c(15:100))
test_that("Check readHIV Age", {
  expect_equal(length(unique(hiv$IIntID)), 67273)
})

hiv <- dplyr::mutate(hiv, 
  HIVNegative = ifelse(HIVResult==0, VisitDate, NA), 
  HIVPositive = ifelse(HIVResult==1, VisitDate, NA))
hiv <- dplyr::mutate(hiv, Year=as.integer(format(VisitDate, "%Y")))
Vars <- c("HIVNegative", "HIVPositive")
hiv[Vars] <- lapply(hiv[Vars], as.Date, origin="1970-01-01")
test_that("Check readHIV End", {
  expect_equal(length(unique(hiv$IIntID)), 67273)
})

context("Test readEpisodes (internal)")
edat <- haven::read_dta(getFiles()$epifile) 
edat <- dplyr::select(edat,
  IIntID=IndividualId, BSIntID=LocationId, 
  Female=Sex, Age,  DoB, DoD,
  Year=CalendarYear,ExpDays=Days,
  ObservationStart=StartDate,
  ObservationEnd=EndDate,
  InMigration, OutMigration,
  Resident)
test_that("Check readEpisodes N 1", {
  expect_equal(length(unique(edat$IIntID)), 231179) 
  expect_equal(length(unique(edat$BSIntID)), 24848) 
  expect_equal(length(edat$Female[edat$Female==1]), 3000277) 
})

edat <- dplyr::filter(edat, Female %in% c(1,2))
edat <- dplyr::mutate(edat,
  IIntID=as.integer(IIntID),
  BSIntID=as.integer(BSIntID),
  Year=as.integer(Year),
  Female=as.integer(ifelse(Female==2, 1, 0)))
edat <- dplyr::arrange(edat, IIntID, ObservationStart)
test_that("Check readEpisodes N 2", {
  expect_equal(length(unique(edat$IIntID)), 231177) 
  expect_equal(length(unique(edat$BSIntID)), 24848) 
  expect_equal(length(edat$Female[edat$Female==0]), 3000277)
})


context("Test readEpisodes with dropTasP")
edat <- readEpisodes(dropTasP=TRUE, write_rda=FALSE)
test_that("Check readEpisodes dropTasP N", {
  expect_equal(length(unique(edat$IIntID)), 178604) 
  expect_equal(length(unique(edat$BSIntID)), 16856) 
  expect_equal(length(edat$Female[edat$Female==0]), 2809200) 
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

# context("Test getWGH getMGH")
# wgh <- getWGH()
# mgh <- getMGH()
# test_that("Check N", {
#   expect_equal(length(unique(mgh$IIntID)), 37787)
#   expect_equal(length(unique(wgh$IIntID)), 50563)
#   expect_equal(sum(mgh$Age), 4732753)
#   expect_equal(sum(wgh$Age), 8471355)
# })
