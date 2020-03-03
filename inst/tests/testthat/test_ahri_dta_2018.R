## Description: Test incidence code
## Project: 
## Author: AV / Created: 28Sep2018 

context("Test getBSData")
bdat <- getBSData()
test_that("Check readBSData N", {
  expect_equal(length(unique(bdat$BSIntID)), 30776)
  expect_equal(sum(bdat$PIPSA[bdat$PIPSA==1], na.rm=TRUE), 18058)
})


context("Test readHIVData")
hdat1 <- readHIVData(dropTasP=FALSE, write_rda=FALSE)
hdat2 <- readHIVData(dropTasP=TRUE, write_rda=FALSE)
test_that("Check getHIVData N", {
  expect_equal(length(unique(hdat1$IIntID)), 63625)
  expect_equal(length(unique(hdat2$IIntID)), 53800)
})

context("Test readHIVData (internal)")
hiv <- haven::read_dta(getFiles()$hivfile) %>% 
  select(IIntID=IIntId, BSIntID=ResidencyBSIntId, VisitDate, 
    HIVResult, Female=Sex, Age=AgeAtVisit)
test_that("Check N", {
  expect_equal(length(unique(hiv$IIntID)), 97352)
})
# hiv <- haven::zap_labels(hiv)
hiv <- filter(hiv, Female %in% c(1,2))
test_that("Check Sex", {
  expect_equal(length(unique(hiv$IIntID)), 97352)
})
hiv <- mutate(hiv,
  IIntID = as.integer(IIntID),
  BSIntID = as.integer(BSIntID),
  Female=as.integer(ifelse(Female==2, 1, 0)))
test_that("Check readHIV Sex", {
  expect_equal(length(unique(hiv$IIntID)), 97352)
})
hiv <- arrange(hiv, IIntID, VisitDate)
hiv <- filter(hiv, HIVResult %in% c(0,1))
test_that("Check readHIV Result", {
  expect_equal(length(unique(hiv$IIntID)), 63687)
})
hiv <- filter(hiv, Age %in% c(15:100))
test_that("Check readHIV Age", {
  expect_equal(length(unique(hiv$IIntID)), 63625)
})
hiv <- mutate(hiv, 
  HIVNegative = ifelse(HIVResult==0, VisitDate, NA), 
  HIVPositive = ifelse(HIVResult==1, VisitDate, NA))
hiv <- mutate(hiv, Year=as.integer(format(VisitDate, "%Y")))
Vars <- c("HIVNegative", "HIVPositive")
hiv[Vars] <- lapply(hiv[Vars], as.Date, origin="1970-01-01")
test_that("Check readHIV End", {
  expect_equal(length(unique(hiv$IIntID)), 63625)
})


context("Test readEpisodes (internal)")
edat <- haven::read_dta(getFiles()$epifile) 
edat <- select(edat,
  IIntID=IndividualId, BSIntID=LocationId, 
  Female=Sex, Age,  DoB, DoD,
  Year,ExpDays=Days,
  ObservationStart=StartDate,
  ObservationEnd=EndDate,
  InMigration, OutMigration,
  Resident)
test_that("Check readEpisodes N 1", {
  expect_equal(length(unique(edat$IIntID)), 220648) 
  expect_equal(length(unique(edat$BSIntID)), 24520) 
  expect_equal(length(edat$Female[edat$Female==1]), 2750897) 
})
edat <- filter(edat, Female %in% c(1,2))
edat <- mutate(edat,
  IIntID=as.integer(IIntID),
  BSIntID=as.integer(BSIntID),
  Year=as.integer(Year),
  Female=as.integer(ifelse(Female==2, 1, 0)))
edat <- arrange(edat, IIntID, ObservationStart)
test_that("Check readEpisodes N 2", {
  expect_equal(length(unique(edat$IIntID)), 220646) 
  expect_equal(length(unique(edat$BSIntID)), 24520) 
  expect_equal(length(edat$Female[edat$Female==0]), 2750897) 
})


context("Test readEpisodes with dropTasP")
edat <- readEpisodes(dropTasP=TRUE, write_rda=FALSE)
test_that("Check readEpisodes dropTasP N", {
  expect_equal(length(unique(edat$IIntID)), 174445) 
  expect_equal(length(unique(edat$BSIntID)), 16440) 
  expect_equal(length(edat$Female[edat$Female==0]), 2645718) 
})


context("Test readMGH readWGH")
wgh0 <- readHealthData(Female=1, write_rda=FALSE)
mgh0 <- readHealthData(Female=0, write_rda=FALSE)
test_that("Check WGH/MGH N and Age", {
  expect_equal(length(unique(mgh0$IIntID)), 37787)
  expect_equal(length(unique(wgh0$IIntID)), 50563)
  expect_equal(sum(mgh0$Age), 4732753)
  expect_equal(sum(wgh0$Age), 8471355)
})



