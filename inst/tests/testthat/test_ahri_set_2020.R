## Description: Test functions
## Project: ahri
## Author: AV / Created: 06Mar2020 
# testthat::test_file('inst/tests/testthat/test_ahri_set_2020.R')

if (exists("getFiles", env = .GlobalEnv))
  getFiles_User <- getFiles

assign("getFiles", 
  setFiles(system.file("exdata", package = "ahri"),
    hivfile = "RD05-99_ACDIS_HIV_Sample.dta",
    bsifile = "RD01-03_ACDIS_BS_Sample.dta",
    epifile = "SurveillanceEpisodes_Sample.dta",
    wghfile = "RD03-99_ACDIS_WGH_Sample.dta",
    mghfile = "RD04-99_ACDIS_MGH_Sample.dta"), 
  env = .GlobalEnv)

hiv0 <- readHIVData()
edat0 <- readEpisodes()
rtdat <- getRTData(hiv0)

context("Test setArgs and setData")
Args <- setArgs(Years=c(2007:2014), Age=list(Fem=c(40, 80)))
hiv1 <- setData(hiv0, Args)
test_that("Check sex and age", {
  expect_equal(unique(hiv1$Female), 1) 
  expect_gte(hiv1$Year, 2007)
  expect_lte(hiv1$Year, 2014)
  expect_gte(hiv1$Age, 40)
  expect_lte(hiv1$Age, 80)
  expect_equal(hiv1$IIntID, 740)
  expect_equal(sum(as.integer(hiv1$AgeCat == "[70,75)")), 1)
})

Args <- setArgs(Years=c(2005:2011), Age=list(Mal=c(20, 25)))
hiv1 <- setData(hiv0, Args)
test_that("Check sex and age", {
  expect_equal(unique(hiv1$Female), 0) 
  expect_gte(hiv1$Year, 2005)
  expect_lte(hiv1$Year, 2011)
  expect_gte(hiv1$Age, 20)
  expect_lte(hiv1$Age, 25)
  expect_equal(hiv1$IIntID, 1356)
  expect_equal(sum(as.integer(hiv1$AgeCat == "[20,25)")), 1)
})

Args <- setArgs(Years=c(2001:2018), 
  AgeCat = seq(15, 105, by = 5), Age=list(All=c(15, 100)))
hiv2 <- setData(hiv0, Args, time2="VisitDate", birthdate = getBirthDate(edat0))
test_that("Check setData Age", {
  expect_equal(sum(as.integer(hiv2$AgeCat == "[20,25)")), 3)
})

Args <- setArgs(Years=c(2010:2018), Age=list(All=c(15, 54)))
hiv1 <- setData(hiv0, Args)
test_that("Check sex and age", {
  expect_equal(sort(unique(hiv1$IIntID)), c(795, 800, 1356, 1436))
  expect_gte(min(hiv1$Year), 2010)
  expect_lte(max(hiv1$Year), 2018)
  expect_gte(min(hiv1$Age), 15)
  expect_lte(max(hiv1$Age), 54)
  expect_equal(sum(as.integer(hiv1$AgeCat == "[15,20)")), 2)
  expect_equal(sum(as.integer(hiv1$AgeCat == "[20,25)")), 2)
  expect_equal(sum(as.integer(hiv1$AgeCat == "[25,30)")), 1)
  expect_equal(sum(as.integer(hiv1$AgeCat == "[30,35)")), 2)
  expect_equal(sum(as.integer(hiv1$AgeCat == "[70,75)")), 0)
})

context("Test makeAgeVars")
Args <- setArgs(Years=c(2010:2018), Age=list(All=c(15, 54)))
hiv2 <- setData(hiv0, Args)
hiv2 <- makeAgeVars(hiv2, age_cut=Args$AgeCat, 
  time2="VisitDate", birthdate = getBirthDate(edat0))
test_that("Check makeAgeVars Age", {
  expect_equal(sort(unique(hiv2$IIntID)), c(795, 800, 1356, 1436))
  expect_gte(min(hiv2$Year), 2010)
  expect_lte(max(hiv2$Year), 2018)
  expect_gte(min(hiv2$Age), 15)
  expect_lte(max(hiv2$Age), 54)
  expect_equal(sum(as.integer(hiv2$AgeCat == "[15,20)")), 2)
  expect_equal(sum(as.integer(hiv2$AgeCat == "[20,25)")), 2)
  expect_equal(sum(as.integer(hiv2$AgeCat == "[25,30)")), 1)
  expect_equal(sum(as.integer(hiv2$AgeCat == "[30,35)")), 2)
  expect_equal(sum(as.integer(hiv2$AgeCat == "[70,75)")), 0)
})

context("Test setHIV")
Args <- setArgs(Years=c(2005:2017), Age=list(All=c(15, 54)))
hiv <- setData(hiv0, Args)
hiv1 <- setHIV(Args, hiv0)
test_that("Check setHIV equal setData", {
  expect_equal(hiv1$IIntID, hiv$IIntID) 
  expect_equal(hiv1$Age, hiv$Age) 
  expect_equal(hiv1$Year, hiv$Year) 
  expect_equal(hiv1, hiv) 
})

context("Test setHIV")
hiv1[hiv1$IIntID == 795, "Female"] <- 5
test_that("Check setHIV equal setData", {
  expect_error(expect_equal(hiv, hiv1))
})

context("Test setEpisodes")
Args = setArgs(Years=c(2010:2020), Age = list(All=c(16, 60)))
edat <- setData(edat0, Args)
edat1 <- setEpisodes(Args, dat = edat0)
test_that("Check setEpisodes equal setData", {
  expect_equal(edat, edat1)
})

context("Test getRTData")
test_that("Check rtdata vars", {
  expect_equal(sort(unique(rtdat$IIntID)), c(740, 1356, 1436))
  expect_equal(as.character(rtdat$obs_start[rtdat$IIntID == 740]), "2007-05-20") 
  expect_equal(as.character(rtdat$late_neg[rtdat$IIntID == 740]), "2017-10-01") 
  expect_equal(as.character(rtdat$obs_start[rtdat$IIntID == 1356]), "2005-05-11") 
  expect_equal(as.character(rtdat$late_neg[rtdat$IIntID == 1356]), "2005-05-11") 
  expect_equal(as.character(rtdat$early_pos[rtdat$IIntID == 1356]), "2011-05-04") 
})

context("Test splitEpisodes")
mdat <- imputeMidPoint(rtdat)
test_that("Check imputeMidPoint", {
  expect_equal(as.character(mdat$sero_date[mdat$IIntID == 1356]),  "2008-05-07" )
})

sdat <- splitAtSeroDate(mdat) 
id740 <- sdat[sdat$IIntID == 740, ]
ni <- nrow(id740)
test_that("Check splitAtSeroDate", {
  expect_equal(id740$Year, c(2007:2017))
  expect_equal(as.character(id740$obs_end[ni]), as.character(id740$late_neg[ni]))
})

id1356 <- sdat[sdat$IIntID == 1356, ]
ni <- nrow(id1356)
test_that("Check splitAtSeroDate", {
  expect_equal(id1356$Year, c(2005:2008))
  expect_equal(as.character(id1356$obs_end[ni]), as.character(id1356$sero_date[ni]))
})

endat <- imputeEndPoint(rtdat)
sdat <- splitAtEarlyPos(endat)
id1356 <- sdat[sdat$IIntID == 1356, ]
ni <- nrow(id1356)
test_that("Check splitAtSeroDate", {
  expect_equal(id1356$Year, c(2005:2011))
  expect_equal(as.character(id1356$obs_end[ni]), as.character(id1356$sero_date[ni]))
})

context("Test splitAtSeroDate")
mdat <- imputeMidPoint(rtdat)
edat <- splitAtSeroDate(mdat)
test_that("Check Time > 0 days, Time <= 366 days", {
  expect_lte(max(edat$Time),  366)
  expect_gt(min(edat$Time),  0)
})

context("Test RTDates")
rtdat1 <- rtdat
rtdat1$obs_start[rtdat1$IIntID==2820]  <- as.Date("2020-01-01", origin="1970-01-01")
test_that("Test RTDates: wrong date", {
  expect_error(testDates(rtdat1))
})

rm(list = c("getFiles"), envir = .GlobalEnv)
if (exists("getFiles_User"))
  getFiles <<- getFiles_User
