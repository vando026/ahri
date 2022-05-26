## Description: Test functions
## Project: ahri
## Author: AV / Created: 06Mar2020 
# testthat:test_file('inst/tests/testthat/test_ahri_set_2020.R')

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

edat0 <- readEpisodes(dropTasP=TRUE, write_rda=FALSE)
hiv0 <- readHIVData(dropTasP=TRUE, write_rda=FALSE)


context("Test setHIV with dropTasP")
Args = setArgs(Years=c(2008:2016), Age = list(All=c(20, 60)))
hiv <- setHIV(Args, dat=hiv0)
test_that("Check getHIV dropTasP N", {
  expect_equal(length(unique(hiv$IIntID)), 4) 
  expect_equal(min(hiv$Age), 20) 
  expect_equal(max(hiv$Age), 60) 
  expect_equal(sum(hiv$Age), 409) 
  expect_equal(min(hiv$Year), 2008) 
  expect_equal(max(hiv$Year), 2015) 
})


context("Test setEpisodes with dropTasP")
Args = setArgs(Years=c(2010:2020), Age = list(All=c(16, 60)))
ydat <- setEpisodes(Args, dat=edat0)
test_that("Check readEpisodes dropTasP N", {
  expect_equal(length(unique(ydat$IIntID)), 2) 
  expect_equal(min(ydat$Year), 2010) 
  expect_equal(max(ydat$Year), 2019) 
  expect_equal(min(ydat$Age), 27) 
  expect_equal(max(ydat$Age), 41) 
  expect_equal(max(ydat$Year), 2019) 
  expect_equal(length(unique(ydat$BSIntID)), 2) 
  expect_equal(length(ydat$Female[ydat$Female==0]), 17) 
})


context("Test makeAgeVars")
bidat <- data.frame(
  IIntID = unique(hiv$IIntID),
  DateOfBirth = as.Date("2000-01-01") - c(10, 20, 6, 38))
Args <- setArgs(Years=c(2001:2018), Age=list(All=c(15, 100)))
hiv <- setHIV(Args, dat=hiv0)
hiv1 <- makeAgeVars(hiv)
hiv2 <- makeAgeVars(hiv, age_cut=Args$AgeCat)
hiv3 <- makeAgeVars(hiv, age_cut=Args$AgeCat, time2="VisitDate",
  birthdate = bidat)
test_that("Check makeAgeVars Age", {
  expect_equal(sum(hiv1$Age), 1280) 
  expect_equal(sum(hiv2$Age), 1280) 
  expect_equal(sum(hiv3$Age, na.rm=TRUE),  274) 
})

context("Test setData")
Args <- setArgs(Years=c(2001:2018), Age=list(All=c(15, 100)))
hiv <- setHIV(Args, dat=hiv0)
hiv1 <- setData(hiv, Args)
hiv2 <- setData(hiv, Args, time2="VisitDate")
Args <- setArgs(Years=c(2005:2018), Age=list(All=c(15, 54)))
hiv3 <- setData(hiv, Args, time2="VisitDate", birthdate = bidat)
test_that("Check setData Age", {
  expect_equal(sum(hiv1$Age), 1280) 
  expect_equal(sum(hiv2$Age), 1280) 
  expect_equal(sum(hiv3$Age), 46) 
})


context("Test setHIV")
Args <- setArgs(Years=c(2005:2017), Age=list(All=c(15, 54)))
hiv <- setHIV(Args, dat=hiv0)
test_that("Check setHIV N", {
  expect_equal(length(unique(hiv$IIntID)), 3) 
  expect_equal(sum(hiv$Age),  410) 
})


context("Test setEpisodes")
Args <- setArgs(Years=c(2005:2018), Age=list(All=c(15, 54)))
ydat <- setEpisodes(Args, dat=edat0)
test_that("Check setEpisodes N", {
  expect_equal(length(unique(ydat$IIntID)), 2)
  expect_equal(nrow(ydat), 31)
})

context("Test new Args defaults")
Args <- setArgs(Years=c(2000:2025), Age=list(All=c(0, 110)))
hiv <- setHIV(Args, dat=hiv0)
epi <- setEpisodes(Args, dat=edat0)
test_that("Check new Args Age", {
  expect_equal(sum(hiv$Age), 1280) 
  expect_equal(sum(epi$Age), 1684) 
})

rm(list = c("getFiles"), envir = .GlobalEnv)
if (exists("getFiles_User"))
  getFiles <<- getFiles_User
