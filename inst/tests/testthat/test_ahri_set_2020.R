## Description: Test functions
## Project: ahri
## Author: AV / Created: 06Mar2020 
# testthat::test_file('inst/tests/testthat/test_ahri_set_2020.R')

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

edat0 <- readEpisodes(dropTasP = TRUE, write_rda = FALSE)
hiv0 <- readHIVData(dropTasP = TRUE, write_rda = FALSE)
bidat <- dplyr::group_by(hiv0, IIntID) %>% dplyr::slice(1) %>%
  dplyr::mutate(DateOfBirth = VisitDate - (Age * 365)) %>% 
  dplyr::select(IIntID, DateOfBirth)

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
Args <- setArgs(Years=c(2001:2018), 
  AgeCat = seq(15, 105, by = 5), Age=list(All=c(15, 100)))
hiv1 <- makeAgeVars(hiv0, age_cut = seq(15, 105, by = 5))
hiv2 <- makeAgeVars(hiv0, age_cut=Args$AgeCat)
hiv3 <- makeAgeVars(hiv0, age_cut=Args$AgeCat, 
  time2="VisitDate", birthdate = bidat)
test_that("Check makeAgeVars Age", {
  expect_equal(sum(hiv1$Age), 1280) 
  expect_equal(sum(as.integer(hiv1$AgeCat == "[20,25)")), 10)
  expect_equal(sum(hiv2$Age), 1280) 
  expect_equal(sum(as.integer(hiv2$AgeCat == "[20,25)")), 10)
  expect_equal(sum(hiv3$Age),  1262) 
  expect_equal(sum(as.integer(hiv3$AgeCat == "[20,25)")), 8)
})

context("Test setData")
Args <- setArgs(Years=c(2001:2018), 
  AgeCat = seq(15, 105, by = 5), Age=list(All=c(15, 100)))
hiv1 <- setData(hiv0, Args)
hiv2 <- setData(hiv0, Args, time2="VisitDate", birthdate = bidat)
test_that("Check setData Age", {
  expect_equal(sum(hiv1$Age), 1280) 
  expect_equal(sum(as.integer(hiv1$AgeCat == "[20,25)")), 10)
  expect_equal(sum(hiv2$Age), 1262) 
  expect_equal(sum(as.integer(hiv2$AgeCat == "[20,25)")), 8)
})

context("Test setHIV")
Args <- setArgs(Years=c(2005:2017), Age=list(All=c(15, 54)))
hiv <- setData(hiv0, Args)
hiv1 <- setHIV(Args, hiv0)
test_that("Check setHIV N", {
  expect_equal(hiv1, hiv) 
})

context("Test setEpisodes")
Args <- setArgs(Years=c(2005:2018), Age=list(All=c(15, 54)))
ydat <- setEpisodes(Args, dat = edat0)
ydat1 <- setData(edat0, Args)
test_that("Check setEpisodes N", {
  expect_equal(ydat, ydat1)
})

rm(list = c("getFiles"), envir = .GlobalEnv)
if (exists("getFiles_User"))
  getFiles <<- getFiles_User
