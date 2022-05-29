## Description: Test functions
## Project: ahri
## Author: AV / Created: 26Sep2020
# testthat::test_file('inst/tests/testthat/test_ahri_hiv_2020.R')

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
rtdat <- getRTData(dat = hiv0)

context("Test getIncData")
Args <- setArgs(imputeMethod = imputeMidPoint)
idat <- getIncData(rtdat, bdat=getBirthDate(edat0), Args)
test_that("Check Time, tscale and sero events", {
  expect_gt(min(idat$tscale), 0)
  expect_lte(max(idat$tscale), 1.02)
  expect_equal(sum(idat$sero_event), 1)
  expect_equal(sum(idat$Time),  6815)
})

context("Test AggFunc")
agedat <- AggByAge(idat)
yeardat <- AggByYear(idat)
test_that("Check seroevent and pyears add up", {
  expect_equal(agedat[agedat$AgeCat == "[15,20)", "sero_event"], 0)
  expect_equal(agedat[agedat$AgeCat == "[20,25)", "sero_event"], 1)
  expect_equal(agedat[agedat$AgeCat == "[70,75)", "sero_event"], 0)
  expect_equal(sum(yeardat$sero_event), sum(idat$sero_event))
  expect_equal(sum(yeardat$pyears), sum(agedat$pyears))
  expect_equal(sum(yeardat$sero_event), sum(agedat$sero_event))
  expect_equal(yeardat[yeardat$Year == 2007, "sero_event"], 0)
  expect_equal(yeardat[yeardat$Year == 2008, "sero_event"], 1)
  expect_equal(yeardat[yeardat$Year == 2009, "sero_event"], 0)
})

AggByYearAgeFem <- AggFunc("Year + AgeCat + Female")
ageyear <- AggByYearAgeFem(idat)
test_that("Check seroevent and pyears add up", {
  expect_equal(sum(yeardat$sero_event), sum(ageyear$sero_event, na.rm = TRUE))
  expect_equal(sum(yeardat$pyears), sum(ageyear$pyears, na.rm = TRUE))
})

context("Test MMIaggregate")
set.seed(1234)
Args <- setArgs(nSim=1, mcores=1)
mdat <- MIdata(rtdat, Args, bdat = getBirthDate(edat0))
mdat <- mitools::imputationList(mdat)
inc <- with(mdat, fun=AggByYear)
yeardat1 <- MIaggregate(inc)
test_that("Check seroevent and pyears add up", {
  expect_equal(yeardat1[yeardat1$Year == 2007, "sero_event"], 0)
  expect_equal(yeardat1[yeardat1$Year == 2008, "sero_event"], 1)
  expect_equal(yeardat1[yeardat1$Year == 2009, "sero_event"], 0)
  expect_equal(sum(yeardat1$sero_event), sum(yeardat$sero_event))
})

# seron <- 3848
# pyearsn <- 111388

# context("Test MMIaggregate 3")
# Args <- setArgs(nSim=2, mcores=1)
# rtdat <- getRTData(dat=getHIV())
# mdat <- MIdata(rtdat, Args)
# mdat <- mitools::imputationList(mdat)
# inc <- with(mdat, fun=AggByYear)
# agedat3 <- MIaggregate(inc)
# test_that("Check seroevent and pyears add up", {
#   expect_equal(round(sum(agedat3$sero_event)), seron)
#   expect_equal(round(sum(agedat3$pyears)), pyearsn)
# })


# set.seed(1234)
# Args <- setArgs(Age=list(All=c(15, 49)), nSim=2, mcores=1)
# rtdat <- getRTData(dat=getHIV())
# mdat <- MIdata(rtdat, Args)
# AggByYearAgeFem <- AggFunc("Year + AgeCat + Female")
# inc <- lapply(mdat, AggByYearAgeFem)
# inc3 <- MIaggregate(inc)
# test_that("Check seroevent and pyears add up", {
#   expect_equal(round(sum(inc3$sero_event)), seron)
#   expect_equal(round(sum(inc3$pyears)), pyearsn)
# })


# context("Test getIncidence (random-point)")
# Args <- setArgs(Age=list(All=c(15, 49)), Years=c(2004:2018), nSim=2)
# age_dat <- getAgeYear(dat=setHIV(Args))
# sformula = "sero_event ~ -1 + as.factor(Year) + Age + as.factor(Year):Age + offset(log(tscale))"
# set.seed(123456)
# inc <- getIncidence(Args, sformula, AggByYear, age_dat)
# test_that("Count sero events and ptime", {
#   expect_equal(sum(inc$agg$sero), 3807)
#   expect_equal(round(sum(inc$agg$pyears)), 109523)
# })

rm(list = c("getFiles"), envir = .GlobalEnv)
if (exists("getFiles_User"))
  getFiles <<- getFiles_User
