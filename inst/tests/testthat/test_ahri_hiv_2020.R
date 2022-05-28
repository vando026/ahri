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
bidat <- dplyr::group_by(hiv0, IIntID) %>% dplyr::slice(1) %>%
  dplyr::mutate(DateOfBirth = VisitDate - (Age * 365)) %>% 
  dplyr::select(IIntID, DateOfBirth)
rtdat <- getRTData(hiv0)
rtdat$early_pos[rtdat$IIntID == 2820] <- as.Date("2019-02-10")
rtdat$sero_event[rtdat$IIntID == 2820] <- 1
rtdat$early_pos[rtdat$IIntID == 2830] <- as.Date("2017-05-01")
rtdat$sero_event[rtdat$IIntID == 2830] <- 1

context("Test getIncData")
Args <- setArgs(Age=list(Mal=c(15, 100), Fem=c(15, 100)), 
  Years=c(2004:2017), imputeMethod=imputeMidPoint)
idat <- getIncData(rtdat, bdat=bidat, Args)
test_that("Check Time, tscale and sero events", {
  expect_gt(min(idat$tscale), 0)
  expect_lte(max(idat$tscale), 1.02)
  expect_equal(sum(idat$sero_event), 2)
  expect_equal(sum(idat$Time),  14258)
})

context("Test AggFunc")
agedat <- AggByAge(idat)
yeardat <- AggByYear(idat)
test_that("Check seroevent and pyears add up", {
  expect_equal(sum(agedat$sero_event), sum(idat$sero_event))
  expect_equal(round(sum(agedat$pyears)), 39)
  expect_equal(sum(yeardat$sero_event), sum(idat$sero_event))
  expect_equal(round(sum(yeardat$pyears)),  39)
})

context("Test MMIaggregate")
set.seed(1234)
Args <- setArgs(nSim=1, mcores=1)
rtdat <- getRTData(dat = hiv0)
mdat <- MIdata(rtdat, Args, bdat = bidat)
mdat <- mitools::imputationList(mdat)
inc <- with(mdat, fun=AggByYear)
agedat1 <- MIaggregate(inc)
test_that("Check seroevent and pyears add up", {
  expect_equal(sum(agedat1$sero_event), 2)
  expect_equal(round(sum(agedat1$pyears)), 159710)
})


seron <- 3848
pyearsn <- 111388

context("Test MMIaggregate 3")
set.seed(1234)
Args <- setArgs(Age=list(All=c(15, 49)), nSim=2, mcores=1)
rtdat <- getRTData(dat=getHIV())
mdat <- MIdata(rtdat, Args)
mdat <- mitools::imputationList(mdat)
inc <- with(mdat, fun=AggByYear)
agedat3 <- MIaggregate(inc)
test_that("Check seroevent and pyears add up", {
  expect_equal(round(sum(agedat3$sero_event)), seron)
  expect_equal(round(sum(agedat3$pyears)), pyearsn)
})


context("Test AggFunc")
set.seed(1234)
Args <- setArgs(Age=list(All=c(15, 49)), nSim=2, mcores=1)
rtdat <- getRTData(dat=getHIV())
mdat <- MIdata(rtdat, Args)
AggByYearAgeFem <- AggFunc("Year + AgeCat + Female")
inc <- lapply(mdat, AggByYearAgeFem)
inc3 <- MIaggregate(inc)
test_that("Check seroevent and pyears add up", {
  expect_equal(round(sum(inc3$sero_event)), seron)
  expect_equal(round(sum(inc3$pyears)), pyearsn)
})


set.seed(1234)
Args <- setArgs(Age=list(All=c(15, 49)), nSim=2, mcores=1)
rtdat <- getRTData(dat=getHIV())
mdat <- MIdata(rtdat, Args)
AggByYearAgeFem <- AggFunc("Year + AgeCat + Female")
inc <- lapply(mdat, AggByYearAgeFem)
inc3 <- MIaggregate(inc)
test_that("Check seroevent and pyears add up", {
  expect_equal(round(sum(inc3$sero_event)), seron)
  expect_equal(round(sum(inc3$pyears)), pyearsn)
})


context("Test getCrudeRate against AggByYear")
test_that("Check seroevent and pyears add up", {
  expect_equal(round(sum(idat$Time)/365.25), round(sum(yeardat$pyears)))
  expect_equal((sum(idat$sero_event)), (sum(yeardat$sero_event)))
})

context("Test getIncidence (mid-point)")
Args <- setArgs(Years=c(2008:2018), 
  Age=list(All=c(15, 45)),
  imputeMethod=imputeMidPoint)
rtdat <- getRTData(dat=getHIV())
idat <- getIncData(rtdat, bdat=getBirthDate(), Args)
inc <- AggByYear(idat)
test_that("Count sero events and ptime", {
  expect_equal(sum(inc$sero_event), 2832)
  expect_equal(round(sum(inc$pyears)), 73506)
})


context("Test getIncidence (random-point)")
Args <- setArgs(Age=list(All=c(15, 49)), Years=c(2004:2018), nSim=2)
age_dat <- getAgeYear(dat=setHIV(Args))
sformula = "sero_event ~ -1 + as.factor(Year) + Age + as.factor(Year):Age + offset(log(tscale))"
set.seed(123456)
inc <- getIncidence(Args, sformula, AggByYear, age_dat)
test_that("Count sero events and ptime", {
  expect_equal(sum(inc$agg$sero), 3807)
  expect_equal(round(sum(inc$agg$pyears)), 109523)
})


