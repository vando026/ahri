## Description: Test functions
## Project: ahri
## Author: AV / Created: 26Sep2020
# testthat::test_file('inst/tests/testthat/test_ahri_hiv_2020.R')


context("Test setData (15-54 years)")
Args <- setArgs(nSim=1, Years=c(2005:2018), Age=list(Mal=c(15, 54)))
hiv   <- getHIV()
rtdat <- getRTData(hiv)
dat <- imputeMidPoint(rtdat)
edat <- splitAtSeroDate(dat)
adat <- setData(edat, Args, time2="obs_end")
test_that("Check Time, Age and sero events", {
  expect_equal(sum(adat$Time),  16282483)
  expect_equal(sum(adat$sero_event), 930)
  expect_equal(sum(adat$Age), 1388066)
})


context("Test setData (25-29 years)")
Args <- setArgs(nSim=1, Years=c(2005:2018), Age=list(Mal=c(25, 29)))
hiv   <- getHIV()
rtdat <- getRTData(hiv)
dat <- imputeMidPoint(rtdat)
edat <- splitAtSeroDate(dat )
adat <- setData(edat, Args, time2="obs_end")
test_that("Check Time, Age and sero events", {
  expect_equal(sum(adat$Time), 2355857)
  expect_equal(sum(adat$sero_event), 262)
  expect_equal(sum(adat$Age), 194327)
})


context("Test getIncData")
Args <- setArgs(Age=list(Mal=c(15, 54), Fem=c(15, 54)), 
  Years=c(2004:2017), imputeMethod=imputeMidPoint)
hiv <- getHIV()
rtdat <- getRTData(hiv)
idat <- getIncData(rtdat, bdat=getBirthDate(), Args)
test_that("Check Time, Age and sero events", {
  expect_equal(sum(idat$Time), 42204082)
  expect_equal(sum(idat$sero_event), 3845)
  expect_equal(sum(idat$Age),  3854380)
})

context("Test AggFunc")
agedat <- AggByAge(idat)
yeardat <- AggByYear(idat)
test_that("Check seroevent and pyears add up", {
  expect_equal(sum(agedat$sero_event), sum(idat$sero_event))
  expect_equal(round(sum(agedat$pyears)), 115548)
  expect_equal(sum(yeardat$sero_event), sum(idat$sero_event))
  expect_equal(round(sum(yeardat$pyears)),  115548)
})

context("Test MMIaggregate")
set.seed(1234)
Args <- setArgs(nSim=1, mcores=1)
rtdat <- getRTData(dat=getHIV())
mdat <- MIdata(rtdat, Args)
mdat <- mitools::imputationList(mdat)
inc <- with(mdat, fun=AggByYear)
agedat1 <- MIaggregate(inc)
test_that("Check seroevent and pyears add up", {
  expect_equal(round(sum(agedat1$sero_event)), 4007)
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


