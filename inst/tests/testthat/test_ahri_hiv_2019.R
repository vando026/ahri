## Description: Test functions
## Project: ahri
## Author: AV / Created: 06Mar2020 


context("Test setData (15-54 years)")
Args <- setArgs(nSim=1, Years=c(2005:2018), Age=list(Mal=c(15, 54)))
hiv   <- getHIV()
rtdat <- getRTData(hiv)
dat <- imputeMidPoint(rtdat)
edat <- splitAtSeroDate(dat )
adat <- setData(edat, Args, time2="obs_end")
test_that("Check Time, Age and sero events", {
  expect_equal(sum(adat$Time), 15444914)
  expect_equal(sum(adat$sero_event), 889)
  expect_equal(sum(adat$Age), 1332425)
})


context("Test setData (25-29 years)")
Args <- setArgs(nSim=1, Years=c(2005:2018), Age=list(Mal=c(25, 29)))
hiv   <- getHIV()
rtdat <- getRTData(hiv)
dat <- imputeMidPoint(rtdat)
edat <- splitAtSeroDate(dat )
adat <- setData(edat, Args, time2="obs_end")
test_that("Check Time, Age and sero events", {
  expect_equal(sum(adat$Time), 2182634)
  expect_equal(sum(adat$sero_event), 241)
  expect_equal(sum(adat$Age), 182523)
})


context("Test getIncData")
Args <- setArgs(Years=c(2004:2017), imputeMethod=imputeMidPoint)
hiv <- getHIV()
rtdat <- getRTData(hiv)
idat <- getIncData(rtdat, bdat=getBirthDate(), Args)
test_that("Check Time, Age and sero events", {
  expect_equal(sum(idat$Time), 41053711)
  expect_equal(sum(idat$sero_event), 3739)
  expect_equal(sum(idat$Age),  3769591)
})

context("Test AggFunc")
agedat <- AggByAge(idat)
yeardat <- AggByYear(idat)
test_that("Check seroevent and pyears add up", {
  expect_equal(sum(agedat$sero_event), sum(idat$sero_event))
  expect_equal(round(sum(agedat$pyears)), 112399)
  expect_equal(sum(yeardat$sero_event), sum(idat$sero_event))
  expect_equal(round(sum(yeardat$pyears)), 112399)
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
  expect_equal(sum(inc$sero_event), 2691)
  expect_equal(round(sum(inc$pyears)), 69288)
})


context("Test getIncidence (random-point)")
Args <- setArgs(nSim=2)
set.seed(123456)
inc <- getIncidence(Args)
test_that("Count sero events and ptime", {
  expect_equal(sum(inc$agg$sero), 3574)
  expect_equal(round(sum(inc$agg$pyears)), 109593)
})


# context("Get HIV Eligible")
# dat <- getHIVEligible()
# dat <- filter(dat, Year %in% c(2005:2017)) 
# test_that("Sum vars", {
#   expect_equal(alainr::dx(dat$IIntID)[2], 79264)
# })

# context("Test getHIVCumTest")
# Args <- setArgs(Year = c(2005:2017))
# # debugonce(getHIVCumTest)
# adat <- getHIVCumTest(Args)
# test_that("Sum vars", {
#   expect_equal(sum(adat$EverTest), 141909)
# })

