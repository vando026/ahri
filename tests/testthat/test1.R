## Description: Test incidence code
## Project: 
## Author: AV / Created: 28Sep2018 

context("Test getBSData")
bdat <- getBSData()
test_that("Check vars", {
  expect_equal(length(unique(bdat$BSIntID)), 30776)
  expect_equal(sum(bdat$PIPSA[bdat$PIPSA==1], na.rm=TRUE), 18058)
})

context("Test getHIVData")
hdat1 <- readHIVData(dropTasP=FALSE)
hdat2 <- readHIVData(dropTasP=TRUE)
test_that("Check vars", {
  expect_equal(length(unique(hdat1$IIntID)), 63004)
  expect_equal(length(unique(hdat2$IIntID)), 53563)
})

context("Test getHIV with dropTasP")
hiv <- getHIV()
test_that("Check N", {
  expect_equal(length(unique(hiv$IIntID)), 53563) 
})


context("Test readEpisodes with dropTasP")
edat <- readEpisodes(dropTasP=TRUE)
test_that("Check N", {
  expect_equal(length(unique(edat$IIntID)), 174445) 
  expect_equal(length(unique(edat$BSIntID)), 16440) 
  expect_equal(length(edat$Female[edat$Female==0]), 1911066) 
})


context("Test makeAgeVars")
Args <- setArgs(Years=c(2001:2018), Age=list(All=c(15, 100)))
hiv <- getHIV()
hiv1 <- makeAgeVars(hiv)
hiv2 <- makeAgeVars(hiv, age_cut=Args$AgeCat)
hiv3 <- makeAgeVars(hiv, age_cut=Args$AgeCat, time2="VisitDate")
test_that("Check N", {
  expect_equal(sum(hiv1$Age), 5769477) 
  expect_equal(sum(hiv2$Age), 5769477) 
  expect_equal(sum(hiv3$Age, na.rm=TRUE), 5755001) 
})


context("Test setData")
Args <- setArgs(Years=c(2001:2018), Age=list(All=c(15, 100)))
hiv <- getHIV()
hiv1 <- setData(hiv, Args)
hiv2 <- setData(hiv, Args, time2="VisitDate")
Args <- setArgs(Years=c(2005:2018), Age=list(All=c(15, 54)))
hiv3 <- setData(hiv, Args, time2="VisitDate")
test_that("Check N", {
  expect_equal(sum(hiv1$Age), 5769477) 
  expect_equal(sum(hiv2$Age), 5754931) 
  expect_equal(sum(hiv3$Age), 3332202) 
})


context("Test setHIV")
Args <- setArgs(Years=c(2005:2017))
hiv <- setHIV(Args)
test_that("Check N", {
  expect_equal(length(unique(hiv$IIntID)), 41609) 
  expect_equal(sum(hiv$Age), 3079186) 
})


context("Test setEpisodes")
Args <- setArgs(Years=c(2005:2018))
ydat <- setEpisodes(Args)
test_that("Check N", {
  expect_equal(length(unique(ydat$IIntID)), 97477)
  expect_equal(nrow(ydat), 1818654)
})


context("Test getWGH getMGH")
wgh <- getWGH()
mgh <- getMGH()
test_that("Check N", {
  expect_equal(length(unique(mgh$IIntID)), 37787)
  expect_equal(length(unique(wgh$IIntID)), 50563)
  expect_equal(sum(mgh$Age), 4732753)
  expect_equal(sum(wgh$Age), 8471355)
})




# context("Test RTDates")
# testDates <- function(dat=NULL) {
#   testNeg <- filter(dat, is.finite(obs_start) & is.finite(late_neg))
#   if(any(with(testNeg, obs_start > late_neg))) 
#       stop("Some obs_start > late_neg") 
#   testPos <- filter(dat, is.finite(early_pos) & is.finite(late_neg))
#   if(any(with(testPos, late_neg >= early_pos))) 
#       stop("Some late_neg >= early_pos") 
# }
# # debugonce(getRTData)
# rtdat <- getRTData(hiv)
# rtdat$obs_start[rtdat$IIntID==17]  <- as.Date("2020-01-01", origin="1970-01-01")
# test_that("Test wrong date", {
#   expect_error(testDates(rtdat))
# })


# context("Test splitEpisodes")
# Args <- setArgs(imputeMethod=imputeMidPoint)
# hiv <- getHIV()
# rtdat <- getRTData(hiv)
# dat <- Args$imputeMethod(rtdat)
# edat <- splitAtSeroDate(dat) 
# edat2 <- splitAtEarlyPos(dat)
# test_that("Check splitAtSeroDate", {
#   expect_equal(nrow(edat), 176649)
#   expect_equal(sum(as.numeric(edat$obs_end)), 2696155985)
# })
# test_that("Check splitAtEarlyPos", {
#   expect_equal(nrow(edat2), 184285)
#   expect_equal(sum(as.numeric(edat2$obs_end)), 2817291260)
# })

# context("Test splitEpisodes 2")
# Args <- setArgs(Years=c(2005:2017), imputeMethod=imputeMidPoint)
# hiv <- getHIV()
# rtdat <- getRTData(hiv)
# dat <- Args$imputeMethod(rtdat)
# edat <- splitAtEarlyPos(dat)
# edat <- setData(edat, Args, time2="obs_end")
# edat <- mutate(edat, Time = as.numeric(obs_end - as.Date("2005-01-01", origin="1970-01-01")))
# test_that("Check splitAtSeroDate", {
#   expect_equal(nrow(edat), 129955)
#   expect_equal(sum((edat$Time)), 316741021)
# })

# context("Test getIncData")
# Args <- setArgs(Years=c(2004:2017), imputeMethod=imputeMidPoint)
# hiv <- getHIV()
# rtdat <- getRTData(hiv)
# idat <- getIncData(rtdat, bdat=getBirthDate(), Args)
# test_that("Sum vars", {
#   expect_equal(sum(idat$Time), 40646590)
#   expect_equal(sum(idat$sero_event), 3710)
#   expect_equal(sum(idat$Age),  3740215)
# })

# context("Test AggFunc")
# agedat <- AggByAge(idat)
# yeardat <- AggByYear(idat)
# test_that("seroevent and pyears add up", {
#   expect_equal(sum(agedat$sero_event), sum(idat$sero_event))
#   expect_equal(round(sum(agedat$pyears)), 111284)
#   expect_equal(sum(yeardat$sero_event), sum(idat$sero_event))
#   expect_equal(round(sum(yeardat$pyears)), 111284)
# })

# context("Test getCrudeRate against AggByYear")
# test_that("Sum of pyears", {
#   expect_equal(round(sum(idat$Time)/365.25), round(sum(yeardat$pyears)))
#   expect_equal((sum(idat$sero_event)), (sum(yeardat$sero_event)))
# })

# context("Test calcInc")
# Args <- setArgs(Years=c(2004:2017), imputeMethod=imputeMidPoint)
# hiv <- getHIV()
# rtdat <- getRTData(hiv)
# calcInc <- setInc(rtdat, Args)
# dat <- parallel::mclapply(seq(1), calcInc, mc.cores=1)
# sero <- dat[[1]][["year"]]
# test_that("seroevent and pyears add up", {
#   expect_equal(sum(sero$sero_event), 3710)
#   expect_equal(round(sum(sero$pyears)), 111284)
# })

# context("Test calcRubin")
# e <- c(2, 4)
# s <- c(0.5, 0.5)
# x <- calcRubin(e, s)
# test_that("Estimate, standard error, df", {
#   expect_equivalent(x[1], exp(3))
#   # expect_equivalent(x[2], sqrt(3.25))
#   # expect_equivalent(round(x[4], 4), 19.2649)
# })

# context("Test getIncidence (mid-point)")
# Args <- setArgs(nSim=1, imputeMethod=imputeMidPoint)
# inc <- getIncidence(Args)
# test_that("Count sero events and ptime", {
#   expect_equal(sum(inc$agg$sero), 3710)
#   expect_equal(round(sum(inc$agg$pyears)), 111284)
# })


# context("Test getIncidence (random-point)")
# Args <- setArgs(nSim=2)
# set.seed(123456)
# inc <- getIncidence(Args)
# test_that("Count sero events and ptime", {
#   expect_equal(sum(inc$agg$sero), 3664)
#   expect_equal(round(sum(inc$agg$pyears)), 111320)
# })


# context("Test getIncidence (random-point) 2018")
# Args <- setArgs(Year=c(2005:2018), nSim=2)
# set.seed(123456)
# inc <- getIncidence(Args)
# test_that("Count sero events and ptime", {
#   expect_equal(sum(inc$agg$sero), 3664)
#   expect_equal(round(sum(inc$agg$pyears)), 111320)
# })


# context("Test setData 1")
# Args <- setArgs(nSim=1, Years=c(2005:2017), Age=list(Mal=c(15, 54)))
# hiv   <- getHIV()
# rtdat <- getRTData(hiv)
# dat <- imputeMidPoint(rtdat)
# edat <- splitAtSeroDate(dat )
# adat <- setData(edat, Args, time2="obs_end")
# test_that("Sum vars", {
#   expect_equal(sum(adat$Time), 14925036)
#   expect_equal(sum(adat$sero_event), 878)
#   expect_equal(sum(adat$Age), 1275122)
# })


# context("Test setData 2")
# Args <- setArgs(nSim=1, Years=c(2005:2017), Age=list(Mal=c(25, 29)))
# hiv   <- getHIV()
# rtdat <- getRTData(hiv)
# dat <- imputeMidPoint(rtdat)
# edat <- splitAtSeroDate(dat )
# adat <- setData(edat, Args, time2="obs_end")
# test_that("Sum vars", {
#   expect_equal(sum(adat$Time), 2106328)
#   expect_equal(sum(adat$sero_event), 240)
#   expect_equal(sum(adat$Age), 174324)
# })

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

# context("Test getIncidenceMI")
# set.seed(1235)
# Args <- setArgs(Year=c(2005:2017), nSim=2) 
# incmi <- getIncidenceMI(Args)
# test_that("Sum sero and pyears", {
#   expect_equal(floor(sum(incmi$agg$sero_event)), 3493)
#   expect_equal(round(sum(incmi$agg$pyears)), 105950)
# })



# context("Test getIncidenceMI 2018")
# set.seed(1235)
# Args <- setArgs(Year=c(2005:2018), nSim=2) 
# incmi <- getIncidenceMI(Args)
# test_that("Sum sero and pyears", {
#   expect_equal(sum(incmi$agg$sero_event), 3540)
#   expect_equal(round(sum(incmi$agg$pyears)), 108275)
# })


# Args <- setArgs(nSim=300, mcores=8)
# formulas <- list(
#   crude = "sero_event ~ -1 + Year + offset(log(tscale))",
#   adj = "sero_event ~ -1 + Year + Age + Year:Age + offset(log(tscale))")
# getIncidenceMI(Args, formulas)

# Args <- setArgs(nSim=2, mcores=1)
# hiv <- getHIV()
# rtdat <- getRTData(hiv)
# age_dat <- getAgeYear(Args)
# xx <- parallel::mclapply(seq(Args$nSim), function(i) {
#   cat(i, ""); MIdata(rtdat, Args)},
#   mc.cores=Args$mcores)
# MIdata(rtdat, Args)
