## Description: Test functions
## Project: ahri
## Author: AV / Created: 06Mar2020 
# testthat::test_file('inst/tests/testthat/test_ahri_set_2020.R')

edat0 <- readEpisodes(dropTasP=TRUE, write_rda=FALSE)
hiv0 <- readHIVData(dropTasP=TRUE, write_rda=FALSE)


context("Test setHIV with dropTasP")
Args = setArgs(Years=c(2000:2020), Age = list(All=c(0, 150)))
hiv <- setHIV(Args, dat=hiv0)
test_that("Check getHIV dropTasP N", {
  expect_equal(length(unique(hiv$IIntID)), 55256) 
})


context("Test setEpisodes with dropTasP")
Args = setArgs(Years=c(2000:2020), Age = list(All=c(0, 150)))
ydat <- setEpisodes(Args, dat=edat0)
test_that("Check readEpisodes dropTasP N", {
  expect_equal(length(unique(ydat$IIntID)), 178604) 
  expect_equal(length(unique(ydat$BSIntID)), 16856) 
  expect_equal(length(ydat$Female[ydat$Female==0]), 2809200) 
})


context("Test makeAgeVars")
Args <- setArgs(Years=c(2001:2018), Age=list(All=c(15, 100)))
hiv <- setHIV(Args, dat=hiv0)
hiv1 <- makeAgeVars(hiv)
hiv2 <- makeAgeVars(hiv, age_cut=Args$AgeCat)
hiv3 <- makeAgeVars(hiv, age_cut=Args$AgeCat, time2="VisitDate")
test_that("Check makeAgeVars Age", {
  expect_equal(sum(hiv1$Age), 5822397) 
  expect_equal(sum(hiv2$Age), 5822397) 
  expect_equal(sum(hiv3$Age, na.rm=TRUE),  5799749) 
})

context("Test makeAgeVars new Args defaults")
Args <- setArgs(Years=c(2000:2025), Age=list(All=c(0, 100)))
hiv <- setHIV(Args, dat=hiv0)
hiv1 <- makeAgeVars(hiv)
hiv2 <- makeAgeVars(hiv, age_cut=Args$AgeCat)
hiv3 <- makeAgeVars(hiv, age_cut=Args$AgeCat, time2="VisitDate")
test_that("Check makeAgeVars Age", {
  expect_equal(sum(hiv1$Age), 6116966) 
  expect_equal(sum(hiv2$Age), 6116966) 
  expect_equal(sum(hiv3$Age, na.rm=TRUE), 6092308) 
})

context("Test setData")
Args <- setArgs(Years=c(2001:2018), Age=list(All=c(15, 100)))
hiv <- setHIV(Args, dat=hiv0)
hiv1 <- setData(hiv, Args)
hiv2 <- setData(hiv, Args, time2="VisitDate")
Args <- setArgs(Years=c(2005:2018), Age=list(All=c(15, 54)))
hiv3 <- setData(hiv, Args, time2="VisitDate")
test_that("Check setData Age", {
  expect_equal(sum(hiv1$Age), 5822397) 
  expect_equal(sum(hiv2$Age), 5799679) 
  expect_equal(sum(hiv3$Age), 3356566) 
})


context("Test setHIV")
Args <- setArgs(Years=c(2005:2017), Age=list(All=c(15, 54)))
hiv <- setHIV(Args, dat=hiv0)
test_that("Check setHIV N", {
  expect_equal(length(unique(hiv$IIntID)), 41608) 
  expect_equal(sum(hiv$Age),  3078595) 
})


context("Test setEpisodes")
Args <- setArgs(Years=c(2005:2018), Age=list(All=c(15, 54)))
ydat <- setEpisodes(Args, dat=edat0)
test_that("Check setEpisodes N", {
  expect_equal(length(unique(ydat$IIntID)), 97220)
  expect_equal(nrow(ydat), 2635649)
})

context("Test new Args defaults")
Args <- setArgs(Years=c(2000:2025), Age=list(All=c(0, 110)))
hiv <- setHIV(Args, dat=hiv0)
epi <- setEpisodes(Args, dat=edat0)
test_that("Check new Args Age", {
  expect_equal(sum(hiv$Age), 6116966) 
  expect_equal(sum(epi$Age), 146273028) 
})



