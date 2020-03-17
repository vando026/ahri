## Description: Test functions
## Project: ahri
## Author: AV / Created: 06Mar2020 

edat0 <- readEpisodes(dropTasP=TRUE, write_rda=FALSE)
hiv0 <- readHIVData(dropTasP=TRUE, write_rda=FALSE)


context("Test setHIV with dropTasP")
Args = setArgs(Years=c(2000:2020), Age = list(All=c(0, 150)))
hiv <- setHIV(Args, dat=hiv0)
test_that("Check getHIV dropTasP N", {
  expect_equal(length(unique(hiv$IIntID)), 53800) 
})


context("Test setEpisodes with dropTasP")
Args = setArgs(Years=c(2000:2020), Age = list(All=c(0, 150)))
ydat <- setEpisodes(Args, dat=edat0)
test_that("Check readEpisodes dropTasP N", {
  expect_equal(length(unique(ydat$IIntID)), 174445) 
  expect_equal(length(unique(ydat$BSIntID)), 16440) 
  expect_equal(length(ydat$Female[ydat$Female==0]), 2645718) 
})


context("Test makeAgeVars")
Args <- setArgs(Years=c(2001:2018), Age=list(All=c(15, 100)))
hiv <- setHIV(Args, dat=hiv0)
hiv1 <- makeAgeVars(hiv)
hiv2 <- makeAgeVars(hiv, age_cut=Args$AgeCat)
hiv3 <- makeAgeVars(hiv, age_cut=Args$AgeCat, time2="VisitDate")
test_that("Check makeAgeVars Age", {
  expect_equal(sum(hiv1$Age), 5822611) 
  expect_equal(sum(hiv2$Age), 5822611) 
  expect_equal(sum(hiv3$Age, na.rm=TRUE), 5807805) 
})


context("Test setData")
Args <- setArgs(Years=c(2001:2018), Age=list(All=c(15, 100)))
hiv <- setHIV(Args, dat=hiv0)
hiv1 <- setData(hiv, Args)
hiv2 <- setData(hiv, Args, time2="VisitDate")
Args <- setArgs(Years=c(2005:2018), Age=list(All=c(15, 54)))
hiv3 <- setData(hiv, Args, time2="VisitDate")
test_that("Check setData Age", {
  expect_equal(sum(hiv1$Age), 5822611) 
  expect_equal(sum(hiv2$Age), 5807721) 
  expect_equal(sum(hiv3$Age), 3361665) 
})


context("Test setHIV")
Args <- setArgs(Years=c(2005:2017))
hiv <- setHIV(Args, dat=hiv0)
test_that("Check setHIV N", {
  expect_equal(length(unique(hiv$IIntID)), 41609) 
  expect_equal(sum(hiv$Age), 3079186) 
})


context("Test setEpisodes")
Args <- setArgs(Years=c(2005:2018))
ydat <- setEpisodes(Args, dat=edat0)
test_that("Check setEpisodes N", {
  expect_equal(length(unique(ydat$IIntID)), 97477)
  expect_equal(nrow(ydat), 2613221)
})


