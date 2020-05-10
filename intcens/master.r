## Description: master.r
## Project: IncidenceMethod
## Author: AV / Created: 5May2020

rm(list=ls())

###############################################################################################
######################################## Packages #############################################
###############################################################################################
library(tidyverse)
library(ahri)
# library(alainr)

###############################################################################################
######################################## Paths ################################################
###############################################################################################
home <- file.path(Sys.getenv("HOME"), "Seafile/Programs/R/ahri_home")
getFiles <- ahri::setFiles('~/AHRI_Data/2019')

Args <- setArgs(
  Years=c(2005:2018), nSim=50,
  Age=list(Mal=c(15, 54)))


set.seed(100500)
hiv <- setHIV(Args)
###
keepid <- unique(hiv$IIntID)[1:2000]
hiv <- hiv[hiv$IIntID %in% keepid, ]
##
rtdat <- getRTData(hiv)
# rtdat$IIntID <- base::sample(nrow(rtdat), nrow(rtdat), replace=FALSE)
dat <- splitAtEarlyPos(rtdat)
# dat <- makeAgeVars(dat, time2="obs_end",
    # age_cut=Args$AgeCat, birthdate=getBirthDate())
# dat <- setAge(dat, Args)
dat <- setData(dat, Args, time2="obs_end")
# dat <- filter(dat, obs_start >= late_neg)
dat <- mutate(dat, 
  Age0 = round(Age - mean(Age), 1),
  Age2 = round(Age0^2, 1))
dat <- addCircumVar(dat)
dat <- group_by(dat, IIntID) %>% 
  mutate(Time = as.integer(obs_end - min(obs_start)))
dat <- mutate(dat, Time = round(Time/30.44))


# Keep only what is needed
idat <- select(dat, IIntID, Time, sero_event, Age0, Age2, EverCircum)
write.table(idat, file=file.path(home, "intcens/input_data.txt"),
  row.names=FALSE, quote=FALSE)
saveRDS(rtdat, file.path(home, "rtdat.Rda"))

uniReg(
  InFile = file.path(home, "intcens/input_data.txt"),
  OutFile = file.path(home, "intcens/res_dat.txt"),
  Model = "(Time, sero_event) = Age0 + Age2 + EverCircum",
  ID="IIntID", printout=TRUE, ign_stout=FALSE, cthresh=0.01
)

ic_res <- readUniRegResults(
  File=file.path(home, "intcens/res_dat.txt"))

gdates <- gImpute(dat, ic_res, nSim=Args$nSim, tscale=30.44)

doGImpute_ <- function(rtdat, gdates, bdat) {
  function(i) {
    gdat <- getGImpute(rtdat, gdates, i)
    gdat <- splitAtSeroDate(gdat)
    gdat <- setData(gdat, Args, time2="obs_end", birthdate=bdat)
    gdat <- mutate(gdat, Year = as.factor(Year), tscale = Time/365.25)
    gdat
  }
}

# rtdat <- readRDS(file.path(home, "rtdat.Rda"))
doGImpute <- doGImpute_(rtdat, gdates, bdat=getBirthDate())
gdat <- lapply(seq(Args$nSim), doGImpute)

# gdat1 <- lapply(gdat, AggByYear)
# gres1 <- MIaggregate(gdat1)
# crude <- calcPoisExact(gdat, byVar="Year")


mdat <- mitools::imputationList(gdat)
sformula = "sero_event ~ -1 + Year + offset(log(tscale))"
mods <- with(mdat, glm(as.formula(sformula), family=poisson))
mres <- mitools::MIcombine(mods)
pdat <- data.frame(Year = as.factor(Args$Years), tscale=rep(1, length(Args$Years)))
debugonce(MIpredict)
MIpredict(mres, mods[[1]], pdat)


######################################################################
######################### Diff Time ##################################
######################################################################

dat1 <- group_by(dat, IIntID) %>% 
  mutate(Time = round(Time/(365.25/12)))

# Keep only what is needed
idat1 <- select(dat1, IIntID, Time, sero_event, Age0, Age2, EverCircum)

write.table(idat1, file=file.path(home, "intcens/input_data2.txt"),
  row.names=FALSE, quote=FALSE)

uniReg(
  InFile = file.path(home, "intcens/input_data2.txt"),
  OutFile = file.path(home, "intcens/res_dat2.txt"),
  Model = "(Time, sero_event) = Age0 + Age2 + EverCircum",
  ID="IIntID", printout=TRUE, ign_stout=FALSE, cthresh=0.01
)

ic_res <- readUniRegResults(
  File=file.path(home, "intcens/res_dat2.txt"))

gdates <- gImpute(dat1, ic_res, Args, tscale=30.44)

gdat <- lapply(seq(Args$nSim), function(i)
  splitAtSeroDate(getGImpute(rtdat, gdates, i))) 
gdat <- lapply(gdat, AggByYear)
gres2 <- MIaggregate(gdat)



