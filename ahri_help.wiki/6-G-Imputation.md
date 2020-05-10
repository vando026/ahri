-   [Step 1](#step-1)
    -   [The unireg function](#the-unireg-function)
    -   [The readUniRegResults
        function](#the-readuniregresults-function)
-   [Step 2](#step-2)
    -   [The gImpute function](#the-gimpute-function)
    -   [The getGImpute function](#the-getgimpute-function)
    -   [Calculating incidence](#calculating-incidence)
    -   [Standard errors using Rubin’s
        rules](#standard-errors-using-rubins-rules)
-   [gImpute: Changing the time scale](#gimpute-changing-the-time-scale)
-   [gImpute and categorical
    variables](#gimpute-and-categorical-variables)

This documentation focuses on the G-imputation method for incidence rate
estimation. Briefly, G-imputation uses auxiliary data to infer the
timing of the HIV seroconversion date. This approach is an improvement
over single random-point imputation, which assumes that the
seroconversion date is uniformly distributed over the censored interval.
The G-imputation method is described in detail here:
<a href="https://journals.sagepub.com/doi/full/10.1177/0962280219829892" class="uri">https://journals.sagepub.com/doi/full/10.1177/0962280219829892</a>

The G-imputation method has two steps. In step 1, a G-transformation
model is used to estimate the effects of covariates on the interval
censored infection times. A baseline cumulative hazard function is also
estimated. In step 2, a unique cumulative distribution function is
calculated for each participant and infection times are multiply imputed
conditional on the paricipant’s covariate values.

Step 1
======

The unireg function
-------------------

The first step of G-imputation leverages a semi-parametric
G-transformation model described by Zeng et al., which can be found
here:
<a href="https://academic.oup.com/biomet/article/103/2/253/1744298" class="uri">https://academic.oup.com/biomet/article/103/2/253/1744298</a>.
Additional information can be found on their website:
<a href="http://dlin.web.unc.edu/software/intcens/" class="uri">http://dlin.web.unc.edu/software/intcens/</a>,
which includes the `unireg` executable to run the G-transformation
model, along with the documentation.

The `unireg` executable ships with the `ahri` package and can be found
on your system using:

``` r
system.file("intcens", "unireg.exe", package = "ahri")
```

To demonstrate the G-transformation model, I use several `ahri`
functions to prepare the data. The code below creates a mean centered
age variable (`Age0`), Age0 squared (`Age2`), and an ever circumcised
variable (`EverCircum`), which are time-varying.

``` r
library(ahri)

# Set file paths
home <- file.path(Sys.getenv("HOME"), "Seafile/Programs/R/ahri_home")
getFiles <- setFiles('~/AHRI_Data/2019')

# Set the global arguments
Args <- setArgs(
  Years=c(2005:2018), nSim=50,
  Age=list(Mal=c(15, 54)))

# Make the datasets and covariates
hiv <- setHIV(Args)
# Use only first 2000 participants for this example
keepid <- unique(hiv$IIntID)[1:2000]
hiv <- hiv[hiv$IIntID %in% keepid, ]
# Get repeat-testers
rtdat <- getRTData(hiv)
# Make the data time-varying 
gdat <- splitAtEarlyPos(rtdat)
gdat <- setData(gdat, Args, time2="obs_end")
# Make covariates
gdat <- mutate(gdat, 
  Age0 = round(Age - mean(Age), 1),
  Age2 = round(Age0^2, 1))
gdat <- addCircumVar(gdat)
# Make the time variable
gdat <- group_by(gdat, IIntID) %>% 
  mutate(Time = as.integer(obs_end - min(obs_start)))
select(gdat, -c(Age, AgeCat, Year, Time))
# A tibble: 6,042 x 10
# Groups:   IIntID [1,022]
   IIntID Female late_neg   early_pos  obs_start  obs_end    sero_event  Age0  Age2 EverCircum
    <int>  <int> <date>     <date>     <date>     <date>          <dbl> <dbl> <dbl>      <dbl>
 1     28      0 2006-10-06 NA         2005-10-19 2006-01-01          0   9.4  88.4          0
 2     28      0 2006-10-06 NA         2006-01-01 2006-10-06          0  10.4 108.           0
 3     29      0 2013-03-22 NA         2011-06-02 2012-01-01          0  -9.6  92.2          0
 4     29      0 2013-03-22 NA         2012-01-01 2013-01-01          0  -8.6  74            0
 5     29      0 2013-03-22 NA         2013-01-01 2013-03-22          0  -8.6  74            0
 6     30      0 2011-05-21 NA         2010-05-17 2011-01-01          0  -2.6   6.8          0
 7     30      0 2011-05-21 NA         2011-01-01 2011-05-21          0  -2.6   6.8          0
 8     36      0 2013-03-23 NA         2011-05-21 2012-01-01          0  -9.6  92.2          0
 9     36      0 2013-03-23 NA         2012-01-01 2013-01-01          0  -8.6  74            0
10     36      0 2013-03-23 NA         2013-01-01 2013-03-23          0  -8.6  74            0
# ... with 6,032 more rows
```

The `unireg` executable takes a .txt file as input, so we must convert
the .Rdata file to .txt.

``` r
# Select only needed variables
idat <- select(gdat, IIntID, Time, sero_event, Age0, Age2, EverCircum)
write.table(idat, file=file.path(home, "intcens/input_data.txt"),
  row.names=FALSE, quote=FALSE)
saveRDS(rtdat, file.path(home, "intcens/rtdat.Rda"))
```

If you do not have access to the AHRI datasets, you can access
truncated, ficticious versions of the `gdat` and `idat` datasets on the
main Github page:
<a href="https://github.com/vando026/ahri" class="uri">https://github.com/vando026/ahri</a>.

We can now run the G-transformation model. The `ahri` library provides a
convenient wrapper around the `unireg` executable, so that you can call
it from the R console. The wrapper is called `uniReg` and includes
various argument options.

``` r
uniReg
function(InFile, OutFile, Model, ID=NULL, inf="Inf",
  iter=5000, cthresh=0.0001, r=1.0, printout=FALSE, ign_stout=TRUE) {
    InFile <- paste("--in", InFile)
    OutFile <- paste("--out", OutFile)
    Model <- paste("--model", shQuote(Model))
    ID <- ifelse(is.null(ID), "", paste("--subject_id", ID))
    Sep <- paste("--sep", shQuote(" "))
    inf <- paste("--inf_char", inf)
    R <- paste("--r", r)
    iter <- paste("--max_itr", iter)
    cthresh <- paste("--convergence_threshold", cthresh)
    xpath <- system.file("intcens", "unireg.exe", package = "ahri")
    if (Sys.getenv("OS") == "Windows_NT") {
      system(command=paste(xpath, InFile, OutFile, Model, 
        ID, Sep, iter, R, inf, cthresh, collapse=" "),
        show.output.on.console=printout)
    } else {
      system(command=paste(xpath, InFile, OutFile, Model, 
        ID, Sep, iter, R, inf, cthresh, collapse=" "),
        ignore.stdout=ign_stout)
    }
}
<environment: namespace:ahri>
```

The code below shows how to run the model. It takes a .txt file for
input, as discussed, and a string for the time, event, and covariate
variables. The `IIntID` ,`Time` and `sero_event` names are compulsory to
ensure integration with other `ahri` functions.

``` r
uniReg(
  InFile = file.path(home, "intcens/input_data.txt"),
  OutFile = file.path(home, "intcens/res_dat.txt"),
  Model = "(Time, sero_event) = Age0 + Age2 + EverCircum",
  ID="IIntID", printout=TRUE, ign_stout=FALSE, cthresh=0.01
)
[1] 0
```

The readUniRegResults function
------------------------------

The `unireg` will produce a .txt output file with the name given to the
`OutFile` argument. We can read this .txt file using the `ahri` function
`readUniRegResults`. The output file includes the cumulative baseline
hazard function (CHF), the covariance matrix, and the coefficients
estimates, as shown below.

``` r
ic_res <- readUniRegResults(
  File=file.path(home, "intcens/res_dat.txt"))
# The CHF
ic_res$sdat[1:10, ]
   Time     Estimate
1   123 0.000000e+00
2   148 2.854275e-18
3   160 6.053202e-12
4   178 4.129545e-08
5   194 8.336065e-08
6   214 1.214390e-07
7   229 1.585801e-07
8   242 1.935939e-07
9   244 2.286314e-07
10  253 2.636784e-07
# The coefficient estimates
ic_res$edat
   Covariate     Estimate   Std_Error     Z-Stat      P-value
1       Age0  0.076858365 0.025263398  3.0422813 0.0023479234
2       Age2 -0.004908128 0.001451133 -3.3822741 0.0007188838
3 EverCircum  0.078147437 0.350729680  0.2228139 0.8236803728
# The covariance matrix
ic_res$cdat
                    Age0          Age2    EverCircum
Age0        0.0006382393 -3.144440e-05  9.235326e-04
Age2       -0.0000314444  2.105786e-06 -4.150468e-05
EverCircum  0.0009235326 -4.150468e-05  1.230113e-01
```

Note that the covariates were estimated with interval censored data, or
in other words, without needing to impute an infection event for each
positive repeat-tester. If we were only interested in estimating the
effect of these covariates on the interval censored infection times,
then we would stop here. However, if we wish to impute the infection
times then an additional step is needed.

Step 2
======

The gImpute function
--------------------

We have developed the G-imputation method to impute infection times
conditional on auxiliary data. To do this, we use the `gImpute`
function, which takes as arguments the `gdat` dataset and the `ic_res`
results from `readUniRegResults` (see the above code). We asked for 50
imputed infection times per repeat-tester, thus we should get a dataset
with the ID, latest-negative and earliest-positive dates and 50 columns
of infection dates.

``` r
# Uncomment line below to get the R dataset
# rtdat <- readRDS(file.path(home, "rtdat.Rda"))
gdates <- gImpute(gdat, ic_res, nSim=Args$nSim)
gdates[1:5, 1:10]
  IIntID start_date obs_start late_neg early_pos   g1   g2   g3   g4   g5
1    456      13067     13067     2166      3064 2999 2871 2446 2180 2999
2   1267      16641     16641        0       348  325  328  335  326  324
3   1290      17110     17110        0       531  447  440  324  331  332
4   1356      12914     12914        0      2184  697  710 1413 1812 1191
5   1432      13312     13312        0      2726  472  709 1785 2642 1819
```

Note that the *i* th column of infection times are prefixed by the
letter `g` and represents the number of days since the `start_date`.
Thus the infection date is `start_date + gi`. For each participant, the
*gi* th value should be between `late_neg` and `early_pos`, which is the
censoring interval.

The getGImpute function
-----------------------

The next step is to merge these results back into the repeat-testers
dataset (`rtdat`). Thus we should have 50 new datasets, each with the
*i* th column of imputed infections. We can use the `getGImpute`
function to achieve this task, which takes the repeat-testers (`rtdat`)
dataset, imputed dates (`gdates`), and a value for *i*. The code below
shows how to add the 1st column of `getGImpute` to `rtdat`. (If you are
using your own repeat-tester dataset, it nust contain the variables
`IIntID`, `obs_start`, `late_neg` and `early_pos`.)

``` r
# Uncomment the line below if you are using this example dataset
# rtdat <- readRDS(file.path(home, "rtdat.Rda"))
# Combine rtdat with the 1st iteration
gdat_ex <- getGImpute(rtdat, gdates, 1)
gdat_ex[gdat_ex$sero_event==1, ]
# A tibble: 93 x 7
   IIntID Female obs_start  late_neg   early_pos  sero_event sero_date 
    <dbl>  <int> <date>     <date>     <date>          <dbl> <date>    
 1    456      0 2005-10-11 2011-09-16 2014-03-02          1 2013-12-27
 2   1267      0 2015-07-25 2015-07-25 2016-07-07          1 2016-06-14
 3   1290      0 2016-11-05 2016-11-05 2018-04-20          1 2018-01-26
 4   1356      0 2005-05-11 2005-05-11 2011-05-04          1 2007-04-08
 5   1432      0 2006-06-13 2006-06-13 2013-11-29          1 2007-09-28
 6   1445      0 2005-04-20 2007-07-12 2009-03-22          1 2008-07-24
 7   1566      0 2006-08-21 2010-02-03 2013-06-27          1 2013-01-01
 8   1668      0 2005-11-22 2005-11-22 2008-06-24          1 2008-01-22
 9   2113      0 2006-07-27 2009-03-26 2012-07-21          1 2009-10-30
10   2417      0 2005-03-02 2011-01-24 2013-04-20          1 2012-05-26
# ... with 83 more rows
```

For all imputed datasets, we then right censor at the imputed infection
times or at the latest-negative date if the repeat-tester did not
seroconvert during the observation period.

``` r
gdat50 <- lapply(seq(Args$nSim), function(i)
  splitAtSeroDate(getGImpute(rtdat, gdates, i))) 
```

Calculating incidence
---------------------

We can now use the function described in earlier documentation to
aggregate the seroconversion events and person-years.

``` r
inc <- lapply(gdat50, AggByYear)
MIaggregate(inc)
   Year sero_event    pyears
1  2005       0.50 174.00772
2  2006       5.76 381.13719
3  2007       8.94 447.93763
4  2008       7.78 482.45120
5  2009       8.98 458.84342
6  2010       7.84 443.40545
7  2011       7.14 426.11411
8  2012       8.30 383.16249
9  2013      12.00 369.09914
10 2014       8.70 358.46204
11 2015       5.44 352.17265
12 2016       5.28 287.12712
13 2017       4.96 209.42248
14 2018       1.38  91.83474
```

Standard errors using Rubin’s rules
-----------------------------------

To estimate the standard errors, we need to account for the variation in
the point estimates and the variation across the simulations. Working
from the `gdat50` dataset, we can use the `MIpredict` function to get
the appropriate standard errors. First, we have to set the time
variables.

``` r
setTime <- function(dat) {
  mutate(dat, Year = as.factor(Year), 
  tscale = as.numeric(Time/365.25))
}
gdat50 <- lapply(gdat50, setTime)
```

Then we can run Poisson regression models on each dataset.

``` r
mdat <- mitools::imputationList(gdat50)
sformula = "sero_event ~ -1 + Year + offset(log(tscale))"
mods <- with(mdat, glm(as.formula(sformula), family=poisson))
mres <- mitools::MIcombine(mods)
mres
Multiple imputation results:
      with(mdat, glm(as.formula(sformula), family = poisson))
      MIcombine.default(mods)
            results          se
Year2005 -13.055335 355.3617291
Year2006  -4.217199   0.4869455
Year2007  -3.944863   0.4337534
Year2008  -4.168415   0.4797985
Year2009  -3.985673   0.5072750
Year2010  -4.061101   0.4365059
Year2011  -4.151285   0.5456946
Year2012  -3.880357   0.4867997
Year2013  -3.450688   0.3751399
Year2014  -3.765891   0.4874439
Year2015  -4.231651   0.5876837
Year2016  -4.062127   0.6000846
Year2017  -3.841189   0.6974967
Year2018  -5.606292 191.7898075
```

The results show the estimates and standard errors in the untransformed
scale. To transform to a rate per 100 person-years, we have to
exponentiate and make a new predictor dataset.

``` r
pdat <- data.frame(Year = as.factor(Args$Years), tscale=rep(1, length(Args$Years)))
pdat
   Year tscale
1  2005      1
2  2006      1
3  2007      1
4  2008      1
5  2009      1
6  2010      1
7  2011      1
8  2012      1
9  2013      1
10 2014      1
11 2015      1
12 2016      1
13 2017      1
14 2018      1
```

which we can pass to `MIpredict`.

``` r
MIpredict(mods, newdata=pdat)
              fit      se.fit           lci         uci
2005 0.0002138651  0.07599948   -0.14874238   0.1491701
2006 1.4739866694  0.71775124    0.06722008   2.8807533
2007 1.9353868797  0.83948069    0.29003496   3.5807388
2008 1.5476777666  0.74257346    0.09226052   3.0030950
2009 1.8579928382  0.94251337    0.01070058   3.7052851
2010 1.7230030987  0.75210102    0.24891218   3.1970940
2011 1.5744176312  0.85915127   -0.10948792   3.2583232
2012 2.0643449342  1.00492251    0.09473301   4.0339569
2013 3.1723815451  1.19008697    0.83985394   5.5049091
2014 2.3146968514  1.12828488    0.10329912   4.5260946
2015 1.4528382207  0.85380936   -0.22059738   3.1262738
2016 1.7212371927  1.03288796   -0.30318602   3.7456604
2017 2.1468050815  1.49738944   -0.78802429   5.0816345
2018 0.3674669193 70.47640969 -137.76375784 138.4986917
```

Obviously, these estimates are nonsensical because we selected only 1500
participants for this example.

gImpute: Changing the time scale
================================

For large datasets and many unique infection events, the
G-transformation model (`unireg`) can be computationally intensive. We
can speed up performance by changing the time scale. The default setting
of `gImpute` is to impute an infection time in days. But when in days,
there are almost 2000 unique jump points (in the cumulative baseline
hazard function). One option is to change the time scale from days to
months, or any other period, such as weeks, etc. This will reduce the
number of jump points at the cost of precision. In this case, we would
infer the month in which the serocoversion occurs, rather than the day.
With respect to annual HIV incidence rate estimation, the trade-off
between speed and precision would be neglible, since the aggregation is
taking place at the year level. (However, setting the time scale to a
year or even bi-annually would defeat the purpose of G-imputation for
this particular application.)

Let’s transition to a month time scale using the following approach.
First, we construct a new `Time` variable for `gdat`.

``` r
# Convert to month
gdat_mnth <- group_by(gdat, IIntID) %>% mutate(Time = round(Time/30.44))
select(gdat_mnth, IIntID, obs_start, obs_end, Time, Year, sero_event)
# A tibble: 6,042 x 6
# Groups:   IIntID [1,022]
   IIntID obs_start  obs_end     Time  Year sero_event
    <int> <date>     <date>     <dbl> <int>      <dbl>
 1     28 2005-10-19 2006-01-01     2  2005          0
 2     28 2006-01-01 2006-10-06    12  2006          0
 3     29 2011-06-02 2012-01-01     7  2011          0
 4     29 2012-01-01 2013-01-01    19  2012          0
 5     29 2013-01-01 2013-03-22    22  2013          0
 6     30 2010-05-17 2011-01-01     8  2010          0
 7     30 2011-01-01 2011-05-21    12  2011          0
 8     36 2011-05-21 2012-01-01     7  2011          0
 9     36 2012-01-01 2013-01-01    19  2012          0
10     36 2013-01-01 2013-03-23    22  2013          0
# ... with 6,032 more rows
# Keep only what is needed
idat_mnth <- select(gdat_mnth, IIntID, Time, sero_event, Age0, Age2, EverCircum)
write.table(idat_mnth, file=file.path(home, "intcens/input_data_mnth.txt"),
  row.names=FALSE, quote=FALSE)
```

Now we can run the G-transformation model and get the results.

``` r
uniReg(
  InFile = file.path(home, "intcens/input_data_mnth.txt"),
  OutFile = file.path(home, "intcens/res_dat_mnth.txt"),
  Model = "(Time, sero_event) = Age0 + Age2 + EverCircum",
  ID="IIntID", printout=TRUE, ign_stout=FALSE, cthresh=0.01)
[1] 0

ic_res_mnth <- readUniRegResults(
  File=file.path(home, "intcens/res_dat_mnth.txt"))
```

So far, we have made one change to the `Time` variable. We need to make
one last change in the `gImpute` function, where we give the `tscale`
argument its proper value, in this case, the scale for a month.

``` r
# Scale by month
gdates_mnth <- gImpute(gdat_mnth, ic_res_mnth, nSim=Args$nSim, tscale=30.44)

gdat50_mnth <- lapply(seq(Args$nSim), function(i)
  splitAtSeroDate(getGImpute(rtdat, gdates_mnth, i))) 
gdat50_mnth <- lapply(gdat50_mnth, AggByYear)
MIaggregate(gdat50_mnth)
   Year sero_event   pyears
1  2005       1.08 173.7710
2  2006       6.90 380.3462
3  2007       7.64 447.2618
4  2008       8.60 482.3232
5  2009       8.14 458.6550
6  2010       7.56 442.8991
7  2011       6.82 426.3619
8  2012       9.82 382.6507
9  2013      12.08 367.9939
10 2014       7.68 358.0142
11 2015       4.90 351.8704
12 2016       5.78 287.0098
13 2017       4.44 209.1483
14 2018       1.56  91.8210
```

For this example, we don’t see much improvement in time because the
datasets are small, around 6,000 rows. The full AHRI dataset has 46,000
rows just for the men. When using the day scale with the `uniReg`
function, my Windows PC freezes and crashes. (I therefore have to run
`uniReg` on a Linux RedHat server to get the results.) But when
transitioning to a month scale, `uniReg` on my Windows PC completes
within 30 seconds. Thus, changing to a reasonable time scale for large
datasets and many jump points should give you a large speed advantage
with little cost in precision.

gImpute and categorical variables
=================================

The `gImpute` function can only take continuous or dichotomous
variables. To accept variables with three or more levels you would have
to make dummy variables of each level. Let’s say you are interested in
Age as a categorical variable with three levels
`[15, 25), [25, 40), [40, 54)`:

You could make dummy variables as follows:

``` r
gdat <- mutate(gdat, 
  Age15_24 = as.numeric(Age %in% c(15:24)),
  Age25_39 = as.numeric(Age %in% c(25:39)),
  Age40_54 = as.numeric(Age %in% c(40:54)))
```

You then pass the dummy variables to the `uniReg` call:

``` r
uniReg(
  InFile = file.path(home, "intcens/input_data.txt"),
  OutFile = file.path(home, "intcens/res_dat.txt"),
  Model = "(Time, sero_event) = Age15_24 + Age25_39 + Age40_54",
  ID="IIntID", printout=TRUE, ign_stout=FALSE, cthresh=0.01
)
```

These dummy variables should be present in the datset that goes to
`gImpute`.

(This document was compiled with `ahri` version 1.0.1 )
