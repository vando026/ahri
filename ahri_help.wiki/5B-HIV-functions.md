-   [Multiple imputation](#multiple-imputation)
    -   [The MIdata function](#the-midata-function)
    -   [The AggBy and MIaggregate
        functions](#the-aggby-and-miaggregate-functions)
    -   [Age adjusted incidence](#age-adjusted-incidence)
    -   [Confidence intervals and the MIcombine
        function](#confidence-intervals-and-the-micombine-function)
-   [The MIpredict function](#the-mipredict-function)
-   [Putting it all together](#putting-it-all-together)
    -   [The getIncidence function](#the-getincidence-function)

Part B of this documentation focuses on several `ahri` functions for
multiple imputation and incidence rate estimation. Multiple imputation
is needed because of the interval censoring problem, as described here:
<a href="https://academic.oup.com/ije/article/47/1/236/4079903" class="uri">https://academic.oup.com/ije/article/47/1/236/4079903</a>.

Interval censoring occurs because there is an average time of 3 years
between the latest HIV-negative and earliest HIV-positive test dates in
the AHRI surveillance system. This means that we don’t know the year in
which the seroconversion event occurs, leading to uncertainty about our
incidence rate estimates.

One approach to quantify this uncertainty is to impute a single
seroconversion date for each HIV-positive repeat-tester. The data is
then right censored, and incidence rate estimates are calculated. This
proceess is repeated *M* times (for m = 1,…M) to generate *M* datasets
and *M* estimates. Using Rubin’s rules, the average of the *M* estimates
is obtained with 95% confidence intervals.

Multiple imputation
===================

The MIdata function
-------------------

The `MIdata` function simplifies the process of imputing the
seroconversion date and generating multiple imputed datasets. It takes
the data generated from `getRTData` and arguments from `setArgs`. Below
is the code for `MIdata`, which calls some simpler functions that we
have already described.

``` r
MIdata
function(rtdat, Args, f=identity) {
  bdat=getBirthDate()
  parallel::mclapply(seq(Args$nSim),
    function(i) { 
      cat(i, "")
      getIncData(rtdat, bdat, Args, func=f)},
      mc.cores=Args$mcores)
}
<environment: namespace:ahri>
```

An important paramater in `setArgs` is `nSim`, which gives the number of
datasets to be generated. For this example, we select `nSim = 3`. In the
code above, you will notice that the `parallel` library is used if the
argument `mcores` to `setArgs` is greater than 1. Thus, if your system
has more than 1 core, multiple datasets can be generated in parallel,
which is more efficient. The function will print out a counter to let
you know how far along in the process it is.

``` r
# From scratch
Args <- setArgs(Year=c(2010:2018),
  Age=list(All=c(15, 35)),
  nSim=3, mcores=1)
hiv <- getHIV()
rtdat <- getRTData(hiv)
mdat <- MIdata(rtdat, Args)
1 2 3 
length(mdat)
[1] 3
```

We see that the `MIdata` generates three datasets as instructed. For
demonstration, I show the same repeat-testers from the three imputed
datasets.

``` r
ex_ids <- c(207, 268,  456, 500) 
lapply(mdat, function(x) x[x$IIntID %in% ex_ids, ])
[[1]]
# A tibble: 15 x 13
   IIntID Female late_neg   early_pos  sero_date  obs_start  obs_end    sero_event  Year  Time   Age AgeCat  tscale
    <int>  <int> <date>     <date>     <date>     <date>     <date>          <dbl> <int> <dbl> <dbl> <fct>    <dbl>
 1    207      0 2018-08-10 NA         NA         2014-08-03 2015-01-01          0  2014   151    18 [15,20) 0.413 
 2    207      0 2018-08-10 NA         NA         2015-01-01 2016-01-01          0  2015   365    19 [15,20) 0.999 
 3    207      0 2018-08-10 NA         NA         2016-01-01 2017-01-01          0  2016   366    20 [20,25) 1.00  
 4    207      0 2018-08-10 NA         NA         2017-01-01 2018-01-01          0  2017   365    21 [20,25) 0.999 
 5    207      0 2018-08-10 NA         NA         2018-01-01 2018-08-10          0  2018   221    21 [20,25) 0.605 
 6    268      1 2010-03-17 2013-02-28 2011-03-10 2010-01-01 2011-01-01          0  2010   365    19 [15,20) 0.999 
 7    268      1 2010-03-17 2013-02-28 2011-03-10 2011-01-01 2011-03-10          1  2011    68    19 [15,20) 0.186 
 8    456      0 2011-09-16 2014-03-02 2014-01-27 2010-01-01 2011-01-01          0  2010   365    22 [20,25) 0.999 
 9    456      0 2011-09-16 2014-03-02 2014-01-27 2011-01-01 2012-01-01          0  2011   365    23 [20,25) 0.999 
10    456      0 2011-09-16 2014-03-02 2014-01-27 2012-01-01 2013-01-01          0  2012   366    24 [20,25) 1.00  
11    456      0 2011-09-16 2014-03-02 2014-01-27 2013-01-01 2014-01-01          0  2013   365    25 [25,30) 0.999 
12    456      0 2011-09-16 2014-03-02 2014-01-27 2014-01-01 2014-01-27          1  2014    26    25 [25,30) 0.0712
13    500      0 2017-08-12 NA         NA         2015-04-23 2016-01-01          0  2015   253    19 [15,20) 0.693 
14    500      0 2017-08-12 NA         NA         2016-01-01 2017-01-01          0  2016   366    20 [20,25) 1.00  
15    500      0 2017-08-12 NA         NA         2017-01-01 2017-08-12          0  2017   223    20 [20,25) 0.611 

[[2]]
# A tibble: 15 x 13
   IIntID Female late_neg   early_pos  sero_date  obs_start  obs_end    sero_event  Year  Time   Age AgeCat  tscale
    <int>  <int> <date>     <date>     <date>     <date>     <date>          <dbl> <int> <dbl> <dbl> <fct>    <dbl>
 1    207      0 2018-08-10 NA         NA         2014-08-03 2015-01-01          0  2014   151    18 [15,20) 0.413 
 2    207      0 2018-08-10 NA         NA         2015-01-01 2016-01-01          0  2015   365    19 [15,20) 0.999 
 3    207      0 2018-08-10 NA         NA         2016-01-01 2017-01-01          0  2016   366    20 [20,25) 1.00  
 4    207      0 2018-08-10 NA         NA         2017-01-01 2018-01-01          0  2017   365    21 [20,25) 0.999 
 5    207      0 2018-08-10 NA         NA         2018-01-01 2018-08-10          0  2018   221    21 [20,25) 0.605 
 6    268      1 2010-03-17 2013-02-28 2012-02-01 2010-01-01 2011-01-01          0  2010   365    19 [15,20) 0.999 
 7    268      1 2010-03-17 2013-02-28 2012-02-01 2011-01-01 2012-01-01          0  2011   365    20 [20,25) 0.999 
 8    268      1 2010-03-17 2013-02-28 2012-02-01 2012-01-01 2012-02-01          1  2012    31    20 [20,25) 0.0849
 9    456      0 2011-09-16 2014-03-02 2013-07-10 2010-01-01 2011-01-01          0  2010   365    22 [20,25) 0.999 
10    456      0 2011-09-16 2014-03-02 2013-07-10 2011-01-01 2012-01-01          0  2011   365    23 [20,25) 0.999 
11    456      0 2011-09-16 2014-03-02 2013-07-10 2012-01-01 2013-01-01          0  2012   366    24 [20,25) 1.00  
12    456      0 2011-09-16 2014-03-02 2013-07-10 2013-01-01 2013-07-10          1  2013   190    25 [25,30) 0.520 
13    500      0 2017-08-12 NA         NA         2015-04-23 2016-01-01          0  2015   253    19 [15,20) 0.693 
14    500      0 2017-08-12 NA         NA         2016-01-01 2017-01-01          0  2016   366    20 [20,25) 1.00  
15    500      0 2017-08-12 NA         NA         2017-01-01 2017-08-12          0  2017   223    20 [20,25) 0.611 

[[3]]
# A tibble: 12 x 13
   IIntID Female late_neg   early_pos  sero_date  obs_start  obs_end    sero_event  Year  Time   Age AgeCat  tscale
    <int>  <int> <date>     <date>     <date>     <date>     <date>          <dbl> <int> <dbl> <dbl> <fct>    <dbl>
 1    207      0 2018-08-10 NA         NA         2014-08-03 2015-01-01          0  2014   151    18 [15,20)  0.413
 2    207      0 2018-08-10 NA         NA         2015-01-01 2016-01-01          0  2015   365    19 [15,20)  0.999
 3    207      0 2018-08-10 NA         NA         2016-01-01 2017-01-01          0  2016   366    20 [20,25)  1.00 
 4    207      0 2018-08-10 NA         NA         2017-01-01 2018-01-01          0  2017   365    21 [20,25)  0.999
 5    207      0 2018-08-10 NA         NA         2018-01-01 2018-08-10          0  2018   221    21 [20,25)  0.605
 6    268      1 2010-03-17 2013-02-28 2010-12-29 2010-01-01 2010-12-29          1  2010   362    19 [15,20)  0.991
 7    456      0 2011-09-16 2014-03-02 2012-10-25 2010-01-01 2011-01-01          0  2010   365    22 [20,25)  0.999
 8    456      0 2011-09-16 2014-03-02 2012-10-25 2011-01-01 2012-01-01          0  2011   365    23 [20,25)  0.999
 9    456      0 2011-09-16 2014-03-02 2012-10-25 2012-01-01 2012-10-25          1  2012   298    24 [20,25)  0.816
10    500      0 2017-08-12 NA         NA         2015-04-23 2016-01-01          0  2015   253    19 [15,20)  0.693
11    500      0 2017-08-12 NA         NA         2016-01-01 2017-01-01          0  2016   366    20 [20,25)  1.00 
12    500      0 2017-08-12 NA         NA         2017-01-01 2017-08-12          0  2017   223    20 [20,25)  0.611
```

`MIdata` also includes an argument `f` which takes a function, thus
improving its flexibility. For example, lets say that after creating
multiple imputed datasets I wanted to keep specific variables. I would
first create a function and then pass it as an argument to `f`.

``` r
keepVars <- function(dat) dplyr::select(dat, IIntID, obs_start, obs_end)
mdat1 <- MIdata(rtdat, Args, f=keepVars)
1 2 3 
# Only has these Vars
lapply(mdat1, function(x) x[x$IIntID %in% ex_ids, ])
[[1]]
# A tibble: 13 x 3
   IIntID obs_start  obs_end   
    <int> <date>     <date>    
 1    207 2014-08-03 2015-01-01
 2    207 2015-01-01 2016-01-01
 3    207 2016-01-01 2017-01-01
 4    207 2017-01-01 2018-01-01
 5    207 2018-01-01 2018-08-10
 6    268 2010-01-01 2011-01-01
 7    268 2011-01-01 2011-05-12
 8    456 2010-01-01 2011-01-01
 9    456 2011-01-01 2012-01-01
10    456 2012-01-01 2012-05-25
11    500 2015-04-23 2016-01-01
12    500 2016-01-01 2017-01-01
13    500 2017-01-01 2017-08-12

[[2]]
# A tibble: 15 x 3
   IIntID obs_start  obs_end   
    <int> <date>     <date>    
 1    207 2014-08-03 2015-01-01
 2    207 2015-01-01 2016-01-01
 3    207 2016-01-01 2017-01-01
 4    207 2017-01-01 2018-01-01
 5    207 2018-01-01 2018-08-10
 6    268 2010-01-01 2011-01-01
 7    268 2011-01-01 2012-01-01
 8    268 2012-01-01 2012-03-04
 9    456 2010-01-01 2011-01-01
10    456 2011-01-01 2012-01-01
11    456 2012-01-01 2013-01-01
12    456 2013-01-01 2013-11-27
13    500 2015-04-23 2016-01-01
14    500 2016-01-01 2017-01-01
15    500 2017-01-01 2017-08-12

[[3]]
# A tibble: 14 x 3
   IIntID obs_start  obs_end   
    <int> <date>     <date>    
 1    207 2014-08-03 2015-01-01
 2    207 2015-01-01 2016-01-01
 3    207 2016-01-01 2017-01-01
 4    207 2017-01-01 2018-01-01
 5    207 2018-01-01 2018-08-10
 6    268 2010-01-01 2011-01-01
 7    268 2011-01-01 2011-11-16
 8    456 2010-01-01 2011-01-01
 9    456 2011-01-01 2012-01-01
10    456 2012-01-01 2013-01-01
11    456 2013-01-01 2013-11-24
12    500 2015-04-23 2016-01-01
13    500 2016-01-01 2017-01-01
14    500 2017-01-01 2017-08-12
```

Again, this is useful for applying a single operation to mutiple
datasets.

The AggBy and MIaggregate functions
-----------------------------------

In Section 5A of this documentation we saw how to get aggregated
seroconversion events and person years by one or more grouping variables
for a single imputed dataset. We can extend this approach to multiply
imputed datsets. Let’s say we want to aggregate by age category and sex:

``` r
AggByAgeFem = AggFunc("AgeCat + Female")
inc3  <- lapply(mdat, AggByAgeFem)
inc3
[[1]]
    AgeCat Female sero_event    pyears
1  [15,20)      0         37 6491.3949
2  [20,25)      0        148 6878.2286
3  [25,30)      0        142 4016.3915
4  [30,35)      0         70 2228.1643
5  [35,40)      0          4  314.9459
6  [15,20)      1        342 7398.5079
7  [20,25)      1        570 8865.7933
8  [25,30)      1        335 5727.2580
9  [30,35)      1        146 3353.5332
10 [35,40)      1         10  560.4134

[[2]]
    AgeCat Female sero_event    pyears
1  [15,20)      0         42 6492.9144
2  [20,25)      0        152 6915.6605
3  [25,30)      0        137 4019.6715
4  [30,35)      0         77 2227.2170
5  [35,40)      0          5  315.9398
6  [15,20)      1        336 7421.5387
7  [20,25)      1        580 8894.3244
8  [25,30)      1        351 5731.8905
9  [30,35)      1        157 3367.0527
10 [35,40)      1         17  558.7515

[[3]]
    AgeCat Female sero_event    pyears
1  [15,20)      0         46 6495.7207
2  [20,25)      0        157 6882.3546
3  [25,30)      0        127 4032.2300
4  [30,35)      0         75 2227.6797
5  [35,40)      0          7  320.1971
6  [15,20)      1        351 7421.8371
7  [20,25)      1        561 8874.3984
8  [25,30)      1        334 5735.7864
9  [30,35)      1        149 3348.0383
10 [35,40)      1         11  556.4244
```

Above, we see the aggregated data for the three imputed datasets. We can
get the average seroconversion events and person years across the *M*
imputed datasets using `MIaggregate`.

``` r
MIaggregate(inc3)
    AgeCat Female sero_event    pyears
1  [15,20)      0  41.666667 6493.3434
2  [20,25)      0 152.333333 6892.0812
3  [25,30)      0 135.333333 4022.7643
4  [30,35)      0  74.000000 2227.6870
5  [35,40)      0   5.333333  317.0276
6  [15,20)      1 343.000000 7413.9612
7  [20,25)      1 570.333333 8878.1720
8  [25,30)      1 340.000000 5731.6450
9  [30,35)      1 150.666667 3356.2081
10 [35,40)      1  12.666667  558.5298
```

Age adjusted incidence
----------------------

The above example showed how to calculate crude incidence rates from
multiple imputed datasets. What if we wanted to calculate age-adjusted
incidence rates, for example for the *M* imputed datasets. We can use
Poisson regression and the R package `mitools` for this purpose.
`mitools` provides greater code flexibility and enables users to run
their own models, for purposes other than incidence rate estimation. In
this case, a useful function is `imputationList`, which collects the
imputed datasets in special list that can be used with a function. So
taking the multiple imputed datasets (`mdat`) from above.

``` r
mdat <- mitools::imputationList(mdat)
sformula = "sero_event ~ -1 + as.factor(Year) + Age + as.factor(Year):Age + offset(log(tscale))"
mods <- with(mdat, stats::glm(as.formula(sformula), family=poisson))
mods
[[1]]

Call:  stats::glm(formula = as.formula(sformula), family = poisson)

Coefficients:
    as.factor(Year)2010      as.factor(Year)2011      as.factor(Year)2012      as.factor(Year)2013      as.factor(Year)2014  
               -2.57241                 -3.25020                 -3.21667                 -3.36423                 -3.90314  
    as.factor(Year)2015      as.factor(Year)2016      as.factor(Year)2017      as.factor(Year)2018                      Age  
               -4.38387                 -4.25847                 -5.04074                 -4.41875                 -0.01913  
as.factor(Year)2011:Age  as.factor(Year)2012:Age  as.factor(Year)2013:Age  as.factor(Year)2014:Age  as.factor(Year)2015:Age  
                0.02031                  0.02397                  0.03028                  0.05070                  0.06212  
as.factor(Year)2016:Age  as.factor(Year)2017:Age  as.factor(Year)2018:Age  
                0.05574                  0.07082                  0.06216  

Degrees of Freedom: 56330 Total (i.e. Null);  56312 Residual
Null Deviance:      91990 
Residual Deviance: 15490    AIC: 19130

[[2]]

Call:  stats::glm(formula = as.formula(sformula), family = poisson)

Coefficients:
    as.factor(Year)2010      as.factor(Year)2011      as.factor(Year)2012      as.factor(Year)2013      as.factor(Year)2014  
               -2.70862                 -3.58492                 -3.42404                 -3.38109                 -4.03827  
    as.factor(Year)2015      as.factor(Year)2016      as.factor(Year)2017      as.factor(Year)2018                      Age  
               -4.16713                 -4.77649                 -4.03026                 -4.61318                 -0.01573  
as.factor(Year)2011:Age  as.factor(Year)2012:Age  as.factor(Year)2013:Age  as.factor(Year)2014:Age  as.factor(Year)2015:Age  
                0.03549                  0.03209                  0.03062                  0.05394                  0.05060  
as.factor(Year)2016:Age  as.factor(Year)2017:Age  as.factor(Year)2018:Age  
                0.07067                  0.03287                  0.05653  

Degrees of Freedom: 56464 Total (i.e. Null);  56446 Residual
Null Deviance:      92280 
Residual Deviance: 15880    AIC: 19630

[[3]]

Call:  stats::glm(formula = as.formula(sformula), family = poisson)

Coefficients:
    as.factor(Year)2010      as.factor(Year)2011      as.factor(Year)2012      as.factor(Year)2013      as.factor(Year)2014  
               -2.61857                 -3.27953                 -3.16955                 -3.44058                 -3.73801  
    as.factor(Year)2015      as.factor(Year)2016      as.factor(Year)2017      as.factor(Year)2018                      Age  
               -4.39919                 -4.68804                 -4.50002                 -4.30381                 -0.02286  
as.factor(Year)2011:Age  as.factor(Year)2012:Age  as.factor(Year)2013:Age  as.factor(Year)2014:Age  as.factor(Year)2015:Age  
                0.03032                  0.02612                  0.03853                  0.04993                  0.06601  
as.factor(Year)2016:Age  as.factor(Year)2017:Age  as.factor(Year)2018:Age  
                0.07541                  0.06190                  0.04425  

Degrees of Freedom: 56388 Total (i.e. Null);  56370 Residual
Null Deviance:      92100 
Residual Deviance: 15580    AIC: 19250

attr(,"call")
with(mdat, stats::glm(as.formula(sformula), family = poisson))
```

Confidence intervals and the MIcombine function
-----------------------------------------------

In the above code, the Poisson results for each imputed dataset was
obtained. How to combine them so that you can calculate the appropriate
model estimates and standard errors across all the imputed datasets? By
appropriate standard errors, I mean standard errors that account for
both the variation in the point estimates and the variation across the
*M* simulated datasets. We can use the `mitools` function `MIcombine`
for this. Please see the `mitools::MIcombine` for what methods is works
with (specifically the `coef` and `cvov` methods).

``` r
res <- mitools::MIcombine(mods)
res
Multiple imputation results:
      with(mdat, stats::glm(as.formula(sformula), family = poisson))
      MIcombine.default(mods)
                            results         se
as.factor(Year)2010     -2.63319942 0.31152263
as.factor(Year)2011     -3.37155003 0.37439407
as.factor(Year)2012     -3.27008862 0.34431093
as.factor(Year)2013     -3.39529919 0.29480408
as.factor(Year)2014     -3.89314024 0.33978927
as.factor(Year)2015     -4.31673145 0.35516127
as.factor(Year)2016     -4.57433272 0.47058085
as.factor(Year)2017     -4.52367455 0.72314239
as.factor(Year)2018     -4.44524733 0.64504325
Age                     -0.01923947 0.01364950
as.factor(Year)2011:Age  0.02870636 0.02037315
as.factor(Year)2012:Age  0.02739504 0.01885739
as.factor(Year)2013:Age  0.03314260 0.01856124
as.factor(Year)2014:Age  0.05152493 0.01778212
as.factor(Year)2015:Age  0.05957377 0.02056309
as.factor(Year)2016:Age  0.06727500 0.02235987
as.factor(Year)2017:Age  0.05519821 0.03148409
as.factor(Year)2018:Age  0.05431317 0.03026237
```

To get the estimates and standard errors using Rubin’s rules, you can
pass the result to the `summary` function, as shown below:

``` r
summary(res)
Multiple imputation results:
      with(mdat, stats::glm(as.formula(sformula), family = poisson))
      MIcombine.default(mods)
                            results         se       (lower       upper) missInfo
as.factor(Year)2010     -2.63319942 0.31152263 -3.245382748 -2.021016094      7 %
as.factor(Year)2011     -3.37155003 0.37439407 -4.155948455 -2.587151602     39 %
as.factor(Year)2012     -3.27008862 0.34431093 -3.962733619 -2.577443618     24 %
as.factor(Year)2013     -3.39529919 0.29480408 -3.973317670 -2.817280700      3 %
as.factor(Year)2014     -3.89314024 0.33978927 -4.587751518 -3.198528953     31 %
as.factor(Year)2015     -4.31673145 0.35516127 -5.026444867 -3.607018035     20 %
as.factor(Year)2016     -4.57433272 0.47058085 -5.632697385 -3.515968062     55 %
as.factor(Year)2017     -4.52367455 0.72314239 -6.418079034 -2.629270063     74 %
as.factor(Year)2018     -4.44524733 0.64504325 -5.714223968 -3.176270699      8 %
Age                     -0.01923947 0.01364950 -0.046126537  0.007647588     10 %
as.factor(Year)2011:Age  0.02870636 0.02037315 -0.012127070  0.069539793     22 %
as.factor(Year)2012:Age  0.02739504 0.01885739 -0.009663186  0.064453264      7 %
as.factor(Year)2013:Age  0.03314260 0.01856124 -0.003394324  0.069679533      9 %
as.factor(Year)2014:Age  0.05152493 0.01778212  0.016664890  0.086384978      2 %
as.factor(Year)2015:Age  0.05957377 0.02056309  0.018244956  0.100902592     23 %
as.factor(Year)2016:Age  0.06727500 0.02235987  0.021256133  0.113293870     33 %
as.factor(Year)2017:Age  0.05519821 0.03148409 -0.018984343  0.129380753     62 %
as.factor(Year)2018:Age  0.05431317 0.03026237 -0.005539890  0.114166234     13 %
```

The `MIcombine` example is to show you how to use the `mitools` and
`ahri` functions to achieve your objectives.

The MIpredict function
======================

We can use the `ahri` function `MIpredict` to obtain the incidence rates
per 100 person years with confidence intervals using Rubin’s rules. The
`MIpredict` function is similar to R’s `glm.predict` function in that it
takes an object and a new data.frame to look for variables with which to
predict. Specifically for age-adjusted incidence by year, the `ahri`
library has a `getAgeYear` function to create this new dataset of
predictor variables:

``` r
age_dat <- getAgeYear(dat=setHIV(Args))
age_dat
# A tibble: 9 x 3
  Year    Age tscale
  <fct> <dbl>  <dbl>
1 2010   22.4      1
2 2011   22.7      1
3 2012   22.3      1
4 2013   22.0      1
5 2014   22.5      1
6 2015   22.6      1
7 2016   22.6      1
8 2017   23.2      1
9 2018   23.0      1
```

Now, we can give the `MIpredict` function model results and the new
data.frame.

``` r
MIpredict(object=mods, newdata=age_dat)
          fit    se.fit      lci      uci
2010 4.668410 0.4484972 3.789372 5.547449
2011 4.257775 0.3984926 3.476744 5.038807
2012 4.557786 0.3238760 3.923001 5.192571
2013 4.553025 0.3407116 3.885243 5.220808
2014 4.213468 0.3207467 3.584816 4.842120
2015 3.319518 0.2616809 2.806633 3.832403
2016 3.052128 0.3109217 2.442733 3.661524
2017 2.496909 0.4566885 1.601816 3.392002
2018 2.630652 0.7000883 1.258504 4.002800
```

Putting it all together
=======================

To calculate the age-adjusted annual incidence from scratch, we would
do:

``` r
Args <- setArgs(Year=c(2010:2018),
  Age=list(All=c(15, 35)),
  nSim=3, mcores=1)
rtdat <- getRTData()
mdat <- MIdata(rtdat, Args)
mdat <- mitools::imputationList(mdat)
sformula = "sero_event ~ -1 + as.factor(Year) + Age + as.factor(Year):Age + offset(log(tscale))"
mods <- with(mdat, stats::glm(as.formula(sformula), family=poisson))
age_dat <- getAgeYear(dat=setHIV(Args))
MIpredict(mods, age_dat)
```

The getIncidence function
-------------------------

I have described the individual functions used to estimate the incidence
rate using multiple imputation. `getIncidence` packages all of this work
into one function to get the annual age-adjusted HIV estimates and
confidence intervals.

``` r
getIncidence
function(
  Args=setArgs(), sformula="", aggFun=NULL, newdata=NULL) {
  if (Args$nSim < 2) stop('nSim in setArgs() must be > 1')
  rtdat <- getRTData()
  mdat <- MIdata(rtdat, Args)
  agg_inc <- lapply(mdat, aggFun)
  agg_inc <- MIaggregate(agg_inc)
  mdat <- mitools::imputationList(mdat)
  mods <- with(mdat, stats::glm(as.formula(sformula), family=poisson))
  pois_inc <- MIpredict(mods, newdata)
  list(agg=agg_inc, pois_inc=pois_inc)
}
<environment: namespace:ahri>
```

So usings previous paramters and datasets constructed above, I do:

``` r
Args <- setArgs(Years=c(2010:2018),
  Age=list(All=c(15, 40)), nSim=3)
getIncidence(Args, sformula, AggByYear, age_dat)
1 2 3 
$agg
  Year sero_event   pyears
1 2010  283.66667 6583.970
2 2011  283.00000 6395.568
3 2012  263.00000 6075.878
4 2013  262.00000 6205.854
5 2014  253.33333 6297.979
6 2015  218.66667 6218.455
7 2016  174.33333 5625.432
8 2017  112.66667 4548.768
9 2018   60.33333 2064.860

$pois_inc
          fit    se.fit      lci      uci
2010 4.442517 0.5453242 3.373702 5.511333
2011 4.492990 0.3042309 3.896708 5.089271
2012 4.522953 0.4537624 3.633595 5.412311
2013 4.264662 0.3006899 3.675320 4.854003
2014 3.984777 0.3071953 3.382685 4.586869
2015 3.433683 0.2643854 2.915497 3.951869
2016 3.010276 0.2458629 2.528394 3.492159
2017 2.418011 0.3185917 1.793583 3.042439
2018 2.792228 0.4768290 1.857660 3.726795
```

You could adapt the individual functions to make your own `getIncidence`
function using the example I have provided here.

(This document was compiled with `ahri` version 1.0.1 )
