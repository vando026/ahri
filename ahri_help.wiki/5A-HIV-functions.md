-   [Preparing the HIV incidence
    data](#preparing-the-hiv-incidence-data)
    -   [The getRTData function](#the-getrtdata-function)
-   [Imputation](#imputation)
    -   [Seroconversion date
        imputation](#seroconversion-date-imputation)
-   [Censoring and splitting the
    data](#censoring-and-splitting-the-data)
    -   [The splitAtSeroDate function](#the-splitatserodate-function)
    -   [The splitAtEarlyPos function](#the-splitatearlypos-function)
-   [Combining the functions](#combining-the-functions)
    -   [The getIncData function](#the-getincdata-function)
-   [Calculating HIV incidence](#calculating-hiv-incidence)
    -   [The AggBy functions](#the-aggby-functions)
    -   [The AggFunc factory](#the-aggfunc-factory)
    -   [Crude incidence](#crude-incidence)
    -   [Age-adjusted incidence](#age-adjusted-incidence)

In Part A of this documentation, I will describe functions to work with
the HIV data, including imputation of the seroconversion date and
estimating the annual HIV incidence rate.

Preparing the HIV incidence data
================================

The getRTData function
----------------------

The `getRTData` functions creates a cohort of repeat-testers, defined as
any participant at least 15 years old with an earliest HIV-negative test
date followed by at least one (HIV-negative or HV-positive) test date.

You must pass the dataframe generated from `getHIV` as its first
argument. The repeat-testers dataset shows one row per participant with
an `obs_start` (earliest HIV-negative) date and either a latest
HIV-negative or earliest HIV-positive date. If the repeat-tester has an
earliest HIV-positive date, then `sero_event==1` is an indicator
variable for a recorded HIV-positive event.

``` r
Args <- setArgs(Years=c(2010:2018), 
  Age=list(All=c(15, 49)))
hiv  <- getHIV()
rtdat <- getRTData(hiv)
rtdat
# A tibble: 27,827 x 6
   IIntID Female obs_start  late_neg   early_pos  sero_event
    <int>  <int> <date>     <date>     <date>          <dbl>
 1     16      1 2009-06-02 2011-05-23 NA                  0
 2     17      1 2004-02-09 2011-08-06 2017-10-17          1
 3     19      1 2009-08-04 2014-08-06 NA                  0
 4     22      1 2006-06-01 2015-07-15 NA                  0
 5     28      0 2004-10-07 2006-10-06 NA                  0
 6     29      0 2011-06-02 2013-03-22 NA                  0
 7     30      0 2004-09-19 2011-05-21 NA                  0
 8     34      1 2004-10-04 2018-08-08 NA                  0
 9     36      0 2011-05-21 2013-03-23 NA                  0
10     45      0 2010-09-11 2011-09-06 NA                  0
# ... with 27,817 more rows
```

Imputation
==========

Seroconversion date imputation
------------------------------

A typical next step in working with the repeat-testers dataset is to
impute a seroconversion date. The `ahri` library provides three *ad hoc*
methods for doing this: mid-point, end-point, and single random-point
imputation.

The `imputeMidPoint` function takes a dataset from `getRTData`, which
must contain the variables `late_neg` and `early_pos`, and imputes a
seroconversion date at the mid-point of the latest HIV-negative and
earliest HIV-positive test dates.

``` r
mdat <- imputeMidPoint(rtdat)
ex_ids <- c(16, 17, 47, 236, 241, 259)
filter(mdat, IIntID %in% ex_ids)
# A tibble: 6 x 7
  IIntID Female obs_start  late_neg   early_pos  sero_event sero_date 
   <int>  <int> <date>     <date>     <date>          <dbl> <date>    
1     16      1 2009-06-02 2011-05-23 NA                  0 NA        
2     17      1 2004-02-09 2011-08-06 2017-10-17          1 2014-09-11
3     47      1 2004-03-25 2005-08-23 2016-11-24          1 2011-04-09
4    236      1 2003-07-02 2005-04-16 2005-08-29          1 2005-06-22
5    241      1 2003-07-02 2003-07-02 2005-04-14          1 2004-05-23
6    259      0 2010-07-30 2011-07-15 NA                  0 NA        
```

The `imputeEndPoint` function imputes the seroconversion date at the
earliest HIV-positive test date, which is not recommended for incidence
rate estimation with the AHRI data.

``` r
edat <- imputeEndPoint(rtdat) 
filter(edat, IIntID %in% ex_ids)
# A tibble: 6 x 7
  IIntID Female obs_start  late_neg   early_pos  sero_event sero_date 
   <int>  <int> <date>     <date>     <date>          <dbl> <date>    
1     16      1 2009-06-02 2011-05-23 NA                  0 NA        
2     17      1 2004-02-09 2011-08-06 2017-10-17          1 2017-10-17
3     47      1 2004-03-25 2005-08-23 2016-11-24          1 2016-11-24
4    236      1 2003-07-02 2005-04-16 2005-08-29          1 2005-08-29
5    241      1 2003-07-02 2003-07-02 2005-04-14          1 2005-04-14
6    259      0 2010-07-30 2011-07-15 NA                  0 NA        
```

The `imputeRandomPoint` imputes a single random seroconversion date
between the latest HIV-negative and earliest HIV-positive test dates.
The random date is drawn from a uniform distribution bound by these two
dates.

``` r
set.seed(1234)
rdat <- imputeMidPoint(rtdat)
filter(rdat, IIntID %in% ex_ids)
# A tibble: 6 x 7
  IIntID Female obs_start  late_neg   early_pos  sero_event sero_date 
   <int>  <int> <date>     <date>     <date>          <dbl> <date>    
1     16      1 2009-06-02 2011-05-23 NA                  0 NA        
2     17      1 2004-02-09 2011-08-06 2017-10-17          1 2014-09-11
3     47      1 2004-03-25 2005-08-23 2016-11-24          1 2011-04-09
4    236      1 2003-07-02 2005-04-16 2005-08-29          1 2005-06-22
5    241      1 2003-07-02 2003-07-02 2005-04-14          1 2004-05-23
6    259      0 2010-07-30 2011-07-15 NA                  0 NA        
```

Censoring and splitting the data
================================

The splitAtSeroDate function
----------------------------

We can use the `splitAtSeroDate` function to right censor the data at
the imputed or latest HIV-negative date, and create annual episodes for
each repeat-tester. So using the `rdat` (random-point) dataset from
above:

``` r
sdat <- splitAtSeroDate(rdat)
sdat
# A tibble: 178,638 x 10
   IIntID Female late_neg   early_pos  sero_date  obs_start  obs_end    sero_event  Year  Time
    <int>  <int> <date>     <date>     <date>     <date>     <date>          <dbl> <int> <dbl>
 1     16      1 2011-05-23 NA         NA         2009-06-02 2010-01-01          0  2009   213
 2     16      1 2011-05-23 NA         NA         2010-01-01 2011-01-01          0  2010   365
 3     16      1 2011-05-23 NA         NA         2011-01-01 2011-05-23          0  2011   142
 4     17      1 2011-08-06 2017-10-17 2014-09-11 2004-02-09 2005-01-01          0  2004   327
 5     17      1 2011-08-06 2017-10-17 2014-09-11 2005-01-01 2006-01-01          0  2005   365
 6     17      1 2011-08-06 2017-10-17 2014-09-11 2006-01-01 2007-01-01          0  2006   365
 7     17      1 2011-08-06 2017-10-17 2014-09-11 2007-01-01 2008-01-01          0  2007   365
 8     17      1 2011-08-06 2017-10-17 2014-09-11 2008-01-01 2009-01-01          0  2008   366
 9     17      1 2011-08-06 2017-10-17 2014-09-11 2009-01-01 2010-01-01          0  2009   365
10     17      1 2011-08-06 2017-10-17 2014-09-11 2010-01-01 2011-01-01          0  2010   365
# ... with 178,628 more rows
```

The data is now split into episodes for each repeat-tester, with a
`Time` variable thrown in for free. This variable gives the number of
exposure days in the respective episode, which is useful for computing
incidence later on.

The splitAtEarlyPos function
----------------------------

We can also split the data into episodes using the earliest HIV-positive
date.

``` r
ep_dat <- splitAtEarlyPos(rdat)
ep_dat
# A tibble: 186,366 x 10
   IIntID Female late_neg   early_pos  sero_date  obs_start  obs_end    sero_event  Year  Time
    <int>  <int> <date>     <date>     <date>     <date>     <date>          <dbl> <int> <dbl>
 1     16      1 2011-05-23 NA         NA         2009-06-02 2010-01-01          0  2009   213
 2     16      1 2011-05-23 NA         NA         2010-01-01 2011-01-01          0  2010   365
 3     16      1 2011-05-23 NA         NA         2011-01-01 2011-05-23          0  2011   142
 4     17      1 2011-08-06 2017-10-17 2014-09-11 2004-02-09 2005-01-01          0  2004   327
 5     17      1 2011-08-06 2017-10-17 2014-09-11 2005-01-01 2006-01-01          0  2005   365
 6     17      1 2011-08-06 2017-10-17 2014-09-11 2006-01-01 2007-01-01          0  2006   365
 7     17      1 2011-08-06 2017-10-17 2014-09-11 2007-01-01 2008-01-01          0  2007   365
 8     17      1 2011-08-06 2017-10-17 2014-09-11 2008-01-01 2009-01-01          0  2008   366
 9     17      1 2011-08-06 2017-10-17 2014-09-11 2009-01-01 2010-01-01          0  2009   365
10     17      1 2011-08-06 2017-10-17 2014-09-11 2010-01-01 2011-01-01          0  2010   365
# ... with 186,356 more rows
```

Combining the functions
=======================

The getIncData function
-----------------------

The `getIncData` function combines the above steps by imputing the sero
dates, splitting the data at the right censoring date, and creating an
age variable by episode. We can select the imputation method you want to
use by passing it to the `setArgs` function. For example, if we want to
use single random-point imputation, we can do:

``` r
# Start from scratch
Args <- setArgs(Years=c(2008:2018), 
  Age=list(All=c(15, 45)),
  imputeMethod=imputeRandomPoint)
hiv <- getHIV()
rtdat <- getRTData(hiv)
idat <- getIncData(rtdat, bdat=getBirthDate(), Args)
idat
# A tibble: 83,063 x 13
   IIntID Female late_neg   early_pos  sero_date  obs_start  obs_end    sero_event  Year  Time   Age AgeCat  tscale
    <int>  <int> <date>     <date>     <date>     <date>     <date>          <dbl> <int> <dbl> <dbl> <fct>    <dbl>
 1     17      1 2011-08-06 2017-10-17 2014-05-06 2008-01-01 2009-01-01          0  2008   366    40 [40,45)  1.00 
 2     17      1 2011-08-06 2017-10-17 2014-05-06 2009-01-01 2010-01-01          0  2009   365    41 [40,45)  0.999
 3     17      1 2011-08-06 2017-10-17 2014-05-06 2010-01-01 2011-01-01          0  2010   365    42 [40,45)  0.999
 4     17      1 2011-08-06 2017-10-17 2014-05-06 2011-01-01 2012-01-01          0  2011   365    43 [40,45)  0.999
 5     17      1 2011-08-06 2017-10-17 2014-05-06 2012-01-01 2013-01-01          0  2012   366    44 [40,45)  1.00 
 6     17      1 2011-08-06 2017-10-17 2014-05-06 2013-01-01 2014-01-01          0  2013   365    45 [45,50)  0.999
 7     17      1 2011-08-06 2017-10-17 2014-05-06 2014-01-01 2014-05-06          1  2014   125    45 [45,50)  0.342
 8     29      0 2013-03-22 NA         NA         2011-06-02 2012-01-01          0  2011   213    18 [15,20)  0.583
 9     29      0 2013-03-22 NA         NA         2012-01-01 2013-01-01          0  2012   366    19 [15,20)  1.00 
10     29      0 2013-03-22 NA         NA         2013-01-01 2013-03-22          0  2013    80    19 [15,20)  0.219
# ... with 83,053 more rows
```

Behind the scenes, the `getIncData` calls some functions that you should
already be familiar with.

``` r
getIncData 
function(rtdat, bdat, Args, func=identity) {
  dat <- Args$imputeMethod(rtdat)
  edat <- splitAtSeroDate(dat) 
  edat <- setData(edat, Args, time2="obs_end", birthdate=bdat)
  edat <- mutate(edat, tscale = Time/365.25)
  func(edat)
}
<environment: namespace:ahri>
```

It also creates a `tscale` variable that will be used later in the
calculation of the incidence using Poisson regression. In addition, you
can pass a custom function to the `func` argument in case you want to do
further data processing–for example, creating a function to restrict the
data to only circumcised men. This is useful for applying a function to
multiple dataset in the multiple imputation approach. On this point, we
also pass the birth-date function as an argument to create an age
variable for each episode, and therefore avoid having to generate the
same birthdate data at each iteration.

Calculating HIV incidence
=========================

The AggBy functions
-------------------

We are now ready to calculate the HIV incidence. The first task is to
aggregate the number of seroconversion events and person-time by either
year or age. We can do this with the `AggByYear` or `AggByAge`
functions. Starting by year:

``` r
idat_yr <- AggByYear(idat)
idat_yr
   Year sero_event   pyears
1  2008        305 7829.377
2  2009        292 7437.952
3  2010        306 7224.649
4  2011        274 7033.659
5  2012        272 6684.249
6  2013        297 6776.074
7  2014        268 6807.849
8  2015        221 6694.396
9  2016        167 6048.197
10 2017        134 4900.868
11 2018         47 2217.254
```

And by age:

``` r
# Show age groups
levels(idat$AgeCat)
[1] "[15,20)" "[20,25)" "[25,30)" "[30,35)" "[35,40)" "[40,45)" "[45,50)"
idat_age <- AggByAge(idat)
idat_age
   AgeCat sero_event    pyears
1 [15,20)        514 17933.462
2 [20,25)        966 20750.842
3 [25,30)        591 12075.828
4 [30,35)        279  6787.496
5 [35,40)        109  5298.642
6 [40,45)         96  5571.025
7 [45,50)         28  1237.229
```

Notice that the incidence is automatically calculated by age-groups,
which you would have set in the `setArgs` function. See also the
`setData` and `makeAgeVars` functions that I described previously. If
you want to calculate by different age groups, you could do:

``` r
idat2 <- makeAgeVars(idat, time2="obs_start", 
  age_cut= c(15, 20, 25, 30, 40, 50))
idat_age2 <- AggByAge(idat2)
idat_age2
   AgeCat sero_event    pyears
1 [15,20)        607 22696.893
2 [20,25)        957 18848.767
3 [25,30)        548 10777.593
4 [30,40)        358 11542.374
5 [40,50)        113  5788.246
```

The AggFunc factory
-------------------

Let’s say that you wanted to do multiple imputations on more than one
grouping variable, say by age category and sex. To do this we use the
function factory `AggFunc`, which is a wrapper around R’s `aggregate`
function. In this case, we can use R forumla syntax to pass the grouping
variables as an argument to `AggFun` like so:

``` r
AggByAgeFem = AggFunc("AgeCat + Female")
idat_age3 = AggByAgeFem(idat2)
idat_age3
    AgeCat Female sero_event    pyears
1  [15,20)      0         77 10558.122
2  [20,25)      0        232  8154.450
3  [25,30)      0        163  4494.875
4  [30,40)      0        128  4076.167
5  [40,50)      0         31  1482.245
6  [15,20)      1        530 12138.771
7  [20,25)      1        725 10694.316
8  [25,30)      1        385  6282.719
9  [30,40)      1        230  7466.207
10 [40,50)      1         82  4306.001
```

So to recap. First, you create a new function by passing a right hand
side formula of grouping variables. For example, you could expand the
example above to `("AgeCat + Female + Year")`. Then your newly created
function takes an incidence dataset as an argument.

Crude incidence
---------------

We are ready to calculate the crude incidence with 95% confidence
intervals using the Poisson exact method, which is what the
`calcPoisExact` function does.

``` r
calcPoisExact(idat_yr, byVar="Year")
     sero_event   pyears  rate   lci   uci conf.level
2008        305 7829.377 3.896 3.471 4.358       0.95
2009        292 7437.952 3.926 3.488 4.403       0.95
2010        306 7224.649 4.235 3.774 4.738       0.95
2011        274 7033.659 3.896 3.448 4.385       0.95
2012        272 6684.249 4.069 3.600 4.583       0.95
2013        297 6776.074 4.383 3.899 4.911       0.95
2014        268 6807.849 3.937 3.479 4.437       0.95
2015        221 6694.396 3.301 2.880 3.766       0.95
2016        167 6048.197 2.761 2.358 3.213       0.95
2017        134 4900.868 2.734 2.291 3.238       0.95
2018         47 2217.254 2.120 1.558 2.819       0.95

calcPoisExact(idat_age2, byVar="AgeCat")
        sero_event    pyears  rate   lci   uci conf.level
[15,20)        607 22696.893 2.674 2.466 2.896       0.95
[20,25)        957 18848.767 5.077 4.761 5.409       0.95
[25,30)        548 10777.593 5.085 4.668 5.529       0.95
[30,40)        358 11542.374 3.102 2.789 3.440       0.95
[40,50)        113  5788.246 1.952 1.609 2.347       0.95
```

Another method would be to obtain Poisson regression coefficients and
standard errors. This is what the `doPoisCrude` function does. The
function takes in a data.frame of the estimates (fit) and a data.frame
of the standard errors (se.fit) from the Poisson regression. We can then
use the function called `calcPoisCI` to obtain the confidence intervals.

``` r
pois_yr <- doPoisCrude(idat)
pois_yr
               fit     se.fit
Year2008 -3.245326 0.05725983
Year2009 -3.237597 0.05852057
Year2010 -3.161669 0.05716619
Year2011 -3.245334 0.06041221
Year2012 -3.201707 0.06063390
Year2013 -3.127421 0.05802588
Year2014 -3.234845 0.06108472
Year2015 -3.410863 0.06726728
Year2016 -3.589522 0.07738232
Year2017 -3.599328 0.08638683
Year2018 -3.853877 0.14586483

pois_yr_ci <- calcPoisCI(pois_yr)
pois_yr_ci
       rate      lci      uci
1  3.895584 3.482027 4.358260
2  3.925812 3.500385 4.402944
3  4.235500 3.786551 4.737678
4  3.895554 3.460552 4.385238
5  4.069268 3.613297 4.582779
6  4.383069 3.911881 4.911011
7  3.936632 3.492436 4.437325
8  3.301269 2.893489 3.766517
9  2.761153 2.372583 3.213362
10 2.734210 2.308330 3.238663
11 2.119739 1.592648 2.821273
```

We can also use Poisson regression to estimate HIV incidence by age.

``` r
pois_age <- doPoisAge(idat)
pois_age
              fit     se.fit
[15,20) -3.552200 0.04410452
[20,25) -3.067178 0.03217401
[25,30) -3.017145 0.04113395
[30,35) -3.191626 0.05986744
[35,40) -3.883858 0.09577491
[40,45) -4.060986 0.10204776
[45,50) -3.788425 0.18897032

pois_age_ci <- calcPoisCI(pois_age)
pois_age_ci
      rate      lci      uci
1 2.866150 2.628793 3.124939
2 4.655233 4.370734 4.958251
3 4.894075 4.514989 5.304989
4 4.110500 3.655396 4.622265
5 2.057131 1.705048 2.481917
6 1.723202 1.410819 2.104752
7 2.263121 1.562619 3.277650
```

Please note that these incidence rate estimates are for a single dataset
with one seroconversion date imputed for each HIV-positive
repeat-tester. Ideally, we would want to generate multiple imputed
datasets (as discussed in Part B of this documentation.)

Age-adjusted incidence
----------------------

We can use the `doPoisYear` and `calcPoisCI` functions to calculate the
age-standardized HIV incidence rates and 95% confidence intervals using
Poisson regression. For the `doPoisYear`, we will need to calculate
aggregate age data by year, which we do with the `getAgeYear` function.
The `getAgeYear` takes a dataset with a `Year` and `Age` variable.

``` r
hdat <- setHIV(Args)
age_dat <- getAgeYear(hdat)
```

Then:

``` r
pois_yr <- doPoisYear(idat, age_dat)
pois_yr
         fit     se.fit
1  -3.243666 0.05731502
2  -3.262580 0.06010868
3  -3.170228 0.05776607
4  -3.277319 0.06250387
5  -3.197672 0.06065363
6  -3.122304 0.05802888
7  -3.247937 0.06192436
8  -3.410526 0.06726913
9  -3.588484 0.07739038
10 -3.599044 0.08705274
11 -3.880976 0.15095628
```

We can then take the result from `doPoisYear` and calculate the
confidence intervals with `calcPoisCI`.

``` r
pois_yr_ci <- calcPoisCI(pois_yr)
pois_yr_ci
       rate      lci      uci
1  3.902059 3.487436 4.365976
2  3.828946 3.403406 4.307694
3  4.199402 3.749868 4.702826
4  3.772927 3.337905 4.264643
5  4.085722 3.627767 4.601487
6  4.405555 3.931927 4.936234
7  3.885427 3.441340 4.386820
8  3.302384 2.894456 3.767803
9  2.764021 2.375009 3.216750
10 2.734986 2.305974 3.243814
11 2.063067 1.534676 2.773383
```

So from scratch, to calculate the HIV incidence rate for one imputed
dataset, you would do:

``` r
Args <- setArgs(Years=c(2008:2018), 
  Age=list(All=c(15, 45)),
  imputeMethod=imputeRandomPoint)
age_dat <- getAgeYear(dat=setHIV(Args))
rtdat <- getRTData(dat=getHIV())
idat <- getIncData(rtdat, bdat=getBirthDate(), Args)
pois_yr <- doPoisYear(idat, age_dat)
calcPoisCI(pois_yr)
       rate      lci      uci
1  3.780344 3.371960 4.238188
2  3.696543 3.278260 4.168196
3  4.181626 3.729800 4.688186
4  4.031682 3.586090 4.532641
5  4.001692 3.544991 4.517231
6  3.969061 3.518640 4.477141
7  3.949249 3.502682 4.452751
8  3.329625 2.919203 3.797750
9  2.801028 2.408916 3.256965
10 2.539980 2.127044 3.033080
11 2.254026 1.705546 2.978889
```

In the above code, you can change the imputation method and other
variable such as age and sex using the `setArgs` function. In addition,
you could run your own Poisson model on the data generated from
`getIncData`. To see how to do this, you can use the code from
`doPoisYear` or `doPoisAge` as a template. Here, you will have to
familiarize yourself with the `predict` function in R.

``` r
doPoisYear
function(dat, 
  age_dat=eval.parent(quote(age_dat))) {
  dat <- mutate(dat, Year = as.factor(.data$Year))
  mod <- stats::glm(sero_event ~ -1 + Year + Age + Year:Age 
    + offset(log(tscale)), data=dat, family=poisson)
  data.frame(predict.glm(mod, age_dat, se.fit=TRUE)[c(1,2)])
}
<environment: namespace:ahri>

doPoisAge
function(dat) {
  mod <- stats::glm(sero_event ~ AgeCat + offset(log(tscale)),
    data=dat, family=poisson)
  nage <- seq(unique(dat$AgeCat))
  ndat <- data.frame(tscale=1,
    AgeCat = factor(nage, levels = nage, labels = levels(dat$AgeCat)))
  out <- data.frame(predict.glm(mod, ndat, se.fit=TRUE)[c(1, 2)])
  rownames(out) <- ndat$AgeCat
  out
}
<environment: namespace:ahri>
```

The above code shows how you can use specific functions from the `arhi`
library to build more complex functions or do a variety of tasks.

(This document was compiled with `ahri` version 0.9.1 )
