-   [Making variables](#making-variables)
    -   [The makeAgeVars function](#the-makeagevars-function)
    -   [The makeMigrVars and addMigrVars
        functions](#the-makemigrvars-and-addmigrvars-functions)
    -   [The makePropRes function](#the-makepropres-function)
-   [Getting variables](#getting-variables)
    -   [The getBSMax function](#the-getbsmax-function)
    -   [The getDates function](#the-getdates-function)
    -   [The getEverART and calcARTCov
        functions](#the-geteverart-and-calcartcov-functions)

In this document I discuss a range of functions for making new variables
or getting variables from existing datasets.

Making variables
================

The makeAgeVars function
------------------------

We have seen with the `setData` function that an `Age` variable will be
created if one doesn’t exist. Under the hood, `setData` is calling a
function called `makeAgeVars`. This function will also create an age
category variable called `AgeCat`.

``` r
Args <- setArgs(Years=c(2005:2018), Age=list(All=c(15, 49)))
hiv <- setHIV(Args)
levels(hiv$AgeCat)
[1] "[15,20)" "[20,25)" "[25,30)" "[30,35)" "[35,40)" "[40,45)" "[45,50)"
```

The length of the categories is determined by an argument to
`setArgs$AgeBy`, with a default length of 5 years. You can change this
length. For example, let’s say we want 10 year age categories.

``` r
Args <- setArgs(Years=c(2005:2018), Age=list(All=c(15, 49)), AgeBy=10)
hiv <- setHIV(Args)
levels(hiv$AgeCat)
[1] "[15,25)" "[25,35)" "[35,45)" "[45,55)"
```

Or you could create custom age categories using the `AgeCat` argument:

``` r
Args <- setArgs(Years=c(2005:2018), 
  Age=list(All=c(15, 49)), AgeCat=c(15, 20, 25, 30, 40, 50))
hiv <- setHIV(Args)
levels(hiv$AgeCat)
[1] "[15,20)" "[20,25)" "[25,30)" "[30,40)" "[40,50)"
```

We can generate an `AgeCat` variable for any dataset so long as we have
an `Age` or `time2` variable.

``` r
Args <- setArgs(Years=c(2005:2018), 
  Age=list(All=c(15, 49)), AgeCat=c(15, 20, 25, 30, 40, 50))
hiv <- getHIV() 
# Age is an existing variable in hiv
hiv <- setAge(hiv, Args)
hiv <- makeAgeVars(hiv, age_cut=Args$AgeCat)
levels(hiv$AgeCat)
[1] "[15,20)" "[20,25)" "[25,30)" "[30,40)" "[40,50)"
```

The makeMigrVars and addMigrVars functions
------------------------------------------

This function creates two migration related variables from the Episodes
dataset: a variable that measures the cumulative time spent outside the
PIP surveillance area, and the number of in and out migration events of
the PIP surveillance in a year.

``` r
Args <- setArgs(Year=c(2010:2018))
mdat <- makeMigrVars(Args)
mdat
# A tibble: 532,421 x 8
   IIntID  Year DaysIn DaysOut InMigr OutMigr MigrCount CumTimeOut
    <int> <int>  <dbl>   <dbl>  <dbl>   <dbl>     <dbl>      <dbl>
 1     15  2012    185     181      1       0         1       0.49
 2     15  2013    365       1      0       0         0       0.25
 3     15  2014    365       1      0       0         0       0.17
 4     15  2015    365       1      0       0         0       0.13
 5     15  2016    366       0      0       0         0       0.1 
 6     15  2017      2     364      0       0         0       0.25
 7     17  2010    365       1      0       0         0       0   
 8     17  2011    365       1      0       0         0       0   
 9     17  2012    366       0      0       0         0       0   
10     17  2013    365       1      0       0         0       0   
# ... with 532,411 more rows
```

The `addMigrVars` function then merges (left-join) these variables with
an existing dataset, which must have the `IIntID` and `Year` variables.
By default, the function will carry any missing values from the
`MigrCount` and `CumTimeOut` variables forwards and backwards.

``` r
epi <- setEpisodes(Args)
epi2 <- addMigrVars(epi, mdat, carry=TRUE)
epi2
# A tibble: 1,752,042 x 25
   IIntID BSIntID Female   Age DoB        DoD         Year ExpDays ObservationStart ObservationEnd
    <int>   <int>  <int> <dbl> <date>     <date>     <int>   <dbl> <date>           <date>        
 1     15    8507      0    34 1977-08-05 NA          2012      36 2012-06-30       2012-08-04    
 2     15    8507      0    35 1977-08-05 NA          2012     149 2012-08-05       2012-12-31    
 3     15    8507      0    35 1977-08-05 NA          2013      59 2013-01-01       2013-02-28    
 4     15    8507      0    35 1977-08-05 NA          2013     157 2013-03-01       2013-08-04    
 5     15    8507      0    36 1977-08-05 NA          2013     149 2013-08-05       2013-12-31    
 6     15    8507      0    36 1977-08-05 NA          2014     216 2014-01-01       2014-08-04    
 7     15    8507      0    37 1977-08-05 NA          2014      20 2014-08-05       2014-08-24    
 8     15    8507      0    37 1977-08-05 NA          2014     129 2014-08-25       2014-12-31    
 9     15    8507      0    37 1977-08-05 NA          2015     216 2015-01-01       2015-08-04    
10     15    8507      0    38 1977-08-05 NA          2015      20 2015-08-05       2015-08-24    
# ... with 1,752,032 more rows, and 15 more variables: InMigration <dbl+lbl>, OutMigration <dbl+lbl>,
#   Resident <dbl+lbl>, AssetIndex <dbl>, EarliestHIVPos <date>, EarliestARTInitDate <date>,
#   OnART <dbl+lbl>, PIPSA <chr>, AgeCat <fct>, DaysIn <dbl>, DaysOut <dbl>, InMigr <dbl>, OutMigr <dbl>,
#   MigrCount <dbl>, CumTimeOut <dbl>
```

The makePropRes function
------------------------

This function will make a variable called `PropRes`, which measures the
proportion of time (in days) that a participant spent in the PIP
surveillance area for each year under observation. This variable is
useful if you want to include only observations in your analysis where
the participant spent more than, for example, 50% of the year in the PIP
surveillance area.

``` r
Args <- setArgs(Years=c(2012:2018), Age=list(All=c(15, 49)))
adat <- makePropRes(Args)
adat 
# A tibble: 392,055 x 3
   IIntID  Year PropRes
    <int> <int>   <dbl>
 1     15  2012   0.505
 2     15  2013   0.997
 3     15  2014   0.997
 4     15  2015   0.997
 5     15  2016   1    
 6     15  2017   0.005
 7     17  2012   1    
 8     17  2013   0.997
 9     17  2014   0.997
10     17  2015   0.997
# ... with 392,045 more rows
dat <- setEpisodes(Args) 
dat <- dplyr::left_join(dat, adat, by=c("IIntID", "Year"))
dat <- dplyr::select(dat, IIntID, Year, ExpDays, Resident, PropRes)
dat2 <- dplyr::filter(dat, PropRes >= 0.5)
dat2
# A tibble: 718,354 x 5
   IIntID  Year ExpDays  Resident PropRes
    <int> <int>   <dbl> <dbl+lbl>   <dbl>
 1     15  2012      36         1   0.505
 2     15  2012     149         1   0.505
 3     15  2013      59         1   0.997
 4     15  2013     157         1   0.997
 5     15  2013     149         1   0.997
 6     15  2014     216         1   0.997
 7     15  2014      20         1   0.997
 8     15  2014     129         1   0.997
 9     15  2015     216         1   0.997
10     15  2015      20         1   0.997
# ... with 718,344 more rows
```

Getting variables
=================

We have aleady seen an example of this type of function with
`getBirthDate`; there are several others.

The getBSMax function
---------------------

This function gets the most time a participant spent in a bounded
structure (BS) in a given year. Note that a participant can reside in
multiple bounded structures (BS) throughout the year, but can only be
physically present at one BS at any point in time. Such data is often
used to produce an input dataset for a geospatial analysis, where only
one physical location per participant per year is allowed.

``` r
bmax <- getBSMax()
bmax
# A tibble: 1,403,031 x 3
   IIntID  Year BSIntID
    <int> <int>   <int>
 1     11  2000    2830
 2     11  2001    2830
 3     11  2002    2830
 4     11  2003    2830
 5     11  2004    2830
 6     12  2000    2460
 7     12  2001    2460
 8     12  2002    2460
 9     12  2003    2460
10     12  2004    2460
# ... with 1,403,021 more rows
```

The getDates function
---------------------

-   The `getDates` function gets the earliest and latest dates of a date
    variable by `IIntID`. This is useful for determining, for example,
    the latest HIV-negative or the earliest HIV-positive test or the
    last visit date.

-   Specifically, the `getDatesMin` function gets the earliest (minimum)
    date of a variable. The first argument is a data.frame, the second
    the name of the date variable, and the third argument is the name of
    the new variable.

-   The `getDateMax` does the same thing, but gets the latest (maximum)
    date from an existing variable.

``` r
hiv <- getHIV()
# Gets the earliest HIV-negative test date
getDatesMin(hiv, "HIVNegative", "early_neg")
# A tibble: 42,238 x 2
   IIntID early_neg 
    <int> <date>    
 1     16 2009-06-02
 2     17 2004-02-09
 3     19 2009-08-04
 4     22 2006-06-01
 5     28 2004-10-07
 6     29 2011-06-02
 7     30 2004-09-19
 8     34 2004-10-04
 9     36 2011-05-21
10     41 2011-05-22
# ... with 42,228 more rows
# Gets the latest HIV-negative test date
getDatesMax(hiv, "HIVNegative", "late_neg")
# A tibble: 42,238 x 2
   IIntID late_neg  
    <int> <date>    
 1     16 2011-05-23
 2     17 2011-08-06
 3     19 2014-08-06
 4     22 2015-07-15
 5     28 2006-10-06
 6     29 2013-03-22
 7     30 2011-05-21
 8     34 2018-08-08
 9     36 2013-03-23
10     41 2011-05-22
# ... with 42,228 more rows
# Gets the earliest HIV-positive test date
getDatesMin(hiv, "HIVPositive", "late_neg")
# A tibble: 15,489 x 2
   IIntID late_neg  
    <int> <date>    
 1     17 2017-10-17
 2     23 2004-10-07
 3     25 2004-10-19
 4     26 2016-07-13
 5     27 2011-07-06
 6     33 2009-05-13
 7     39 2009-01-30
 8     42 2004-10-24
 9     47 2016-11-24
10     72 2016-02-26
# ... with 15,479 more rows
```

These functions are used with the HIV data and in the `getRTData`
function to determine the repeat-testers for HIV. Here is an example
with the Episodes dataset, which gets the earliest episode start date.

``` r
epi <- getEpisodes()
getDatesMin(epi, "ObservationStart", "obs_start")[140000:1400008, ]
# A tibble: 1,260,009 x 2
   IIntID obs_start 
    <int> <date>    
 1 152421 2009-04-21
 2 152422 2010-01-01
 3 152423 2009-12-07
 4 152424 2010-01-01
 5 152425 2010-01-26
 6 152426 2009-10-06
 7 152427 2009-10-06
 8 152428 2009-10-06
 9 152429 2009-10-06
10 152430 2009-10-06
# ... with 1,259,999 more rows
```

The getEverART and calcARTCov functions
---------------------------------------

The `getEverART` function determines if HIV-positive participants were
ever on ART. It takes the dataset from `getEpisodes` as input.

``` r
getEverART(dat=getEpisodes())
# A tibble: 400,813 x 10
   IIntID  Year   Age Female DateOfInitiation EarliestHIVPos     OnART YearPos YearOfInitiation EverART
    <int> <int> <dbl>  <int> <date>           <date>         <dbl+lbl>   <int>            <int>   <int>
 1     17  2017    48      1 2015-01-15       2017-10-17             1    2017             2015       1
 2     17  2017    48      1 2015-01-15       2017-10-17             3    2017             2015       1
 3     17  2017    48      1 2015-01-15       2017-10-17             1    2017             2015       1
 4     17  2017    48      1 2015-01-15       2017-10-17             1    2017             2015       1
 5     17  2017    48      1 2015-01-15       2017-10-17             3    2017             2015       1
 6     17  2017    48      1 2015-01-15       2017-10-17             1    2017             2015       1
 7     17  2017    48      1 2015-01-15       2017-10-17             3    2017             2015       1
 8     17  2017    49      1 2015-01-15       2017-10-17             3    2017             2015       1
 9     17  2017    49      1 2015-01-15       2017-10-17             1    2017             2015       1
10     17  2018    49      1 2015-01-15       2017-10-17             1    2017             2015       1
# ... with 400,803 more rows
```

We can use this function to get a crude estimate of ART coverage in the
study area. For this we use the `calcARTCov` function, which takes the
dataset from `getEverART` as well as a list of arguments from `setArgs`.

``` r
Args <- setArgs(Years=c(2010:2018), Age=list(All=c(15, 49)))
calcARTCov(dat=getEverART(), Args)
  Year     N crude.rate   lci   uci
1 2010 20578      25.37 24.77 25.97
2 2011 22243      31.31 30.70 31.92
3 2012 22306      37.42 36.78 38.05
4 2013 26151      45.04 44.44 45.65
5 2014 27339      47.87 47.28 48.47
6 2015 31498      54.70 54.14 55.25
7 2016 36723      58.36 57.86 58.87
8 2017 38464      57.67 57.17 58.16
9 2018 30083      54.59 54.03 55.16
```

Please note that this is likely to be an overestimate of ART coverage
given that perfect adherence is assumed from the date of initiation, so
use with caution. As of yet, no-one has systematically looked at getting
a robust and accurate ART coverage measure from the AHRI data, which
would likely involve some advanced imputation or Bayesian procedure.

(This document was compiled with `ahri` version 0.8.9 )

(This document was compiled with `ahri` version 0.8.9 )
