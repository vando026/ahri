-   [Setting the data](#setting-the-data)
    -   [The setArgs function](#the-setargs-function)
    -   [The setHIV and setEpisodes
        functions](#the-sethiv-and-setepisodes-functions)
    -   [The setAge function](#the-setage-function)
    -   [The getBirthDate function](#the-getbirthdate-function)
    -   [The setData function](#the-setdata-function)
    -   [The dropTasP function](#the-droptasp-function)

Setting the data
================

The setArgs function
--------------------

An important and useful function for managing settings and parameters
throughout various `ahri` functions is the `setArgs` function. The
general idea is to set and keep paramaters in a single function called
`setArgs`, which is assigned to a name, and that name is passed as an
argument to other functions.

You can see the arguments in the help file if you type `?setArgs` or
print them to the console.

``` r
Args <- setArgs()
# This will show default arguments
Args$Years
 [1] 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025
Args$Age
$All
[1]   0 100
```

`setArgs` is most useful when trying to set the data, for example, by
year, sex, and age. We just set the arguments once, and use them for the
relevant functions. Let’s say we only want to work with observations
from years 2010 to 2018 and for men and women aged 15 to 49 years. We
pass these conditions to the `Years` and `Age` arguments:

``` r
Args <- setArgs(
  Years=c(2010:2018), 
  Age=list(All=c(15, 49)))
```

The setHIV and setEpisodes functions
------------------------------------

The `setHIV` and `setEpisodes` are two functions that take `setArgs` as
an argument.

``` r
hiv <- setHIV(Args)
sort(unique(hiv$Year))
[1] 2010 2011 2012 2013 2014 2015 2016 2017 2018
unique(hiv$Female)
[1] 1 0
summary(hiv$Age)[c(1, 6)]
Min. Max. 
  15   49 
```

The `hiv` dataset is now subset by year, age, and sex. We can restrict
the data to only men aged 15 to 25 years as follows.

``` r
Args <- setArgs(Years=c(2010:2018), 
  Age=list(Mal=c(15, 25)))
hiv <- setHIV(Args)
unique(hiv$Female)
[1] 0
summary(hiv$Age)[c(1, 6)]
Min. Max. 
  15   25 
```

We can set different ages for men and women:

``` r
Args <- setArgs(Years=c(2010:2018), 
  Age=list(Mal=c(15, 25), Fem=c(15, 35)))
hiv <- setHIV(Args)
summary(hiv$Age[hiv$Female==1])[c(1, 6)]
Min. Max. 
  15   35 
summary(hiv$Age[hiv$Female==0])[c(1, 6)]
Min. Max. 
  15   25 
```

This is the same for the `setEpisodes` function.

``` r
Args <- setArgs(
  Years=c(2005:2018),
  Age=list(All=c(25, 40)))
epi <- setEpisodes(Args)
sort(unique(epi$Year))
 [1] 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018
summary(epi$Age)[c(1, 6)]
Min. Max. 
  25   40 
```

The setAge function
-------------------

The `setAge` function can set the age by sex for any dataset, providing
it has the required `Age` and `Female` variables.

``` r
hiv <- getHIV()
Args <- setArgs(Years=c(2005:2018), Age=list(Fem=c(15, 19), Mal=c(15, 25)))
hiv <- setAge(hiv, Args)
summary(hiv$Age[hiv$Female==1])[c(1, 6)]
Min. Max. 
  15   19 
summary(hiv$Age[hiv$Female==0])[c(1, 6)]
Min. Max. 
  15   25 
```

The getBirthDate function
-------------------------

A useful function is `getBirthDate`, which extracts the `DoB` (Date of
Birth) variable from the `getEpisodes` dataset.

``` r
bdat <- getBirthDate()
bdat
# A tibble: 174,293 x 2
   IIntID DateOfBirth
    <int> <date>     
 1     11 1945-01-10 
 2     12 1940-12-26 
 3     13 1961-03-07 
 4     14 1974-12-24 
 5     15 1977-08-05 
 6     16 1952-11-16 
 7     17 1968-06-05 
 8     18 1979-04-05 
 9     19 1939-08-18 
10     20 1973-06-25 
# ... with 174,283 more rows
```

This is mainly a helper fuction used elsewhere, as demonstrated below.

The setData function
--------------------

For any dataset, the `setData` function will subset by year, age, and
sex. For this function, the `Year` and `Female` variables are needed. If
the `Age` variable is not available, then `setData` will create one for
you.

For example, pretend you just created a dataset that doesnt have an age
variable, and all you have is the start or end date of the episode, and
the date of birth. This is typically the case when you split data into
episodes, and you need to recalculate the age by episode. Using the
`getBirthDate` function, we can get the birth dates and then pass that
data as an argument to `setData`.

``` r
epi <- getEpisodes()
# No Age variable
epi2 <- dplyr::select(epi, IIntID, Year, Female, ObservationStart, ObservationEnd)
names(epi2)
[1] "IIntID"           "Year"             "Female"           "ObservationStart" "ObservationEnd"  
Args <- setArgs(Years=c(2005:2018), Age=list(All=c(15, 35)))
# Make age at the start of the episode
epi3 <- setData(epi2, Args, time2="ObservationStart", birthdate=getBirthDate())
summary(epi3$Age)[c(1, 6)]
Min. Max. 
  15   35 
```

You can also pass on a custom function to `setData` through `setArgs` to
do further data processing. For example, let’s keep participants
according to some value defined in the custom function below.

``` r
dropID <- function(dat) {
  dat <- dplyr::mutate(dat, Val = rbinom(nrow(dat), 1, 0.4))
  # keep participants only if val==1
  dplyr::filter(dat, Val==1)
}
Args <- setArgs(Years=c(2005:2018), Age=list(All=c(15, 19)), setFun=dropID)
epi4 <- setData(epi2, Args, time2="ObservationStart", birthdate=getBirthDate())
epi4
# A tibble: 223,821 x 8
   IIntID  Year Female ObservationStart ObservationEnd   Age AgeCat    Val
    <int> <int>  <int> <date>           <date>         <dbl> <fct>   <int>
 1     21  2005      0 2005-05-19       2005-07-17        15 [15,20)     1
 2     21  2005      0 2005-07-18       2005-08-22        15 [15,20)     1
 3     21  2005      0 2005-10-14       2005-12-31        15 [15,20)     1
 4     21  2006      0 2006-01-01       2006-04-27        15 [15,20)     1
 5     21  2006      0 2006-11-29       2006-12-31        16 [15,20)     1
 6     29  2009      0 2009-09-06       2009-12-31        15 [15,20)     1
 7     29  2010      0 2010-01-01       2010-09-05        16 [15,20)     1
 8     29  2011      0 2011-06-02       2011-07-20        17 [15,20)     1
 9     29  2011      0 2011-07-21       2011-09-05        17 [15,20)     1
10     29  2011      0 2011-11-21       2011-12-31        18 [15,20)     1
# ... with 223,811 more rows
```

The `setFun` argument in `setArgs` becomes useful when you have to
process data in the ith iteration of i = 1,…K iterations.

The dropTasP function
---------------------

Earlier we saw the use of `dropTasP` as an argument in `readEpisodes`
and `readHIVData`. This is a standalone function that can be used for
any dataset so long as it has a `BSIntID` variable. For example, let’s
say we read in the data with TasP observations and later we want to drop
these observations.

``` r
hiv <- readHIVData(dropTasP=FALSE, write_rda=FALSE)
nrow(hiv)
[1] 173110
hiv1 <- dropTasPData(hiv)
nrow(hiv1)
[1] 160836
```

(This document was compiled with `ahri` version 0.9.2 )
