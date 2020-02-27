-   [Setting the data](#setting-the-data)
    -   [The setArgs function](#the-setargs-function)
    -   [The setHIV and setEpisodes
        functions](#the-sethiv-and-setepisodes-functions)
    -   [The setAge function](#the-setage-function)
    -   [The getBirthDate function](#the-getbirthdate-function)
    -   [The setData function](#the-setdata-function)
    -   [The dropTasP function](#the-droptasp-function)
-   [Making variables](#making-variables)
    -   [The makeAgeVars function](#the-makeagevars-function)

Setting the data
================

The setArgs function
--------------------

An important and useful function for managing settings and parameters
throughout various `ahri` functions is the `setArgs` function. The
general idea is to set and keep paramaters in a single function called
`setArgs`, which is assigned to a name, and that name is passed as an
argument to other functions.

This is most useful when trying to set the data, for example, by year,
sex, and age. Let’s say we only want to work with observations from
years 2010 to 2018 and for men and women aged 15 to 49 years. We pass
these conditions to the `Years` and `Age` arguments:

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
Args <- setArgs(Age=list(Fem=c(15, 19), Mal=c(15, 25)))
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
epi2 <- select(epi, IIntID, Year, Female, ObservationStart, ObservationEnd)
names(epi2)
[1] "IIntID"           "Year"             "Female"           "ObservationStart" "ObservationEnd"  
Args <- setArgs(Age=list(All=c(15, 35)))
# Make age at the start of the episode
epi3 <- setData(epi2, Args, time2="ObservationStart", birthdate=getBirthDate())
summary(epi3$Age)[c(1, 6)]
Min. Max. 
  15   35 
```

You can also pass on a custom function to `setArgs` to do further data
processing. For example, let’s drop participants according to some
criteria defined in the custom function.

``` r
dropID <- function(dat) {
  dat <- mutate(dat, Val = rbinom(nrow(dat), 1, 0.4))
  filter(dat, Val==1)
}
Args <- setArgs(Age=list(All=c(15, 19)), setFun=dropID)
epi4 <- setData(epi2, Args, time2="ObservationStart", birthdate=getBirthDate())
epi4
# A tibble: 223,714 x 8
   IIntID  Year Female ObservationStart ObservationEnd   Age AgeCat    Val
    <int> <int>  <int> <date>           <date>         <dbl> <fct>   <int>
 1     21  2005      0 2005-10-14       2005-12-31        15 [15,20)     1
 2     21  2007      0 2007-01-01       2007-04-10        16 [15,20)     1
 3     29  2009      0 2009-03-29       2009-09-05        15 [15,20)     1
 4     29  2010      0 2010-01-01       2010-09-05        16 [15,20)     1
 5     29  2011      0 2011-04-13       2011-06-01        17 [15,20)     1
 6     29  2013      0 2013-01-01       2013-01-13        19 [15,20)     1
 7     29  2013      0 2013-01-26       2013-03-22        19 [15,20)     1
 8     29  2013      0 2013-03-23       2013-09-05        19 [15,20)     1
 9     32  2013      0 2013-01-01       2013-01-05        16 [15,20)     1
10     32  2013      0 2013-01-06       2013-01-25        16 [15,20)     1
# ... with 223,704 more rows
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
hiv <- readHIVData(dropTasP=FALSE, write=FALSE)
nrow(hiv)
[1] 173110
hiv1 <- dropTasPData(hiv)
nrow(hiv1)
[1] 160836
```

Making variables
================

Some functions can also make and add variables to your existing dataset.
Provided that the existing data has the required variables.

The makeAgeVars function
------------------------

We have seen with the `setData` function that an `Age` variable will be
created if one doesn’t exist. Under the hood, `setData` is calling a
function called `makeAgeVars`. This function will also create an age
category variable called `AgeCat`.

``` r
Args <- setArgs(Age=list(All=c(15, 49)))
hiv <- setHIV(Args)
levels(hiv$AgeCat)
[1] "[15,20)" "[20,25)" "[25,30)" "[30,35)" "[35,40)" "[40,45)" "[45,50)"
```

The length of the categories is determined by an argument to
`setArgs$AgeBy`, with a default length of 5 years. You can change this
length. For example, let’s say we want 10 year age categories.

``` r
Args <- setArgs(Age=list(All=c(15, 49)), AgeBy=10)
hiv <- setHIV(Args)
levels(hiv$AgeCat)
[1] "[15,25)" "[25,35)" "[35,45)" "[45,55)"
```

Or you could create custom age categories using the `AgeCat` argument:

``` r
Args <- setArgs(Age=list(All=c(15, 49)), AgeCat=c(15, 20, 25, 30, 40, 50))
hiv <- setHIV(Args)
levels(hiv$AgeCat)
[1] "[15,20)" "[20,25)" "[25,30)" "[30,40)" "[40,50)"
```

We can generate an `AgeCat` variable for any dataset so long as we have
an `Age` or `time2` variable.

``` r
Args <- setArgs(Age=list(All=c(15, 49)), AgeCat=c(15, 20, 25, 30, 40, 50))
hiv <- getHIV() 
# Age is an existing variable in hiv
hiv <- setAge(hiv, Args)
hiv <- makeAgeVars(hiv, age_cut=Args$AgeCat)
levels(hiv$AgeCat)
[1] "[15,20)" "[20,25)" "[25,30)" "[30,40)" "[40,50)"
```
