-   [Reading in the data](#reading-in-the-data)
-   [The readHIVData function](#the-readhivdata-function)
    -   [The dropTasP argument](#the-droptasp-argument)
    -   [The write\_rda argument](#the-write_rda-argument)
    -   [An example](#an-example)
-   [The readEpisodes function](#the-readepisodes-function)
-   [The readHealthData function](#the-readhealthdata-function)
-   [The getBSData function](#the-getbsdata-function)
-   [Package philosophy](#package-philosophy)
    -   [Version control](#version-control)
    -   [Motivation and performance](#motivation-and-performance)

Reading in the data
===================

In this document, I describe functions for reading in the .dta datasets,
writing the .Rda datasets, and loading them into memory.

The readHIVData function
========================

The `readHIVData` reads in the `RD05-99 ACDIS HIV All.dta` dataset, does
some basic data processing, and harmonizes variable names for future
merging. You need only to run the `readHIVData` function once, which
will save an .Rda dataset to the same folder with your default .dta
datasets.

``` r
readHIVData()
```

The .Rda file path is:

``` r
getFiles()$hiv_rda
[1] "~/AHRI_Data/2019/ACDIS_HIV_All.Rda"
```

Once you have generated the .Rda file, you can run `getHIV` to load in
the data quickly and repeatedly.

``` r
hiv <- getHIV()
hiv
# A tibble: 160,836 x 10
   IIntID BSIntID VisitDate  HIVResult Female   Age PIPSA          HIVNegative HIVPositive  Year
    <int>   <int> <date>         <dbl>  <int> <dbl> <chr>          <date>      <date>      <int>
 1     16    2830 2009-06-02         0      1    56 Southern PIPSA 2009-06-02  NA           2009
 2     16    2830 2011-05-23         0      1    58 Southern PIPSA 2011-05-23  NA           2011
 3     17    9274 2004-02-09         0      1    35 Southern PIPSA 2004-02-09  NA           2004
 4     17    9274 2005-06-02         0      1    36 Southern PIPSA 2005-06-02  NA           2005
 5     17    9274 2006-06-01         0      1    37 Southern PIPSA 2006-06-01  NA           2006
 6     17    9274 2010-08-19         0      1    42 Southern PIPSA 2010-08-19  NA           2010
 7     17    9274 2011-08-06         0      1    43 Southern PIPSA 2011-08-06  NA           2011
 8     17   18272 2017-10-17         1      1    49 Southern PIPSA NA          2017-10-17   2017
 9     17   18272 2018-10-03         1      1    50 Southern PIPSA NA          2018-10-03   2018
10     19    7923 2009-08-04         0      1    69 Southern PIPSA 2009-08-04  NA           2009
# ... with 160,826 more rows
```

The dropTasP argument
---------------------

`readHIVData` can perform one important task, which is to keep or drop
observations from the Treatment-as-Prevention (TasP) trial areas. In
2017, TasP areas to the North of the PIP surveillance area were added to
the datasets. As the default behavior, and to keep the analysis
consistent with previous years, `readHIVData` drops TasP observations
using the `dropTasP=TRUE` argument. If you want to include the TasP
study areas then set `dropTasP=FALSE`.

``` r
readHIVData(dropTasP=FALSE)
```

The write\_rda argument
-----------------------

Note that the default behavior of `readHIVData` is to write an .Rda
dataset. This means that the default to drop TasP areas will be written
to the .Rda dataset. This decision will persist through all `ahri`
functions that call the .Rda dataset. If you don’t want `readHIVData` to
write to file, then you can do:

``` r
readHIV <- readHIVData(dropTasP=TRUE, write_rda=FALSE)
```

which will assign the dataset to the name `readHIV` (or whatever name
you choose) in the gobal environment.

An example
----------

To make these behaviors more concrete, consider the example below. Let’s
say we want to compare the difference in the total number of
observations before and after dropping the TasP areas.

``` r
# Write rda to file and global environment using default behavior
dat_on_file = readHIVData(dropTasP=TRUE, write_rda=TRUE)
# Keep TasP areas, dont write to file
dat_keep_tasp = readHIVData(dropTasP=FALSE, write_rda=FALSE)
n_keep_tasp = nrow(dat_keep_tasp)
n_keep_tasp
[1] 173110
# Drop TasP areas dont write to file
dat_drop_tasp = readHIVData(dropTasP=TRUE, write_rda=FALSE)
n_drop_tasp = nrow(dat_drop_tasp)
n_drop_tasp
[1] 160836
# difference in obs
n_keep_tasp - n_drop_tasp
[1] 12274
```

Now the question is: what will be the total number of observations of
`getHIV` be, will it include the TasP observations or not? (Hint: look
at `dat_on_file` in the code above.)

``` r
identical(nrow(getHIV()), nrow(dat_on_file))
[1] TRUE
```

The answer is that the `getHIV` observations will be the same as
`dat_on_file` because `getHIV` calls the Rda dataset made by
`readHIVData(dropTasP=TRUE, write_rda=TRUE)`.

The readEpisodes function
=========================

With the large `SurveillanceEpisodesExtended.dta` dataset, you run
`readEpisodes`:

``` r
readEpi <- readEpisodes()
# Or if you want to keep TasP obs.
readEpi <- readEpisodes(dropTasP=FALSE)
```

On my system, it takes almost a minute to read in the
`SurveillanceEpisodesExtended.dta` dataset. Once the corresponding .Rda
data file has been generated it takes less than a second or so to load
it into memory.

``` r
epidat <- getEpisodes()
epidat
# A tibble: 5,612,161 x 18
   IIntID BSIntID Female   Age DoB        DoD         Year ExpDays ObservationStart ObservationEnd
    <int>   <int>  <int> <dbl> <date>     <date>     <int>   <dbl> <date>           <date>        
 1     11    2830      0    54 1945-01-10 2004-12-12  2000       9 2000-01-01       2000-01-09    
 2     11    2830      0    55 1945-01-10 2004-12-12  2000      33 2000-01-10       2000-02-11    
 3     11    2830      0    55 1945-01-10 2004-12-12  2000     324 2000-02-12       2000-12-31    
 4     11    2830      0    55 1945-01-10 2004-12-12  2001       9 2001-01-01       2001-01-09    
 5     11    2830      0    56 1945-01-10 2004-12-12  2001     356 2001-01-10       2001-12-31    
 6     11    2830      0    56 1945-01-10 2004-12-12  2002       9 2002-01-01       2002-01-09    
 7     11    2830      0    57 1945-01-10 2004-12-12  2002     172 2002-01-10       2002-06-30    
 8     11    2830      0    57 1945-01-10 2004-12-12  2002     184 2002-07-01       2002-12-31    
 9     11    2830      0    57 1945-01-10 2004-12-12  2003       9 2003-01-01       2003-01-09    
10     11    2830      0    58 1945-01-10 2004-12-12  2003     356 2003-01-10       2003-12-31    
# ... with 5,612,151 more rows, and 8 more variables: InMigration <dbl+lbl>, OutMigration <dbl+lbl>,
#   Resident <dbl+lbl>, AssetIndex <dbl>, EarliestHIVPos <date>, EarliestARTInitDate <date>,
#   OnART <dbl+lbl>, PIPSA <chr>
```

The `SurveillanceEpisodesExtended.dta` dataset is a bit overwhelming, so
by default the `readEpisodes` keeps only variables related to migration
(and one or two others), for which this dataset is best suited. You can
however include addtional variables using the `addVars` argument. For
example, let’s say you are interested in adding the employment
variables, then:

``` r
readEpi <- readEpisodes(addVars="UnEmployment|CurrentlyEmployed")
```

Behind the hood, the `addVars` argument is a regular expression, hence
variable names must be separated as shown above. However, since “Employ”
is the common substring, you could achieve the same outcome using:

``` r
readEpi <- readEpisodes(addVars="Employ")
```

The file path of the .Rda file is:

``` r
getFiles()$epi_rda
[1] "~/AHRI_Data/2019/SurveillanceEpisodesExtended.Rda"
```

If you do not want `readEpisodes` to write to file, then do:

``` r
epidat <- readEpisodes(write_rda=FALSE)
```

Please note that the default behavior of `readEpisodes` is to drop
observations from the TasP areas.

``` r
readEpisodes(dropTasP=TRUE, write_rda=TRUE)
```

The readHealthData function
===========================

AHRI has seperate health datasets for men and women. We can use the
`readHealthData` function to read in these datasets. Lets read in the
men and then the women. Again, the default
`RD03-99 ACDIS WGH/MGH ALL.dta` dataset is read and the corresponding
.Rda file is saved. Set `Female=1` to read in the women’s version
otherwise `Female=0` for the men’s version.

``` r
mgh <- readHealthData(Female=0)
getFiles()$mgh_rda
[1] "~/AHRI_Data/2019/ACDIS_MGH_ALL.Rda"
wgh <- readHealthData(Female=1)
getFiles()$wgh_rda
[1] "~/AHRI_Data/2019/ACDIS_WGH_ALL.Rda"
```

We can load the men or women’s .Rda dataset into memory quickly using:

``` r
mgh_dat <- getMGH()
wgh_dat <- getWGH()
```

The getBSData function
======================

We can read and load the `RD01-03 ACDIS BoundedStructures.dta` dataset
using the `getBSData` function.

``` r
bs_dat <- getBSData()
bs_dat
# A tibble: 30,776 x 18
   BSIntID BSStartDate BSStartType BSEndDate  BSEndType Isigodi LocalArea  Ward IsUrbanOrRural PIPSA
     <int> <date>        <dbl+lbl> <date>     <dbl+lbl> <chr>   <chr>     <dbl> <chr>          <chr>
 1      11 2000-01-01           22 2018-02-10        10 KwaMsa~ KwaMsane~     1 Urban          Sout~
 2      12 2000-01-01           22 9997-01-01       200 KwaMsa~ KwaMsane~     1 Urban          Sout~
 3      13 2000-01-01           22 2009-03-17        10 KwaMsa~ KwaMsane~     1 Urban          Sout~
 4      14 2000-01-01           22 9997-01-01       200 KwaMsa~ KwaMsane~     1 Urban          Sout~
 5      15 2000-01-01           22 2018-09-12        10 KwaMsa~ KwaMsane~     1 Urban          Sout~
 6      16 2000-01-01           22 2018-05-31        10 KwaMsa~ KwaMsane~     1 Urban          Sout~
 7      17 2000-01-01           22 2018-10-30        10 Nkolok~ Zululiya~    14 Rural          Sout~
 8      18 2000-01-01           22 2018-10-30        10 Nkolok~ Zululiya~    14 Rural          Sout~
 9      19 2000-01-01           22 2018-09-20        10 KwaMsa~ Gugulethu     2 Peri-Urban     Sout~
10      20 2000-01-01           22 2014-09-03        32 Machib~ Machibini    15 Rural          Sout~
# ... with 30,766 more rows, and 8 more variables: NearestClinic <dbl+lbl>, KmToNearestClinic <dbl>,
#   NearestSecondarySchool <dbl+lbl>, KmToNearestSecondarySchool <dbl>, NearestPrimarySchool <dbl+lbl>,
#   KmToNearestPrimarySchool <dbl>, KmToNearestLevel1Road <dbl>, KmToNearestLevel2Road <dbl>
```

Package philosophy
==================

Version control
---------------

Following best data practices, the `ahri` package will never overwrite
the default .dta datasets. If you for some reason change a default. dta
dataset, make sure you save it as a different file to a different folder
outside of your root folder.

Always run `test_ahri_dta` if you think you somehow corrupted or wrote
over the default .dta dataset(s). (See document `7 Test functions` of
the documentation).

If you set `write_rda=TRUE`, the `readHIVData` and `readEpisodes`
functions will write/overwrite the .Rda datasets to the folder set in
`getFiles`. I don’t think it makes sense to set different .dta and .Rda
folders, otherwise confusion would result.

Motivation and performance
--------------------------

The inquisitive user may ask why use functions such as `readHIVData` and
`readEpisodes` at all? Also, doesn’t the `dropTasP` and `write_rda` make
the code more complex and the data flow more confusing?

First, where necessary, `ahri` writes the datasets to Rda files to
improve performance. The Episodes `SurveillanceEpisodesExtended.dta`
file is very large. It takes time to read it in. `readEpisodes` is
designed to speed this step up by writing an Rda to file once. You can
then load the dataset into memory using `getEpisodes`. Here is a time
comparison of both steps on my system:

``` r
# The time to read the Episodes dataset each time
system.time(readEpisodes(dropTasP=FALSE, write_rda=FALSE))
   user  system elapsed 
  67.18    6.11   73.42 
# The time to load the Episodes.Rda dataset
system.time(getEpisodes())
   user  system elapsed 
   2.67    0.02    2.70 
```

Since several functions use the Episodes dataset, `getEpisodes` greatly
increases performance throughout the `ahri` library. And it should for
you too.

Second, I think it is safe to drop TasP observations if your analysis
includes data prior to 2017. No-one has systematically studied the
demographic and health differences between the TasP and PIP surveillance
areas. In addition, setting `dropTasP` at the very beginning of with
`readEpisodes` or `getHIVData` will greatly simply your work flow.
Decide if you want to keep TasP observations or not at the very
beginning, and then move on with your analysis without worrying about it
again. If you don’t like this approach, then you should do
`readHIVData(dropTasP=FALSE)` (and the same for `readEpisodes`). Also
note that there is a standalone function called `dropTasP` that you can
use to manually drop TasP areas (see the document `3 Set functions` for
more information).

(This document was compiled with `ahri` version 0.9.2 )
