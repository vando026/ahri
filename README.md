-   [Getting started](#getting-started)
    -   [Install the library](#install-the-library)
    -   [Setting the file paths: the setFiles
        function](#setting-the-file-paths-the-setfiles-function)
    -   [Help and documentation](#help-and-documentation)
-   [Reading in data](#reading-in-data)
    -   [The readHIVData function](#the-readhivdata-function)
    -   [The readEpisodes function](#the-readepisodes-function)
    -   [The readHealthData function](#the-readhealthdata-function)
    -   [The getBSData and getBSMax
        functions](#the-getbsdata-and-getbsmax-functions)

Getting started
===============

This document serves as a short introduction to the ahri library. For
this library to work, you will need to request five default datasets
from the AHRI Data Centre. These default datasets are called:

-   RD05-99 ACDIS HIV All.dta

-   SurveillanceEpisodesExtended.dta

-   RD03-99 ACDIS WGH ALL.dta

-   RD04-99 ACDIS MGH ALL.dta

-   RD01-03 ACDIS BoundedStructures.dta

-   RD06-99 ACDIS HSE-H All.dta

Put all these default .dta (Stata) datasets in the same folder. For
example, my path to this folder is: `C:\Users\alainv\AHRI_Data`.

Install the library
-------------------

The ahri library is in development, which you can download from GitHub.
First, install the `remotes` package. Second, ask me for the GitHub
token.

``` r
library(remotes)
install_github('vando026/ahri', ref="ahri_dev", auth_token="secret_token")
library(ahri)
```

Setting the file paths: the setFiles function
---------------------------------------------

The first thing to do set the file paths to the AHRI datasets. We use
the `setFiles` function for this and assign it to the name `getFiles`.
The `getFiles` name is required.

``` r
getFiles <- setFiles(folder="C:/Users/alainv/AHRI_Data")
```

You can inspect the file paths by printing them out:

``` r
getFiles()[1:5]
$hivfile
[1] "C:/Users/alainv/AHRI_Data/RD05-99 ACDIS HIV All.dta"

$epifile
[1] "C:/Users/alainv/AHRI_Data/SurveillanceEpisodesExtended.dta"

$wghfile
[1] "C:/Users/alainv/AHRI_Data/RD03-99 ACDIS WGH ALL.dta"

$mghfile
[1] "C:/Users/alainv/AHRI_Data/RD04-99 ACDIS MGH ALL.dta"

$bsifile
[1] "C:/Users/alainv/AHRI_Data/RD01-03 ACDIS BoundedStructures.dta"
```

If you ommit the folder argument then `setFiles()` will bring up a
graphical dialogue box where you can point and click your way to the
folder.

Help and documentation
----------------------

-   The README files provide an overview of the `ahri` library. Please
    use these as a guide to getting started.
-   The `ahri` package has help files and documentation. Type `?ahri` to
    get to the help pages. For more information on a specific function,
    for example `setFiles`, type `?setFiles`.
-   Please consult the issues page on this Github site for more
    information and for answers to questions someone before you may have
    already asked.
-   If you have questions, post them as an issue so that I or others can
    answer.
    (<https://help.github.com/en/github/managing-your-work-on-github/creating-an-issue>)

Reading in data
===============

The readHIVData function
------------------------

The `readHIVData` reads in the `RD05-99 ACDIS HIV All.dta` dataset, does
some basic data processing, and harmonizes variable names for future
merging.

``` r
readHIV <- readHIVData()
```

You need only to run the `readHIVData` function once, which will save an
.Rda dataset to the same folder with your default .dta datasets.
(However, if you download an updated `RD05-99 ACDIS HIV All.dta` to the
default folder, then you will need to run `readHIVData` again.) The file
path of the .Rda file is:

``` r
getFiles()$hiv_rda
[1] "C:/Users/alainv/AHRI_Data/ACDIS_HIV_All.Rda"
```

Once you have generated the .Rda file, you can run `getHIV` to load in
the data quickly and repeatedly.

``` r
hiv <- getHIV()
hiv
# A tibble: 159,471 x 10
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
# ... with 159,461 more rows
```

If you don’t want `readHIVData` to write to file, then you can do:

``` r
readHIV <- readHIVData(dropTasP=TRUE, write_rda=FALSE)
```

which will assign the dataset to the gobal environment.

The default setting of `readHIVData` is to drop observations from the
TasP (Northern PIPSA) study area. In 2017, TasP areas were added to the
datasets. If you want to include the TasP study areas then set
`dropTasP=FALSE`.

``` r
readHIV <- readHIVData(dropTasP=FALSE)
```

The readEpisodes function
-------------------------

With the large `SurveillanceEpisodesExtended.dta` dataset, you run
`readEpisodes`:

``` r
readEpi <- readEpisodes()
# Or if you want to keep TasP obs.
readEpi <- readEpisodes(dropTasP=FALSE)
```

On my system, it takes almost a minute to read in the
`SurveillanceEpisodesExtended.dta` dataset. Once the corresponding .Rda
data file has been generated it takes less than a second or two to load
it into memory `getEpisodes`.

``` r
epidat <- getEpisodes()
epidat
# A tibble: 5,612,161 x 14
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
# ... with 5,612,151 more rows, and 4 more variables: InMigration <dbl+lbl>, OutMigration <dbl+lbl>,
#   Resident <dbl+lbl>, PIPSA <chr>
```

The `SurveillanceEpisodesExtended.dta` dataset is a bit overwhelming, so
by default the `readEpisodes` keeps only variables related to migration,
for which this dataset is best suited. You can however include addtional
variables using the `addVars` argument. For example, let’s say you are
intersted in adding the ART variables, then:

``` r
readEpi <- readEpisodes(addVars="OnART|ARTInitiation|EarliestARTInitDate")
```

Behind the hood, the `addVars` argument is a regular expression, hence
variable names must be separated as shown above. However, since “ART” is
the common substring, you could achieve the same outcome using:

``` r
readEpi <- readEpisodes(addVars="ART")
```

The file path of the .Rda file is:

``` r
getFiles()$epi_rda
[1] "C:/Users/alainv/AHRI_Data/SurveillanceEpisodesExtended.Rda"
```

If you do not want `readEpisodes` to write to file, then do:

``` r
epidat <- readEpisodes(dropTasP=TRUE, write_rda=FALSE)
```

The readHealthData function
---------------------------

AHRI has seperate health datasets for men and women. We can use the
`readHealthData` function to read in these datasets. Lets read in the
men and then the women. Again, the default
`RD03-99 ACDIS WGH/MGH ALL.dta` dataset is read and the corresponding
.Rda file is saved. Set `Female=1` to read in the women’s version
otherwise `Female=0` for the men’s version.

``` r
mgh <- readHealthData(Female=0)
getFiles()$mgh_rda
[1] "C:/Users/alainv/AHRI_Data/ACDIS_MGH_ALL.Rda"
wgh <- readHealthData(Female=1)
getFiles()$wgh_rda
[1] "C:/Users/alainv/AHRI_Data/ACDIS_WGH_ALL.Rda"
```

We can load the men or women’s .Rda dataset into memory quickly using:

``` r
mgh_dat <- getMGH()
wgh_dat <- getWGH()
```

The getBSData and getBSMax functions
------------------------------------

We can read and load the `RD01-03 ACDIS BoundedStructures.dta` dataset
using the `getBSData` function.

``` r
bs_dat <- getBSData()
bs_dat
# A tibble: 30,776 x 18
   BSIntID BSStartDate BSStartType BSEndDate  BSEndType Isigodi LocalArea  Ward IsUrbanOrRural PIPSA
     <int> <date>        <dbl+lbl> <date>     <dbl+lbl> <dbl+l> <dbl+lbl> <dbl>      <dbl+lbl> <dbl>
 1      11 2000-01-01           22 2018-02-10        10       5        32     1              4     1
 2      12 2000-01-01           22 9997-01-01       200       5        32     1              4     1
 3      13 2000-01-01           22 2009-03-17        10       5        32     1              4     1
 4      14 2000-01-01           22 9997-01-01       200       5        32     1              4     1
 5      15 2000-01-01           22 2018-09-12        10       5        32     1              4     1
 6      16 2000-01-01           22 2018-05-31        10       5        33     1              4     1
 7      17 2000-01-01           22 2018-10-30        10      16       107    14              3     1
 8      18 2000-01-01           22 2018-10-30        10      16       107    14              3     1
 9      19 2000-01-01           22 2018-09-20        10       4        24     2              2     1
10      20 2000-01-01           22 2014-09-03        32       7        39    15              3     1
# ... with 30,766 more rows, and 8 more variables: NearestClinic <dbl+lbl>, KmToNearestClinic <dbl>,
#   NearestSecondarySchool <dbl+lbl>, KmToNearestSecondarySchool <dbl>,
#   NearestPrimarySchool <dbl+lbl>, KmToNearestPrimarySchool <dbl>, KmToNearestLevel1Road <dbl>,
#   KmToNearestLevel2Road <dbl>
```

A participant can reside in multiple bounded structures (BS). For a
given year, it is useful to identify the BS that the participant spent
the most time in, which is what the `getBSMax` function does. The
`getBSMax` function is often used to produce an input dataset for
geospatial analyses, where for a single year, only one residence per
participant is allowed.

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
