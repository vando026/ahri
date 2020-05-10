-   [AHRI default datasets](#ahri-default-datasets)
    -   [Folder structure](#folder-structure)
    -   [Install the library](#install-the-library)
-   [Setting the file paths](#setting-the-file-paths)
    -   [The setFiles function](#the-setfiles-function)
    -   [Changing to different release
        versions](#changing-to-different-release-versions)

AHRI default datasets
=====================

For the `ahri` library to work, you will need to request five default
datasets from the AHRI Data Centre. For the 2019 release cycle, these
default datasets are called:

-   RD05-99 ACDIS HIV All.dta

-   SurveillanceEpisodesExtended.dta

-   RD03-99 ACDIS WGH ALL.dta

-   RD04-99 ACDIS MGH ALL.dta

-   RD01-03 ACDIS BoundedStructures.dta

Folder structure
----------------

AHRI releases updates of the default .dta datasets annually, so it is
important to agree on a common folder structure. A suggested folder
structure is to create a root folder with subfolders for the release
year. For example, the path to my root folder is
`C:\Users\alainv\AHRI_Data`, and the folder structure looks like:

    AHRI_Data
    |
    |--- 2017
    |     RD05-99 ACDIS HIV All.dta
    |     SurveillanceEpisodesExtended.dta
    |     RD03-99 ACDIS WGH ALL.dta
    |     RD04-99 ACDIS MGH ALL.dta
    |     RD01-03 ACDIS BoundedStructures.dta      
    |---2018
    |     RD05-99 ACDIS HIV All.dta
    |     SurveillanceEpisodesExtended.dta
    |     RD03-99 ACDIS WGH ALL.dta
    |     RD04-99 ACDIS MGH ALL.dta
    |     RD01-03 ACDIS BoundedStructures.dta      
    |---2019
    |     RD05-99 ACDIS HIV All.dta
    |     SurveillanceEpisodesExtended.dta
    |     RD03-99 ACDIS WGH ALL.dta
    |     RD04-99 ACDIS MGH ALL.dta
    |     RD01-03 ACDIS BoundedStructures.dta      

So folder 2019 holds the datasets from the 2019 release year. Please
make sure you are putting the datasets in the proper release year
folder.

Install the library
-------------------

The `ahri` library is in development, and can be downloaded from GitHub:
<a href="https://github.com/vando026/ahri_dev" class="uri">https://github.com/vando026/ahri_dev</a>.
Install the `remotes` package and ask me for the GitHub token.

``` r
library(remotes)
install_github('vando026/ahri_dev', ref="ahri_dev", auth_token="secret_token")
```

Then load the library.

``` r
library(ahri)
```

Setting the file paths
======================

The setFiles function
---------------------

The first thing to do set the file paths to the AHRI datasets. We use
the `setFiles` function for this and assign it to the name `getFiles`.
**The `getFiles` name is required.** In this example, I point `getFiles`
to the 2019 release cycle.

``` r
getFiles <- setFiles(folder="C:/Users/alainv/AHRI_Data/2019")
```

You can inspect the file paths by printing them out:

``` r
getFiles()[1:5]
$hivfile
[1] "C:/Users/alainv/AHRI_Data/2019/RD05-99 ACDIS HIV All.dta"

$epifile
[1] "C:/Users/alainv/AHRI_Data/2019/SurveillanceEpisodesExtended.dta"

$wghfile
[1] "C:/Users/alainv/AHRI_Data/2019/RD03-99 ACDIS WGH ALL.dta"

$mghfile
[1] "C:/Users/alainv/AHRI_Data/2019/RD04-99 ACDIS MGH ALL.dta"

$bsifile
[1] "C:/Users/alainv/AHRI_Data/2019/RD01-03 ACDIS BoundedStructures.dta"
```

If you ommit the folder argument then `setFiles()` will bring up a
graphical dialogue box where you can point and click your way to the
folder.

Changing to different release versions
--------------------------------------

Since the default .dta names should be consistent, the only parameter
that needs to be changed is the folder path. Thus, to call in the .dta
datasets from the 2018 release cycle, I would do:

``` r
getFiles <- setFiles(folder="C:/Users/alainv/AHRI_Data/2018")
```

Each time you start a new `R` session, the `ahri` library will prompt
you to do `getFiles`. It makes sense to have to manually do `getFiles`
each time, since there is no way to guess which data folder you want for
a given project.

`setFiles` also has capabilities for changing the file names, but choose
to do this only if you know what you are doing.

``` r
getFiles <- setFiles(
  folder="C:/Users/alainv/AHRI_Data/2018",
  hivfile="RD06-99 HIV Surveillance")
```

(This document was compiled with `ahri` version 0.8.9 )
