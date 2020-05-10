-   [Test the data](#test-the-data)
-   [Test the code](#test-the-code)

The `ahri` library calls the `testthat` package to test the data and
code. Tests are constructed for 2019 onwards only.

Test the data
=============

The `test_ahri_dta` function runs some basic tests on selected variables
to make sure that the default .dta datasets correspond with the
‘official’ .dta datasets from the AHRI repository, or that you did not
corrupt or overwrite the .dta datasets along the way.

`test_ahri_dta` works by testing that your .dta datasets give the
expected results. For example, in the HIV surveillance dataset, it
checks that the number of unique participants (`IIntID`) is equal to
some value before and afer dropping TasP observations. Please note that
the functions can take up to three minutes on more to test the large
datasets sizes.

``` r
test_ahri_dta(report='summary')
```

Test the code
=============

We also want to test whether the functions work as expected, and give
the same result across different users and platforms. Users may also
want to modify or enhance the code, therefore it is important that the
function returns the original and expected result. The following test
functions are available.

``` r
test_ahri_set(report='summary')
test_ahri_get(report='summary')
test_ahri_hiv(report='summary')
```

The test files are located in the `ahri` library on your system. You can
see what the tests do at:

``` r
system.file("tests/testthat", "test_ahri_dta_2019.R", package = "ahri")
[1] "C:/Users/alain.vandormael/Seafile/Programs/R/ahri/inst/tests/testthat/test_ahri_dta_2019.R"
```
