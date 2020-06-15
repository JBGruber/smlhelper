
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smlhelper

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/JBGruber/smlhelper.svg?branch=master)](https://travis-ci.com/JBGruber/smlhelper)
[![Codecov test
coverage](https://codecov.io/gh/JBGruber/smlhelper/branch/master/graph/badge.svg)](https://codecov.io/gh/JBGruber/smlhelper?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of smlhelper is to help me batch evaluate a number of machine
learning algorithms using a number of different preprocessing
combinations.

## Installation

If you want to install this, beware that the package is very raw and the
target audience so far is just me.

## Example

Construct a list of dfms containing different combinations of
preprocessing options:

``` r
library(quanteda)
library(smlhelper)
corp <- corpus(c(d1 = "Chinese Beijing Chinese",
                 d2 = "Chinese Chinese Shanghai",
                 d3 = "Chinese Macao",
                 d4 = "Tokyo Japan Chinese",
                 d5 = "Chinese Chinese Chinese Tokyo Japan"))

docvars(corp, "class") <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
docvars(corp, "training") <- c(TRUE, TRUE, TRUE, TRUE, FALSE)

test_dfms <- batch_prep(corp)
```

Now this can be used to run several supervised machine learning
algorithms in batch mode on all processing steps:

``` r
results <- batch_validate(x = test_dfms,
                          y = "class",
                          set = "training",
                          alg = list(textmodel_nb = quanteda.textmodels::textmodel_nb,
                                     textmodel_svm = quanteda.textmodels::textmodel_svm),
                          pred = predict)
```

``` r
library(tidyverse)
results %>% 
  slice_max(order_by = accuracy, n = 10, with_ties = FALSE)
#> # A tibble: 10 x 10
#>    algorithm prep  false_negative false_positive true_negative true_positive
#>    <chr>     <chr>          <dbl>          <dbl>         <dbl>         <dbl>
#>  1 textmode… P-N-…              0              0             1             0
#>  2 textmode… N-L-…              0              0             1             0
#>  3 textmode… P-L-…              0              0             1             0
#>  4 textmode… L-S-…              0              0             1             0
#>  5 textmode… P-N-…              0              0             1             0
#>  6 textmode… N-S-…              0              0             1             0
#>  7 textmode… P-S-…              0              0             1             0
#>  8 textmode… S-W-…              0              0             1             0
#>  9 textmode… P-N-…              0              0             1             0
#> 10 textmode… N-L-…              0              0             1             0
#> # … with 4 more variables: accuracy <dbl>, precision <dbl>, recall <dbl>,
#> #   f1 <dbl>
```

# Working SML Algorithms

The `batch_validate` function should work with a large number of
algorithms. However, these are the ones I already tested:

| package             | alg            | pred    | install                                 |
| :------------------ | :------------- | :------ | :-------------------------------------- |
| quanteda.textmodels | textmodel\_nb  | predict | install.packages(‘quanteda.textmodels’) |
| quanteda.textmodels | textmodel\_svm | predict | install.packages(‘quanteda.textmodels’) |
