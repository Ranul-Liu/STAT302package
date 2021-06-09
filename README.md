
# STAT302package

<!-- badges: start -->
[![R-CMD-check](https://github.com/Ranul-Liu/STAT302package/workflows/R-CMD-check/badge.svg)](https://github.com/Ranul-Liu/STAT302package/actions)
[![codecov](https://codecov.io/gh/Ranul-Liu/STAT302package/branch/master/graph/badge.svg)](https://codecov.io/gh/Ranul-Liu/STAT302package)
<!-- badges: end -->

The goal of `STAT302package` package is for Stat302. It includes `my_t.test`, `my_lm`, `my_knn_cv` and `my_rf_cv`

## Installation

You can install the released version of STAT302package from Github using:

``` r
devtools::install_github("Ranul-Liu/STAT302package")
library(STAT302package)
```

## Use

To view the vignette:

``` r
devtools::install_github("Ranul-Liu/STAT302package", build_vignette = TRUE, build_opts = c())
library(STAT302package)
# Use this to view the vignette in the STAT302package HTML help
help(package = "STAT302package", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "STAT302package")
```
