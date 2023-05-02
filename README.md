
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="man/figures/optical.png" align="right" height="138"/>

# optical

<!-- badges: start -->

![GitHub R package
version](https://img.shields.io/github/r-package/v/scenic555/optical?label=Optical&logo=github)
![GitHub](https://img.shields.io/github/downloads/scenic555/optical/total?logo=github)
![GitHub](https://img.shields.io/github/license/scenic555/optical?logo=github)
[![R-CMD-check](https://github.com/scenic555/optical/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/scenic555/optical/actions/workflows/R-CMD-check.yaml)  
[![pages-build-deployment](https://github.com/scenic555/optical/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/scenic555/optical/actions/workflows/pages/pages-build-deployment)
[![test-coverage](https://github.com/scenic555/optical/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/scenic555/optical/actions/workflows/test-coverage.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/optical)](https://CRAN.R-project.org/package=optical)

<!--[![](https://github.com/scenic555.png?size=50)](https://github.com/scenic555)-->
<!--[![Codecov test coverage](https://codecov.io/gh/scenic555/optical/branch/main/graph/badge.svg)](https://app.codecov.io/gh/scenic555/optical?branch=main)-->
<!-- badges: end -->

`optical` package provides function that are used for Optimal Item
Calibaration in computerized achievement tests

## Installation

The easiest way to install the
[**optical**](https://scenic555.github.io/optical/) package from CRAN
using:

``` r
install.packages("optical")
```

You can install the development version of
[**optical**](https://scenic555.github.io/optical/) from
[GitHub](https://github.com/) with the following code:

``` r
# if not installed already on your computer, install devtools
install.packages("devtools")

# Install the package
devtools::install_github("scenic555/optical")

# Load the optical package
library(optical)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(optical)

# 2PL-models with difficulty and common discrimination parameters
ip <- cbind(c(1.6, 1.6),  c(-1, 1))

yyy <- optical(ip, oc="D", uncert=FALSE, ipop,
               imf=c(0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.45),
               maxiter=rep(300, 6), eps=rep(0.002, 6),
               nnn=c(0, 50, 50, 200, 200, 200),
               nsp=c(0.001, 0.0001, 0.0001, 0.00001, 0.00001, 0.00001),
               sss=0.001, falpha=1.08, ig=3, ex=0)
#> [1;31m---> Outer iteration = 1 [0m
#> ++++++++++++++++++
#> [1;31m---> Adapt grid; outer iteration = 2 [0m
#> ++

# Table of interval boundaries for optimal design with items and probabilities
yyy$ht
#>      Lower    Upper Item Probability
#> 1     -Inf -0.73445    1   0.2313373
#> 2 -0.73445  0.00005    2   0.2686827
#> 3  0.00005  0.73445    1   0.2686428
#> 4  0.73445      Inf    2   0.2313373

drawdesign(yyy=yyy, ip=ip, ylowl=-1000, refline=0.002, layout=1)
```

<img src="man/figures/README-example-1.png" width="100%" />

# Licence

This package is free and open source software, licensed under GPL (\>=
3).

# References

Ul Hassan and Miller (2021). An exchange algorithm for optimal
calibration of items in computerized achievement tests. Computational
Statistics and Data Analysis, 157: 107177.
<https://doi.org/10.1016/j.csda.2021.107177>

Bjermo, Fackle-Fornius, and Miller (2021). Optimizing Calibration
Designs with Uncertaintyin Abilities. Manuscript.
[urn:nbn:se:su:diva-198065](https://urn.kb.se/resolve?urn=urn%3Anbn%3Ase%3Asu%3Adiva-198065)
