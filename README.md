![GitHub R package version](https://img.shields.io/github/r-package/v/scenic555/optical?label=Optical&logo=github)
![GitHub](https://img.shields.io/github/license/scenic555/optical?logo=github)

# optical
This Package provides function that are used for Optimal Item Calibaration in computerized achievement tests

## Installation

``` r
# The easiest way to install the optical package:
# install.packages("optical")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("scenic555/optical")
```

## Usage

```{r example}
library(optical)

# 1PL-models with common discrimination parameter
ip <- cbind(c(1.6, NA), c(-1, 1))

yyy <- optical(ip, oc="D", uncert=FALSE, ipop,
               imf=c(0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.45),
               maxiter=rep(300, 6), eps=rep(0.002, 6),
               nnn=c(0, 50, 50, 200, 200, 200),
               nsp=c(0.001, 0.0001, 0.0001, 0.00001, 0.00001, 0.00001),
               sss=0.001, falpha=1.08, ig=3, ex=0)

drawdesign(yyy=yyy, ip=ip, ylowl=-1000, refline=0.002, layout=1)
```

# Licence
This package is free and open source software, licensed under GPL (>= 3).
