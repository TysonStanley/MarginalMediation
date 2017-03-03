
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/.svg?branch=master)](https://travis-ci.org/)

Marginal Mediation
==================

The `marginalMediation` package provides the ability to perform **marginal mediation**. It provides a useful framework from which to interpret the coefficients in a mediation analysis, especially when the mediator(s) and/or outcome is binary.

You can install it via:

``` r
devtools::install_github("tysonstanley/marginsMediation")
```

The main function is `margins_mediation()`. The syntax is clean and `tidyverse` possible. Below is the general outline of the `margins_mediation()` function:

``` r
margins_mediation(data,
                  formula1,
                  formula2,
                  family = c("model1", "model2"),
                  ind_effects = c("apath-bpath"))
```
