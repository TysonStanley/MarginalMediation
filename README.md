
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/TysonStanley/MarginalMediation.svg?branch=master)](https://travis-ci.org/TysonStanley/MarginalMediation)

Marginal Mediation
==================

The `MarginalMediation` package provides the ability to perform **marginal mediation analysis**. It provides a useful framework from which to interpret the coefficients in a mediation analysis, especially when the mediator(s) and/or outcome is binary or a count (other types of outcomes will be added).

You can install it via:

``` r
devtools::install_github("tysonstanley/MarginalMediation")
```

The main function is `mma()`:

``` r
mma(data,
    formula1,
    formula2,
    family = c("model1", "model2"),
    ind_effects = c("apath-bpath"))
```

The `ind_effects` is a vector of requested mediated paths. These estimates are in terms of the average marginal effects using the `a x b` method of estimating indirect paths. Any number of these can be included, although it is limited to the number of variables available in the models.

### A Quick Example

Below is an example, where the theoretical backing of such a model is not very stable, but it is useful to show how to use the function and the output.

``` r
library(MarginalMediation)
library(furniture)
#> furniture 1.5.5: learn more at tysonstanley.github.io

data("nhanes_2010")

mma(nhanes_2010,
    marijuana ~ home_meals + gender + age + asthma,
    home_meals ~ gender + age + asthma,
    family = c(binomial, gaussian, gaussian),
    ind_effects = c("genderFemale-home_meals",
                    "age-home_meals",
                    "asthmaNo-home_meals"),
    boot = 500)
#> --- 
#>  Marginal Mediation 
#>                            Apath    Bpath Estimate    Lower   Upper
#> genderFemale-home_meals -1.34831 -0.00972  0.01311  0.00326 0.02365
#> age-home_meals          -0.05689 -0.00972  0.00055 -0.00003 0.00128
#> asthmaNo-home_meals     -0.00428 -0.00972  0.00004 -0.00564 0.00580
#> ---
#>  Confidence Interval at 0.95
```

The `a` path, `b` path, the indirect effect and the confidence interval of the indirect effects are all reported. These are all average marginal effects, and are, therefore, in terms of the corresponding endogenous variable's units.

### Conclusions

This is currently beta but I am excited to provide an initial working release. Let me know if you find any bugs or want to discuss the method (<t.barrett@aggiemail.usu.edu>).
