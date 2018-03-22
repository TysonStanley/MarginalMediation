
<!-- README.md is generated from README.Rmd. Please edit that file -->


[![CRAN](https://www.r-pkg.org/badges/version/MarginalMediation)](https://www.r-pkg.org/badges/version/MarginalMediation)
[![Rdoc](http://www.rdocumentation.org/badges/version/MarginalMediation)](http://www.rdocumentation.org/packages/MarginalMediation)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/MarginalMediation)](https://cranlogs.r-pkg.org/badges/grand-total/MarginalMediation)
[![Build
Status](https://travis-ci.org/TysonStanley/MarginalMediation.svg?branch=master)](https://travis-ci.org/TysonStanley/MarginalMediation)

# MarginalMediation: 0.5.1 <img src="man/figures/mma_hex.jpg" align="right" />

The `MarginalMediation` package provides the ability to perform
**marginal mediation analysis**. It provides a useful framework from
which to interpret the coefficients in a mediation analysis, especially
when the mediator(s) and/or outcome is binary or a count (other types of
outcomes will be added).

You can install it via:

``` r
install.packages("MarginalMediation")
```

or

``` r
devtools::install_github("tysonstanley/MarginalMediation")
```

The main function is `mma()`:

``` r
mma(...,
    ind_effects = c("apath-bpath"))
```

where `...` consists of 2 or more model objects. The first is the `b`
and `c'` path model, while the others are the `a` path models.

The `ind_effects` is a vector of requested mediated paths. These
estimates are in terms of the average marginal effects using the `a x b`
method of estimating indirect paths. Any number of these can be
included, although it is limited to the number of variables available in
the models.

### A Quick Example

Below is an example, where the theoretical backing of such a model is
not very stable, but it is useful to show how to use the function and
the output.

``` r
## Data for the example
library(furniture)
#> furniture 1.7.6: learn more at tysonbarrett.com
data(nhanes_2010)

## The MarginalMediation package
library(MarginalMediation)
#> MarginalMediation 0.5.1: This is beta software.
#> Please report any bugs (t.barrett@aggiemail.usu.edu).
pathbc = glm(marijuana ~ home_meals + gender + age + asthma, 
             data = nhanes_2010, 
             family = "binomial")
patha = glm(home_meals ~ gender + age + asthma,
            data = nhanes_2010, 
            family = "gaussian")
mma(pathbc, patha,
    ind_effects = c("genderFemale-home_meals",
                    "age-home_meals",
                    "asthmaNo-home_meals"),
    boot = 500)
#> 
#> calculating a paths... b and c paths... Done.
                                                                                 
#> ┌───────────────────────────────┐
#> │  Marginal Mediation Analysis  │
#> └───────────────────────────────┘
#> A marginal mediation model with:
#>    1 mediators
#>    3 indirect effects
#>    3 direct effects
#>    500 bootstrapped samples
#>    95% confidence interval
#>    n = 1417 
#> 
#> Formulas:
#>    ◌ marijuana ~ home_meals + gender + age + asthma
#>    ◌ home_meals ~ gender + age + asthma 
#> 
#> Unstandardized Effects
#> ⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺
#> ── Indirect Effects ──
#>                           A-path   B-path Indirect    Lower   Upper
#> genderFemale-home_meals -1.34831 -0.00973  0.01312  0.00429 0.02562
#> age-home_meals          -0.05689 -0.00973  0.00055  0.00003 0.00139
#> asthmaNo-home_meals     -0.00428 -0.00973  0.00004 -0.00639 0.00672
#> 
#> ── Direct Effects ──
#>                Direct    Lower   Upper
#> genderFemale  0.10430  0.04813 0.15967
#> age           0.00066 -0.00603 0.00848
#> asthmaNo     -0.00172 -0.06947 0.07061
#> -----
```

The print method provides:

1.  the `a` path,
2.  the `b` path,
3.  the indirect effect with the confidence interval, and
4.  the direct effect with the confidence interval.

These are all average marginal effects, and are, therefore, in terms of
the corresponding endogenous variable’s units.

### Conclusions

This is currently beta but I am excited to provide an initial working
release. Let me know if you find any bugs or want to discuss the method
(<t.barrett@aggiemail.usu.edu>).

### Final Notes

More will be done to `MarginalMediation` as it is under development,
including:

1.  More bootstrapping options (e.g., Bias-Corrected Bootstrap)
2.  More distributional options as currently only gaussian, binomial,
    and Poisson are available (e.g., zero-inflated distributions,
    multinomial distributions)

Thanks\!
