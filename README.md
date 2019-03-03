
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![CRAN](https://www.r-pkg.org/badges/version/MarginalMediation)](https://www.r-pkg.org/badges/version/MarginalMediation)
[![Rdoc](http://www.rdocumentation.org/badges/version/MarginalMediation)](http://www.rdocumentation.org/packages/MarginalMediation)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/MarginalMediation)](https://cranlogs.r-pkg.org/badges/grand-total/MarginalMediation)
[![Build
Status](https://travis-ci.org/TysonStanley/MarginalMediation.svg?branch=master)](https://travis-ci.org/TysonStanley/MarginalMediation)

# MarginalMediation: 0.7.0 <img src="man/figures/mma_hex.jpg" align="right" width="30%" height="30%"/>

The `MarginalMediation` package provides the ability to perform
**marginal mediation analysis**. It provides a useful statistical
framework from which to interpret the coefficients in a mediation
analysis, especially when the mediator(s) and/or outcome is binary or a
count (other types of outcomes will be added).

You can install it via:

``` r
install.packages("MarginalMediation")
```

or

``` r
install.packages("remotes")
remotes::install_github("tysonstanley/MarginalMediation")
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
#> ── furniture 1.9.0 ─────────────────────────────────────────────────────────────── learn more at tysonbarrett.com ──
#> ✔ furniture attached
#> ✔ No potential conflicts found
data(nhanes_2010)
```

``` r
## The MarginalMediation package
library(MarginalMediation)
```

    #> Loading MarginalMediation
    #> ── MarginalMediation 0.7.0 ─────────────────────────────────────────────────────── learn more at tysonbarrett.com ──
    #> ✔ MarginalMediation attached
    #> ✔ No potential conflicts found

``` r
pathbc <- glm(marijuana ~ home_meals + gender + age + asthma, 
              data = nhanes_2010, 
              family = "binomial")
patha <- glm(home_meals ~ gender + age + asthma,
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
#> Regression Models: 
#> 
#>      marijuana ~ 
#>                           Est      SE   Est/SE P-Value
#>         (Intercept)  -0.39400 0.38028 -1.03608 0.30017
#>         home_meals   -0.04062 0.01363 -2.98051 0.00288
#>         genderFemale  0.43161 0.11723  3.68169 0.00023
#>         age           0.00276 0.01470  0.18754 0.85123
#>         asthmaNo     -0.00717 0.15004 -0.04778 0.96189
#> 
#>      home_meals ~ 
#>                           Est      SE   Est/SE P-Value
#>         (Intercept)   6.56883 0.76462  8.59100 0.00000
#>         genderFemale -1.34831 0.23910 -5.63913 0.00000
#>         age          -0.05689 0.03017 -1.88565 0.05955
#>         asthmaNo     -0.00428 0.31293 -0.01368 0.98909
#> 
#> Unstandardized Mediated Effects: 
#> 
#>    Indirect Effects: 
#> 
#>      marijuana ~ 
#>                                    Indirect    Lower   Upper
#>         genderFemale => home_meals  0.01312  0.00429 0.02562
#>         age => home_meals           0.00055  0.00003 0.00139
#>         asthmaNo => home_meals      0.00004 -0.00639 0.00672
#> 
#>    Direct Effects: 
#> 
#>      marijuana ~ 
#>                        Direct    Lower   Upper
#>         genderFemale  0.10430  0.04813 0.15967
#>         age           0.00066 -0.00603 0.00848
#>         asthmaNo     -0.00172 -0.06947 0.07061
```

The print method provides:

1.  the individual regression results,
2.  the `a` paths,
3.  the `b` paths,
4.  the indirect effect with the confidence interval, and
5.  the direct effect with the confidence interval.

The regressions are in their original (non-AME) units while the indirect
and direct effects are in the AME units—the units of the outcome—in this
case, risk of using marijuana.

### Conclusions

Let me know if you find any bugs or want to discuss the method
(<t.barrett@aggiemail.usu.edu>).
