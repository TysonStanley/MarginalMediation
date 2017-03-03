library(testthat)
library(furniture)
library(MarginalMediation)

test_that("Multiple Indirect Effects",
          expect_s3_class(margins_mediation(nhanes10,
                                            income ~ hinsur + gender + age + fmsize,
                                            hinsur ~ gender + age + fmsize,
                                            family = c("gaussian", "binomial"),
                                            ind_effects = c("genderFemale-hinsur",
                                                            "age-hinsur",
                                                            "fmsize-hinsur"),
                                            boot = 5), "ame"))
