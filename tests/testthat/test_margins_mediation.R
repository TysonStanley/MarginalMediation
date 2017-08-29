library(testthat)
library(furniture)
library(MarginalMediation)

test_that("Multiple Indirect Effects",
          expect_s3_class(mma(nhanes_2010,
                              marijuana ~ home_meals + gender + age + asthma,
                              home_meals ~ gender + age + asthma,
                              age ~ gender + asthma,
                              family = c(binomial, gaussian, gaussian),
                              ind_effects = c("genderFemale-home_meals",
                                              "age-home_meals",
                                              "asthmaNo-home_meals",
                                              "genderFemale-age",
                                              "asthmaNo-age"),
                              boot = 500), "mma"))
