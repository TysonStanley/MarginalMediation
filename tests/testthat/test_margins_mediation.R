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
                              boot = 10), "mma"))

test_that("Output",
          expect_output(mma(nhanes_2010,
                              marijuana ~ home_meals + gender + age + asthma,
                              home_meals ~ gender + age + asthma,
                              age ~ gender + asthma,
                              family = c(binomial, gaussian, gaussian),
                              ind_effects = c("genderFemale-home_meals",
                                              "age-home_meals",
                                              "asthmaNo-home_meals",
                                              "genderFemale-age",
                                              "asthmaNo-age"),
                              boot = 10), "calculating"))

test_that("Error catching", {
          expect_error(mma(nhanes_2010,
                              marijuana ~ home_meals + gender + age + asthma,
                              home_meals ~ gender + age + asthma,
                              "age ~ gender + asthma",
                              family = c(binomial, gaussian, gaussian),
                              ind_effects = c("genderFemale-home_meals",
                                              "age-home_meals",
                                              "asthmaNo-home_meals",
                                              "genderFemale-age",
                                              "asthmaNo-age"),
                              boot = 10))
          expect_error(mma(nhanes_2010,
                           marijuana ~ home_meals + gender + age + asthma,
                           home_meals ~ gender + age + asthma,
                           age ~ gender + asthma,
                           family = c(not_a_function, gaussian, gaussian),
                           ind_effects = c("genderFemale-home_meals",
                                           "age-home_meals",
                                           "asthmaNo-home_meals",
                                           "genderFemale-age",
                                           "asthmaNo-age"),
                           boot = 10))
          expect_error(mma(nhanes_2010,
                           marijuana ~ home_meals + gender + age + asthma,
                           home_meals ~ gender + age + asthma,
                           age ~ gender + asthma,
                           family = c(binomial, gaussian, gaussian),
                           ind_effects = c(1,
                                           "age-home_meals",
                                           "asthmaNo-home_meals",
                                           "genderFemale-age",
                                           "asthmaNo-age"),
                           boot = 10))
  })


