library(testthat)
library(furniture)
library(MarginalMediation)

test_that("Multiple Indirect Effects", {
  fit1 = glm(marijuana ~ home_meals + gender + age + asthma, data = nhanes_2010, family = "binomial")
  fit2 = glm(home_meals ~ gender + age + asthma, data = nhanes_2010, family = "gaussian")
  expect_s3_class(mma(fit1, fit2,
                      ind_effects = c("genderFemale-home_meals",
                                      "age-home_meals",
                                      "asthmaNo-home_meals"),
                      boot = 10), "mma")
  })

test_that("No Logicals", {
  set.seed(100)
  samp_data = data.frame(a = rnorm(n = 1000),
                         b = sample(c(FALSE, TRUE), size = 1000, replace = TRUE),
                         c = rnorm(n = 1000))
  samp_data$b2 = factor(samp_data$b)
  bcpath = glm(c ~ a + b, data = samp_data,
               family = "gaussian")
  apath = glm(b ~ a, data = samp_data,
              family = "binomial")
  expect_error(
    mma(bcpath, apath,
        ind_effects = "a-b",
        boot = 10)
  )
})

test_that("Error catching", {
  fit1 = glm(marijuana ~ home_meals + gender + age + asthma, data = nhanes_2010, family = "binomial")
  fit2 = glm(home_meals ~ gender + age + asthma, data = nhanes_2010, family = "gaussian")
  fit3 = NULL
  expect_error(mma(fit1, fit2,
                  ind_effects = c("genderFemale-home_meals",
                                  "age-home_meals",
                                  "asthmaNo-home_meals",
                                  "genderFemale-age",
                                  "asthmaNo-age"),
                  boot = 10))
  expect_error(mma(fit1, fit2, fit3,
                   ind_effects = c("genderFemale-home_meals",
                                   "age-home_meals",
                                   "asthmaNo-home_meals"),
                   boot = 10))
  expect_error(mma(fit1, fit2,
                   ind_effects = c(1,
                                   "age-home_meals",
                                   "asthmaNo-home_meals"),
                   boot = 10, 
                   ci = 10))
  })


