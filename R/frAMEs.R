#' @title Average Marginal Effects
#' @author Tyson S. Barrett
#' @description Provides the average marginal effects of a GLM model with 
#' bootstrapped confidence intervals.
#' 
#' @param data the data frame containing the variables for the model
#' @param formula the model formula
#' @param family the family of the glm (e.g., binomial, poisson)
#' @param ci_type the type of boostrapped confidence interval; options are "perc", "basic", "bca"
#' @param boot the number of bootstrapped samples; default is 100
#' @param ci the confidence interval; the default is .975 which is the 95\% confidence interval.
#' 
#' @details Using the average marginal effects as discussed by Tamas Bartus (2005), 
#' the coefficients are transformed into probabilities (for binary outcomes) or remain
#' in their original units (continuous outcomes).
#' 
#' @references Bartus, T. (2005). Estimation of marginal effects using margeff. 
#' The Stata Journal, 5(3), 309–329.
#' 
#' @import stats
#' 
#' @export
frames = function(data, formula, family, ci_type = "perc", boot=100, ci=.95){
  data = data.frame(data)
  forms = formula
  
  ## checks
  .call = match.call()
  .boot_checker(boot)
  .ci_checker(ci)
  
  ## Bootstrapped Samples and Statistics
  cat("\nBootstrapping...")
  booted = boot(data = data, 
                statistic = .run_mod, 
                R = boot, 
                formula = forms,
                family = family)
  
  cat('Done.')
  
  cis = vector("list", 0L)
  for (i in seq_along(names(booted$t0))){
    cis[[i]] = boot.ci(booted, index = i, type = ci_type)
  }
  
  estimates = data.frame(do.call("rbind", cis))
  estimates = data.frame("Estimate" = sapply(estimates$t0, function(x) x[1]), 
                         "Lower"    = sapply(estimates$percent, function(x) x[4]), 
                         "Upper"    = sapply(estimates$percent, function(x) x[5]))
  final = structure(
    list("estimates"    = estimates,
         "data"         = data, 
         "boot"         = boot, 
         "model"        = forms,
         "call"         = .call),
    class = c("frames", "list")
  )
  cat('\r', rep(' ', 40), '\r')
  final
}

#' @export
print.frames = function(x, ...){
  cat("\u250C", rep("\u2500", 28), "\u2510\n", sep = "")
  cat("\u2502", " Average Marginal Effects ", "\u2502")
  cat("\n\u2514", rep("\u2500", 28), "\u2518\n", sep = "")
  cat("\n")
  print(round(x[[1]], 4), ...)
  cat("\u2500\u2500\u2500\u2500")
}

