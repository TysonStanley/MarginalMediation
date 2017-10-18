#' @title Marginal Mediation
#' @author Tyson S. Barrett
#' @description Provides the ability to perform marginal mediation. Marginal mediation
#' is particularly useful for situations where the mediator or outcome is categorical,
#' a count, or some other non-normally distributed variable.
#' The results provide the average marginal effects of the models, providing simple
#' interpretation of the indirect effects.
#' 
#' @param data the data frame with the data for the models
#' @param ... formulas for the models; the first is the model with the outcome while the others are the mediated effects ("a" paths)
#' @param family the vector of the families of the model. Either \code{binomial} for binary outcomes or \code{gaussian} for continuous. Needs to have a length equal to the number of models.
#' @param ind_effects a vector of the desired indirect effects. Has the form \code{"var1-var2"}.
#' @param boot the number of bootstrapped samples; default is 100
#' @param ci the confidence interval; the default is .975 which is the 95\% confidence interval.
#' 
#' @details Using the average marginal effects as discussed by Tamas Bartus (2005), 
#' the coefficients are transformed into probabilities (for binary outcomes) or remain
#' in their original units (continuous outcomes).
#' 
#' @return A list of class \code{mma} containing:
#' \item{ind_effects}{the indirect effects reported in the average marginal effect}
#' \item{dir_effects}{the direct effects reported in the average marginal effect}
#' \item{ci_level}{the confidence level}
#' \item{data}{the original data frame}
#' \item{reported_ind}{the indirect effects the user requested (in the \code{...})}
#' \item{boot}{the number of bootstrap samples}
#' \item{model}{the formulas of the individual sub-models}
#' \item{call}{the original function call}
#' 
#' 
#' @examples
#' \dontrun{
#' library(furniture)
#' data(nhanes_2010)
#' (fit = mma(nhanes_2010,
#'            marijuana ~ home_meals + gender + age + asthma,
#'            home_meals ~ gender + age + asthma,
#'            age ~ gender + asthma,
#'            family = c(binomial, gaussian, gaussian),
#'            ind_effects = c("genderFemale-home_meals",
#'                            "age-home_meals",
#'                            "asthmaNo-home_meals",
#'                            "genderFemale-age",
#'                            "asthmaNo-age"),
#'            boot = 500))
#' }
#' 
#' @references Bartus, T. (2005). Estimation of marginal effects using margeff. 
#' The Stata Journal, 5(3), 309–329.
#' 
#' @import stats
#' @import magrittr
#' @import boot
#' 
#' @export
mma = function(data, ..., family, ind_effects, boot=100, ci=.95){
  data = data.frame(data)
  forms = list(...)
  
  ## checks
  .call = match.call()
  .family_checker(family)
  .arg_checker(...)
  .boot_checker(boot)
  .ind_checker(ind_effects)
  .ci_checker(ci)
  .var_checker(data, forms)
  
  ## Bootstrapped Samples and Statistics
  cat("\ncalculating a paths... ")
  bootfit_a = lapply(seq_along(forms)[-1], function(i) { 
    boot(data = data, 
         statistic = .run_mod, 
         R = boot, 
         formula = forms[[i]],
         family = family[[i]])
  })
  
  cat('b and c paths... ')
  bootfit_b = boot(data = data, 
                   statistic = .run_mod, 
                   R = boot, 
                   formula = forms[[1]],
                   family = family[[1]])
  
  cat('Done.')
  
  nams = list()
  for (j in seq_along(forms)[2:length(forms)]){
    nams[[j - 1]] = paste(as.formula(forms[[j]]))[2]
  }
  names(bootfit_a) = unlist(nams)
  
  ## Each Indirect Effect ##
  apa = bpa = est = low = hi = list()
  for (i in ind_effects){
    ap = gsub("\\-.*$", "", i)
    bp = gsub("^.*\\-", "", i)
    
    apa[[i]] = bootfit_a[[bp]]$t0[ap]
    bpa[[i]] = bootfit_b$t0[bp]
    
    est[[i]] = bootfit_a[[bp]]$t0[ap] * bootfit_b$t0[bp]
    low[[i]] = quantile(bootfit_a[[bp]]$t %>%
                          data.frame %>%
                          setNames(names(bootfit_a[[bp]]$t0)) %>%
                          .[[ap]] *
                          bootfit_b$t %>%
                          data.frame %>%
                          setNames(names(bootfit_b$t0)) %>%
                          .[[bp]], (1-ci)/2,
                        na.rm=TRUE)
    hi[[i]] = quantile(bootfit_a[[bp]]$t %>%
                         data.frame %>%
                         setNames(names(bootfit_a[[bp]]$t0)) %>%
                         .[[ap]] *
                         bootfit_b$t %>%
                         data.frame %>%
                         setNames(names(bootfit_b$t0)) %>%
                         .[[bp]], (1-(1-ci)/2),
                       na.rm=TRUE)
  }
  .ame_ind = data.frame(do.call("rbind", apa),
                        do.call("rbind", bpa),
                        do.call("rbind", est),
                        do.call("rbind", low),
                        do.call("rbind", hi))
  names(.ame_ind) = c("Apath", "Bpath", "Indirect", "Lower", "Upper")
  
  ## Each Direct Effect ##
  eff = low2 = hi2 = list()
  dir_effects = lapply(ind_effects, function(i) gsub("\\-.*$", "", i)) %>%
    unlist %>%
    unique
  for (i in dir_effects){
    ap = gsub("\\-.*$", "", i)

    eff[[i]] = bootfit_b$t0[ap]

    low2[[i]] = quantile(bootfit_b$t %>%
                          data.frame %>%
                          setNames(names(bootfit_b$t0)) %>%
                          .[, ap], (1-ci)/2)
    hi2[[i]] = quantile(bootfit_b$t %>%
                         data.frame %>%
                         setNames(names(bootfit_b$t0)) %>%
                         .[, ap], (1-(1-ci)/2))
  }
  .ame_dir = data.frame(do.call("rbind", eff),
                        do.call("rbind", low2),
                        do.call("rbind", hi2))
  names(.ame_dir) = c("Direct", "Lower", "Upper")
  
  final = structure(
    list("ind_effects" = .ame_ind,
         "dir_effects" = .ame_dir,
         "ci_level" = ci,
         "data" = data, 
         "reported_ind" = ind_effects, 
         "boot" = boot, 
         "model" = forms,
         "call" = .call),
    class = c("mma", "list")
  )
  cat('\r', rep(' ', 40), '\r')
  final
}

#' import crayon
#' @export
print.mma = function(x, ...){
  cat(crayon::bold$inverse("\n  Marginal Mediation Analysis  \n") %+% "\n")
  cat("A marginal mediation model with:\n")
  cat("  ", length(x$model)-1, "mediators\n")
  cat("  ", length(x$ind_effects), "indirect effects\n")
  cat("  ", length(x$dir_effects), "direct effects\n")
  cat("  ", x$boot, "bootstrapped samples\n") 
  cat("   ", x$ci_level * 100, "% confidence interval\n", sep = "")
  cat("   n =", length(x$data[[1]]), "\n\n")
  
  cat("-- " %+% crayon::bold("Indirect Effect(s)") %+% " --\n")
  print.data.frame(round(x$ind_effects, 5), ...)
  
  cat("\n-- " %+% crayon::bold("Direct Effect(s)") %+% " --\n")
  print.data.frame(round(x$dir_effects, 5), ...)
  cat("---------------------------")
}



