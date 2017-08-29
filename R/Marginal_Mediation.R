#' @title PDF of logistic, probit, and poisson models
#' @author Tyson S. Barrett
#' @description Internal function for \code{margins_mediation()}. More functionality to be added later.
#' 
#' @param model the model
#' 
#' @import stats
#' 
#' @export
pdfed = function(model){
  
  ## Initial Model and Data
  data   = model$data
  family = model$family
  if(!family[[2]] %in% c("probit", "logit", "poisson", "identity")){
    stop(message("Must be probit, logit, log, or idenitity linked"))
  }
  ## Derivatives
  pdf  = ifelse(family[[2]]=="probit",
                mean(dnorm(predict(model, type = "link")), na.rm=TRUE),
         ifelse(family[[2]]=="logit", 
                mean(dlogis(predict(model, type = "link")), na.rm=TRUE),
         ifelse(family[[1]]=="poisson",
                mean(dpois(predict(model, type = "link")), na.rm=TRUE),
         ifelse(family[[2]]=="identity", 1, NA))))
  ## Average Marginal Effects
  aveMarg = pdf*coef(model)
  aveMarg
}


#' @title Bootstrapped Models 
#' @author Tyson S. Barrett
#' @description Internal function for \code{margins_mediation()}.
#' 
#' @param data the data.frame with the data for the models.
#' @param indices the indices for each bootstrapped model (internal in \code{boot()})
#' @param formula the formula of the model
#' @param family the family of the model
#' 
#' @import stats
#' @import magrittr
#' 
#' @export
run_mod = function(data, indices, formula, family){
  data[indices, ] %>%
    glm(formula, family = family, data = .) %>% 
    pdfed
}


#' @title Marginal Mediation
#' @author Tyson S. Barrett
#' @description Provides the ability to perform marginal mediation. Marginal mediation
#' is particularly useful for situations where the mediator or outcome is dichotomous.
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
#' @references Bartus, T. (2005). Estimation of marginal effects using margeff. 
#' The Stata Journal, 5(3), 309–329.
#' 
#' @import stats
#' @import magrittr
#' @import boot
#' 
#' @export
mma = function(data, ..., family, ind_effects, boot=100, ci=.95){
  ## Models
  forms = list(...)
  
  ## Bootstrapped Samples and Statistics
  bootfit_b = boot(data = data, 
                   statistic = run_mod, 
                   R = boot, 
                   formula = forms[[1]],
                   family = binomial)
  bootfit_a = lapply(seq_along(forms)[-1], function(i) { 
    boot(data = data, 
         statistic = run_mod, 
         R = boot, 
         formula = forms[[i]],
         family = gaussian)
  })
  
  nams = list()
  for (j in seq_along(forms)[2:length(forms)]){
    nams[[j - 1]] = paste(as.formula(forms[[j]]))[2]
  }
  names(bootfit_a) = unlist(nams)
  
  ## Each Indirect Effect
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
                          .[[bp]], (1-ci)/2)
    hi[[i]] = quantile(bootfit_a[[bp]]$t %>%
                         data.frame %>%
                         setNames(names(bootfit_a[[bp]]$t0)) %>%
                         .[[ap]] *
                         bootfit_b$t %>%
                         data.frame %>%
                         setNames(names(bootfit_b$t0)) %>%
                         .[[bp]], (1-(1-ci)/2))
  }
  
  .ame = data.frame(do.call("rbind", apa),
                    do.call("rbind", bpa),
                    do.call("rbind", est),
                    do.call("rbind", low),
                    do.call("rbind", hi))
  names(.ame) = c("Apath", "Bpath", "Estimate", "Lower", "Upper")
  .ame$Levels = ci
  
  class(.ame) = c("mma", "data.frame")
  .ame
}

#' @export
print.mma = function(x, ...){
  cat("--- \n Marginal Mediation \n")
  print.data.frame(round(x[,1:5], 5), ...)
  cat("---")
  cat("\n Confidence Interval at", x$Level[1], "\n")
}


`%>%` = magrittr::`%>%`

