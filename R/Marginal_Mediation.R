#' @title Bootstrapped Models 
#' @author Tyson S. Barrett
#' @description Internal function for \code{margins_mediation()}.
#' 
#' @param data the data.frame with the data for the models.
#' @param mods the formulas for the models.
#' @param family the family of the GLM model.
#' @param boot the number of bootstrapped samples.
#' 
#' @import stats
#' 
#' @export
run_mods = function(data, mods, family, boot=100){
  ## Initialize
  forms = mods
  fit = outcome = list()
  dims = dim(data)[1]
  
  ## Bootstrap Samples
  reps1 = replicate(boot+1, sample(1:dims, size = dims, replace = TRUE))
  reps1[,1] = 1:dims
  
  ## Model for each
  for (i in seq_along(forms)){
    fit[[i]] = apply(reps1, 2, function(x) 
      glm(forms[[i]], family = family[[i]], data = data[x,]))
    outcome[[i]] = paste(forms[[i]])[[2]]
  }
  
  ## Output
  outcome = do.call("rbind", outcome)
  predictors = ifelse(!names(coef(fit[[1]][[1]])) %in% outcome, names(coef(fit[[1]][[1]])), NA)
  predictors = predictors[!is.na(predictors) & predictors != "(Intercept)"]
  names(fit) = c(paste(outcome)[1], 
                 paste("Mediator:", outcome[-1]))
  return(list("Models"     = fit, 
              "Outcome"    = outcome[1], 
              "Mediators"  = outcome[-1], 
              "Predictors" = predictors))
}

#' @title PDF of Logistic and Gaussian Models
#' @author Tyson S. Barrett
#' @description Internal function for \code{margins_mediation()}.
#' 
#' @param x the model
#' @param family family of the model
#' 
#' @import stats
#' 
#' @export
pdfed = function(x, family){
  pdf  = ifelse(family=="binomial", 
                mean(dlogis(predict(x, type = "link")), na.rm=TRUE),
         ifelse(family=="gaussian", 1, NA))
  return(pdf)
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
#' 
#' @export
margins_mediation = function(data, ..., family, ind_effects, boot=100, ci=.975){
  ## Models
  mods = list(...)
  fit = run_mods(data=data, mods=mods, family=family, boot=boot)
  
  ## Initialize
  ide = ind_effects
  apath = bpath = indirect = final = list()

  ## Effects 
  for (i in 1:length(ide)){
    ## Individual Paths AME
    ap  = gsub("\\-.*$", "", ide[[i]])
    bp  = gsub("^.*\\-", "", ide[[i]])
    ap2 = lapply(fit[[1]][[2]], function(x) coef(x)[ap])
    ap3 = lapply(fit[[1]][[2]], function(x) pdfed(x, family[2]))
    bp2 = lapply(fit[[1]][[1]], function(x) coef(x)[bp]) 
    bp3 = lapply(fit[[1]][[1]], function(x) pdfed(x, family[1]))
    apath[[i]] = unlist(ap2)[-1] * unlist(ap3)[-1]
    bpath[[i]] = unlist(bp2)[-1] * unlist(bp3)[-1]
    indirect[[i]] = apath[[i]] * bpath[[i]]
    indirect[[i]] = unlist(indirect[[i]])
    
    ## AME Mediation
    apa = unlist(ap2)[1] * unlist(ap3)[1]
    bpa = unlist(bp2)[1] * unlist(bp3)[1]
    est = unlist(ap2)[1] * unlist(ap3)[1] * unlist(bp2)[1] * unlist(bp3)[1]
    low = quantile(indirect[[i]], probs=1-ci, na.rm=TRUE)
    upp = quantile(indirect[[i]], probs=ci, na.rm=TRUE)
    final[[i]] = data.frame("APath"=apa,
                            "BPath"=bpa,
                            "Indirect"=est, 
                            "Lower"=low, 
                            "Upper"=upp,
                            row.names = ide[[i]])
  }
  
  final = do.call("rbind", final)
  .ame = list("Indirect"=final)
  class(.ame) = c("ame", "list")
  return(.ame)
}

