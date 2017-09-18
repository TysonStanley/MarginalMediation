#' @title PDF of logistic, probit, and poisson models
#' @author Tyson S. Barrett
#' @description Internal function for \code{mma()}. More functionality to be added later.
#' 
#' @param model the model
#' 
#' @import stats
#' 
#' @export
pdfed = function(model){
  
  data   = model$data
  family = model$family

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

## function to bootstrap
.run_mod = function(data, indices, formula, family){
  data[indices, ] %>%
    glm(formula, family = family, data = .) %>% 
    pdfed
}

## checks
.family_checker = function(family){
  j = 0
  fam = list()
  for (i in family){
    j = j + 1
    fam[[j]] = i()$link
    fam = unlist(fam)
  }
  if(!all(fam %in% c("probit", "logit", "log", "identity"))){
    stop(message("The link function must be probit, logit, log, or idenitity."))
  }
}

.arg_checker = function(...){
  j = 0
  arg = list()
  for (i in c(...)){
    j = j + 1
    arg[[j]] = inherits(i, "formula")
    arg = unlist(arg)
  }
  if(!all(arg)){
    stop(message("The ... (ellipses) must be formulas."))
  }
}


#' Piping operator re-exported from \code{magrittr}
#' 
#' @param lhs left-hand side
#' @param rhs right-hand side
#' 
#' @export
`%>%` = magrittr::`%>%`