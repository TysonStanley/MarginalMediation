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

#' Piping operator re-exported from \code{magrittr}
#' 
#' @param lhs left-hand side
#' @param rhs right-hand side
#' 
#' @export
`%>%` = magrittr::`%>%`