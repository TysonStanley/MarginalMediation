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
                              mean(model$y, na.rm=TRUE), 
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
    stop("The link function must be probit, logit, log, or idenitity.")
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
    stop("The ... (ellipses) must be formulas.")
  }
}

.boot_checker = function(boot){
  if(boot > 1000){
    cat("A bootstrap size above 1000 may take a long time to compute...")
  }
}

.ind_checker = function(ind_effects){
  yesno = lapply(ind_effects, function(x) is.character(x) & grepl("-", x)) %>%
    unlist %>%
    all
  if (!yesno){
    stop("The 'ind_effects' must be a character vector of effect paths.")
  }
}

.ci_checker = function(ci){
  if (ci > 1 | ci < 0){
    stop("CI must be between 0 and 1.")
  }
}

.var_checker = function(data, forms){
  for (i in seq_along(forms)){
    yes_no = model.matrix(forms[[i]], data)[, -1] %>%
      data.frame %>%
      sapply(function(x) length(unique(x)) == 1) %>%
      any()

    if (yes_no){
      warning(paste("Variable(s) to be used in data frame for the formula number", i, "are constant"), call. = FALSE) 
    }
  }
}


`%>%` = magrittr::`%>%`
