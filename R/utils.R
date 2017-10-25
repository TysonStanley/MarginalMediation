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
  coefs  = attr(model$terms, "term.labels")
  
  aveMarg = vector("numeric", 0L)
  for (i in seq_along(coefs)){
    d = data[[coefs[i]]]
    
    if (is.numeric(d)){
      ## Derivatives
      pdf  = ifelse(family[[2]]=="probit",
                    mean(dnorm(predict(model, type = "link")), na.rm=TRUE),
                    ifelse(family[[2]]=="logit", 
                           mean(dlogis(predict(model, type = "link")), na.rm=TRUE),
                           ifelse(family[[1]]=="poisson",
                                  mean(model$y, na.rm=TRUE), 
                                  ifelse(family[[2]]=="identity", 1, NA))))
      ## Average Marginal Effects
      aveMarg[coefs[i]] = pdf*coef(model)[i+1]
    } else if (is.factor(d) | is.character(d)) {
      ref   = levels(d)[1]
      levs  = levels(d)[-1]
      d0 = d1 = data
      d0[[coefs[i]]] = ref
      pred0 = predict(model, newdata = d0, type = "response")
      
      for (j in levs){
        d1[[coefs[i]]] = j
        pred1 = predict(model, newdata = d1, type = "response")
        
        aveMarg[paste0(coefs[i], j)] = mean(pred1 - pred0, na.rm=TRUE)
      }
    }
  }
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
  if(boot > 5000){
    cat("A bootstrap size above 5000 may take a long time to compute...")
  }
}

.ind_checker = function(ind_effects, forms=NULL){
  yesno = lapply(ind_effects, function(x) is.character(x) & grepl("-", x)) %>%
    unlist %>%
    all
  if (!yesno){
    stop("The 'ind_effects' must be a character vector of effect paths.",
         call. = FALSE)
  }
  
  forms = lapply(paste(forms)[-1], function(x) paste(as.formula(x))[2]) %>% unlist
  yesno2 = all(gsub("^.*\\-", "", ind_effects) %in% forms)
  if (!yesno2){
    stop("One of the ind_effects has a mediator listed that is not a mediator in the formulas.",
         call. = FALSE)
  }
}

.ci_checker = function(ci){
  if (ci > 1 | ci < 0){
    stop("CI must be between 0 and 1.",
         call. = FALSE)
  }
}

.var_checker = function(data, forms){
  for (i in seq_along(forms)){
    yes_no = model.matrix(forms[[i]], data)[, -1] %>%
      data.frame %>%
      sapply(function(x) length(unique(x)) == 1) %>%
      any()

    if (yes_no){
      warning(paste("Variable(s) to be used in data frame for the formula number", i, "are constant"), 
              call. = FALSE) 
    }
  }
}


`%>%` = magrittr::`%>%`



## Functions from boot for confidence intervals

## # part of R package boot
## # copyright (C) 1997-2001 Angelo J. Canty
## # corrections (C) 1997-2014 B. D. Ripley
## #
## # Unlimited distribution is permitted

## norm inter
norm.inter <- function(t,alpha){
  #
  #  Interpolation on the normal quantile scale.  For a non-integer
  #  order statistic this function interpolates between the surrounding
  #  order statistics using the normal quantile scale.  See equation
  #  5.8 of Davison and Hinkley (1997)
  #
  t  <- t[is.finite(t)]
  R  <- length(t)
  rk <- (R+1)*alpha
  if (!all(rk>1 & rk<R))
    warning("extreme order statistics used as endpoints")
  k    <- trunc(rk)
  inds <- seq_along(k)
  out  <- inds
  kvs  <- k[k>0 & k<R]
  tstar <- sort(t, partial = sort(union(c(1, R), c(kvs, kvs+1))))
  ints  <- (k == rk)
  if (any(ints)) out[inds[ints]] <- tstar[k[inds[ints]]]
  out[k == 0] <- tstar[1L]
  out[k == R] <- tstar[R]
  not <- function(v) xor(rep(TRUE,length(v)),v)
  temp <- inds[not(ints) & k != 0 & k != R]
  temp1 <- qnorm(alpha[temp])
  temp2 <- qnorm(k[temp]/(R+1))
  temp3 <- qnorm((k[temp]+1)/(R+1))
  tk <- tstar[k[temp]]
  tk1 <- tstar[k[temp]+1L]
  out[temp] <- tk + (temp1-temp2)/(temp3-temp2)*(tk1 - tk)
  cbind(round(rk, 2), out)
}

basic.ci <- function(t0, t, conf = 0.95, hinv = function(t) t){
  #  Basic bootstrap confidence method
  qq <- norm.inter(t,(1+c(conf,-conf))/2)
  cbind(matrix(hinv(2*t0-qq[,2L]),ncol=2L))
}

perc.ci <- function(t, conf = 0.95, hinv = function(t) t){
  #  Bootstrap Percentile Confidence Interval Method
  alpha <- (1+c(-conf,conf))/2
  qq <- norm.inter(t,alpha)
  cbind(matrix(hinv(qq[,2]),ncol=2L))
}


