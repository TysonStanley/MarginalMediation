#' @title Average Marginal Effects
#' @author Tyson S. Barrett
#' @description Internal function for \code{mma()}. Based on the same strategy as 
#' \code{margins} by T. Leeper.
#' 
#' @param model the model object
#' 
#' @import stats
#' 
#' @export
amed <- function(model){
  
  if (class(model)[1] == "glm"){
    data   <- model$data
    coefs  <- attr(model$terms, "term.labels")
  } else {
    data    <- model$model
    coefs   <- attr(model$terms$full, "term.labels")
  }

  ## Remove interactions and other adjustments
  coefs <- ifelse(grepl(":|^[A-Za-z].*)$", coefs), NA, coefs) %>%
    na.omit %>%
    unclass
  
  if (any(class(data) == "environment") || is.null(data)){
    stop("GLM (or betareg) model must contain a data argument", call. = FALSE)
  }
  
  aveMarg <- vector("numeric", 0L)
  for (i in seq_along(coefs)){
    d <- eval(parse(text = coefs[i]), data)
    
    if (is.character(d)) 
      d <- factor(d)
    
    if (is.numeric(d)){
      ## Average Marginal Effects
      aveMarg[coefs[i]] <- dydx_continuous(data, model, coefs[i])
    } else if (is.factor(d)) {
      ref   <- levels(d)[1]
      levs  <- levels(d)[-1]
      d0 = d1 = data
      d0[[coefs[i]]] <- ref
      pred0 <- predict(model, newdata = d0, type = "response")
      
      for (j in levs){
        d1[[coefs[i]]] <- j
        pred1 <- predict(model, newdata = d1, type = "response")
        
        aveMarg[paste0(coefs[i], j)] <- mean(pred1 - pred0, na.rm=TRUE)
      }
    }
  }
  aveMarg
}


## continuous variable AME
## see margins package by Leeper for more on this
setstep <- function(x) {
  x + (max(abs(x), 1, na.rm = TRUE) * sqrt(1e-7)) - x
}

dydx_continuous <- function(data, model, variable){
  d0 = d1 = data
  
  d0[[variable]] <- d0[[variable]] - setstep(d0[[variable]])
  d1[[variable]] <- d1[[variable]] + setstep(d1[[variable]])
  
  P0 = predict(model, newdata = d0, type = "resp")
  P1 = predict(model, newdata = d1, type = "resp")
  
  out <- (P1 - P0) / (d1[[variable]] - d0[[variable]])
  mean(out, na.rm=TRUE)
}


## function to bootstrap
.run_mod <- function(data, indices, model){
  model$call["data"] <- parse(text = "data[indices, ]")
  eval(model$call) %>% 
    amed
}
.run_mod_svy <- function(data, indices, model){
  model <- model
  data  <- model$data[indices, ]
  data$weights = data$.survey.prob.weights
  glm(formula = model$formula, 
      data = data, 
      family = model$family, 
      control = model$control,
      weights = weights, 
      contrasts = model$contrasts) %>% 
    amed
}

## all vars used
all_used_vars <- function(forms){
  sapply(forms, paste) %>% 
    .[!grepl("~", .)] %>% 
    stringr::str_split(., " ") %>% 
    unlist() %>% 
    .[!grepl("\\+|I\\(|\\^|\\/", .)] %>% 
    stringr::str_trim(.) %>% 
    unique()
}


## checks
.boot_checker <- function(boot){
  if(boot > 5000){
    message("A bootstrap size above 5000 may take a long time to compute...")
  }
}

#' @importFrom purrr map
.ind_checker <- function(ind_effects, models, forms=NULL){
  yesno = lapply(ind_effects, function(x) is.character(x) & grepl("-", x)) %>%
    unlist %>%
    all
  if (!yesno){
    stop("The 'ind_effects' must be a character vector of effect paths.",
         call. = FALSE)
  }
  
  forms1 = purrr::map(forms, ~paste(.x)) %>%
    purrr::map(~.x[2]) %>% unlist
  yesno2 = gsub("^.*\\-", "", ind_effects) %in% forms1
  if (!all(yesno2)){
    mess = paste(paste(unique(gsub("^.*\\-", "", ind_effects)[!yesno2]), collapse = ", "), "is/are not mediators in the model")
    stop(mess,
         call. = FALSE)
  }
  
  vars1 = purrr::map(models, ~names(.x$coefficients)) %>%
    unlist %>% unique
  yesno3 = gsub("\\-.*$", "", ind_effects) %in% vars1
  if (!all(yesno3)){
    mess = paste(paste(unique(gsub("\\-.*$", "", ind_effects)[!yesno3]), collapse = ", "), "is/are not predictors in the model")
    stop(mess,
         call. = FALSE)
  }
}

.ci_checker <- function(ci){
  if (ci > 1 | ci < 0){
    stop("CI must be between 0 and 1.",
         call. = FALSE)
  }
}

.var_checker <- function(data){
  yes_no <- data %>% 
    sapply(function(x) length(unique(x)))
  
    if (any(yes_no < 2)){
      warning(paste("Variable", names(data)[, yes_no < 2], "is constant"), 
              call. = FALSE) 
  }
}

.nrows_checker <- function(model){
  pathbc <- model$pathbc
  pathas <- model$patha
  
  df <- vector("list", length(pathas)+1)
  df[[1]] <- resid(pathbc)
  for (i in 2:(length(pathas)+1)){
    
    df[[i]] <- resid(pathas[[i-1]])
    
  }
  
  yes_no <- sapply(df, length) %>%
    sapply(., function(x) x == .[[1]]) %>%
    all() %>%
    isTRUE()
  
  if (!yes_no){
    warning(paste("The data sets used in the different paths are of different sizes."), 
            call. = FALSE) 
  }
}

is.mma <- function(mods){
  class(mods)[1] == "mma"
}


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

## Other functions
is.mma <- function(x){
  class(x)[1] == "mma"
}

is.pos <- function(x){
  x > 0
}

is.neg <- function(x){
  x < 0
}




#' re-export magrittr pipe operator
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL

## From tidyverse package
text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }
  
  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }
  
  theme <- rstudioapi::getThemeInfo()
  
  if (isTRUE(theme$dark)) crayon::white(x) else crayon::black(x)
  
}

MarginalMediation_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])
  crayon::italic(paste0(version, collapse = "."))
}

search_conflicts <- function(path = search()){
  
  ## Search for conflicts
  confs <- conflicts(path,TRUE)
  ## Grab those with the MarginalMediation package
  MarginalMediation_conflicts <- confs$`package:MarginalMediation`
  
  ## Find which packages have those functions that are conflicted
  if (length(MarginalMediation_conflicts) != 0){
    other_conflicts <- list()
    for (i in MarginalMediation_conflicts){
      other_conflicts[[i]] <- lapply(confs, function(x) any(grepl(i, x))) %>%
        do.call("rbind", .) %>%
        data.frame %>%
        setNames(c("conflicted")) %>%
        tibble::rownames_to_column() %>%
        .[.$conflicted == TRUE &
            .$rowname != "package:MarginalMediation",]
    }
  } else {
    other_conflicts <- data.frame()
  }
  other_conflicts
}




