#' @title Marginal Mediation
#' @author Tyson S. Barrett
#' @description Provides the ability to perform marginal mediation. Marginal mediation
#' is particularly useful for situations where the mediator or outcome is categorical,
#' a count, or some other non-normally distributed variable.
#' The results provide the average marginal effects of the models, providing simple
#' interpretation of the indirect effects.
#' 
#' @param ... the glm model objects; the first is the model with the outcome while the others are the mediated effects ("a" paths)
#' @param ind_effects a vector of the desired indirect effects. Has the form \code{"var1-var2"}.
#' @param ci_type a string indicating the type of bootstrap method to use (currently "perc" and "basic" are available; "perc" is recommended). Further development will allow the Bias-Corrected bootstrap soon.
#' @param boot the number of bootstrapped samples; default is 500.
#' @param ci the confidence interval; the default is .95 which is the 95\% confidence interval.
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
#' 
#' ## A minimal example:
#' 
#' library(furniture)
#' data(nhanes_2010)
#' bcpath = glm(marijuana ~ home_meals + gender + age + asthma, 
#'            data = nhanes_2010, 
#'            family = "binomial")
#' apath = glm(home_meals ~ gender + age + asthma,
#'            data = nhanes_2010, 
#'            family = "gaussian")
#' (fit = mma(bcpath, apath, 
#'            ind_effects = c("genderFemale-home_meals",
#'                            "age-home_meals",
#'                            "asthmaNo-home_meals"),
#'            boot = 10))
#' 
#' @references 
#' Bartus, T. (2005). Estimation of marginal effects using margeff. 
#' The Stata Journal, 5(3), 309–329.
#' 
#' MacKinnon, D. (2008). Introduction to Statistical Mediation Analysis. 
#' Taylor & Francis, LLC.
#' 
#' @import stats
#' @import magrittr
#' @import boot
#' @import stringr
#' 
#' @export
mma = function(..., ind_effects, ci_type = "perc", boot = 500, ci = .95){
  .call = match.call()
  models <- list(...)
  forms <- lapply(models, function(x) x$call$formula)
  
  all_used_vars <- all_used_vars(forms)
  data <- eval(models[[1]]$call$data, parent.frame()) %>% 
    .[, all_used_vars]
  
  ## checks
  stopifnot(length(models) > 1)
  .boot_checker(boot)
  .ind_checker(ind_effects, models, forms)
  .ci_checker(ci)
  .var_checker(data)
  .nrows_checker(models)
  
  ## Use survey weights?
  if (class(models[[1]])[1] == "svyglm" || class(models[[1]])[1] == "svyreg")
    .run_mod <- .run_mod_svy
  
  ## Bootstrapped Samples and Statistics
  cat("\ncalculating a paths... ")
  bootfit_a <- lapply(seq_along(forms)[-1], function(i) { 
    boot(data = data, 
         statistic = .run_mod, 
         R = boot, 
         model = models[[i]])
  })
  
  cat('b and c paths... ')
  bootfit_b <- boot(data,
                    statistic = .run_mod,
                    R = boot,
                    model = models[[1]])
  cat('Done.')
  
  nams = list()
  for (j in seq_along(forms)[2:length(forms)]){
    nams[[j - 1]] <- paste(as.formula(forms[[j]]))[2]
  }
  names(bootfit_a) <- unlist(nams)
  
  ## Each Indirect Effect ##
  apa = bpa = est = low = hi = list()
  for (i in ind_effects){
    ap = gsub("\\-.*$", "", i)
    bp = gsub("^.*\\-", "", i)
    bp_nam = paste0(bp, levels(data[[bp]])[2])
    
    apa[[i]] = bootfit_a[[bp]]$t0[ap]
    bpa[[i]] = bootfit_b$t0[bp_nam]
    
    booted = bootfit_a[[bp]]
    indirect = booted$t %>%
      data.frame %>%
      setNames(names(booted$t0)) %>%
      .[[ap]] * bootfit_b$t %>%
      data.frame %>%
      setNames(names(bootfit_b$t0)) %>%
      .[[bp_nam]]
    
    booted$t = as.matrix(indirect)
    booted$t0 = bootfit_a[[bp]]$t0[ap]
    
    est[[i]] = bootfit_a[[bp]]$t0[ap] * bootfit_b$t0[bp_nam]
    
    if (ci_type == "perc"){
      low[[i]] = perc.ci(booted$t, conf = ci)[1]
      hi[[i]]  = perc.ci(booted$t, conf = ci)[2]
    } else if (ci_type == "basic"){
      warning("Basic is not recommended with indirect effects due to its reliance on normal theory.")
      low[[i]] = basic.ci(booted$t, conf = ci)[1]
      hi[[i]]  = basic.ci(booted$t, conf = ci)[2]
    } else {
      stop("Only 'perc' and 'basic' types of CI are currently available.")
    }
  }
  .ame_ind = data.frame(do.call("rbind", est),
                        do.call("rbind", low),
                        do.call("rbind", hi))
  names(.ame_ind) = c("Indirect", "Lower", "Upper")
  
  ## Each Direct Effect ##
  eff = low2 = hi2 = list()
  dir_effects = lapply(ind_effects, function(i) gsub("\\-.*$", "", i)) %>%
    unlist %>%
    unique
  for (i in dir_effects){
    ap = gsub("\\-.*$", "", i)
    
    eff[[i]] = bootfit_b$t0[ap]
    
    booted2 = bootfit_b$t %>%
      data.frame %>%
      setNames(names(bootfit_b$t0)) %>%
      .[[ap]]
    
    if (ci_type == "perc"){
      low2[[i]] = perc.ci(booted2, conf = ci)[1]
      hi2[[i]]  = perc.ci(booted2, conf = ci)[2]
    } else if (ci_type == "basic"){
      low2[[i]] = basic.ci(booted2, conf = ci)[1]
      hi2[[i]]  = basic.ci(booted2, conf = ci)[2]
    } else {
      stop("Only 'perc' and 'basic' types of CI are currently available.")
    }
  }
  .ame_dir = data.frame(do.call("rbind", eff),
                        do.call("rbind", low2),
                        do.call("rbind", hi2))
  names(.ame_dir) = c("Direct", "Lower", "Upper")
  
  ## Standardized Results
  outcome = data[[paste(forms[[1]])[2]]]
  if (is.factor(outcome) || length(unique(na.omit(outcome))) <= 2) {
    sigma_y = NA
  } else {
    sigma_y = sd(outcome, na.rm=TRUE)
  }

  
  ## Final object
  final = structure(
    list("ind_effects"  = .ame_ind,
         "dir_effects"  = .ame_dir,
         "ci_level"     = ci,
         "data"         = data, 
         "reported_ind" = ind_effects, 
         "boot"         = boot, 
         "model"        = forms,
         "call"         = .call,
         "sigma_y"      = sigma_y,
         "pathbc"       = models[[1]],
         "patha"        = lapply(seq_along(models)[-1], function(i) models[[i]]),
         "AMEbc"        = do.call("rbind", bpa),
         "AMEa"         = do.call("rbind", apa)),
    class = c("mma", "list")
  )
  cat('\r', rep(' ', 40), '\r')
  final
}




#' @export
print.mma = function(x, ..., all=TRUE){
  cat("\u250C", rep("\u2500", 31), "\u2510\n", sep = "")
  cat("\u2502", " Marginal Mediation Analysis ", "\u2502")
  cat("\n\u2514", rep("\u2500", 31), "\u2518\n", sep = "")
  
  cat("A marginal mediation model with:\n")
  cat("  ", length(x$model)-1, "mediators\n")
  cat("  ", dim(x$ind_effects)[1], "indirect effects\n")
  cat("  ", dim(x$dir_effects)[1], "direct effects\n")
  cat("  ", x$boot, "bootstrapped samples\n") 
  cat("   ", x$ci_level * 100, "% confidence interval\n", sep = "")
  cat("   n =", length(x$data[[1]]), "\n\n")
  
  cat("Formulas:\n")
  cat("   \u25cc", paste(x$model, collapse = "\n   \u25cc "), "\n\n")
  
  cat("Regression Models: ", "\n\n", sep = "")
  cat("    ", paste(x$pathbc$formula)[2], "~ \n")
  
  if (class(x$pathbc)[1] == 'betareg'){
    pathbc_coefs <- x$pathbc %>% summary() %>% coef() %>% .$mean %>% data.frame()
  } else {
    pathbc_coefs <- x$pathbc %>% summary() %>% coef() %>% data.frame()
  }
  names(pathbc_coefs) <- c("Est", "SE", "Est/SE", "P-Value")
  print.data.frame(round(pathbc_coefs, 5), row.names = paste("       ", rownames(pathbc_coefs)), ...)
  cat("\n")
  
  for (i in seq_along(x$patha)){
    
    ap <- x$patha[[i]]
    
    cat("    ", paste(ap$formula)[2], "~ \n")
    if (class(ap)[1] == 'betareg'){
      pathbc_coefs <- ap %>% summary() %>% coef() %>% .$mean %>% data.frame()
    } else {
      pathbc_coefs <- ap %>% summary() %>% coef() %>% data.frame()
    }
    
    patha_coefs <- ap %>% summary() %>% coef() %>% data.frame()
    names(patha_coefs) <- c("Est", "SE", "Est/SE", "P-Value")
    print.data.frame(round(patha_coefs, 5), row.names = paste("       ", rownames(patha_coefs)), ...)
    cat("\n")
    
  }
  
  cat("Unstandardized Mediated Effects: ", "\n\n", sep = "")
  cat("   ", "Indirect Effects: ", "\n\n", sep = "")
  cat("    ", paste(x$pathbc$formula)[2], "~ \n")
  pathbc_rows <- gsub("-", " => ", rownames(x$ind_effects))
  print.data.frame(round(x$ind_effects, 5), row.names = paste("       ", pathbc_rows), ...)
  
  cat("\n   ", "Direct Effects: ", "\n\n", sep = "")
  cat("    ", paste(x$pathbc$formula)[2], "~ \n")
  print.data.frame(round(x$dir_effects, 5), row.names = paste("       ", rownames(x$dir_effects)), ...)
  
  if (all & !is.na(x$sigma_y)){
    cat("\n\n")
    sigma_y = x$sigma_y
    cat("Standardized Mediated Effects: ", "\n\n", sep = "")
    
    std_ind = x$ind_effects/sigma_y
    cat("   ", "Indirect Effects: ", "\n\n", sep = "")
    cat("    ", paste(x$pathbc$formula)[2], "~ \n")
    patha_rows <- gsub("-", " => ", rownames(x$ind_effects))
    print.data.frame(round(std_ind, 5), row.names = paste("       ", patha_rows), ...)
    
    std_dir = x$dir_effects/sigma_y
    cat("\n   ", "Direct Effects: ", "\n\n", sep = "")
    cat("    ", paste(x$pathbc$formula)[2], "~ \n")
    print.data.frame(round(std_dir, 5), row.names = paste("       ", rownames(std_dir)), ...)
  }
  cat("\n")
}



