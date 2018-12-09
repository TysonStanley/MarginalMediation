#' Percent Mediation
#' 
#' To obtain the percent of the total effect that is mediated through the specified
#' indirect path: indirect / (total) * 100.
#' 
#' @param model mma fit object
#' @param effect the indirect effect to be compared to its direct path
#' 
#' @export
perc_med <- function(model, effect){
  if (!is.mma(model)){
    stop("Model must be mma object", call. = FALSE)
  }
  
  ind = model$ind_effects[effect, 3]
  dir = model$dir_effects[gsub("-.*$", "", effect), 1]
  
  if ((is.pos(ind) & is.neg(dir)) | 
      (is.neg(ind) & is.pos(dir))){
    warning("The indirect and direct effects are of opposite signs.\nThere is not a meaningful percent of mediation here.", call. = FALSE)
  }
  
  val = ind/(dir + ind) * 100
  names(val) = effect
  val
}

#' Formula Extraction for MMA
#' 
#' Extracts the formulas from a mma object                                                                                                        
#' 
#' @param model mma fit object
#' 
#' @export
mma_formulas <- function(model){
  if (!is.mma(model)){
    stop("Model must be mma object", call. = FALSE)
  }
  model$model
}

#' Indirect Effects Extraction for MMA
#' 
#' Extracts the formulas from a mma object                                                                                                        
#' 
#' @param model mma fit object
#' 
#' @export
mma_ind_effects <- function(model){
  if (!is.mma(model)){
    stop("Model must be mma object", call. = FALSE)
  }
  model$ind_effects
}

#' Direct Effects Extraction for MMA
#' 
#' Extracts the formulas from a mma object                                                                                                        
#' 
#' @param model mma fit object
#' 
#' @export
mma_dir_effects <- function(model){
  if (!is.mma(model)){
    stop("Model must be mma object", call. = FALSE)
  }
  model$dir_effects
}

#' Standardized Indirect Effects Extraction for MMA
#' 
#' Extracts the formulas from a mma object                                                                                                        
#' 
#' @param model mma fit object
#' 
#' @export
mma_std_ind_effects <- function(model){
  if (!is.mma(model)){
    stop("Model must be mma object", call. = FALSE)
  }
  if (is.na(model$sigma_y)){
    stop("Cannot produce standardized effects with non-numeric outcomes", call. = FALSE)
  }
  model$ind_effects/model$sigma_y
}

#' Standardized Direct Effects Extraction for MMA
#' 
#' Extracts the formulas from a mma object                                                                                                        
#' 
#' @param model mma fit object
#' 
#' @export
mma_std_dir_effects <- function(model){
  if (!is.mma(model)){
    stop("Model must be mma object", call. = FALSE)
  }
  if (is.na(model$sigma_y)){
    stop("Cannot produce standardized effects with non-numeric outcomes", call. = FALSE)
  }
  model$ind_effects/model$sigma_y
}




