#' Indirect vs Direct Comparison
#' 
#' To make comparisons between indirect and direct effects of a variable.
#' The default comparison is indirect / direct.                                                                                                             
#' 
#' @param model mma fit object
#' @param variable the variable in the model that is to be compared
#' 
#' @export
rel_effects = function(model, variable){
  ind = model$ind_effects
  dir = model$dir_effects
  
  ind[variable, ]/dir[variable,]
}

