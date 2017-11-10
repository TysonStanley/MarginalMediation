#' Indirect Effect vs Total Effect
#' 
#' To make comparisons between indirect and direct effects of a variable.
#' The default comparison is indirect / (direct + indirect)                                                                                                             
#' 
#' @param model mma fit object
#' @param effect the indirect effect to be compared to its direct path
#' 
#' @export
rel_effects = function(model, effect){
  ind = model$ind_effects[effect, 3]
  dir = model$dir_effects[gsub("-.*$", "", effect), 1]
  
  ind/(dir + ind)
}

