#' Uncorrelated Residual Assumption Check
#' 
#' Provides the correlations of the residual terms of the model
#' 
#' @param model The mma model object
#' 
#' @importFrom furniture tableC
#' 
#' @export
mma_check <- function(model){
  
  stopifnot(is.mma(model))

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
    stop("The data sets used in the different paths are of different sizes.")
  }
  
  nams <- paste0("resid_mod_", 1:(length(pathas) + 1))
  
  df %>%
    data.frame %>%
    setNames(nams) %>%
    furniture::tableC()
  
}