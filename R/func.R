
##########################################
# Author: D.W.A. Noble
# Email: daniel.noble@anu.edu.au
# Functions used through scripts
##########################################

#' @title replace_names
#' @description Function aids in string matching and replacement. In other words, when you have a phylogeny you often need to replace synonymns. This function helps you do this quickly on a dataset that has repeated names. 
#' @param string This is the main character vector that you wish to substitute values. (i.e., the main column in data)
#' @param match This is a list of unique values contained within your 'string' column of your data frame.
#' @param replace These are the replacement names for the unique values within your column. 'match' and 'replace' should be the same lengths given this maps 1:1 the name in the data to the new name you want to replace it with.

replace_names <- function(string, match, replace){
	string_replaced <-replace[match(string, match)]
	return(string_replaced)
}


#' @title make_table
#' @description Function takes a MLM intercept only metafor model and creates a table. 
#' @param model Metafor intercept only model
#' @param heteroTable The heterogenity table derived from metafor::I2

make_table <- function(model,  heteroTable){
 hetero_table <-  heteroTable
 estimates    <-  predict(model)
 
  tmp <-  data.frame(  n = model$k,
                       std = model$s.nlevels[1],
                       spp = model$s.nlevels[3],
                       est = round(estimates$pred, digits = 2), 
                        CI = paste0(round(estimates$ci.lb, digits = 2), " to ", round(estimates$ci.ub, digits = 2)),
                        PI = paste0(round(estimates$cr.lb, digits = 2), " to ", round(estimates$cr.ub, digits = 2)),
                   i2_stdy = paste0(hetero_table[rownames(hetero_table)=="study",][1]*100, "%", " (", hetero_table[rownames(hetero_table)=="study",][2]*100, " - ", hetero_table[rownames(hetero_table)=="study",][3]*100, "%)" ), 
                  i2_trait = paste0(hetero_table[rownames(hetero_table)=="variable",][1]*100, "%", " (", hetero_table[rownames(hetero_table)=="variable",][2]*100, " - ", hetero_table[rownames(hetero_table)=="variable",][3]*100, "%)" ), 
                i2_species = paste0(hetero_table[rownames(hetero_table)=="species",][1]*100, "%", " (", hetero_table[rownames(hetero_table)=="species",][2]*100, " - ", hetero_table[rownames(hetero_table)=="species",][3]*100, "%)" ), 
                    i2_tot = paste0(hetero_table[rownames(hetero_table)=="total",][1]*100, "%", " (", hetero_table[rownames(hetero_table)=="total",][2]*100, " - ", hetero_table[rownames(hetero_table)=="total",][3]*100, "%)" )
  )
  
  return(tmp)
}

pred_interval <- function(model, tval){

  test.stat <- abs(tval)
  
    sigmas <- sum(model$sigma2)
        PI <- test.stat * sqrt(model$se^2 + sigmas)
  
    tmp <- data.frame (Est = model$beta,
                        SE = model$se,
                        lwr.CI = model$ci.lb,
                         up.CI = model$ci.ub,
                        lower.PI = model$beta - PI,
                        upper.PI = model$beta + PI)
  
  return(tmp)
}


p_value <- function(x){
  if(x <= 0.0001) {tmp = "< 0.0001"}
  if(x <= 0.001 & x >= 0.0001) {tmp ="< 0.001"}
  if(x <= 0.01 & x >= 0.001) {tmp ="< 0.01"}
  if(x >= 0.01) {tmp = round(x, digits =2)}
  return(tmp)
}
