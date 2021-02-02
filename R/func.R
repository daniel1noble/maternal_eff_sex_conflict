
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
