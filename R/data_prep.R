##########################################################
# Author: D.W.A Noble
# Purpose: Script for cleaning and preping data
##########################################################

# Libraries
pacman::p_load(tidyverse, metafor, brms)


# Load data written from phylogeny script. 
	data <- read.csv("./data/meta_data.csv")
	str(data)

# Convert all se to SD

	data <- data %>%
			mutate(sd_T = ifelse(Err_T_Type == "se", Err_T*sqrt(N_T), Err_T),
				   sd_C = ifelse(Err_C_Type == "se", Err_C*sqrt(N_C), Err_C),
				   obs  = 1:n())

# Explore mean and variance relationships
	# Treatment groups		
	plot(log(X_T) ~ log(sd_T), data = data)

	# Control: Seems like a clear outlier at row 1; checked and looks to be correct. 
	plot(log(X_C) ~ log(sd_C), data = data)
	with(data, text(obs, y = log(X_C)+0.5, x=log(sd_C)+0.05, cex = 0.85)) 

# 