##########################################################
# Author: D.W.A Noble
# Purpose: Script for cleaning and preping data
##########################################################

# Load data written from phylogeny script. 
	data <- read.csv("./data/meta_data.csv")
	str(data)

# Convert all se to SD

	data <- data %>%
			mutate(sd_T = ifelse(Err_T_Type == "se", Err_T*sqrt(N_T), Err_T),
				   sd_C = ifelse(Err_C_Type == "se", Err_C*sqrt(N_C), Err_C))

# Explore mean and variance relationships
	plot(log(X_T) ~ log(sd_T), data = data)