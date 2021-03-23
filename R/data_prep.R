##########################################################
# Author: D.W.A Noble
# Purpose: Script for cleaning and preping data
##########################################################

# Libraries
pacman::p_load(tidyverse, metafor, brms, latex2exp)

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

# Explore a bit more of how much data we have across various moderators
	data %>% 
	select_if(is.factor) %>% 
	select(-c(notes, Dependency_decisions, journal, pg, author, estimated.increase.in.harm, Err_C_Type, Err_T_Type, C_flipped, source)) %>% 
	glimpse %>% summary(.)

# From this it is quite clear that there is not too much use in Parental Care as most don't have care. Most of the data is a direct measure of fitness, which is good. Most data are from oviparous, but sensible split possibly.

# Explore species level data
	spp_data <- data.frame(data %>%
	group_by(species) %>% 
	summarise(Parental.care = unique(Parental.care),
			  Gestation     = unique(Gestation),
			  Sperm.compet  = unique(Sperm.compet),
			  SCR.SCI 		= unique(SCR.SCI),
			  treatment     = unique(treatment)) )

	spp_data %>% summary()


# Generate Effect Sizes - Going to use SMDH (Hedges g with heteroscedasticity correction)
data <- escalc(m1i = X_C, sd1i =sd_C, n1i = N_C, m2i= X_T, sd2i= sd_T, n2i= N_T, data = data, append = TRUE, measure = "SMDH", var.names=c("SMDH","v_SMDHq"))

# Clean data up a bit. We don't need all columns
data <- data %>% 
select(study, author, year, class, order, family, genus, species, variable, fitness.estimate, treatment, estimated.increase.in.harm, X_C, sd_C, N_C, X_T, sd_T, N_T, SMDH, v_SMDHq, SME_index, Male.measure, Female.measure, SCR.SCI, Sperm.compet, Harm_type, Parental.care , Gestation, depend, shared.control, source)

# Have a look. Does look like clear evidence for publication bias though. 
ggplot(data, aes(x = SMDH, y = 1 / sqrt(v_SMDHq), xmin = -10, xmax = 28)) +
geom_point() +
labs(x = "SMDH", y = TeX("Precision ($\\frac{1}{\\sqrt{v}}$)")) 

# Find outlier data.There is a clear outlier, which is study 047. I've got to the original paper and noticed data was extracted from boxplot. SD values were very small for the mean data, suggesting data entry. I re-extracted from raw data which was also presented and SD's are much more sensible and way different from original extraction and conversion from box plot.
data %>% filter(SMDH >=10)


