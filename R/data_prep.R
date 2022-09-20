##########################################################
# Author: D.W.A Noble
# Purpose: Script for cleaning and preping data
##########################################################

# Libraries
pacman::p_load(tidyverse, metafor, brms, latex2exp)

# Load data written from phylogeny script. 
  source("./R/phylogeny.R")
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
	select_if(is.character) %>% 
	dplyr::select(-c(Dependency_decisions, journal, pg, author, estimated.increase.in.harm, Err_C_Type, Err_T_Type, C_flipped, source)) %>% 
	glimpse %>% summary(.)

# From this it is quite clear that there is not too much use in Parental Care as most don't have care. Most of the data is a direct measure of fitness, which is good. Most data are from oviparous, but sensible split possibly. We can actually reclassify ovoviviparous to oviparous

	data <- data %>%
			mutate(Gestation = ifelse(Gestation == "ovoviviparous", "oviparous", as.character(Gestation)))
	table(data$Gestation)
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
data <- escalc(m1i = X_C, sd1i =sd_C, n1i = N_C, m2i= X_T, sd2i= sd_T, n2i= N_T, data = data, append = TRUE, measure = "SMDH", var.names=c("SMDH","v_SMDH"))
data <- escalc(m1i = X_C, sd1i =sd_C, n1i = N_C, m2i= X_T, sd2i= sd_T, n2i= N_T, data = data, append = TRUE, measure = "CVR", var.names=c("logCVR","v_logCVR"))
data <- escalc(m1i = X_C, sd1i =sd_C, n1i = N_C, m2i= X_T, sd2i= sd_T, n2i= N_T, data = data, append = TRUE, measure = "VR", var.names=c("logVR","v_logVR"))

# Clean data up a bit. We don't need all columns
data <- data %>% 
select(obs, study, author, year, class, order, family, genus, species, variable, fitness.estimate, treatment, estimated.increase.in.harm, X_C, sd_C, N_C, X_T, sd_T, N_T, SMDH, v_SMDH, logCVR,v_logCVR, logVR,v_logVR, SME_index, Male.measure, Female.measure, SCR.SCI, Sperm.compet, Harm_type, Parental.care , Gestation, depend, shared.control, source, M_lifespan, F_lifespan)

# Have a look. Does look like clear evidence for publication bias. 
ggplot(data, aes(x = SMDH, y = 1 / sqrt(v_SMDH), xmin = -6, xmax = 6)) +
geom_point(aes(colour = class)) +
labs(x = "SMDH", 
	 y = TeX("Precision $\\left(\\frac{1}{\\sqrt{v_{SMDH}}}\\right)$"),
	 colour = "Class") +
scale_color_grey() + 
#scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
theme_bw() 

# Find outlier data.There is a clear outlier, which is study 047. I've got to the original paper and noticed data was extracted from boxplot. SD values were very small for the mean data, suggesting data entry. I re-extracted from raw data which was also presented and SD's are much more sensible and way different from original extraction and conversion from box plot.
#data %>% filter(SMDH >=10) # NOTE: Outlier fixed. Raw data re-extracted as it was not correct.

# What is the correspondence between study and species
stdy_spp <- data.frame(data %>% group_by(species) %>% summarise(n_stdy = length(unique(study))))
stdy_spp %>% filter(n_stdy > 1) %>% nrow(.) # Most species from one study but still 9 species across more than one study so should be able to decouple effects
#####
# What are effect sizes per groups looking like

ggplot(data, aes(x = Gestation, y = SMDH, size = 1/sqrt(v_SMDH))) + geom_violin() + geom_point() + theme_bw() + labs(x = "Gestation", 
	 y = "SMDH",
	 size = TeX("Precision $\\left(\\frac{1}{\\sqrt{v_{SMDH}}}\\right)$"))

# Nothing we can do with parental care. Only 1 data point. Not useful. 
ggplot(data, aes(x = Parental.care, y = SMDH, size = 1/sqrt(v_SMDH))) + geom_violin() + geom_point() + theme_bw() + labs(x = "Gestation", 
	 y = "SMDH",
	 size = TeX("Precision $\\left(\\frac{1}{\\sqrt{v_{SMDH}}}\\right)$")) 

# How about measure of sexual dimorphism; could do this a few ways. Probably nicest is to just use lnRR as a SSD measure. Deals with big variation among species (log), and also nicely interpretable. 
data <- data %>% mutate(     SSD = Male.measure - Female.measure,
						SSD_lnRR = log(Male.measure / Female.measure))

ggplot(data, aes(x = SSD_lnRR, y = SMDH, size = 1/sqrt(v_SMDH), colour = species)) + geom_point() + theme_bw() + labs(x = "SSD", 
	 y = "SMDH",
	 size = TeX("Precision $\\left(\\frac{1}{\\sqrt{v_{SMDH}}}\\right)$"),
	 colour = "Species") +
	guides(colour = guide_legend(), 
			 size = guide_legend(direction = "horizontal"))

# Harm type
	ggplot(data, aes(x = Harm_type, y = SMDH, size = 1/sqrt(v_SMDH))) + geom_violin() + geom_point() + theme_bw() + 
		theme(legend.background = element_rect(fill = "white"),
			  legend.key = element_rect(fill = NA, colour = NA),
			  legend.key.size = unit(3, "mm")) + labs(x = "Harm Type", 
	 y = "SMDH",
	 size = TeX("Precision $\\left(\\frac{1}{\\sqrt{v_{SMDH}}}\\right)$")) 

# Sperm competition
	ggplot(data, aes(x = SCR.SCI, y = SMDH, size = 1/sqrt(v_SMDH))) + geom_violin() + geom_point() + theme_bw() + labs(x = "Sperm Competition", 
	 y = "SMDH",
	 size = TeX("Precision $\\left(\\frac{1}{\\sqrt{v_{SMDH}}}\\right)$")) 

## How that we've had an explore of the data, cleaned up the messiness and checked effect size data we are ready to analyse. Will export final data

	write.csv(data, "./data/meta_data.csv", row.names=FALSE)
	