
##########################################################
# Author: D.W.A Noble
# Purpose: Script for building and finding taxa in the
# dataset to create a phylogenetic tree for the 
# phylogenetic covariance matrix
##########################################################

# Libraries 
	pacman::p_load(rotl, tidyverse, ape, phytools, readxl)
	source("./R/func.R")

# Datasets; it's in excel and has two sheets, so will use readxl package. We need sheet one

	data <- read_excel("./data/Data_MA.xlsx", sheet = 1)

# Extract the species list, clear whitespace, have a look at unique names

	spp <- data %>% pull(species) %>% trimws()

# Get some summary stats:
	# How many species?
		length(unique(spp)) #38

	# What are the unique species; note that Calopteryx doesn't have a species binomial classification
		species_names <- unique(spp)

# Lets use ROTL to find taxa, and possible synonymns
		 matching <- tnrs_match_names(species_names)  
		spp_match <- matching %>% pull(unique_name)

# Now that we have the list of most up-to-date names from ROTL, we can do a few things to get a phylogeny. First, export list for TimeTree
		write(spp_match, "./phylogeny/spp_list")

# Now grab the tree based on taxonomy and molecular data
		tree <-  tol_induced_subtree(matching$ott_id)
		plot(tree, cex = 0.8)

# Replace the species names in data with the matched names in ROTL so that when a covariance matrix is made the node names match exactly to the names in data. 
data <- data %>%
			mutate(species=replace_names(species, species_names, spp_match))

###########################################################
# Branch lengths and VCV matrix
###########################################################
# Create branch lengths for tree using Grafen's method
tree_bl <- compute.brlen(tree, method = "Grafen", power = 1)

# Fix tip labels
tree_bl$tip.label <- gsub("_", " ", gsub("_ott.+", "", tree_bl$tip.label))

write.tree(tree_bl, "./phylogeny/tree")
write.csv(data, "./data/meta_data.csv")
