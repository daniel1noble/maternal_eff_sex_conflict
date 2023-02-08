# Male harm suppresses female fitness, affecting the dynamics of adaptation and evolutionary rescue

This repository contains the data and code used for the meta-analysis in the following publication:

Miguel Gómez-Llano, Gonçalo S. Faria, Roberto García-Roa, Daniel W. A. Noble, Pau Carazo. (2023) Male harm suppresses female fitness, affecting the dynamics of adaptation and evolutionary rescue. Evolution Letters, [https://doi.org/10.1093/evlett/qrac002](https://academic.oup.com/evlett/advance-article/doi/10.1093/evlett/qrac002/7017519?login=false#394250071).

# How to use?

Readers can (should) be able to reproduce the meta-analysis results and figures by first opening the `.Rproj` file. This will set the working directory to the current folder. 

The relevant raw data can be found in the `data/Data_MA.xlsx` data file and the two processed data files are also found in the same folder `meta_dat_long.csv` (used for the arm-based analyses) and `meta_data.csv` used for the contrast-based meta-analysis (SMDH & lnCVR).

The relevant file with code is `ms.Rmd`. This file is a Rmarkdown file that contains the code blocks to: 1) run the relevant meta-analytic models; 2) generate tables and figures and 3) indicate where the relevant statistics come from that are provided in the results text.

There are also additional code scripts in the `R` folder. `data_prep.R` provides the code for exploring and cleaning up the raw data file. `phylogeny.R` provides the code for generating the phylogenetic tree used in the models. `func.R` contains relevant functions used throughout the analysis.

**Note**: The resulting text may not be a perfect representation of what is found in the manuscript given that the text was edited when it was merged within the main manuscript, but it should be fairly clear that the vast majority of the text in the main MS and supplement file matches what the knitted `ms.Rmd` file produces. We just all need to write entire papers in Rmarkdown ;) 

# Figures

The final figures are found in the `output/figs` folder. 
