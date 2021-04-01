# Do large-scale environmental gradients affect inter- and intraspecific patterns of conspecific negative density dependence in temperate forests?
This repo contains R code used to analyze [Carolina Vegetation Survey](http://cvs.bio.unc.edu/) data for A. Brown's PhD dissertation. This project is still in prep, and R code has not yet been finalized. 

### R code
- **data_mgmt_Jan2019** This code combines and restructures Carolina Vegetation Survey datasets to prepare for modeling. CVS datasets used include: plot data; stem data within plots; soil data within plots; Virginia CVS plot information (not available within main CVS dataset). It also uses National Vegetation Classification assignments to determine the vegetation type found within plots.
- **chap3_plots_\*** These code files prepare datasets and run Bayesian models. 
- **plotting** This code creates plots for use in the manuscript (in prep). 

### Background
Conspecific negative density dependence (CNDD) is thought to be a major influence on   tree diversity in temperate forests. Under CNDD, intraspecific competition and pathogen accumulation (among other mechanisms) lead to limited recruitment of seedlings in close proximity to conspecific adults. While it is increasingly apparent that this is an important recruitment pattern in temperate forests, it is unclear why we see differential susceptibility to conspecifics, both between species and within species. 

We evaluated whether susceptibility to CNDD varies with the environmental context, either in an intraspecific or a non-species-specific (group) way. Using hardwood and mixed hardwood-conifer forest plots from across North and South Carolina (USA), we combined plot-level stem counts of saplings and adult trees with plot-level soil and topographic measurements. We constructed negative binomial hierarchical models to determine whether site-level abiotic conditions influence the direction or magnitude of conspecific density dependence. 

We found that environmental conditions modify CNDD effects within species and that this modification of CNDD is more important than the non-species-specific (group) response . While we found both positive and negative conspecific density dependence across species, negative responses (20 of 47 species) outnumbered positive responses (4 species). Plot elevation and soil fertility were the environmental effects that most commonly shifted intraspecific variation in CNDD, while topographic measures and soil texture did not appear to be important. 

We concluded that the environment can modify the direction and magnitude of conspecific density dependence, and this effect often varies across species. This study is the first to our knowledge that explicitly tests for differential susceptibility to CNDD and whether it is modulated by the environment.
