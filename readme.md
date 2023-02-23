[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7669934.svg)](https://doi.org/10.5281/zenodo.7669934)

# ColonyJam!

This R project implements the analyses for the paper "Flexible control of vocal timing in bats" by Ava Kiai, Jan Clemens, Manfred Koessl, David Poeppel, and Julio Hechavarria. 

### Requirements

The analyses require a recent version of R, RStudio, and the following packages, which can be downloaded by running the following command:
```
packages <- c("circular", "CircStats", "CircSpaceTime", "sjPlot", "yarrr", "janitor", "broom", "knitr",  "kableExtra", "car", "MASS", "emmeans", "rcompanion", "pscl", "randomForest", "chisq.posthoc.test", "viridis", "scales","ggh4x","ggside", "ggdist", "ggridges","gganimate","plotly","htmlwidgets","tidyverse","piggyback")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
```
Package will also be automatically installed when the analysis script is run, if they are not already.

### Usage
Compiled analysis reports for the two experiments in this study can be found in `vignettes`, 
and figures and tables populate the subfolders in `R/3-results`. 

To generate results anew:
1. Open the R project `colonyjam.Rproj` in RStudio.
2. Download raw data, plus cached data and models from repository assets by running `src/0-download.R`. 
3. Generate reports by running `src/1-generate_report.R` New reports will be saved to the `vignettes` folder as .html files and labelled with the time/date at runtime.

Cached data and models are provided to speed up analysis (~ 2 mins vs. ~ 2 hours given 32 GB RAM). If you wish to generate all results from raw data only, execute only the command for downloading raw data in `0-download.R`. If already downloaded, simply delete the subfolder `R/1-data/output` and `R/3-results/models` - the script will recreate these.

### Contents
- `R` 
	+ `/1-data` 
		+ `/input` [after step 2 above] contains the raw data for experiment 1 and experiment 2
		+ `/output` [after step 2 above] contains 'processed' caches of the data generated at various points by the analysis script. To speed up analyses and report generation, these are only computed and saved if they are not already in this file. 
	+ `/2-analysis` contains the main .Rmd file containing all analyses and visualizations used in the paper and supplementary materials
	+ `/3-results` results are saved to one of the following subfolders:
		+ `/figures`
		+ `/models`
		+ `/supplementary_figures`
		+ `/tables`
- `src` contains files for downloading data and compiling the analysis report
- `vignettes` contains compiled reports as .html files

