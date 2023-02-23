# ColonyJam!

This R project implements the analyses for the paper "Flexible control of vocal timing in bats" by Ava Kiai, Jan Clemens, Manfred Koessl, David Poeppel, and Julio Hechavarria. 

### Requirements

The analyses require a recent version of R, RStudio, and the following packages, which can be downloaded by running the following command:
```
packages <- c("circular", "CircStats", "CircSpaceTime", "sjPlot", "yarrr", "janitor", "broom", "knitr",  "kableExtra", "car", "MASS", "emmeans", "rcompanion", "pscl", "randomForest", "chisq.posthoc.test", "viridis", "scales","ggh4x","ggside", "ggdist", "ggridges","gganimate","plotly","htmlwidgets","tidyverse")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
```
Package will also be automatically installed when the analysis script is run, if they are not already.

### Installation
Download this repository by running the following in the R console:
```
if (!require("devtools")) install.packages("devtools") # install devtools if not installed
devtools::install_github("avakiai/colonyjam")
```

### Usage
The root directory of this repository contains a script `run.R` that, when sourced, will generate two analysis reports (one for each experiment). These reports will be saved to the analysis folder as .html files with the runtime timestamp in the filename. 

### Contents
- `1-data` contains the raw data used in these analyses
	- `/input' this folder contains the raw data for experiment 1 and experiment 2
	- `/output` this folder contains 'processed' versions of the data generated at various points by the analysis script. To speed up analyses and report generation, these are only computed and saved if they are not already in this file. 
- `2-analysis` contains the main file containing all statistical analyses and visualizations used in the paper and supplementary materials. A `/functions` folder contains helper functions. 
- `3-results` generated result output are saved to one of the following subfolders:
	- `/figures`
	- `/models`
	- `/supplementary_figures`
	- `/tables`

By default, `data/output` and `results/...` are populated. To generate results anew, simply delete the subfolder `1-data/output` as well as all subfolders under `3-results/`. Note that this will result in longer wait times (~ 2 hours given 32 GB RAM vs. < 2 minutes) for generating the analysis report. 


### Deprecated:
The folder `_call_detection` contains all materials necessary for evaluating DAS models for call detection. Labelled subfolders contain training evaluation reports as .html files for each experiment. For more info on training and evaluating DAS models, see [here](https://github.com/janclemenslab/das). 
