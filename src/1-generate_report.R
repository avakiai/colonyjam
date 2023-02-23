# parametrically generate analysis reports

# source 
source("./functions/render_analysis.R")

# one report will be generated for each experiment (edit if desired):
make <- c("exp_1","exp_2")

# run
for (exp in make) {
  render_analysis(exp, seed = 42)
  }

