# generate an .Rmd file containing all analyses

# write a function to parametrically generate analysis reports
# -- do not edit:
render_mainanalysis <- function(exp, plot_sm, use_all, data_path, results_path, seed) {
  rmarkdown::render(
    './2-analysis/1_main_v1.Rmd',
    output_file = paste0('./main-analyses', '_', gsub("_","-",exp), '_', gsub(" ","_",gsub(":","-", Sys.time())), '.html'),
    params = list(data_path = data_path, 
                  results_path = results_path,
                  exp = exp, 
                  plot_sm  = plot_sm,
                  use_all = use_all,
                  seed = seed),
    envir = parent.frame()
  )
}
# ----

# generate reports
for (exp in c("exp_1","exp_2")) {
  render_mainanalysis(exp, 
                      data_path = './../1-data', # where to source data
                      results_path = './../3-results', # where to save
                      plot_sm = TRUE, # generate and save supplementary figures?
                      use_all = TRUE, # use all data? if FALSE, circular stats computed with subsamples
                      seed = 42 # set seed 
                      )
}
