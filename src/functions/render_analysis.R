
render_analysis <- function(exp = "exp_1", 
                            plot_sm = TRUE, 
                            use_all = TRUE, 
                            data_path = './../1-data', 
                            results_path = './../3-results', 
                            seed = 42) {
  rmarkdown::render(
    './R/2-analysis/master_analysis.Rmd',
    output_file = paste0('./../../vignettes/master-analyses', '_', gsub("_","-",exp), '_', gsub(" ","_",gsub(":","-", Sys.time())), '.html'),
    params = list(data_path = data_path, 
                  results_path = results_path,
                  exp = exp, 
                  plot_sm  = plot_sm,
                  use_all = use_all,
                  seed = seed),
    envir = parent.frame()
  )
}