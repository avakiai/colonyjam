#' Wrapper for `get_hist_data` function which cleans up data into a few helpful formats
#' @params data
#' @params bins bins to be used for histograms
#' @params vars
#' @params units whether to calculate histograms based on "period" or "phase"


get_avg_hist <- function(data = NULL, bins = NULL, vars = NULL, 
                         units = "period",
                         by_group = FALSE, by_session = FALSE) {



  call.counts <- get_hist_data(data, bins, units, vars) 

  # generic
  call.summary <- call.counts %>% 
    group_by(modulation, condition, factor(mids)) 

  summaries <- list(generic = call.summary)
  
  # subgroups
  if (by_group) {
    call.gr.summary <- call.counts %>%
      group_by(group, modulation, condition, factor(mids))
    summaries <- append(summaries, list(group = call.gr.summary))
    }
  
  if (by_session) {
    call.se.summary <- call.counts %>%
      group_by(session, modulation, condition, factor(mids))
    summaries <- append(summaries, list(session = call.se.summary))
    }


   call.avgs <- lapply(summaries, function(x)
                    x %>% summarise(mids = mids,
                                   n_calls = n_calls,
                                   n_calls_sum = sum(n_calls),
                                   n_calls_mu = mean(n_calls, na.rm = T),
                                   n_calls_sd = sd(n_calls, na.rm = T),
                                   n_calls_se = sd(n_calls, na.rm = T)/sqrt(length(n_calls)),
                                   
                                   # density = density,
                                   # density_mu = mean(density, na.rm = T),
                                   # density_sd = sd(density, na.rm = T),
                                   # density_se = sd(density, na.rm = T)/sqrt(length(density))
                                   ) %>%
                    mutate(per = round(1/as.numeric(modulation),3)) %>%
                    mutate(modulation = factor(modulation, 
                                               levels = sort(unique(x$modulation)), 
                                               labels = paste0(as.character(sort(unique(x$modulation))),"Hz")))
      
      )
  
  

  
  return(call.avgs)
}