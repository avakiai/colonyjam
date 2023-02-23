# Helper functions for random forest analysis

require(tidyverse)
# also requires (data) to exist in global variables...
cormat_to_df <- function(x, val = "n") {
  if (!is.data.frame(x)) {
    y <- x %>% as.data.frame() %>%
      rownames_to_column("observed") %>%
      pivot_longer(-c(observed), names_to = "predicted", values_to = val)
    
  } else {
    y <- x %>% mutate(across(where(is.character), as_factor))
    colnames(y) <- c("observed", "predicted", "n")
    
  }
  y <- y %>% mutate(across(where(is.character), as_factor)) %>%
    mutate(observed = factor(observed,
                             levels = as.character(sort(unique(data$modulation))))) %>%
    mutate(predicted = factor(predicted,
                              levels = as.character(sort(unique(data$modulation)))))
  
  return(y)
}


importance_to_df <- function(x) {
  y <- x %>% as.data.frame() %>%
    mutate(across(where(is.numeric), round, 2)) %>% 
    rownames_to_column("metric") %>%
    pivot_longer(ends_with("Hz"), "modulation") %>%
    pivot_longer(starts_with("Mean"), names_to = "measure_name", values_to = "measure") %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(modulation = factor(modulation, 
                               levels = as.character(sort(unique(data$modulation)))))
  
  return(y)
}