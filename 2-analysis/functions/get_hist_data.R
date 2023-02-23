#' Histogram calculator
#' @description This function takes the data frame of all calls and returns histogram data for 
# detected calls in each experimental condition, grouped by variable N of grouping variables.
#' @param calls a df with a columns `start_per` or `start_phase` with the start 
#' times of each call as the period or phase value in the modulation cycle, respectively,
#' and `condition` 
#' @param bins n bins to use for the histogram. default is 1 ms.
#' @param p_or_p whether to use period or phase, default is "period"
#' @param ... variable number of grouping variables

get_hist_data <- function(calls, bins=NULL, p_or_p = "period", ...){

  
  require(dplyr)
  
  if (sum(names(calls)=="condition") == 0)
    error("grouping variable `condition` (i.e. masking conditions) missing")

  if (p_or_p == "period") {
    dep_var = c("start_per")
    if (sum(names(calls)=="start_per") == 0)
      error("outcome variable `start_per` missing or badly named")
  } else if (p_or_p == "phase") {
    dep_var = c("start_phase")
    if (sum(names(calls)=="start_phase") == 0)
      error("outcome variable `start_phase` missing or badly named")
  }
  if (is.null(bins)) { default_bins = TRUE } else {default_bins = FALSE}

  
  hist_ <- list()  # storage for all histograms
  names_ <- list() # storage for all name labels
  vars <- unlist(list(...))
  varsin <- c(vars,"condition")
  
  mods <- as.numeric(as.character(unique(calls$modulation))) # how many modulation rates are in this data set?
  
  for (i in seq_along(mods)) {
    f <- mods[i]
    
    # prepare to split df into a list with separate sublist for each cell (modulation x input variables)
    to_split <- calls[calls$modulation==f,] %>% group_by_at(varsin) %>% dplyr::select(c(varsin, dep_var))

    if (p_or_p == "period") {to_split$start_period <- round(to_split$start_per,4)
    
        if (default_bins) { 
          #bins = ceiling((1/f)*1000)
          #breaks = bins
          breaks = seq.int(0.000,round(1/f,4),by=round(1/f,4)/ceiling(round(1/f,4)*1000)) # 1 ms or thereabouts
          #breaks[length(breaks)] = round(1/f,4)
          breaks = as.vector(breaks)
        } else {
          breaks = seq.int(0.000,round(1/f,4),by=(round(1/f,4)/bins))
          breaks[length(breaks)] = round(1/f,4)
          breaks = as.vector(breaks)
        }
    
    }
    if (p_or_p == "phase") {to_split$start_phase <- round(to_split$start_phase,3)
    
        if (default_bins) bins = 60
        breaks = seq.int(0.000,round(2*pi,3),by=round(2*pi,3)/bins)
        breaks[length(breaks)] = 2*pi
        breaks = as.vector(breaks)
    }
    
    # split into sublists
    data_split <- group_split(to_split)
    
    # prepare name labels for each list
    names.list <- lapply(data_split,function(x) x[seq_along(varsin)][1,]) # names for naming sublists
    # names as dfs
    names <- lapply(names.list, 
                    function(x) # for each sublist, take... 
                    rbind(x %>% unlist() %>% names() %>% substr(.,1,1), # column names, i.e. variable categories
                          x %>% unname() %>% str_c("_")) %>% # and bind them to the identifier
                      c() %>% paste(collapse = "") %>% substr(.,1,nchar(.)-1) 
                    )
    
    # apply names to each sublist, and keep only period/phase data
    separated <- lapply(data_split, function(x) x[length(varsin)+1]) %>% setNames(names)
    
    # calculate histograms
    if (p_or_p == "period") {
      h <- lapply(separated, function(x) h <- hist(unlist(x), breaks = (breaks), plot = FALSE))

    } else if (p_or_p == "phase") {
      h <- lapply(separated, function(x) h <- hist(unlist(x), breaks = (breaks), plot = FALSE))
    }
    
    hist_ <- c(hist_,list(h))
    names_ <- c(names_,list(names.list))
    
  } 
  # get list of names
  names(hist_) <- mods
  names(names_) <- mods
  
  # wrangle into a dataframe, with counts, density, and midpoints, and total n as variables
  calls_ <- list()
  for (ix in seq_along(mods)) {
    wrangle <- lapply(seq_along(hist_[[ix]]), 
                      function(i,x,n) data.frame(do.call(rbind,replicate(as.data.frame(n[[i]]),
                                                                         n = length(x[[i]]$counts),simplify = FALSE)),
                                                 n_calls = x[[i]]$counts,
                                                 density = x[[i]]$density,
                                                 mids  = round(x[[i]]$mids,4)),
                      hist_[[ix]],names_[[ix]])
    
    
    wrangle <- do.call(rbind,wrangle) %>% mutate(modulation = mods[ix],.before = 1)
    calls_ <- c(calls_,list(wrangle))
  }
  names(calls_) <- unique(calls$modulation)
  
  call_d <- do.call(rbind, calls_)
  
  return(call_d)
}
