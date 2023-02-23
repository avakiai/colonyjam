#' Wrap circular confidence intervals
#' 
#' @description Helper function for plotting angular vector data. If confidence 
#' intervals for angular means span 0 phase, then intervals may be displayed
#' incorrectly in ggplot(). The solution is to split CIs that cross 0 into two rows 
#' and let ggplot() display the two segments. 
#' @param circ_summary data frame with columns "theta_bar" (circular mean),
#' "r_bar" (vector length), "bs_mu_ci_l" and "bs_mu_ci_h" (bootstrapped circular 
#' confidence intervals for circular mean). Additionally, "condition" and "modulation".

confint_wrap <- function(circ_summary) {
  require(tidyverse)
  
  # add condition labels
  circ_summary_plot <- circ_summary %>% 
    # plotting requires xmin, xmax to be in order, so sort by value 
    mutate(xmin = ifelse(bs_mu_ci_l>bs_mu_ci_h, bs_mu_ci_h, bs_mu_ci_l),
           xmax = ifelse(bs_mu_ci_l<bs_mu_ci_h, bs_mu_ci_h, bs_mu_ci_l)) %>%
    mutate(mle_lo = NA,
           mle_hi = NA)
  
  # insert extra rows to invert range if range crosses zero (defined by whether theta bar lies within range or not)
  extra_rows <- data.frame()
  for (r in seq.int(nrow(circ_summary_plot))) {
    if (!(circ_summary_plot$theta_bar[r] > circ_summary_plot$xmin[r] & 
          circ_summary_plot$theta_bar[r] < circ_summary_plot$xmax[r])) {
      circ_summary_plot$mle_lo[r] <- 0
      circ_summary_plot$mle_hi[r] <- circ_summary_plot$xmin[r]
      extra_rows <- rbind(extra_rows,
                          cbind(circ_summary_plot[r,] %>% dplyr::select(-mle_lo,-mle_hi),
                                mle_lo = circ_summary_plot$xmax[r],
                                mle_hi = 2*pi) %>% remove_rownames())
    } else {
      circ_summary_plot$mle_lo[r] <- circ_summary_plot$xmin[r]
      circ_summary_plot$mle_hi[r] <- circ_summary_plot$xmax[r]
    }
  }
  
  circ_summary_plot <- rbind(circ_summary_plot,extra_rows) %>% arrange(modulation, condition)
  
  
  circ_summary_plot <- circ_summary_plot
  
  
  circ_summary_plot %>% dplyr::select(modulation, condition, theta_bar, r_bar, bs_mu_ci_l:mle_hi)
  
  return(circ_summary_plot)
}