# ---- Primary plotting functions ----

require(tidyverse)
require(scales)
require(ggdist)
require(ggh4x)
require(ggside)
require(CircSpaceTime)

## ---- Utility functions ---- 

N <- function(dat, vars = ...) {
  n <- dat %>% group_by_at(vars) %>% summarise(n = n()) 
  return(n)
}

## ---- Define ColonyJam Light Theme ----


theme_jamlight <- function(){ 
  font <- "Arial"   
  
  theme_minimal() %+replace%  
    
    theme(
      
      # panel elements
      panel.border = element_blank(),
      panel.spacing.y = unit(0.3, "cm"),
      
      # facet elements
      strip.text = element_text(colour = 'black', 
                                size = 10, 
                                margin = ggplot2::margin(2,2,2,2)),
      strip.background = element_rect(colour = "black",
                                        fill = "white"),
      # axes elements
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      
      # legend elements
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 11)
    )
}


theme_jamdark <- function(){ 
  font <- "Arial"   
  
  theme_jamlight() %+replace%  
    
    theme(
      # plot elements
      plot.background = element_rect(fill='transparent', color=NA),
      plot.title = element_text(color="white"),
      
      # panel elements
      panel.border = element_blank(),
      panel.background = element_rect(fill='transparent', color=NA),
      
      # grid elements
      panel.grid = element_line(color="white"),
      panel.grid.major = element_line(color="white"),
      panel.grid.minor = element_line(color="white"),

      # facet elements
      strip.text = element_text(color = "white"),
      strip.background = element_rect(colour = "white",
                                      fill="#212121"),
      
      # axes elements
      axis.line = element_line(color="white"),
      axis.text = element_text(color="white"),
      axis.ticks = element_line(color="white"),
      axis.title = element_text(color="white"),
      
      # legend elements
      legend.text = element_text(color="white"), 
      legend.background = element_rect(fill='transparent', color=NA),
      legend.title = element_text(color="white"),
      legend.key = element_rect(fill = 'transparent', color=NA),
      legend.box.background = element_rect(fill='transparent', color="white")
    )
}

## ---- Polar labels ----

piplot <- c("0",
            expression(paste(pi,"/2")),
            expression(pi),
            expression(paste("3"*pi,"/2")),
            expression(paste("2",pi)))


## ---- Basic histogram plots ---- 

p.hist <- function(dat = NULL, mod = NULL, N = NULL, label = NULL, bins = 60) {
# plot histograms by major conditions
  
# dat needs cols: start.per, condition
# mod needs cols: period, phase, condition
# N needs cols: condition, N
# label

  plot <- ggplot(data = dat) +
        # plot histogram of calling
        geom_histogram(mapping = aes(start_phase, color = condition, fill = condition), 
                       breaks = seq(0, 2*pi, (2*pi)/bins)) +
    
        scale_x_continuous(name = "phase", 
                           breaks = seq(0, 2*pi, length.out = 5)[1:4], 
                           limits = c(0,2*pi),
                           labels = piplot[1:4],
                           expand = c(0,0)) +
    
        scale_y_continuous(name = "call count", expand = c(0, 0)) +
    
        scale_color_manual(values = c_pal, name = NULL) +
        scale_fill_manual(values = c_pal, name = NULL) + 
        
        facet_wrap(. ~ condition, scales = "free_y") +
        ggtitle(label = label) +
        guides(color=FALSE, fill=FALSE)+
        theme_jamlight()
  
  if (!is.null(mod)) {
    # plot AM phase model
    plot <- plot + 
      geom_line(data = mod, 
              mapping = aes(period, phase, group = condition, linetype = condition), 
              color = "white", inherit.aes = FALSE, size = 1) +
      scale_linetype_manual(values=c(2,1,1),guide = NULL) }
    
  if (!is.null(N)) {
    # plot N calls

    plot <- plot + 
      geom_text(data = N, 
              mapping = aes(label = paste("N =", scales::comma(n))), 
              x = -Inf, y = -Inf, hjust = -0.05, vjust = -0.3, 
              size = 4, color = "white", inherit.aes = FALSE) }
  
    
    return(plot)
}

p.hist_kwargs <- function(dat = NULL, mod = NULL, facets = NULL, 
                          N = NULL, label = NULL, bins = 60,
                          ncol = NULL, nrow = NULL) {
# plot period values but let user define arbitrary facets
  
  plot <- ggplot(data = dat) +
    # plot histogram of calling
    geom_histogram(mapping = aes(start_phase, color = condition, fill = condition), 
                   breaks = seq(0, 2*pi, (2*pi)/bins)) +
    
    scale_x_continuous(name = "phase", 
                       breaks = seq(0, 2*pi, length.out = 5)[1:4], 
                       limits = c(0,2*pi),
                       labels = piplot[1:4],
                       expand = c(0,0)) +
    
    scale_y_continuous(name = "call count", expand = c(0, 0)) +
    
    scale_color_manual(values = c_pal, name = NULL) +
    scale_fill_manual(values = c_pal, name = NULL) + 
    
    facet_wrap(as.formula(paste0(sprintf('%s',facets[1]), sprintf('~%s',facets[2]))), #@@ fix
               scales = "free_y", 
               ncol = ncol, nrow = nrow) +
    
    ggtitle(label = label) +
    guides(color=FALSE, fill=FALSE)+
    theme_jamlight()
  
  if (!is.null(mod)) {
    # plot AM phase model
    plot <- plot + 
      geom_line(data = mod, 
                mapping = aes(period, phase, group = condition, linetype = condition), 
                color = "white", inherit.aes = FALSE, size = 1) +
      scale_linetype_manual(values=c(2,1,1),guide = NULL) }
  
  if (!is.null(N)) {
    # plot N calls

    plot <- plot + 
      geom_text(data = N, 
                mapping = aes(label = paste("N =", scales::comma(n))), 
                x = -Inf, y = -Inf, hjust = -0.05, vjust = -0.3, 
                size = 4, color = "white", inherit.aes = FALSE) }
  
  
  return(plot)
}

p.hist_peak <- function(dat = NULL, scale = 1000, binwidth = 5) {

  plot <- ggplot(dat, aes(t_from_peak*scale, color = modulation)) +
    geom_histogram(aes(fill = modulation, alpha = 0.5), binwidth = binwidth, position = "identity") +
    scale_x_continuous(name = "time from peak (ms)", 
                       limits = c(-150, 150),
                       n.breaks = 5,
                       expand = c(0,0)) +
    scale_y_continuous(name = "angular means",
                       expand = c(0,0)) +
    facet_grid(cols = vars(condition), scales = "free_x") +
    geom_vline(xintercept = 0, color = "tomato3", linetype = "dashed") +
    scale_color_brewer(palette = "Spectral") +
    scale_fill_brewer(palette = "Spectral") +
    guides(alpha = FALSE) +
    theme_jamlight()
    
  return(plot)
}

p.hist_trough <- function(dat = NULL, scale = 1000, binwidth = 5) {
  
  plot <- ggplot(dat, aes(t_to_trough*scale, color = modulation)) +
    geom_histogram(aes(fill = modulation, alpha = 0.5), binwidth = binwidth, position = "identity") +
    scale_x_reverse(name = "time to trough (ms)", 
                    limits = c(150, 0),
                    n.breaks = 3,
                    expand = c(0,0)) +
    scale_y_continuous(name = "angular means",
                       expand = c(0,0)) +
    facet_grid(cols = vars(condition), scales = "free_x") +
    geom_vline(xintercept = 0, color = "tomato3", linetype = "dashed") +
    scale_color_brewer(palette = "Spectral") +
    scale_fill_brewer(palette = "Spectral") +
    guides(alpha = FALSE) +
    theme_jamlight()
  
  return(plot)
}

## ---- Speciality Histograms ----
p.composite_peak_trough <- function(mu_bs_time, scale = 1000) {
  
  plot_mus <- mu_bs_time %>% 
    dplyr::filter(!condition=="silence", !condition=="half-band masker") %>%
    pivot_longer(cols = c(t_to_trough, t_from_peak), names_to = "metric", values_to = "time")
  
  plot <- ggplot(plot_mus, aes(y=modulation, x=time*scale, color = modulation, fill = modulation)) +
    ggdist::stat_halfeye(adjust = 0.8, 
                         width = 1, .width = 0, 
                         justification = -.2, point_colour = NA,
                         alpha = 0.7) + 
    geom_jitter(width = .1, height = 0.1, 
                alpha = .3, size = 1) +  
    geom_boxplot(width = 0.3, 
                 position = position_nudge(x=0), 
                 color = "black", outlier.shape = NULL,outlier.color = NULL, outlier.size = 0,
                 alpha = 0.2) + 
    #ggdist::stat_dots(side = "left", dotsize = .4, justification = 1.1, binwidth = .05) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    #scale_y_continuous(name = "time (ms)") +
    #scale_x_continuous(name = "time (ms)", limits = c(0,125)) +
    facet_wrap(.~metric,scales = "free_x",               
               labeller = as_labeller(c(`t_to_trough` = "time to trough",
                                        `t_from_peak` = "time from peak"))) +
    ggh4x::facetted_pos_scales(x = list(
      scale_x_continuous(name = "time (ms)", 
                         limits = c(min(mu_bs_time$t_from_peak),1/min(mu_bs_time$f)/2*scale),
                         breaks = ceiling(c(0, 1/min(mu_bs_time$f)/4*scale,1/min(mu_bs_time$f)/2*scale))
      ), 
      scale_x_reverse(name = "time (ms)", 
                      limits = c(1/min(mu_bs_time$f)/2*scale, min(mu_bs_time$t_from_peak)),
                      breaks = ceiling(c(1/min(mu_bs_time$f)/2*scale,1/min(mu_bs_time$f)/4*scale, 0))
      )
    )) +
    theme_jamlight()
  return(plot)
  }
  
p.peak_trough <- function(dat = NULL, scale = 1000, binwidth = 5) {
  
  plot <- ggplot(dat, aes(x=t_from_peak*scale, y=t_to_trough*scale, 
                                          color = modulation, fill = modulation)) +
    geom_point(size = 2, alpha = 0.4) +
    geom_xsidehistogram(aes(y = after_stat(density)), binwidth = binwidth,
                        position = "identity", alpha = 0.5) +
    geom_ysidehistogram(aes(x = after_stat(density)), binwidth = binwidth,
                        position = "identity", alpha = 0.5) +
    facet_grid(rows=vars(condition)) +
    scale_color_brewer(palette = "Spectral") +
    scale_fill_brewer(palette = "Spectral") +
    scale_x_continuous(name = "time from peak (ms)",
                       limits = c(min(dat$t_from_peak),1/min(dat$f)/2*scale),
                       breaks = ceiling(c(0, 1/min(dat$f)/4*scale, 1/min(dat$f)/2*scale))
                       ) +
    scale_y_continuous(name = "time to trough (ms)",
                       breaks = ceiling(c(0, 1/min(dat$f)/4*scale, 1/min(dat$f)/2*scale)),
                       limits = c(0,1/min(dat$f)/2*scale)
                       ) + # this should be
    scale_xsidey_continuous(breaks = NULL) +
    scale_ysidex_continuous(breaks = NULL) +
    ggside(x.pos = "top", y.pos = "right") +
    theme_jamlight() +
    theme(ggside.panel.scale = 0.25)
  
  return(plot)
}

## ---- Histogram average plots ---- 

p.hist_avg_dot <- function(dat = NULL) {
# averages over histograms

  scale <- 1000
  
  plot <- ggplot(data = dat, aes(mids*scale, n_calls_mu, color = condition, fill = condition)) + 
    geom_point(size = 1.5) +
    geom_ribbon(aes(ymin = n_calls_mu-n_calls_se, ymax = n_calls_mu+n_calls_se), linetype = "blank", alpha=0.3) +
    scale_x_continuous(name = paste0(" period (ms)"), 
                       breaks = c(0, max(dat$mids)*scale), 
                       labels = c(0, max(dat$per)*scale),
                       expand = c(0, 0)) +
    scale_y_continuous(name = "mean # calls", 
                       breaks = scales::breaks_pretty(5), 
                       expand = c(0, 0)) +
    
    scale_color_manual(values = c_pal, name = NULL) +
    scale_fill_manual(values = c_pal, name = NULL) + 
    
    facet_grid(col = vars(modulation), row = vars(condition), scales = "free") +

    guides(color=FALSE, fill=FALSE) +
    theme_jamlight()

  # if (!is.null(mod)) {
  #   # plot AM phase model
  #   plot <- plot + 
  #     geom_line(data = mod, 
  #               mapping = aes(period*scale, phase, group = condition, linetype = condition), 
  #               color = "black", inherit.aes = FALSE, size = 1) +
  #     scale_linetype_manual(values=c(2,1,1), guide = NULL) }
  
  return(plot)  
}

p.hist_avg_dot_kwargs <- function(dat = NULL, facets = NULL) {
  # averages over histograms

  scale <- 1000
  show_max <- function(x) max(x)
  
  
  plot <- ggplot(data = dat, aes(mids*scale, n_calls_mu, color = condition, fill = condition)) + 
    geom_point(size = 1.5) +
    geom_ribbon(aes(ymin = n_calls_mu-n_calls_se, ymax = n_calls_mu+n_calls_se), linetype = "blank", alpha=0.3) +
    
    scale_color_manual(values = c_pal, name = NULL) +
    scale_fill_manual(values = c_pal, name = NULL) + 
    
    ggh4x::facet_grid2(as.formula(paste0(sprintf('%s',facets[1]), sprintf('~%s',facets[2]))),
                scales = "free", 
                independent = "y",
                #space = "free_x"
                ) +
    scale_x_continuous(name = paste0(" period (ms)"), 
                       breaks = c(0, max(dat$mids)*scale), 
                       labels = c(0, max(dat$per)*scale),
                       expand = c(0, 0)) +  
    scale_y_continuous(name = "mean # calls", 
                       breaks = scales::breaks_pretty(3), 
                       expand = c(0, 0)) +
    
    guides(color=FALSE, fill=FALSE) +
    theme_jamlight()
  
  return(plot)  
}

p.hist_avg_dot_kwargs_fixedy <- function(dat = NULL, facets = NULL) {
  # averages over histograms

  scale <- 1000
  
  plot <- ggplot(data = dat, aes(mids*scale, n_calls_mu, color = condition, fill = condition)) + 
    geom_point(size = 1.5) +
    geom_ribbon(aes(ymin = n_calls_mu-n_calls_se, ymax = n_calls_mu+n_calls_se), linetype = "blank", alpha=0.3) +
    scale_x_continuous(name = paste0(" period (ms)"), 
                       breaks = c(0, max(dat$mids)*scale), 
                       labels = c(0, max(dat$per)*scale),
                       expand = c(0, 0)) +
    
    scale_color_manual(values = c_pal, name = NULL) +
    scale_fill_manual(values = c_pal, name = NULL) + 
    
    ggh4x::facet_grid2(as.formula(paste0(sprintf('%s',facets[1]), sprintf('~%s',facets[2]))),
                       scales = "free_x") +
    
    scale_y_continuous(name = "mean # calls", 
                       breaks = scales::breaks_pretty(3), 
                       expand = c(0, 0)) +
    
    guides(color=FALSE, fill=FALSE) +
    theme_jamlight()
  
  return(plot)  
}

p.hist_avg_col <- function(dat = NULL, mod = NULL, facets = NULL, ncol = NULL, nrow=NULL) {
  # averages over histograms
  lcl_cpal <- c("#69b3a2", "#404080")
  scale <- 1000
  plot <- ggplot(data = dat, aes(color = condition, fill = condition)) + 
    geom_col(aes(mids*scale, n_calls_mu), position = "identity", 
             color = "black",
             alpha = 0.5) +
    scale_x_continuous(name = paste0(" period (ms)"), 
                       #breaks = c(0, max(dat$per)*scale), 
                       n.breaks = 3,
                       expand = c(0, 0)) +
    scale_y_continuous(name = "mean calls", 
                       n.breaks = 5, 
                       expand = c(0, 0)) +
      
    #scale_color_manual(values = lcl_cpal, name = NULL) +
    scale_fill_manual(values = lcl_cpal, name = NULL) + 
    
   #facet_wrap(as.formula(paste0(sprintf('%s',facets[1]), sprintf('~%s',facets[2]))),
   #                     scales = "free", ncol = ncol, nrow = nrow) +
    ggh4x::facet_grid2(as.formula(paste0(sprintf('%s',facets[1]), sprintf('~%s',facets[2]))),
                       scales = "free", independent = "all") +
    #ggtitle(label = unique(dat$modulation)) +
    
    guides(color=FALSE, fill=FALSE) +
    theme_jamlight()
  
  if (!is.null(mod)) {
    # plot AM phase model
    plot <- plot + 
      geom_line(data = mod, 
                mapping = aes(period*scale, phase, group = condition, linetype = condition), 
                color = "black", inherit.aes = FALSE, size = 1) +
      scale_linetype_manual(values=c(2,1,1), guide = NULL) }
  
  return(plot)  
}

p.hist_avg_dens_dot <- function(dat = dat) {
# averages over histograms, but uses density instead of raw

  plot <- ggplot(data = dat, aes(mids, density_mu, color = condition, fill = condition)) + 
    geom_point(size = 1.5) +
    geom_ribbon(aes(ymin = density_mu-density_se, ymax = density_mu+density_se), linetype = "blank", alpha=0.1) +
    
    scale_x_continuous(name = paste0(" period (ms)"), 
                       breaks = c(0, max(dat$mids)*scale), 
                       labels = c(0, max(dat$per)*scale),
                       expand = c(0, 0)) +
    scale_y_continuous(name = "call density", 
                       breaks = scales::breaks_pretty(5), 
                       expand = c(0, 0)) +
    
    scale_color_manual(values = c_pal, name = NULL) +
    scale_fill_manual(values = c_pal, name = NULL) + 
    
    facet_grid(col = vars(modulation), row = vars(condition), scales = "free") +
    
    guides(color=FALSE, fill=FALSE) +
    
    theme_jamlight() 
  
  return(plot)  
}

## ---- Number of observed calls ---- 
p.obs_n <- function(dat = NULL) {
  # # calls averaged over sessions, separate for groups, conditions & modulation rates
  
  N.calls.plot <- dat %>% group_by(modulation, condition, group) %>% 
    summarise(n = n())
  
  plot <- ggplot(N.calls.plot, aes(condition, n, color = group)) + 
    geom_line(aes(group = group), color = "grey") +
    geom_point(aes(color = group),  size = 3) +

    scale_color_brewer(palette = "Pastel2") +
    scale_y_continuous(name = "total # of calls") + 
    
    facet_grid(. ~ modulation) + 
    
    theme_jamlight() +
    theme(axis.text.x = element_text(angle = 90))
  
  return(plot)
}

p.obs_mean_group <- function(dat = NULL) {
  # # calls averaged over sessions, separate for groups, conditions & modulation rates

  N.calls.plot <- dat %>% group_by(modulation, condition, group, session) %>% 
    summarise(n = n()) %>%
    group_by(modulation, group, condition) %>%
    summarise(mean = mean(n),
              sd = sd(n,na.rm = T),
              se = sd(n,na.rm = T)/sqrt(length(n)))
  
  plot <- ggplot(N.calls.plot, aes(condition, mean, color = group)) + 
    geom_errorbar(aes(ymin = mean-se, ymax = mean+se), alpha = 0.5, width = 0.1) +
    geom_line(aes(group = group), color = "grey") +
    geom_point(aes(color = group),  size = 3) +
    
    scale_y_continuous(name = "mean # of calls") + 
    scale_color_brewer(palette = "Pastel2") +
    
    facet_grid(. ~ modulation) + 
    
    theme_jamlight() +
    theme(axis.text.x = element_text(angle = 90))
  
  return(plot)
}


p.obs_mean <- function(dat = NULL) {
  # averaged over sessions and groups, for conditions & modulation rates
  N.calls.plot <- dat %>% group_by(modulation, condition, group, session) %>% 
    summarise(n = n()) %>%
    group_by(modulation, condition) %>%
    summarise(mean = mean(n),
              sd = sd(n,na.rm = T),
              se = sd(n,na.rm = T)/sqrt(length(n)))
  
  plot <- ggplot(N.calls.plot, aes(condition, mean, color = modulation)) + 
    geom_errorbar(aes(ymin = mean-se, ymax = mean+se), alpha = 0.5, width = 0.1) +
    geom_line(aes(group = modulation, color = modulation)) +
    geom_point(aes(color = modulation),  size = 3) +
    
    scale_y_continuous(name = "mean # of calls") + 
    scale_color_brewer(palette = "Spectral") +
    
    theme_jamlight()
  
  return(plot)
}

p.n_pred <- function(preddat = NULL) {

  plot <- ggplot(preddat, aes(x = condition)) +
    geom_pointrange(aes(y = fit, ymin = fit-se.fit, ymax = fit+se.fit,
                        group = modulation, 
                        #alpha = modulation,
                        color = modulation), 
                    position = position_dodge(width = 0.6), 
                    #color = "tomato"
                    ) +
    geom_point(aes(y = true_mean, 
                   group = modulation,
                   color = modulation,
                   alpha = modulation), 
               position = position_dodge2(width = 0.65, padding = 0.3), 
               #color = "darkslategray", 
               size = 3) +
    labs(x = NULL, 
         y = "mean calling rate per hour") + 
    scale_alpha_manual(values = seq(0.2,0.8,0.08)) +
    scale_color_brewer(palette = "Spectral") +
    scale_fill_brewer(palette = "Spectral") +
    theme_jamlight() +
    theme(legend.position = c(0.96,0.6))

  return(plot)
}
## ---- Number of observed calls across minutes ---- 

p.raster <- function(dat = dat, bins = 60){

  plot <- ggplot(dat, aes(x = start_phase, y = minute)) +
    #stat_density_2d(aes(fill = after_stat(density)), 
    #                n = bins,
    #                geom = "raster", 
    #                contour = F) +
    #geom_tile(aes(fill = ..density..)) +
    geom_bin2d(bins = bins) +
    scale_x_continuous(breaks = seq(0, 2*pi, length.out = 5),
                       labels = piplot,
                       expand = c(0, 0),
                       name = NULL) +
    scale_y_continuous(expand = c(0, 0),
                       trans = "reverse") +
    facet_grid(rows = vars(modulation), cols = vars(condition)) +
    #ggh4x::facet_grid2(vars(modulation), vars(condition),
    #                   scales = "free",
    #                   independent = "y") +
    
    scale_fill_viridis() +
    theme_jamlight()
  
  return(plot)
}

p.minute <- function(dat = dat, max = 61, level = 2) {

  if (level == 1) {
    plot <- ggplot(dat, aes(as.numeric(minute), after_stat(density))) + 
      geom_histogram(breaks = seq(1,max,1)) +
      scale_x_continuous(expand = c(0,1), name = "minute") +
      scale_y_continuous(expand = c(0,0)) +
      theme_jamlight()
    
    
  } else if (level == 2) {
    plot <- ggplot(dat, aes(as.numeric(minute), after_stat(density), fill = condition, color = condition)) + 
      geom_histogram(breaks = seq(1,max,1)) +
      scale_x_continuous(expand = c(0,1), name = "minute") +
      scale_y_continuous(expand = c(0,0)) +
      scale_color_manual(values = c_pal) +
      scale_fill_manual(values = c_pal) +
      facet_grid(modulation ~ condition, scales = "free") +
      theme_jamlight()
    
    
  } else if (level == 3) {
    plot <- ggplot(dat, aes(as.numeric(minute), after_stat(density), fill = group, color = group)) + 
      geom_histogram(breaks = seq(1,max,1)) +
      scale_x_continuous(expand = c(0,1), name = "minute") +
      scale_y_continuous(expand = c(0,0)) +
      scale_color_brewer(palette = "Pastel2") +
      scale_fill_brewer(palette = "Pastel2") +
      facet_grid(group ~ condition, scales = "free") +
      theme_jamlight()
      
    
  }
  
  return(plot)
  
}


## ---- Polar histograms ---- 

p.circ_density <- function(dat = NULL, circ_dat = NULL, bins = 30) {
  # dat needs cols: start.phase, condition
              
  plot <- ggplot(data = dat) +
    geom_histogram(mapping = aes(start_phase, after_stat(density), 
                                 fill = modulation, # previously, condition
                                 alpha = condition # previously, not there
                                 ), 
                   color = "#4D4D4D", 
                   breaks = seq(0, 2*pi, length.out = bins),
                   position = "identity",
                   ) +
    scale_x_continuous(name = "phase", 
                       breaks = seq(0, 2*pi, length.out = 5)[1:4], 
                       limits = c(0,2*pi),
                       labels = piplot[1:4],
                       expand = c(0,0)) +
    scale_y_continuous(name = "call density", 
                       expand = c(0, 0)) +
    
    #scale_fill_manual(values = c_pal, name = NULL) +
    scale_fill_brewer(palette = "Spectral") +
    
    facet_grid(col = vars(modulation), row = vars(condition))+

    guides(color=FALSE, fill=FALSE,alpha=FALSE) +
    theme_jamlight() +
    theme(strip.text = element_text(size = 8),
          axis.text = element_text(size = 8)) +
    
    coord_polar(start = 3*pi/2, direction = -1)
  
  if (!is.null(circ_dat)) {
    plot <- plot + 
      geom_rect(data = circ_dat,
                mapping = aes(xmin = mle_lo, xmax = mle_hi, ymin = -Inf, ymax = Inf),
                fill = "darkslategray",
                alpha = 0.2,
                ) +
      geom_segment(data = circ_dat, 
                aes(x = theta_bar, y = 0, xend = theta_bar, yend = r_bar),
                arrow = arrow(length = unit(0.03, "npc")),
                color = "white",
                size = 1
      )
    
  }
  return(plot)
}



p.angular_vectors <- function(circ_dat = NULL) {
  # dat needs cols: start.phase, condition
  
  plot <- ggplot(data = circ_dat) +
    geom_rect(aes(xmin = mle_lo, xmax = mle_hi, ymin = -Inf, ymax = Inf,
                  fill = condition),
              #fill = "darkslategray",
              alpha = 0.2) +
    geom_segment(aes(x = theta_bar, y = 0, xend = theta_bar, yend = r_bar,
                     color = condition),
                 arrow = arrow(length = unit(0.03, "npc")),
                 #color = "white",
                 size = 1) +
    scale_x_continuous(name = "phase", 
                       breaks = seq(0, 2*pi, length.out = 5)[1:4], 
                       limits = c(0,2*pi),
                       labels = piplot[1:4],
                       expand = c(0,0)) +
    scale_y_continuous(name = "call density", 
                       expand = c(0, 0)) +
    
    scale_fill_manual(values = c_pal, name = NULL) +
    scale_color_manual(values = c_pal, name = NULL) +
    #scale_fill_brewer(palette = "Spectral") +
    
    facet_grid(col = vars(modulation)) +
    
    guides(color=FALSE, fill=FALSE,alpha=FALSE) +
    theme_jamlight() +
    theme(strip.text = element_text(size = 8),
          axis.text = element_text(size = 8)) +
    
    coord_polar(start = 3*pi/2, direction = -1)
  
  return(plot)
}

p.circ_params <- function(dat = NULL, compress = FALSE) {

  plot <- ggplot(dat) 
  
  if (!compress) {
  plot <- plot +  
    geom_point(aes(x=mu,y=kappa, color=condition), alpha = 0.4) +
    
    scale_y_continuous(name = expression(kappa)) +
    scale_x_continuous(name = expression(mu), 
                       breaks = seq(0, 2*pi, length.out = 5)[1:4], 
                       limits = c(0,2*pi),
                       labels = piplot[1:4],
                       expand = c(0,0)) +
    
    scale_color_manual(values = c_pal) +
    scale_alpha_manual(values = c(0.5,0.7,0.9)-0.1) +
    
    facet_grid(col = vars(modulation)) +
    
    guides(color=FALSE, alpha=FALSE)
  } else {
    plot <- plot +  
      geom_point(aes(x=mu,y=kappa, color=modulation), alpha = 0.3) +
      
      scale_y_continuous(name = expression(kappa)) +
      scale_x_continuous(name = expression(mu), 
                         breaks = seq(0, 2*pi, length.out = 5)[1:4], 
                         limits = c(0,2*pi),
                         labels = piplot[1:4],
                         expand = c(0,0)) +
      
      scale_color_brewer(palette = "Spectral") +

      facet_grid(rows = vars(condition)) +
      
      guides(alpha=FALSE)
  }
    
  plot <- plot +   
    theme_jamlight() +
    coord_polar(start = 3*pi/2, direction = -1)
  
  return(plot)
}

p.circ_avg <- function(dat = NULL, circ_dat = NULL, bins = 30) {
  # dat needs cols: start.phase, condition
  
  plot <- ggplot(data = dat) +
    geom_col(mapping = aes(mids, density_mu, fill = condition), 
                   color = "black", 
                   breaks = seq(0, 2*pi, length.out = bins),
                   position = "identity",
                   #alpha = 0.8,
                   #size = 0.75
    ) +
    scale_x_continuous(name = "phase", 
                       breaks = seq(0, 2*pi, length.out = 5)[1:4], 
                       limits = c(0,2*pi),
                       labels = piplot,
                       expand = c(0,0)) +
    scale_y_continuous(name = "avg. call density", 
                       expand = c(0, 0)) +
    
    scale_fill_brewer(palette = "Spectral", name = NULL) +
    
    facet_grid(col = vars(modulation), row = vars(condition))+

    theme_jamlight() +
    
    coord_polar(start = 3*pi/2, direction = -1)
  
  if (!is.null(circ_dat)) {
    plot <- plot + 
      geom_rect(data = circ_dat,
                mapping = aes(xmin = mle_lo, xmax = mle_hi, ymin = -Inf, ymax = Inf),
                fill = "darkslategray",
                alpha = 0.4,
                #color=NULL
      ) +
      geom_segment(data = circ_dat, 
                   aes(x = theta_bar, y = 0, xend = theta_bar, yend = r_bar),
                   arrow = arrow(length = unit(0.03, "npc")),
                   color = "white",
                   size = 1
      )
    
  }
  return(plot)
}

## ---- Circular means ---- 

p.mu_kappa <- function(dat = NULL) {

  plot <- ggplot(dat, aes(mu, kappa, color=modulation)) +
    geom_point(size=2, 
               alpha = 0.4) +
    ggside::geom_xsidedensity(aes(y = after_stat(density)), 
                      position = "stack", 
                      size = 6,
                      #position = position_nudge(y=0.5)
    ) +
    ggside::geom_ysidedensity(aes(x = after_stat(density)), 
                      position = "stack", 
                      size = 6,
                      #position = position_nudge(x=0.5)
    ) +
    
    scale_x_continuous(name = expression(mu), 
                       breaks = seq(0, 2*pi, length.out = 5), 
                       limits = c(0,2*pi),
                       labels = piplot,
                       expand = c(0,0)) +
    scale_y_continuous(name=expression(kappa),
                       breaks = scales::pretty_breaks(4),
                       expand = c(0,0)) +
    ggside::scale_xsidey_continuous(breaks = NULL) +
    ggside::scale_ysidex_continuous(breaks = NULL) +
    scale_color_brewer(palette = "Spectral") +
    
    facet_grid(rows=vars(condition), scales = "fixed") +
    
    guides(alpha = FALSE) +  
    theme_jamlight() +
    theme(panel.spacing.y = unit(0.7, "cm"))
  
  
  return(plot)
}

p.rose <- function(dat = NULL, lab = NULL, bins = 15) {

  CircSpaceTime::rose_diag(dat[[1]], bins = bins, start = 3*pi/2, template = "rad", alpha = 0.6, color = c_pal[1], add = F) +
    theme_jamlight()
  CircSpaceTime::rose_diag(dat[[2]], bins = bins, start = 3*pi/2, template = "rad", alpha = 0.6, color = c_pal[2], add = T) +
    theme_jamlight() -> plot
  if (length(dat)>2) {
  CircSpaceTime::rose_diag(dat[[3]], bins = bins, start = 3*pi/2, template = "rad", alpha = 0.6, color = c_pal[3], add = T) +
     theme_jamlight() -> plot
  }
  if (!is.null(lab)) plot <- plot + ggtitle(lab)
  
  return(plot)
}

## ---- Matrices -----
p.mat <- function(dat = NULL, ulim = NULL) {
  
  plot <- ggplot(dat, aes(observed, predicted, fill=n)) + 
      geom_tile() +
      #geom_text(aes(label=round(n,2)), color = "snow1") +
      scale_fill_viridis(discrete = F, option = "inferno", begin = 0.15, limits = c(0, ulim)) +
      scale_x_discrete(name="observed") +
      scale_y_discrete(name="predicted",limits=rev) +
      facet_grid(rows=vars(model)) +
      theme_jamlight() 

  return(plot)
}

## ---- Model Output ----
p.importance <- function(dat) {
  plot <- ggplot(dat, aes(x = measure, y = 1, color = metric)) + 
    geom_point(size = 4) + 
    scale_color_manual(name = NULL, values = c("lightskyblue2","skyblue4")) +
    scale_y_continuous(name = NULL, breaks = NULL, limits = c(1,1)) +
    facet_grid(rows=vars(measure_name), switch = "y", scales = "free_x", 
               shrink = T, 
               labeller = as_labeller(c(`MeanDecreaseGini` = "mean dec. Gini", 
                                        `MeanDecreaseAccuracy` = "mean dec. acc."))) +
    theme_jamlight() +
    theme(legend.position = "top")
  return(plot)
}

p.importance_shuffled <- function(dat) {
  
  plot <- ggplot(dat, aes(x = measure, y = shuffle, color = metric)) + 
    geom_point(size = 3) + 
    scale_color_manual(name = NULL, values = c("lightskyblue2","skyblue4")) +
    #scale_color_brewer() +
    scale_y_discrete(name = NULL, 
                     #breaks = NULL, 
                     limits = rev
    ) +
    facet_grid(#rows=vars(shuffle), 
      cols = vars(measure_name), 
      switch = "y", scales = "free_x", 
      shrink = T, 
      labeller = as_labeller(c(`shuffled peaks` = "shuffled peaks",
                               `shuffled troughs` = "shuffled troughs",
                               `original` = "original",
                               `t_to_trough` = "t to trough",
                               `t_from_peak` = "time from peak",
                               `MeanDecreaseGini` = "mean dec. Gini", 
                               `MeanDecreaseAccuracy` = "mean dec. acc."))) +
    theme_jamlight() +
    theme(legend.position = "top")
  
  return(plot)
}

## ---- Durations ---- 
p.dur_dens <- function(dur_data = NULL, bins = 50) {

  plot <- ggplot(dur_data, aes(duration, fill = modulation)) + 
    geom_histogram(aes(y=..density.., alpha = modulation), bins = bins, position = "identity") +
    geom_vline(aes(xintercept = med, color = modulation), size = 1) +
    
    scale_x_continuous(limits=c(0,0.008),
                     name = "duration (ms)",
                     breaks = c(0,0.002,0.004,0.006), 
                     labels = c(0,0.002,0.004,0.006)*1000) +
  
    scale_color_brewer(palette = "Spectral") +
    scale_fill_brewer(palette = "Spectral") +
    scale_alpha_manual(values = rev(seq(0.3,0.9,0.08))) +
  
    facet_grid(col=vars(condition)) +

    theme_jamlight()
  
  return(plot)
}

p.dur_hist <- function(dur_data = NULL, bins = 50) {
  
  plot <- ggplot(dur_data, aes(duration, fill = modulation)) + 
    geom_histogram(aes(alpha = modulation), bins = 50, position = "identity") +
    geom_vline(aes(xintercept = med, color = modulation), size = 1) +
    
    scale_x_continuous(limits=c(0,0.008),
                       name = "duration (ms)",
                       breaks = c(0,0.002,0.004,0.006), 
                       labels = c(0,0.002,0.004,0.006)*1000) +
    
    scale_color_brewer(palette = "Spectral") +
    scale_fill_brewer(palette = "Spectral") +
    scale_alpha_manual(values = rev(seq(0.3,0.9,0.08))) +
    
    facet_grid(col=vars(condition)) +
    
    theme_jamlight()
  
  return(plot)  
}

## ---- Onsets ---- 


p.oi_dens <- function(onset_data = NULL, bins = 50) {

 plot <- ggplot(onset_data, aes(onset_interval, fill = modulation)) + 
    geom_histogram(aes(y=..density.., alpha = modulation), bins = bins, position = "identity") +
    geom_vline(aes(xintercept = med, color = modulation), size = 1) +
   
    scale_color_brewer(palette = "Spectral") +
    scale_fill_brewer(palette = "Spectral") +
    scale_alpha_manual(values = rev(seq(0.3,0.9,0.08))) +
    
    facet_grid(col=vars(condition)) +
   
    scale_x_continuous(limits=c(0,0.05),
                      expand=c(0,0),
                      name = "onset intervals (ms)",
                      breaks = c(0,0.01,0.03, 0.05),
                      labels = c(0,0.01,0.03, 0.05)*1000) +
   
    theme_jamlight()
  
 return(plot)
 
} 
 
p.oi_hist <- function(onset_data = NULL, bins = 50) {


  plot <- ggplot(onset_data, aes(onset_interval, fill = modulation)) + 
    geom_histogram(aes(alpha = modulation), bins = bins, position = "identity") +
    geom_vline(aes(xintercept = med, color = modulation), size = 1) +
    
    scale_color_brewer(palette = "Spectral") +
    scale_fill_brewer(palette = "Spectral") +
    scale_alpha_manual(values = rev(seq(0.3,0.9,0.08))) +
    
    facet_grid(col=vars(condition)) +
    
    scale_x_continuous(limits=c(0,0.05),
                       expand=c(0,0),
                       name = "onset intervals (ms)",
                       breaks = c(0,0.01,0.03, 0.05),
                       labels = c(0,0.01,0.03, 0.05)*1000) +
    theme_jamlight()

  return(plot)
}

## ---- Specialty Graphics ----
p.curves <- function(mu_bs_time, scale = 1000) {
  # must include a column, f, with modulation rates as numeric
  
  mod_rates <- unique(mu_bs_time$f)
  
  models <- list()
  bins <- 60
  for (f in mod_rates) {
    t <- seq.int(0.000,round(1/f,3),length.out = bins)
    y <- (cos(2*pi*t*f)*-1)*2*pi
    p <- seq.int(0.000,round(1/f,3),length.out = bins)
    models <- c(models, list(data.frame(phase = y, period = p))) }
  names(models) <- unique(mu_bs_time$modulation)
  
  plot_curves <- models %>% map_df(., ~as.data.frame(.x), .id="modulation") %>% 
    mutate(modulation = factor(modulation, levels = rev(levels(mu_bs_time$modulation)))) %>%
    mutate(f = as.numeric(str_remove(modulation,"Hz")),
           period2 = period-(1/f/2))
  
  plot1 <- ggplot(plot_curves, aes(period2*scale, phase, group = modulation, color = modulation)) +
    geom_line(size = 1) +
    #geom_density_ridges(scale = 1, rel_min_height = 0.001) +
    scale_color_manual(values = rev(RColorBrewer::brewer.pal(8,"Spectral"))) +
    scale_x_continuous(name = NULL, #"time (ms)", 
                       limits = c(min(mu_bs_time$t_from_peak),1/min(mu_bs_time$f)/2*scale+5),
                       breaks = NULL #ceiling(c(0, 1/min(mu_bs_time$f)/4*scale,1/min(mu_bs_time$f)/2*scale))
    ) +
    scale_y_continuous(name = NULL, breaks = NULL) +
    facet_grid(rows=vars(modulation), scales = "free_y") +
    theme_jamlight() + 
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_blank())
  
  plot2 <- ggplot(plot_curves, aes(period*scale, phase, group = modulation, color = modulation)) +
    geom_line(size = 1) +
    #geom_density_ridges(scale = 1, rel_min_height = 0.001) +
    scale_color_manual(values = rev(RColorBrewer::brewer.pal(8,"Spectral"))) +
    scale_x_reverse(name = NULL, #"time (ms)", 
                    limits = c(1/min(mu_bs_time$f)/2*scale+2.5, min(mu_bs_time$t_from_peak)),
                    breaks = NULL #ceiling(c(1/min(mu_bs_time$f)/2*scale,1/min(mu_bs_time$f)/4*scale, 0))
    ) +
    scale_y_continuous(name = NULL,breaks = NULL) +
    facet_grid(rows=vars(modulation), scales = "free_y") +
    theme_jamlight() + 
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_blank())
  
  return(list(plot1, plot2))
}
