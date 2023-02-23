# Implement Negative Binomial Model and carry out all checks 
run_negbinom_model <- function(dat, formula = NULL, contformula = NULL, eval = FALSE, model2 = NULL) {
  require(MASS)
  require(car)
  require(janitor)
  require(emmeans)
  require(sjPlot)

  
  who <- unique(dat$modulation)
    if (length(who)>1) who <- "all rates"
  print(paste0("~~~~~~~~~~~~~~  ",who,"  ~~~~~~~~~~~~~~"))
  
# Run Model  
# model
if (is.null(formula)) { 
  calls.glm.nb <- MASS::glm.nb(n ~ condition, data = dat)
} else {
  calls.glm.nb <- MASS::glm.nb(as.formula(formula), data = dat)
}
# model summary
print("Model summary...")
print(summary(calls.glm.nb))

# check for overdispersion
print("Model dispersion...")
disp <- sum(residuals(calls.glm.nb, type="pearson")^2)/df.residual(calls.glm.nb) 
print(disp)

# deviance table: calculate likelihood ratios from Type II tests; 
# but note that this implies a fixed theta term (bad?)
print("Type II Deviance Table...")
print(dt <- car::Anova(calls.glm.nb, type="II", test="LR") )
dt <- dt %>% as.data.frame() %>% mutate(across(where(is.numeric), round, 3))

# Check that deviance and Pearson residuals are near the critical value
# qchisq(0.95, df.residual(calls.glm.nb))
# deviance(calls.glm.nb)
# sum(residuals(calls.glm.nb,"pearson")^2)

# model evaluation
if (eval) {
  require(rcompanion)
  print("Model evaluation...")
  print(me <- rcompanion::nagelkerke(calls.glm.nb))
} else {
  me <- NULL
}

# get incidence rates and CIs
print("Incidence rates...")
print(est <- exp(cbind(Estimate = coef(calls.glm.nb), confint(calls.glm.nb))))
est <- est %>% as.data.frame()

# marginal mean contrasts
print("Estimated marginal mean contrasts...")
if (is.null(contformula)) { 
  mmeans <- emmeans::emmeans(calls.glm.nb,  ~ condition)
  
  contrasts <- emmeans::emmeans(calls.glm.nb, pairwise ~ condition, 
                                adjust = "bonferroni",
                                type = "response")
  nb.margin.table <- contrasts$contrasts %>% summary(infer = TRUE) %>% as.data.frame() %>%
    dplyr::rename("masking condition" = contrast) %>% 
    mutate(across(is.numeric, round, 3)) %>% 
    janitor::clean_names()
  
} else {
  mmeans <- emmeans::emmeans(calls.glm.nb, as.formula(contformula),)
  
  contrasts <- emmeans::emmeans(calls.glm.nb, as.formula(paste("pairwise", contformula)), 
                                adjust = "bonferroni",
                                type = "response")
  
  nb.margin.table <- contrasts$contrasts %>% summary(infer = TRUE) %>% as.data.frame() %>%
    #dplyr::rename("masking condition" = contrast) %>% 
    mutate(across(is.numeric, round, 3)) %>% 
    janitor::clean_names()
}
print(contrasts)

# model comparison
if (!is.null(model2)) {
  require(pscl)
  print("Comparing to provided model...")
  print(mcomp <- pscl::vuong(model2, calls.glm.nb, digits = 4))
}

# Regression Tables ----
# require(kable)
# Posthoc Comparisons
# nb.margin.table  %>%
#   kbl(
#     escape = FALSE, 
#     booktabs = TRUE, 
#     format = "latex",
#     caption = "Estimated Marginal Means") %>%
#   kable_paper() %>%
#   as_image(file = 'binomialreg_contrasts.pdf')

# Deviance Table
# kbl(dt, 
#     escape = FALSE, 
#     booktabs = TRUE, 
#     format = "latex",
#     caption = "Type II Analysis of Deviance") %>%
#   kable_paper() %>%
#   as_image(file = 'binomialreg_devtable.pdf')

# Model Output
# Model Incidence Ratios, plotted
sjplot <- sjPlot::plot_model(calls.glm.nb, 
                             title = "Predicted Incidence of Calling", 
                             show.values = TRUE, show.legend = TRUE, 
                             value.size = 3.5,
                             vline.color = "gray",
                             show.p = TRUE) + 
          theme_jamlight()

# Generate predictions
if (is.null(contformula)) { 
  newdata <- dat %>% group_by(condition) %>% 
    summarise(true_mean = mean(n))
} else {
  newdata <- dat %>% group_by_at(stringr::str_split(contformula," ",simplify = T)[c(2,4)] %>% na.omit()) %>% 
    summarise(true_mean = mean(n))
}

predicted <- cbind(newdata, predict(calls.glm.nb, newdata, type = "response", se.fit=TRUE)) 

model.output <- list(model = calls.glm.nb, 
                     dispersion = disp, 
                     deviance = dt, 
                     eval = me, 
                     IRR = est,
                     marginalmeans = mmeans,
                     contrasts = nb.margin.table,
                     predictions = predicted,
                     plot = sjplot)

  return(model.output)
}