test_random_effect <- function(complex_model, null_formula, data, n_sim = 100) {
  
  # Automatically detect family from complex model
  family <- family(complex_model)
  
  # Fit null model
  m1rand <- update(complex_model, null_formula, data = data)
  
  # Observed likelihood ratio statistic
  lrstat <- as.numeric(2 * (logLik(complex_model) - logLik(m1rand)))
  cat("Observed LR statistic:", round(lrstat, 4), "\n")
  
  # Control to suppress singular fit warnings
  cntrl <- glmerControl(check.conv.singular = .makeCC(action = "ignore", tol = 1e-4))
  
  # Bootstrap
  b.lrstat <- numeric(n_sim)
  for (i in 1:n_sim) {
    y <- unlist(simulate(m1rand))
    bnull <- update(m1rand, y ~ ., control = cntrl)
    balt  <- update(complex_model, y ~ ., control = cntrl)
    b.lrstat[i] <- as.numeric(2 * (logLik(balt) - logLik(bnull)))
  }
  
  p <- mean(b.lrstat > lrstat)
  cat("Bootstrap p-value:", p, "\n")
  
  return(list(lrstat = lrstat, p_value = p))
}

# Usage:
# Gamma model
result1 <- test_random_effect(
  complex_model = complexmodel,
  null_formula  = BAI ~ num_species + tpa_calc + Age_2021 + (1|plot_id:tree_tag),
  data          = cfi
)

# Gaussian/lmer model
result2 <- test_random_effect(
  complex_model = wrc_ht_mod_2015_final,
  null_formula  = month4_ht ~ treat_name + (1|rep),
  data          = WRC_Rows_2015_ht_vals
)