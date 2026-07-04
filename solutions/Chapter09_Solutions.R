# ======================================================================
# Chapter 9 Solutions — Model Diagnostics and Goodness-of-Fit
# Logistic Regression: Bridging Theory and Practice (Doosti, 2026)
# ======================================================================
#
# NOTE: Some solutions build on objects (e.g. `model`, `data`, `Heart`) created by the
#       corresponding chapter script. Run that script first, or source it, e.g.:
#           source("../R/Chapter09_Diagnostics.R")
#       Package dependencies are listed at the top of each solution.


# ------------------------------------------------------------------
# Solution 9.1
# ------------------------------------------------------------------
# Pearson: (y_i - pi_hat_i)/pi_hat_i(1-pi_hat_i); sum of squares  chi^2.
#
# Deviance: sign(y_i - pi_hat_i)d_i; contributes to deviance.
#
# Studentized: Standardized by leverage; better for outlier detection.

# ------------------------------------------------------------------
# Solution 9.2
# ------------------------------------------------------------------
# Residual plots for binary data show discrete patterns (two horizontal bands). Look for: trends across fitted values, systematic patterns vs predictors, and extreme residuals.

# ------------------------------------------------------------------
# Solution 9.3
# ------------------------------------------------------------------
# (a) p = 0.03 suggests model doesn't fit well.
#
# (b) Limitations: sensitive to grouping, lacks power with small samples, only tests calibration not discrimination.
#
# (c) Alternatives: Calibration plots, le Cessie-van Houwelingen test, Brier score decomposition.

# ------------------------------------------------------------------
# Solution 9.4
# ------------------------------------------------------------------

# Solution 9.4
# Diagnostic plots
par(mfrow = c(2, 2))
plot(fitted(model), residuals(model, type = "deviance"),
     xlab = "Fitted", ylab = "Deviance Residuals")
plot(cooks.distance(model), type = "h", ylab = "Cook's Distance")
plot(hatvalues(model), rstudent(model), 
     xlab = "Leverage", ylab = "Studentized Residuals")


# ------------------------------------------------------------------
# Solution 9.5
# ------------------------------------------------------------------
# H-L test with 10 groups typically has appropriate Type I error. Fewer groups (5) may lack power; more groups (20) may be unstable with sparse cells.

# ------------------------------------------------------------------
# Solution 9.6
# ------------------------------------------------------------------
# Le Cessie-van Houwelingen test (implemented in rms::residuals.lrm) is often more powerful for detecting misspecification than H-L test.

# ------------------------------------------------------------------
# Solution 9.7
# ------------------------------------------------------------------

# Solution 9.7
library(titanic)
data(titanic_train)

model <- glm(Survived ~ Pclass + Sex + Age + Fare, 
             family = binomial, data = titanic_train)

# Influential observations
influential <- which(cooks.distance(model) > 4/nrow(titanic_train))
model_excl <- update(model, subset = -influential)

# Compare coefficients
cbind(Original = coef(model), Excluding = coef(model_excl))


# ------------------------------------------------------------------
# Solution 9.8
# ------------------------------------------------------------------
# DFBETAS measures influence on each coefficient: DFBETAS_j,i = (beta_hat_j - beta_hat_j(-i))/SE(beta_hat_j). Cook's distance is a summary; DFBETAS shows which coefficients are affected.
#
# %% ============================================================
# %% CHAPTER 10 SOLUTIONS
# %% ============================================================
