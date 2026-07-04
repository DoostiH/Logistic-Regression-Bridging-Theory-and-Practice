# ======================================================================
# Chapter 5 Solutions — Variable Selection Methods
# Logistic Regression: Bridging Theory and Practice (Doosti, 2026)
# ======================================================================
#
# NOTE: Some solutions build on objects (e.g. `model`, `data`, `Heart`) created by the
#       corresponding chapter script. Run that script first, or source it, e.g.:
#           source("../R/Chapter05_VariableSelection.R")
#       Package dependencies are listed at the top of each solution.


# ------------------------------------------------------------------
# Solution 5.1
# ------------------------------------------------------------------
# (a) All predictors: Lowest bias, highest variance, overfitting risk.
#
# (b) Stepwise: Reduces variance but may introduce bias; discrete selection causes high variability.
#
# (c) Lasso: Continuous shrinkage, biases toward zero, automatic selection.
#
# (d) Ridge: Maximum variance reduction, biases all coefficients, no selection.

# ------------------------------------------------------------------
# Solution 5.2
# ------------------------------------------------------------------
# P-values after stepwise are biased because: (1) Variables chosen for showing association--circular reasoning; (2) Multiple testing not accounted for; (3) Winner's curse inflates effects. Results may not replicate; use independent validation.

# ------------------------------------------------------------------
# Solution 5.3
# ------------------------------------------------------------------
# Critique: Lasso selection isn't a hypothesis test. Better reporting: "Using lasso with 10-fold CV ( = 0.05), glucose was retained with coefficient 0.035 (OR = 1.036). Five of 8 predictors selected. CV-AUC = 0.82."

# ------------------------------------------------------------------
# Solution 5.4
# ------------------------------------------------------------------

# Solution 5.4
library(glmnet)
set.seed(123)

n <- 500; n_sim <- 100
results <- matrix(0, n_sim, 6)

for(sim in 1:n_sim) {
  # Correlated predictors (rho = 0.9)
  X_corr <- MASS::mvrnorm(n, c(0,0), matrix(c(1,0.9,0.9,1), 2))
  X <- cbind(X_corr, rnorm(n))
  y <- rbinom(n, 1, plogis(-0.5 + 0.8 * X[,1]))  # Only x1 is true
  
  cv_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1)
  cv_enet <- cv.glmnet(X, y, family = "binomial", alpha = 0.5)
  
  results[sim, 1:3] <- as.numeric(coef(cv_lasso, s = "lambda.min")[-1] != 0)
  results[sim, 4:6] <- as.numeric(coef(cv_enet, s = "lambda.min")[-1] != 0)
}

# Selection frequencies
colMeans(results)  # Lasso unstable for x1 vs x2; elastic net more stable


# ------------------------------------------------------------------
# Solution 5.5
# ------------------------------------------------------------------

# Solution 5.5: Stability selection
stability_selection <- function(X, y, B = 100) {
  p <- ncol(X)
  selection_matrix <- matrix(0, B, p)
  
  for(b in 1:B) {
    idx <- sample(1:nrow(X), nrow(X), replace = TRUE)
    cv_fit <- cv.glmnet(X[idx,], y[idx], family = "binomial")
    selection_matrix[b,] <- as.numeric(coef(cv_fit, s = "lambda.1se")[-1] != 0)
  }
  
  probs <- colMeans(selection_matrix)
  list(probs = probs,
       sel_50 = which(probs > 0.5),
       sel_70 = which(probs > 0.7),
       sel_90 = which(probs > 0.9))
}


# ------------------------------------------------------------------
# Solution 5.6
# ------------------------------------------------------------------

# Solution 5.6: Coefficient paths
library(glmnet)

# Fit all three
ridge <- glmnet(X, y, family = "binomial", alpha = 0)
lasso <- glmnet(X, y, family = "binomial", alpha = 1)
enet <- glmnet(X, y, family = "binomial", alpha = 0.5)

par(mfrow = c(1, 3))
plot(ridge, xvar = "lambda", main = "Ridge")
plot(lasso, xvar = "lambda", main = "Lasso")
plot(enet, xvar = "lambda", main = "Elastic Net")


# ------------------------------------------------------------------
# Solution 5.7
# ------------------------------------------------------------------
# Apply all methods to Pima data. Typical findings: glucose, BMI, pedigree consistently selected. Recommendation: Lasso for parsimony with key clinical predictors retained.

# ------------------------------------------------------------------
# Solution 5.8
# ------------------------------------------------------------------
# Adaptive lasso uses weights w_j = 1/|beta_hat_j|^gamma. Implementation uses initial estimates for weights, then applies weighted lasso. Improves oracle property compared to standard lasso.
#
# %% ============================================================
# %% CHAPTER 6 SOLUTIONS
# %% ============================================================
