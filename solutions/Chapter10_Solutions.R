# ======================================================================
# Chapter 10 Solutions — Model Validation and Prediction
# Logistic Regression: Bridging Theory and Practice (Doosti, 2026)
# ======================================================================
#
# NOTE: Some solutions build on objects (e.g. `model`, `data`, `Heart`) created by the
#       corresponding chapter script. Run that script first, or source it, e.g.:
#           source("../R/Chapter10_Validation.R")
#       Package dependencies are listed at the top of each solution.


# ------------------------------------------------------------------
# Solution 10.1
# ------------------------------------------------------------------
# Apparent: Performance on training data (optimistic).
#
# Internal: CV or bootstrap on development data.
#
# External: Completely independent data (gold standard).
#
# Apparent overestimates because model is optimized for training data.

# ------------------------------------------------------------------
# Solution 10.2
# ------------------------------------------------------------------
# Optimism = apparent performance - test performance. Bootstrap estimates this by: (1) fit to bootstrap sample; (2) evaluate on bootstrap and original; (3) average difference.

# ------------------------------------------------------------------
# Solution 10.3
# ------------------------------------------------------------------
# (a) AUC 0.85, poor calibration: Better for ranking patients.
#
# (b) AUC 0.75, good calibration: Better for individual risk estimates.
#
# Improve: recalibrate high-AUC model; add predictors to improve discrimination.

# ------------------------------------------------------------------
# Solution 10.4
# ------------------------------------------------------------------

# Solution 10.4: k-fold CV from scratch
cv_logistic <- function(formula, data, k = 10) {
  folds <- sample(rep(1:k, length.out = nrow(data)))
  auc <- numeric(k)
  
  for(i in 1:k) {
    train <- data[folds != i, ]
    test <- data[folds == i, ]
    
    model <- glm(formula, family = binomial, data = train)
    pred <- predict(model, test, type = "response")
    auc[i] <- pROC::auc(test$y, pred)
  }
  
  c(mean = mean(auc), se = sd(auc)/sqrt(k))
}


# ------------------------------------------------------------------
# Solution 10.5
# ------------------------------------------------------------------

# Solution 10.5: Bootstrap optimism correction
bootstrap_optimism <- function(formula, data, B = 200) {
  apparent <- auc(glm(formula, binomial, data), data)
  optimism <- numeric(B)
  
  for(b in 1:B) {
    boot_data <- data[sample(nrow(data), replace = TRUE), ]
    boot_model <- glm(formula, binomial, boot_data)
    boot_apparent <- auc(boot_model, boot_data)
    boot_test <- auc(boot_model, data)
    optimism[b] <- boot_apparent - boot_test
  }
  
  apparent - mean(optimism)
}


# ------------------------------------------------------------------
# Solution 10.6
# ------------------------------------------------------------------

# Solution 10.6: Calibration assessment
library(rms)
cal <- calibrate(lrm_model, B = 200)
plot(cal)

# Calibration slope
recal <- glm(y ~ pred_logit, family = binomial)
coef(recal)  # Slope should be ~1


# ------------------------------------------------------------------
# Solution 10.7
# ------------------------------------------------------------------

# Solution 10.7: Decision curve analysis
library(dcurves)
dca(y ~ pred_prob, data = test_data)


# ------------------------------------------------------------------
# Solution 10.8
# ------------------------------------------------------------------
# Temporal validation: fit on earlier data, validate on later data. Apply recalibration if needed (update intercept and/or slope).

# ------------------------------------------------------------------
# Solution 10.9
# ------------------------------------------------------------------

# IDI and NRI
library(PredictABEL)
reclassification(data, cOutcome = "y", 
                 predrisk1 = simple_pred, predrisk2 = extended_pred,
                 cutoff = c(0, 0.1, 0.2, 1))

# %% ============================================================
# %% CHAPTER 11 SOLUTIONS
# %% ============================================================
