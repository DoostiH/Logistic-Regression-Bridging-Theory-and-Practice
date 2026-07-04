# ======================================================================
# Chapter 3 Solutions — Rare Events Bias
# Logistic Regression: Bridging Theory and Practice (Doosti, 2026)
# ======================================================================
#
# NOTE: Some solutions build on objects (e.g. `model`, `data`, `Heart`) created by the
#       corresponding chapter script. Run that script first, or source it, e.g.:
#           source("../R/Chapter03_RareEvents.R")
#       Package dependencies are listed at the top of each solution.


# ------------------------------------------------------------------
# Solution 3.1
# ------------------------------------------------------------------
# The MLE in logistic regression has O(1/n) bias. With rare events, few events provide most information, creating high variance and likelihood asymmetry.
#
# Direction of bias:
#
#     -  Intercept: Biased downward (more negative), underestimating baseline probability
#     -  Slopes: Biased away from zero, overstating effect sizes

# ------------------------------------------------------------------
# Solution 3.2
# ------------------------------------------------------------------
# (a) Not necessarily good! A model predicting "no fraud" always achieves 99.9% accuracy with 0.1% fraud rate.
#
# (b) Better metrics: Precision, Recall, F1 Score, AUPRC, and confusion matrix analysis.
#
# (c) Stakeholder explanation: "With 100,000 transactions and only 100 frauds, always predicting `no fraud' is 99.9% accurate but catches zero frauds. We need metrics measuring how well we catch actual frauds (recall) and how often our alerts are correct (precision)."

# ------------------------------------------------------------------
# Solution 3.3
# ------------------------------------------------------------------
# Method & Affects coefficients? & Affects classification? \\
#
# Firth's & Yes (reduces bias) & Indirectly \\
# Case-control & Only intercept & No \\
# ROSE/SMOTE & Yes (completely) & Yes (main purpose) \\
#
# ROSE and SMOTE alter the data distribution, making coefficients uninterpretable as population effects.

# ------------------------------------------------------------------
# Solution 3.4
# ------------------------------------------------------------------

# Solution 3.4
library(logistf)
set.seed(123)

event_rates <- c(0.01, 0.02, 0.05, 0.10, 0.20, 0.50)
n <- 2000; n_sim <- 500; true_beta1 <- 0.8
results <- data.frame()

for(rate in event_rates) {
  beta0 <- qlogis(rate)
  bias_mle <- bias_firth <- numeric(n_sim)
  
  for(i in 1:n_sim) {
    x <- rnorm(n)
    y <- rbinom(n, 1, plogis(beta0 + true_beta1 * x))
    
    mle_fit <- glm(y ~ x, family = binomial)
    firth_fit <- logistf(y ~ x, data = data.frame(y, x))
    
    bias_mle[i] <- coef(mle_fit)[2] - true_beta1
    bias_firth[i] <- coef(firth_fit)[2] - true_beta1
  }
  
  results <- rbind(results, data.frame(
    rate = rate, MLE = mean(bias_mle), Firth = mean(bias_firth)))
}

# Plot shows both methods have positive bias at low rates,
# Firth consistently less biased


# ------------------------------------------------------------------
# Solution 3.5
# ------------------------------------------------------------------

# Solution 3.5: Case-control with intercept correction
set.seed(123)
n <- 10000; beta0 <- -4; beta1 <- 0.8
x <- rnorm(n)
y <- rbinom(n, 1, plogis(beta0 + beta1 * x))
p_pop <- mean(y)

ratios <- c(1, 3, 5, 10)
results <- data.frame()

for(ratio in ratios) {
  cases <- which(y == 1)
  controls <- sample(which(y == 0), ratio * length(cases))
  cc_idx <- c(cases, controls)
  
  cc_model <- glm(y[cc_idx] ~ x[cc_idx], family = binomial)
  p_sample <- mean(y[cc_idx])
  
  # Intercept correction
  correction <- log((p_sample/(1-p_sample)) / (p_pop/(1-p_pop)))
  corrected_int <- coef(cc_model)[1] - correction
  
  results <- rbind(results, data.frame(
    ratio = ratio, uncorrected = coef(cc_model)[1],
    corrected = corrected_int, true = beta0))
}


# ------------------------------------------------------------------
# Solution 3.6
# ------------------------------------------------------------------

# Solution 3.6
library(PRROC)
library(ROSE)

# After fitting models (std_model, firth_model, rose_model)
# and getting predictions on test set:

pr_std <- pr.curve(pred_std, weights.class0 = test$fraud, curve = TRUE)
pr_firth <- pr.curve(pred_firth, weights.class0 = test$fraud, curve = TRUE)
pr_rose <- pr.curve(pred_rose, weights.class0 = test$fraud, curve = TRUE)

cat("AUPRC - Standard:", pr_std$auc.integral, "\n")
cat("AUPRC - Firth:", pr_firth$auc.integral, "\n")
cat("AUPRC - ROSE:", pr_rose$auc.integral, "\n")


# ------------------------------------------------------------------
# Solution 3.7
# ------------------------------------------------------------------
# Apply methods to real credit card fraud data following the same structure. Key findings typically show ROSE improves recall but may sacrifice precision. Recommendation depends on cost structure of false positives vs. false negatives.

# ------------------------------------------------------------------
# Solution 3.8
# ------------------------------------------------------------------
# King and Zeng (2001) correction: beta_tilde = beta_hat - (X'WX)^-1 X' W xi, where xi_i = -0.5 Q_ii (1 - 2pi_hat_i)   [equivalently 0.5(2pi_hat_i - 1)Q_ii] and Q_ii is the hat matrix diagonal. Implementation is similar to Firth but computationally simpler for large datasets.
#
# %% ============================================================
# %% CHAPTER 4 SOLUTIONS
# %% ============================================================
