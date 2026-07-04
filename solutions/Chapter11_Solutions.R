# ======================================================================
# Chapter 11 Solutions — Logistic Regression for Longitudinal Data
# Logistic Regression: Bridging Theory and Practice (Doosti, 2026)
# ======================================================================
#
# NOTE: Some solutions build on objects (e.g. `model`, `data`, `Heart`) created by the
#       corresponding chapter script. Run that script first, or source it, e.g.:
#           source("../R/Chapter11_Longitudinal.R")
#       Package dependencies are listed at the top of each solution.


# ------------------------------------------------------------------
# Solution 11.1
# ------------------------------------------------------------------
# Ignoring correlation leads to underestimated SEs (too small), anti-conservative inference, inflated Type I error.

# ------------------------------------------------------------------
# Solution 11.2
# ------------------------------------------------------------------
# GEE (population-averaged): Effect of changing x across the population.
#
# GLMM (subject-specific): Effect of changing x for an individual.
#
# Subject-specific effects are typically larger in magnitude due to the non-collapsibility of the odds ratio.

# ------------------------------------------------------------------
# Solution 11.3
# ------------------------------------------------------------------
# _hat = 0.65 means 65% of within-subject variance is due to stable individual differences. If _hat ~ 0, observations within subjects are essentially independent.

# ------------------------------------------------------------------
# Solution 11.4
# ------------------------------------------------------------------

# Solution 11.4
library(geepack)

# Generate correlated binary data
# ... (simulation code)

gee_ind <- geeglm(y ~ x + time, id = subject, family = binomial,
                  corstr = "independence", data = long_data)
gee_exch <- geeglm(y ~ x + time, id = subject, family = binomial,
                   corstr = "exchangeable", data = long_data)
gee_ar1 <- geeglm(y ~ x + time, id = subject, family = binomial,
                  corstr = "ar1", data = long_data)


# ------------------------------------------------------------------
# Solution 11.5
# ------------------------------------------------------------------

# Solution 11.5: Compare GEE and GLMM
library(lme4)

gee_mod <- geeglm(y ~ x, id = subject, family = binomial,
                  corstr = "exchangeable", data = long_data)
glmm_mod <- glmer(y ~ x + (1|subject), family = binomial, data = long_data)

# GLMM coefficients typically larger
coef(gee_mod)
fixef(glmm_mod)


# ------------------------------------------------------------------
# Solution 11.6
# ------------------------------------------------------------------
# Transition model: logit(P(Y_t=1)) = beta_0 + beta_1 x_t + gamma Y_t-1. The coefficient gamma represents persistence/state dependence.

# ------------------------------------------------------------------
# Solution 11.7
# ------------------------------------------------------------------

# Solution 11.7
library(geepack)
data(respiratory)

# Fit models with different correlation structures
# Compare treatment effects and SEs
# GLMM provides subject-specific estimates


# ------------------------------------------------------------------
# Solution 11.8
# ------------------------------------------------------------------
# Sandwich (robust) SE: Var(beta_hat) = (X'WX)^-1 X'W Var(Y) WX (X'WX)^-1. This is robust because it uses the empirical variance of residuals rather than assuming the working correlation is correct.
#
# %% ============================================================
# %% CHAPTER 12 SOLUTIONS
# %% ============================================================
