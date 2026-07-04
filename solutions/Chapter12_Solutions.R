# ======================================================================
# Chapter 12 Solutions — Multinomial and Ordinal Logistic Regression
# Logistic Regression: Bridging Theory and Practice (Doosti, 2026)
# ======================================================================
#
# NOTE: Some solutions build on objects (e.g. `model`, `data`, `Heart`) created by the
#       corresponding chapter script. Run that script first, or source it, e.g.:
#           source("../R/Chapter12_MultinomialOrdinal.R")
#       Package dependencies are listed at the top of each solution.


# ------------------------------------------------------------------
# Solution 12.1
# ------------------------------------------------------------------
# Multinomial: Unordered categories (e.g., transport mode: car, bus, bike).
#
# Ordinal: Ordered categories (e.g., pain: none, mild, moderate, severe).
#
# Ordinal models exploit the ordering for efficiency.

# ------------------------------------------------------------------
# Solution 12.2
# ------------------------------------------------------------------
# Proportional odds: (P(Y <= j))/(P(Y > j)) = _j - Xbeta
#
# The beta coefficients are the same across all cutpoints--the effect of X is proportional across cumulative odds.

# ------------------------------------------------------------------
# Solution 12.3
# ------------------------------------------------------------------
# (a) p = 0.02 suggests proportional odds may not hold.
#
# (b) Don't automatically switch--consider: effect size of violation, sample size, practical importance.
#
# (c) Alternatives: Partial proportional odds (relax for specific variables), multinomial model, continuation ratio model.

# ------------------------------------------------------------------
# Solution 12.4
# ------------------------------------------------------------------

# Solution 12.4
library(nnet)

# Generate 3-category outcome
# ... simulation

mult_mod <- multinom(y ~ x1 + x2, data = sim_data)
summary(mult_mod)

# Predicted probabilities
predict(mult_mod, type = "probs")


# ------------------------------------------------------------------
# Solution 12.5
# ------------------------------------------------------------------

# Solution 12.5
library(MASS)
library(brant)

po_mod <- polr(y_ord ~ x1 + x2, data = sim_data)
brant(po_mod)  # Test proportional odds


# ------------------------------------------------------------------
# Solution 12.6
# ------------------------------------------------------------------
# Proportional odds has fewer parameters than multinomial (one set of beta vs J-1 sets). When assumption holds, it's more efficient. Compare via AIC.

# ------------------------------------------------------------------
# Solution 12.7
# ------------------------------------------------------------------

# Solution 12.7
library(ordinal)
data(wine)

po_full <- clm(rating ~ temp + contact, data = wine)
brant_test <- nominal_test(po_full)  # Test for each variable

# Partial proportional odds
ppo_mod <- clm(rating ~ contact, nominal = ~ temp, data = wine)


# ------------------------------------------------------------------
# Solution 12.8
# ------------------------------------------------------------------
# Adjacent-category model: (P(Y=j))/(P(Y=j+1)) = _j - Xbeta. Preferred when interest is in moving between adjacent categories (e.g., dose-response).
#
# %% ============================================================
# %% CHAPTER 13 SOLUTIONS
# %% ============================================================
