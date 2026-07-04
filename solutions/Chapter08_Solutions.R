# ======================================================================
# Chapter 8 Solutions — Interaction Effects
# Logistic Regression: Bridging Theory and Practice (Doosti, 2026)
# ======================================================================
#
# NOTE: Some solutions build on objects (e.g. `model`, `data`, `Heart`) created by the
#       corresponding chapter script. Run that script first, or source it, e.g.:
#           source("../R/Chapter08_Interactions.R")
#       Package dependencies are listed at the top of each solution.


# ------------------------------------------------------------------
# Solution 8.1
# ------------------------------------------------------------------
# Additive: Combined risk  sum of individual risks.
#
# Multiplicative: Combined OR  product of individual ORs.
#
# Can have interaction on one scale but not other because scales are non-linearly related.

# ------------------------------------------------------------------
# Solution 8.2
# ------------------------------------------------------------------
# Principle of marginality: Keep main effects when interaction is included. The main effect coefficient represents the effect when the other variable equals zero. Removing it changes the interaction's meaning.

# ------------------------------------------------------------------
# Solution 8.3
# ------------------------------------------------------------------
# The interaction coefficient in logistic regression is on the log-odds scale. The probability-scale interaction requires computing predicted probabilities at different combinations and calculating second differences.

# ------------------------------------------------------------------
# Solution 8.4
# ------------------------------------------------------------------

# Solution 8.4
# Generate data with interactions
x1 <- rnorm(500); x2 <- rnorm(500); x3 <- rbinom(500, 1, 0.5)
y <- rbinom(500, 1, plogis(-0.5 + 0.5*x1 + 0.3*x2 + 0.4*x3 + 
                           0.3*x1*x2 + 0.5*x1*x3))

mod_main <- glm(y ~ x1 + x2 + x3, family = binomial)
mod_int <- glm(y ~ x1*x2 + x1*x3, family = binomial)

# LRT
anova(mod_main, mod_int, test = "LRT")


# ------------------------------------------------------------------
# Solution 8.5
# ------------------------------------------------------------------

# Solution 8.5: Simple slopes
library(interactions)
sim_slopes(mod_int, pred = x1, modx = x2, 
           modx.values = c(-1, 0, 1))  # at -1SD, mean, +1SD


# ------------------------------------------------------------------
# Solution 8.6
# ------------------------------------------------------------------

# Solution 8.6: Johnson-Neyman
library(interactions)
johnson_neyman(mod_int, pred = x1, modx = x2)


# ------------------------------------------------------------------
# Solution 8.7
# ------------------------------------------------------------------
# Test sex x age interaction in heart disease data. If significant, report age effects separately for men and women with predicted probability plots.

# ------------------------------------------------------------------
# Solution 8.8
# ------------------------------------------------------------------
# Ai and Norton (2003): The marginal effect of interaction on probability is:
#
# (^2 P)/( x_1  x_2) = beta_12 pi(1-pi)(1-2pi) + beta_1beta_2pi(1-pi)(1-2pi)
#
# This differs from simply beta_12.
#
# %% ============================================================
# %% CHAPTER 9 SOLUTIONS
# %% ============================================================
