# ======================================================================
# Chapter 4 Solutions — Overdispersion and Multiple Link Functions
# Logistic Regression: Bridging Theory and Practice (Doosti, 2026)
# ======================================================================
#
# NOTE: Some solutions build on objects (e.g. `model`, `data`, `Heart`) created by the
#       corresponding chapter script. Run that script first, or source it, e.g.:
#           source("../R/Chapter04_Overdispersion.R")
#       Package dependencies are listed at the top of each solution.


# ------------------------------------------------------------------
# Solution 4.1
# ------------------------------------------------------------------
# Overdispersion: Var(Y) > np(1-p).
#
# Consequences: (a) Point estimates unaffected; (b) SEs underestimated by ; (c) Tests anti-conservative, CIs too narrow; (d) AIC invalid for model comparison.

# ------------------------------------------------------------------
# Solution 4.2
# ------------------------------------------------------------------
# Sources and solutions: (1) Clustering: Random effects models; (2) Unobserved heterogeneity: Beta-binomial; (3) Misspecification: Add non-linear terms or interactions.

# ------------------------------------------------------------------
# Solution 4.3
# ------------------------------------------------------------------
# Quasi-binomial for quick adjustment; Beta-binomial when dispersion varies with covariates; Random effects with clear cluster structure; DGLM when dispersion is systematic.

# ------------------------------------------------------------------
# Solution 4.4
# ------------------------------------------------------------------

# Solution 4.4
library(VGAM)
set.seed(123)

n <- 200; m <- 20
x <- rnorm(n)
mu <- plogis(-1 + 0.5 * x)
phi <- 0.3

y <- rbetabinom(n, size = m, prob = mu, rho = phi)
data <- data.frame(y, m, x)

binom_mod <- glm(cbind(y, m-y) ~ x, family = binomial, data = data)
qbinom_mod <- glm(cbind(y, m-y) ~ x, family = quasibinomial, data = data)

# Compare SEs
cat("Binomial SE:", sqrt(vcov(binom_mod)[2,2]), "\n")
cat("Quasi SE:", sqrt(vcov(qbinom_mod)[2,2]), "\n")

# Dispersion
disp <- sum(residuals(binom_mod, "pearson")^2) / binom_mod$df.residual
cat("Dispersion:", disp, "\n")


# ------------------------------------------------------------------
# Solution 4.5
# ------------------------------------------------------------------

# Solution 4.5
library(lme4)

data$obs <- factor(1:nrow(data))
glmm_mod <- glmer(cbind(y, m - y) ~ x + (1 | obs), family = binomial, data = data)

# Variance component
VarCorr(glmm_mod)

# Compare SEs to quasi-binomial
sqrt(diag(vcov(glmm_mod)))
sqrt(diag(vcov(qbinom_mod)))


# ------------------------------------------------------------------
# Solution 4.6
# ------------------------------------------------------------------
# Simulation showing coverage drops below 90% when dispersion  > 1.3 approximately. Quasi-binomial maintains nominal coverage.

# ------------------------------------------------------------------
# Solution 4.7
# ------------------------------------------------------------------

# Solution 4.7
library(lme4)
data(cbpp)

# (a) Binomial ignoring herd
mod_bin <- glm(cbind(incidence, size-incidence) ~ period, 
               family = binomial, data = cbpp)

# (b) Test overdispersion
deviance(mod_bin) / df.residual(mod_bin)  # > 1 indicates overdispersion

# (c) Random intercept
mod_glmm <- glmer(cbind(incidence, size-incidence) ~ period + (1|herd),
                  family = binomial, data = cbpp)

# (d) Compare SEs - GLMM SEs larger

# (e) ICC
vc <- as.numeric(VarCorr(mod_glmm)$herd)
icc <- vc / (vc + pi^2/3)


# ------------------------------------------------------------------
# Solution 4.8
# ------------------------------------------------------------------
# For random intercept variance sigma^2: ICC ~ sigma^2/(sigma^2 + pi^2/3). The quasi-binomial dispersion relates through  ~ 1 + (m-1). Approaches equivalent with equal cluster sizes and normal random effects.
#
# %% ============================================================
# %% CHAPTER 5 SOLUTIONS
# %% ============================================================
