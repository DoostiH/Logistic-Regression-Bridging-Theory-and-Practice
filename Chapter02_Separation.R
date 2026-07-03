# =============================================================================
# Chapter 2: Complete and Quasi-Complete Separation
# Figures and Analysis Code
# -----------------------------------------------------------------------------
# Practical Challenges in Logistic Regression Modeling
# Hassan Doosti
#
# This script reproduces all figures and numerical outputs for Chapter 2.
# The endometrial cancer example uses the REAL dataset from Heinze & Schemper
# (2002), distributed with the brglm2 package.
# =============================================================================

# Install required packages (uncomment on first use)
# install.packages(c("tidyverse", "logistf", "brglm2", "detectseparation",
#                    "arm", "cowplot", "reshape2", "pROC", "scales"))

# Load required packages
library(tidyverse)
library(logistf)           # Firth's penalized logistic regression
library(brglm2)            # Bias-reduction methods and the endometrial data
library(detectseparation)  # Formal detection of separation
library(arm)               # Bayesian GLM (bayesglm)
library(cowplot)           # Combining plots
library(reshape2)          # Data reshaping
library(pROC)              # ROC analysis
library(scales)            # Plot formatting

# Resolve namespace conflicts (MASS masks dplyr functions)
select <- dplyr::select
filter <- dplyr::filter

# Set seed for reproducibility (used by the simulated illustrations only)
set.seed(123)

# Create output directories if they don't exist
dir.create("figures", showWarnings = FALSE)
dir.create("output",  showWarnings = FALSE)

# Consistent black & white theme for all plots
theme_chapter <- theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position   = "right",
    panel.grid.minor  = element_blank()
  )

# =============================================================================
# Figure 2.1: Complete Separation Visualization  (illustrative simulation)
# =============================================================================
n  <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
y  <- ifelse(x1 > 0, 1, 0)          # Perfect separation by x1
sep_data <- data.frame(y = y, x1 = x1, x2 = x2)

fig_2_1 <- ggplot(sep_data, aes(x = x1, y = x2, shape = factor(y))) +
  geom_point(size = 3, alpha = 0.7, colour = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30", linewidth = 1) +
  annotate("text", x = -1.5, y = 2.5, label = "All Y = 0", fontface = "bold", size = 4) +
  annotate("text", x =  1.5, y = 2.5, label = "All Y = 1", fontface = "bold", size = 4) +
  annotate("text", x = 0.15, y = -2.5, label = "Separation\nboundary",
           hjust = 0, size = 3.5, colour = "grey30") +
  scale_shape_manual(values = c("0" = 1, "1" = 17), labels = c("Y = 0", "Y = 1")) +
  labs(title = "Complete Separation Example",
       subtitle = expression("The predictor " * x[1] * " perfectly separates the outcome classes"),
       x = expression(x[1]), y = expression(x[2]), shape = "Outcome") +
  theme_chapter +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))
ggsave("figures/fig_2_1_complete_separation.jpeg", fig_2_1,
       width = 7, height = 5.5, dpi = 300, device = "jpeg")
cat("Figure 2.1 saved: Complete separation visualization\n")

# =============================================================================
# Figure 2.2: Quasi-Complete Separation Visualization  (illustrative simulation)
# =============================================================================
set.seed(123)
z <- ifelse(x1 > 0, 1, 0)
# Add some overlap (flip a handful of observations near the boundary)
overlap_indices <- c(which(x1 > 0 & x1 < 0.3)[1:3], which(x1 < 0 & x1 > -0.3)[1:2])
overlap_indices <- overlap_indices[!is.na(overlap_indices)]
z[overlap_indices] <- 1 - z[overlap_indices]
qsep_data <- data.frame(z = z, x1 = x1, x2 = x2)
qsep_data$overlap <- FALSE
qsep_data$overlap[overlap_indices] <- TRUE

fig_2_2 <- ggplot(qsep_data, aes(x = x1, y = x2, shape = factor(z))) +
  geom_point(data = subset(qsep_data, !overlap), size = 3, alpha = 0.7, colour = "black") +
  geom_point(data = subset(qsep_data, overlap),  size = 4, alpha = 1, stroke = 1.5, colour = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30", linewidth = 1) +
  annotate("rect", xmin = -0.5, xmax = 0.5, ymin = -3, ymax = 3, alpha = 0.15, fill = "grey70") +
  annotate("text", x = 0, y = -2.8, label = "Overlap\nregion", size = 3.5, colour = "grey30") +
  scale_shape_manual(values = c("0" = 1, "1" = 17), labels = c("Y = 0", "Y = 1")) +
  labs(title = "Quasi-Complete Separation Example",
       subtitle = "Near-perfect separation with some overlap at the boundary",
       x = expression(x[1]), y = expression(x[2]), shape = "Outcome") +
  theme_chapter +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))
ggsave("figures/fig_2_2_quasi_separation.jpeg", fig_2_2,
       width = 7, height = 5.5, dpi = 300, device = "jpeg")
cat("Figure 2.2 saved: Quasi-complete separation visualization\n")

# =============================================================================
# Model Fitting and Output Capture  (complete-separation illustration)
# =============================================================================
sep_model <- suppressWarnings(
  glm(y ~ x1 + x2, family = binomial(link = "logit"), data = sep_data,
      control = glm.control(maxit = 50))
)
sink("output/ch2_sep_model_summary.txt")
cat("Standard Logistic Regression with Complete Separation\n")
cat("=====================================================\n\n")
print(summary(sep_model))
sink()

firth_model <- logistf(y ~ x1 + x2, data = sep_data)
sink("output/ch2_firth_model_summary.txt")
cat("Firth's Penalized Logistic Regression\n")
cat("=====================================\n\n")
print(summary(firth_model))
sink()

br_model <- glm(y ~ x1 + x2, family = binomial(link = "logit"), data = sep_data,
                method = "brglmFit")
sink("output/ch2_brglm_model_summary.txt")
cat("Bias-Reduced GLM (brglm2)\n")
cat("=========================\n\n")
print(summary(br_model))
sink()

bayes_model <- bayesglm(y ~ x1 + x2, family = binomial(link = "logit"),
                        data = sep_data, prior.scale = 2.5, prior.df = 1)
sink("output/ch2_bayes_model_summary.txt")
cat("Bayesian GLM with Weakly Informative Priors\n")
cat("============================================\n\n")
print(summary(bayes_model))
sink()

sep_check <- glm(y ~ x1 + x2, family = binomial(link = "logit"), data = sep_data,
                 method = "detect_separation")
sink("output/ch2_separation_check.txt")
cat("Separation Detection Results\n")
cat("============================\n\n")
print(sep_check)
sink()
cat("Model outputs saved to output/ directory\n")

# =============================================================================
# Figure 2.3: Coefficient Comparison Across Methods
# =============================================================================
coef_comparison <- data.frame(
  Method    = c("Standard MLE", "Firth", "BR-GLM", "Bayesian"),
  Intercept = c(coef(sep_model)[1], coef(firth_model)[1], coef(br_model)[1], coef(bayes_model)[1]),
  x1        = c(coef(sep_model)[2], coef(firth_model)[2], coef(br_model)[2], coef(bayes_model)[2]),
  x2        = c(coef(sep_model)[3], coef(firth_model)[3], coef(br_model)[3], coef(bayes_model)[3])
)
sink("output/ch2_coefficient_comparison.txt")
cat("Coefficient Comparison Across Methods\n")
cat("=====================================\n\n")
print(coef_comparison, digits = 3)
sink()

coef_long <- coef_comparison %>%
  pivot_longer(cols = c(Intercept, x1, x2), names_to = "Coefficient", values_to = "Estimate") %>%
  mutate(Method = factor(Method, levels = c("Standard MLE", "Firth", "BR-GLM", "Bayesian")))

fig_2_3 <- ggplot(coef_long, aes(x = Coefficient, y = Estimate, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_fill_manual(values = c("Standard MLE" = "grey90", "Firth" = "grey60",
                               "BR-GLM" = "grey35", "Bayesian" = "grey10")) +
  labs(title = "Coefficient Estimates by Method",
       subtitle = "Standard MLE produces extreme estimates under separation",
       x = "Coefficient", y = "Estimate", fill = "Method") +
  theme_chapter +
  theme(axis.text.x = element_text(size = 10)) +
  coord_cartesian(ylim = c(-5, 30))
ggsave("figures/fig_2_3_coefficient_comparison.jpeg", fig_2_3,
       width = 8, height = 5, dpi = 300, device = "jpeg")
cat("Figure 2.3 saved: Coefficient comparison\n")

# =============================================================================
# Figure 2.4: Likelihood Surface Illustration  (schematic; unchanged)
# -----------------------------------------------------------------------------
# NOTE: This figure is an intentional SCHEMATIC of the general shape of the
# standard vs. penalized likelihood under separation. The penalty used here is
# illustrative, not the exact Jeffreys penalty; the curve is not meant to
# locate a specific fitted value. (Figure 2.8 shows the true penalized profile.)
# =============================================================================
beta1_seq <- seq(-5, 25, length.out = 100)
loglik <- sapply(beta1_seq, function(b1) {
  eta <- b1 * sep_data$x1
  p <- plogis(eta); p <- pmax(pmin(p, 1 - 1e-10), 1e-10)
  sum(sep_data$y * log(p) + (1 - sep_data$y) * log(1 - p))
})
pen_loglik <- sapply(beta1_seq, function(b1) {
  eta <- b1 * sep_data$x1
  p <- plogis(eta); p <- pmax(pmin(p, 1 - 1e-10), 1e-10)
  ll <- sum(sep_data$y * log(p) + (1 - sep_data$y) * log(1 - p))
  penalty <- -0.5 * log(1 + b1^2 / 10)   # illustrative penalty (schematic)
  ll + penalty * 5
})
likelihood_data <- data.frame(
  beta1  = rep(beta1_seq, 2),
  loglik = c(loglik, pen_loglik),
  Type   = rep(c("Standard", "Penalized (Firth)"), each = length(beta1_seq))
)
fig_2_4 <- ggplot(likelihood_data, aes(x = beta1, y = loglik, linetype = Type)) +
  geom_line(linewidth = 1.2, colour = "black") +
  geom_vline(xintercept = coef(firth_model)[2], linetype = "dotted",
             colour = "grey40", linewidth = 0.8) +
  annotate("text", x = coef(firth_model)[2] + 1, y = -60,
           label = "Firth\nestimate", hjust = 0, size = 3, colour = "grey40") +
  annotate("segment", x = 20, xend = 24, y = -20, yend = -20,
           arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  annotate("text", x = 17, y = -20, label = expression("MLE " %->% " \u221E"),
           colour = "black", size = 3.5) +
  scale_linetype_manual(values = c("Standard" = "solid", "Penalized (Firth)" = "dashed")) +
  labs(title = "Log-Likelihood Under Separation",
       subtitle = expression("Standard likelihood is monotonic; penalized likelihood has finite maximum"),
       x = expression("Coefficient " * beta[1]), y = "Log-likelihood", linetype = "Method") +
  theme_chapter
ggsave("figures/fig_2_4_likelihood_surface.jpeg", fig_2_4,
       width = 8, height = 5, dpi = 300, device = "jpeg")
cat("Figure 2.4 saved: Likelihood surface illustration (schematic)\n")

# =============================================================================
# Figure 2.5: Predicted Probabilities Comparison
# =============================================================================
predict_logistf <- function(model, newdata) {
  beta <- coef(model)
  formula_terms <- attr(terms(model$formula), "term.labels")
  X <- model.matrix(as.formula(paste("~", paste(formula_terms, collapse = "+"))), data = newdata)
  eta <- as.vector(X %*% beta)
  plogis(eta)
}
grid_data <- data.frame(x1 = seq(-3, 3, length.out = 200), x2 = 0)
pred_standard <- predict(sep_model, newdata = grid_data, type = "response")
pred_firth    <- predict_logistf(firth_model, newdata = grid_data)
pred_brglm    <- predict(br_model, newdata = grid_data, type = "response")
pred_bayes    <- predict(bayes_model, newdata = grid_data, type = "response")

predictions <- data.frame(
  x1 = grid_data$x1,
  `Standard MLE` = pred_standard, `Firth` = pred_firth,
  `BR-GLM` = pred_brglm, `Bayesian` = pred_bayes, check.names = FALSE
)
pred_long <- predictions %>%
  pivot_longer(cols = -x1, names_to = "Method", values_to = "Probability") %>%
  mutate(Method = factor(Method, levels = c("Standard MLE", "Firth", "BR-GLM", "Bayesian")))
data_points <- sep_data %>%
  dplyr::select(x1, y) %>%
  mutate(y_jitter = y + runif(dplyr::n(), -0.03, 0.03))

fig_2_5 <- ggplot() +
  geom_point(data = data_points, aes(x = x1, y = y_jitter, shape = factor(y)),
             alpha = 0.4, size = 2, colour = "grey50", show.legend = FALSE) +
  geom_line(data = pred_long, aes(x = x1, y = Probability, linetype = Method),
            linewidth = 1, colour = "black") +
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey50") +
  scale_shape_manual(values = c("0" = 1, "1" = 17)) +
  scale_linetype_manual(values = c("Standard MLE" = "solid", "Firth" = "dashed",
                                   "BR-GLM" = "dotdash", "Bayesian" = "twodash")) +
  labs(title = "Predicted Probabilities by Method",
       subtitle = "Standard MLE produces step-function predictions; alternatives are smoother",
       x = expression(x[1]), y = "Predicted Probability", linetype = "Method") +
  theme_chapter + theme(legend.position = "right")
ggsave("figures/fig_2_5_predicted_probabilities.jpeg", fig_2_5,
       width = 8, height = 5, dpi = 300, device = "jpeg")
cat("Figure 2.5 saved: Predicted probabilities comparison\n")

# =============================================================================
# Figure 2.6: Standard Errors Comparison
# =============================================================================
se_standard <- summary(sep_model)$coefficients[, 2]
se_firth    <- sqrt(diag(vcov(firth_model)))
se_brglm    <- summary(br_model)$coefficients[, 2]
se_bayes    <- summary(bayes_model)$coefficients[, 2]
se_comparison <- data.frame(
  Method      = rep(c("Standard MLE", "Firth", "BR-GLM", "Bayesian"), each = 3),
  Coefficient = rep(c("Intercept", "x1", "x2"), 4),
  SE          = c(se_standard, se_firth, se_brglm, se_bayes)
)
se_comparison$Method <- factor(se_comparison$Method,
                               levels = c("Standard MLE", "Firth", "BR-GLM", "Bayesian"))
se_comparison$SE_capped <- pmin(se_comparison$SE, 50)
se_comparison$truncated <- se_comparison$SE > 50

fig_2_6 <- ggplot(se_comparison, aes(x = Coefficient, y = SE_capped, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7,
           colour = "black", linewidth = 0.3) +
  geom_text(data = subset(se_comparison, truncated), aes(label = sprintf("%.0f", SE)),
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Standard MLE" = "grey90", "Firth" = "grey60",
                               "BR-GLM" = "grey35", "Bayesian" = "grey10")) +
  labs(title = "Standard Errors by Method",
       subtitle = "Standard MLE produces inflated SEs under separation; alternatives provide stability",
       x = "Coefficient", y = "Standard Error", fill = "Method") +
  theme_chapter +
  coord_cartesian(ylim = c(0, 55)) +
  annotate("text", x = 2, y = 52, label = "(truncated)", size = 3, colour = "grey50")
ggsave("figures/fig_2_6_standard_errors.jpeg", fig_2_6,
       width = 8, height = 5, dpi = 300, device = "jpeg")
cat("Figure 2.6 saved: Standard errors comparison\n")

sink("output/ch2_se_comparison.txt")
cat("Standard Error Comparison Across Methods\n")
cat("=========================================\n\n")
se_wide <- se_comparison %>%
  dplyr::select(-SE_capped, -truncated) %>%
  pivot_wider(names_from = Coefficient, values_from = SE)
print(se_wide, digits = 3)
sink()

# =============================================================================
# Real Data Example: Endometrial Cancer Dataset (Heinze & Schemper, 2002)
# -----------------------------------------------------------------------------
# 79 patients. Outcome HG = histology grade (0/1).
# Predictors: NV = neovascularization (0/1),
#             PI = pulsatility index of the arteria uterina (integer),
#             EH = endometrium height (continuous).
# The NV = 1 group is entirely HG = 1 (zero cell), producing genuine
# quasi-complete separation: the MLE for the NV coefficient diverges to +Inf.
# =============================================================================
data("endometrial", package = "brglm2")
endo_data <- endometrial

# --- Structure / sanity check ------------------------------------------------
sink("output/ch2_endo_structure.txt")
cat("Endometrial Cancer Dataset (Heinze & Schemper, 2002)\n")
cat("====================================================\n\n")
cat("Sample size:", nrow(endo_data), "\n\n")
cat("Structure:\n"); str(endo_data)
cat("\nSummary:\n"); print(summary(endo_data))
sink()

# --- Key cross-tabulation: NV vs HG (the zero cell) --------------------------
sink("output/ch2_endo_crosstab.txt")
cat("Cross-tabulation: Neovascularization vs. Histology Grade\n")
cat("=========================================================\n\n")
print(table(NV = endo_data$NV, HG = endo_data$HG))
cat("\nNote: when NV = 1, ALL patients have HG = 1 (zero cell at NV=1, HG=0).\n")
cat("This is genuine quasi-complete separation: the MLE for the NV\n")
cat("coefficient does not exist (diverges to +Inf).\n")
sink()

# --- Fit all four methods ----------------------------------------------------
endo_glm <- suppressWarnings(
  glm(HG ~ NV + PI + EH, family = binomial, data = endo_data,
      control = glm.control(maxit = 100))
)
endo_firth <- logistf(HG ~ NV + PI + EH, data = endo_data)
endo_brglm <- glm(HG ~ NV + PI + EH, family = binomial, data = endo_data,
                  method = "brglmFit")
endo_bayes <- bayesglm(HG ~ NV + PI + EH, family = binomial, data = endo_data,
                       prior.scale = 2.5, prior.df = 1)

# --- Capture each summary verbatim -------------------------------------------
sink("output/ch2_endo_glm_summary.txt")
cat("Standard MLE (note separation warning / diverging NV coefficient)\n")
cat("=================================================================\n\n")
print(summary(endo_glm))
sink()

sink("output/ch2_endo_firth_summary.txt")
cat("Firth's Penalized Likelihood\n")
cat("============================\n\n")
print(summary(endo_firth))
sink()

sink("output/ch2_endo_brglm_summary.txt")
cat("Bias-Reduced GLM (brglm2)\n")
cat("=========================\n\n")
print(summary(endo_brglm))
sink()

sink("output/ch2_endo_bayes_summary.txt")
cat("Bayesian GLM, Cauchy(0, 2.5) prior\n")
cat("==================================\n\n")
print(summary(endo_bayes))
sink()

# --- Formal separation detection ---------------------------------------------
endo_sep <- glm(HG ~ NV + PI + EH, family = binomial, data = endo_data,
                method = "detect_separation")
sink("output/ch2_endo_separation.txt")
cat("Separation Detection for Endometrial Data\n")
cat("==========================================\n\n")
print(endo_sep)
sink()

# =============================================================================
# Figure 2.7: Endometrial Data - Coefficient Forest Plot
# =============================================================================
get_or_ci <- function(model, method_name) {
  if (inherits(model, "logistf")) {
    coefs <- coef(model); ci <- confint(model)
    data.frame(Method = method_name, Variable = names(coefs),
               OR = exp(coefs), Lower = exp(ci[, 1]), Upper = exp(ci[, 2]))
  } else {
    coefs <- coef(model); se <- summary(model)$coefficients[, 2]
    data.frame(Method = method_name, Variable = names(coefs),
               OR = exp(coefs),
               Lower = exp(coefs - 1.96 * se),
               Upper = exp(coefs + 1.96 * se))
  }
}

forest_data <- bind_rows(
  get_or_ci(endo_glm,   "Standard MLE"),
  get_or_ci(endo_firth, "Firth"),
  get_or_ci(endo_brglm, "BR-GLM"),
  get_or_ci(endo_bayes, "Bayesian")
) %>%
  dplyr::filter(Variable != "(Intercept)") %>%
  mutate(
    Method   = factor(Method, levels = c("Standard MLE", "Firth", "BR-GLM", "Bayesian")),
    Variable = factor(Variable, levels = c("NV", "PI", "EH"),
                      labels = c("Neovascularization", "Pulsatility Index", "Endometrium Height"))
  )
forest_data$Upper_capped <- pmin(forest_data$Upper, 500)
forest_data$OR_capped    <- pmin(forest_data$OR, 500)

fig_2_7 <- ggplot(forest_data, aes(x = OR_capped, y = Method, shape = Method)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper_capped), height = 0.2,
                 linewidth = 0.8, colour = "black") +
  geom_point(size = 3, colour = "black") +
  facet_wrap(~Variable, ncol = 1, scales = "free_x") +
  scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10, 50, 100, 500),
                labels = c("0.1","0.5","1","2","5","10","50","100","500+")) +
  scale_shape_manual(values = c("Standard MLE" = 16, "Firth" = 17,
                                "BR-GLM" = 15, "Bayesian" = 18)) +
  labs(title = "Odds Ratios with 95% Confidence Intervals",
       subtitle = "Endometrial cancer data: NV exhibits quasi-complete separation",
       x = "Odds Ratio (log scale)", y = "") +
  theme_chapter +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))
ggsave("figures/fig_2_7_forest_plot.jpeg", fig_2_7,
       width = 8, height = 6, dpi = 300, device = "jpeg")
cat("Figure 2.7 saved: Forest plot for endometrial data\n")

# --- Table 2.2 source: OR (95% CI) for every method --------------------------
sink("output/ch2_endo_odds_ratios.txt")
cat("Odds Ratio Comparison - Endometrial Cancer Data\n")
cat("================================================\n\n")
cat("NOTE: Firth intervals are profile-likelihood based; BR-GLM and Bayesian\n")
cat("are Wald. Under separation the Wald interval for NV is unreliable.\n\n")
or_wide <- forest_data %>%
  mutate(OR_CI = sprintf("%.2f (%.2f, %.2f)", OR, Lower, pmin(Upper, 9999))) %>%
  dplyr::select(Variable, Method, OR_CI) %>%
  pivot_wider(names_from = Method, values_from = OR_CI)
print(as.data.frame(or_wide), width = 200)
sink()

# --- Raw coefficients + SEs: verify Firth == BR-GLM (logit-link equivalence) --
sink("output/ch2_endo_coef_table.txt")
cat("Raw coefficients and SEs (verifying Firth vs BR-GLM agreement)\n")
cat("=============================================================\n\n")
coef_tab <- data.frame(
  Term  = names(coef(endo_firth)),
  Firth = as.numeric(coef(endo_firth)),
  BRGLM = as.numeric(coef(endo_brglm)),
  Bayes = as.numeric(coef(endo_bayes))
)
print(coef_tab, digits = 4)
cat("\nStandard errors:\n")
se_tab <- data.frame(
  Term     = names(coef(endo_firth)),
  Firth_SE = sqrt(diag(vcov(endo_firth))),
  BRGLM_SE = summary(endo_brglm)$coefficients[, 2],
  Bayes_SE = summary(endo_bayes)$coefficients[, 2]
)
print(se_tab, digits = 4)
sink()
cat("Endometrial (real-data) outputs saved to output/ directory\n")

# =============================================================================
# Figure 2.8: Profile Likelihood for NV Coefficient  (TRUE penalized profile)
# -----------------------------------------------------------------------------
# For each fixed value of beta_NV, profile out the remaining coefficients,
# maximising (a) the ordinary log-likelihood and (b) the Firth-penalised
# log-likelihood  l*(beta) = l(beta) + 0.5 * log|I(beta)|,
# where I(beta) = X' W X and W = diag(p_i (1 - p_i)).
# =============================================================================
X_endo <- model.matrix(HG ~ NV + PI + EH, data = endo_data)
y_endo <- endo_data$HG
nv_col <- which(colnames(X_endo) == "NV")

negll <- function(free, b, penalised) {
  beta <- numeric(ncol(X_endo))
  beta[nv_col]  <- b
  beta[-nv_col] <- free
  eta <- as.vector(X_endo %*% beta)
  p   <- plogis(eta); p <- pmax(pmin(p, 1 - 1e-12), 1e-12)
  ll  <- sum(y_endo * log(p) + (1 - y_endo) * log(1 - p))
  if (penalised) {
    W    <- p * (1 - p)
    info <- t(X_endo) %*% (X_endo * W)
    ll   <- ll + 0.5 * log(max(det(info), 1e-300))
  }
  -ll
}
profile_at <- function(b, penalised) {
  opt <- tryCatch(optim(c(0, 0, 0), negll, b = b, penalised = penalised, method = "BFGS"),
                  error = function(e) NULL)
  if (is.null(opt)) NA_real_ else -opt$value
}

nv_grid <- seq(0, 12, length.out = 200)
ll_std  <- vapply(nv_grid, profile_at, numeric(1), penalised = FALSE)
ll_pen  <- vapply(nv_grid, profile_at, numeric(1), penalised = TRUE)

profile_data <- data.frame(
  beta_NV = rep(nv_grid, 2),
  loglik  = c(ll_std, ll_pen),
  Type    = rep(c("Standard", "Penalized (Firth)"), each = length(nv_grid))
) %>%
  group_by(Type) %>%
  mutate(loglik_norm = loglik - max(loglik, na.rm = TRUE)) %>%
  ungroup()

firth_nv     <- 2.9292733
firth_nv_lo  <- 0.6097244
firth_nv_hi  <- 7.8546317
ci_threshold <- -qchisq(0.95, df = 1) / 2   # = -1.921

fig_2_8 <- ggplot(profile_data, aes(x = beta_NV, y = loglik_norm, linetype = Type)) +
  geom_line(linewidth = 1.2, colour = "black", na.rm = TRUE) +
  geom_hline(yintercept = ci_threshold, linetype = "dotted", colour = "grey50") +
  geom_vline(xintercept = firth_nv, linetype = "dotted", colour = "grey40") +
  annotate("point", x = firth_nv, y = 0, size = 2.5, colour = "black") +
  annotate("text", x = firth_nv + 0.2, y = 0.15,
           label = "Firth estimate\n(2.93)", hjust = 0, size = 3, colour = "grey30") +
  annotate("text", x = 9.5, y = ci_threshold + 0.18,
           label = expression("95% CI boundary (" * chi[1]^2 * " = 3.84)"),
           size = 3, colour = "grey50") +
  annotate("segment", x = firth_nv_lo, xend = firth_nv_hi,
           y = ci_threshold, yend = ci_threshold, colour = "grey40", linewidth = 0.4) +
  annotate("text", x = 11.3, y = -0.55, label = expression("Standard " %->% " no maximum"),
           size = 3, colour = "grey30", hjust = 1) +
  scale_linetype_manual(values = c("Standard" = "solid", "Penalized (Firth)" = "dashed")) +
  labs(title = "Profile Log-Likelihood for the Neovascularization Coefficient",
       subtitle = "Standard profile rises without a finite maximum; the penalized profile peaks at the Firth estimate",
       x = expression("Coefficient " * beta[NV]),
       y = "Normalized profile log-likelihood", linetype = "Method") +
  theme_chapter +
  coord_cartesian(xlim = c(0, 12), ylim = c(-5, 0.5))
ggsave("figures/fig_2_8_profile_likelihood.jpeg", fig_2_8,
       width = 8, height = 5, dpi = 300, device = "jpeg")
cat("Figure 2.8 saved: real penalized profile likelihood\n")

# =============================================================================
# Figure 2.9: Decision Flowchart Data (logic for the TikZ figure in LaTeX)
# =============================================================================
sink("output/ch2_decision_flowchart.txt")
cat("Decision Flowchart for Handling Separation\n")
cat("==========================================\n\n")
cat("1. Fit standard logistic regression\n")
cat("2. Check for warning signs:\n")
cat("   - Large coefficients (|beta| > 10)\n")
cat("   - Large standard errors (SE > 10)\n")
cat("   - Non-convergence warnings\n")
cat("3. Run formal separation test (detectseparation)\n")
cat("4. If separation detected:\n")
cat("   a. Primary: Use Firth's method (logistf)\n")
cat("   b. Alternative: Bayesian with weakly informative priors\n")
cat("   c. Alternative: BR-GLM (brglm2)\n")
cat("5. Report both detection and method used\n")
sink()

# =============================================================================
# Figure 2.10: Simulation Study - Bias Under Separation
# =============================================================================
set.seed(789)
n_sims <- 500
sample_sizes <- c(50, 100, 200)
results_list <- list()
for (n_sim in sample_sizes) {
  bias_standard <- numeric(n_sims)
  bias_firth    <- numeric(n_sims)
  converged     <- logical(n_sims)
  for (i in 1:n_sims) {
    x <- rnorm(n_sim)
    true_beta <- 2
    prob <- plogis(true_beta * x)
    yy <- rbinom(n_sim, 1, prob)
    fit_std   <- suppressWarnings(glm(yy ~ x, family = binomial))
    fit_firth <- suppressWarnings(logistf(yy ~ x, data = data.frame(yy = yy, x = x), pl = FALSE))
    bias_standard[i] <- coef(fit_std)[2]   - true_beta
    bias_firth[i]    <- coef(fit_firth)[2] - true_beta
    converged[i]     <- fit_std$converged
  }
  results_list[[as.character(n_sim)]] <- data.frame(
    n = n_sim,
    Method = rep(c("Standard MLE", "Firth"), each = n_sims),
    Bias   = c(bias_standard, bias_firth)
  )
}
sim_results <- bind_rows(results_list) %>%
  mutate(n = factor(n, levels = sample_sizes, labels = paste("n =", sample_sizes)))

fig_2_10 <- ggplot(sim_results, aes(x = Method, y = Bias, fill = Method)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1, colour = "black") +
  facet_wrap(~n) +
  scale_fill_manual(values = c("Standard MLE" = "grey80", "Firth" = "grey40")) +
  labs(title = "Bias in Coefficient Estimates: Simulation Study",
       subtitle = expression("True " * beta * " = 2; 500 simulations per sample size"),
       x = "", y = expression("Bias (" * hat(beta) * " - " * beta * ")")) +
  theme_chapter +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(-2, 5))
ggsave("figures/fig_2_10_simulation_bias.jpeg", fig_2_10,
       width = 9, height = 5, dpi = 300, device = "jpeg")
cat("Figure 2.10 saved: Simulation study results\n")

sink("output/ch2_simulation_summary.txt")
cat("Simulation Study Summary\n")
cat("========================\n\n")
sim_summary <- sim_results %>%
  group_by(n, Method) %>%
  summarise(
    Mean_Bias   = mean(Bias, na.rm = TRUE),
    Median_Bias = median(Bias, na.rm = TRUE),
    SD_Bias     = sd(Bias, na.rm = TRUE),
    RMSE        = sqrt(mean(Bias^2, na.rm = TRUE)),
    .groups = "drop"
  )
print(sim_summary, digits = 3)
sink()

# =============================================================================
# Chapter Summary Statistics
# =============================================================================
sink("output/ch2_chapter_summary.txt")
cat("Chapter 2 Summary Statistics\n")
cat("============================\n\n")
cat("Complete Separation Example (simulated):\n")
cat("- Sample size: ", n, "\n")
cat("- Outcome prevalence: ", mean(sep_data$y), "\n")
cat("- Standard MLE x1 coefficient: ", coef(sep_model)[2], "\n")
cat("- Firth x1 coefficient: ", coef(firth_model)[2], "\n\n")
cat("Endometrial Cancer Example (real data, Heinze & Schemper 2002):\n")
cat("- Sample size: ", nrow(endo_data), "\n")
cat("- Outcome prevalence: ", mean(endo_data$HG), "\n")
cat("- Cases with NV=1 and HG=0: ", sum(endo_data$NV == 1 & endo_data$HG == 0), "\n")
cat("- Cases with NV=1 and HG=1: ", sum(endo_data$NV == 1 & endo_data$HG == 1), "\n")
cat("- Standard MLE NV coefficient (diverges): ", coef(endo_glm)["NV"], "\n")
cat("- Firth NV coefficient: ", coef(endo_firth)["NV"], "\n")
cat("- BR-GLM NV coefficient: ", coef(endo_brglm)["NV"], "\n\n")
cat("Key Recommendations:\n")
cat("1. Always check for separation with detect_separation()\n")
cat("2. Use Firth's method as primary solution\n")
cat("3. Bayesian approaches with weakly informative priors as alternative\n")
cat("4. Report separation detection and correction method\n")
sink()

cat("\n=== All Chapter 2 figures and outputs generated successfully ===\n")
cat("Figures saved to: figures/\n")
cat("Outputs saved to: output/\n")
