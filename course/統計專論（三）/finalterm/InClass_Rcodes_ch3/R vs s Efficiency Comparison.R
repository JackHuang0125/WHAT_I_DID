# ============================================
# R vs s Efficiency Comparison (Publication-level)
# ============================================

rm(list = ls())

setwd("E:/統計品質管制/Week10")

dir.create("figs", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

library(ggplot2)
library(dplyr)

set.seed(123)

# ----------------------------
# Function: simulate sigma estimators
# ----------------------------
simulate_efficiency <- function(m, Nsim = 100000) {
  
  # generate data
  X <- matrix(rnorm(Nsim * m), ncol = m)
  
  # sample statistics
  R <- apply(X, 1, function(x) max(x) - min(x))
  s <- apply(X, 1, sd)
  
  # constants
  d1 <- mean(R)
  d3 <- mean(s)
  
  # estimators of sigma
  sigma_R <- R / d1
  sigma_s <- s / d3
  
  # variances
  var_R <- var(sigma_R)
  var_s <- var(sigma_s)
  
  # relative efficiency (R vs s)
  RE <- var_s / var_R
  
  return(data.frame(
    m = m,
    var_R = var_R,
    var_s = var_s,
    RE = RE
  ))
}

# ----------------------------
# Run for different m
# ----------------------------
m_values <- 2:30

results <- do.call(rbind, lapply(m_values, simulate_efficiency))

# ----------------------------
# Plot 1: Variance comparison
# ----------------------------
p1 <- ggplot(results, aes(x = m)) +
  geom_line(aes(y = var_R, linetype = "R estimator"), size = 1) +
  geom_line(aes(y = var_s, linetype = "s estimator"), size = 1) +
  labs(
    x = "Subgroup size (m)",
    y = "Variance of sigma estimator",
    linetype = "Estimator",
    title = "Variance Comparison: R vs s Estimators"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )

# ----------------------------
# Plot 2: Relative Efficiency
# ----------------------------
p2 <- ggplot(results, aes(x = m, y = RE)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  annotate("text", x = 12, y = 1.1, label = "Crossover ~ m = 10", size = 5) +
  labs(
    x = "Subgroup size (m)",
    y = "Relative Efficiency (R vs s)",
    title = "Relative Efficiency: R vs s"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# ----------------------------
# Save figures
# ----------------------------
dir.create("figs", showWarnings = FALSE)

ggsave("figs/variance_comparison.png", p1, width = 7, height = 5, dpi = 300)
ggsave("figs/relative_efficiency.png", p2, width = 7, height = 5, dpi = 300)

# ----------------------------
# Export table
# ----------------------------
dir.create("outputs", showWarnings = FALSE)
write.csv(results, "outputs/efficiency_summary.csv", row.names = FALSE)

# ----------------------------
# Print summary
# ----------------------------
print(results)