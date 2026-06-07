# ============================================================
# Mean-CUSUM vs X-bar Shewhart Chart under Variance Shift
# Output:
# figs/ch4_example48_cusum_vs_xbar_variance_shift.png
# ============================================================

rm(list = ls())

setwd("E:/統計品質管制/Week12")

packages <- c("ggplot2", "dplyr", "tidyr", "patchwork")
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

dir.create("figs", showWarnings = FALSE, recursive = TRUE)

set.seed(20260510)

# -----------------------------
# Simulation settings
# -----------------------------
N <- 120
tau <- 60

mu0 <- 0
sigma0 <- 1
sigma1 <- 2.5

k <- 0.5
h <- 5

# -----------------------------
# Generate standardized observations
# Mean unchanged, variance increases
# -----------------------------
x <- c(
  rnorm(tau, mean = mu0, sd = sigma0),
  rnorm(N - tau, mean = mu0, sd = sigma1)
)

z <- (x - mu0) / sigma0

# -----------------------------
# Mean-CUSUM
# -----------------------------
Cplus <- numeric(N)
Cminus <- numeric(N)

for (n in 1:N) {
  if (n == 1) {
    Cplus[n]  <- max(0, z[n] - k)
    Cminus[n] <- min(0, z[n] + k)
  } else {
    Cplus[n]  <- max(0, Cplus[n - 1] + z[n] - k)
    Cminus[n] <- min(0, Cminus[n - 1] + z[n] + k)
  }
}

cusum_signal <- which(Cplus > h | Cminus < -h)
cusum_signal_time <- ifelse(length(cusum_signal) == 0, NA, min(cusum_signal))

# -----------------------------
# X-bar / individual Shewhart chart
# Here n = 1, so this is an Individuals chart for standardized Z_n.
# -----------------------------
ucl_xbar <- 3
lcl_xbar <- -3

xbar_signal <- which(z > ucl_xbar | z < lcl_xbar)
xbar_signal_time <- ifelse(length(xbar_signal) == 0, NA, min(xbar_signal))

# -----------------------------
# Data frames
# -----------------------------
cusum_df <- data.frame(
  n = 1:N,
  Cplus = Cplus,
  Cminus = Cminus
) |>
  pivot_longer(
    cols = c(Cplus, Cminus),
    names_to = "Statistic",
    values_to = "Value"
  )

xbar_df <- data.frame(
  n = 1:N,
  Z = z
)

# -----------------------------
# Plot 1: Mean-CUSUM
# -----------------------------
p1 <- ggplot(cusum_df, aes(x = n, y = Value, linetype = Statistic)) +
  geom_line(linewidth = 1.0) +
  geom_hline(yintercept = h, linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = -h, linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = tau, linetype = "dotted", linewidth = 0.9) +
  annotate(
    "text",
    x = tau + 3,
    y = h + 1,
    label = "variance increases",
    hjust = 0,
    size = 4.6
  ) +
  annotate(
    "text",
    x = 8,
    y = h + 1,
    label = paste0("Mean-CUSUM: k = ", k, ", h = ", h),
    hjust = 0,
    size = 4.6
  ) +
  labs(
    title = "Mean-CUSUM",
    x = NULL,
    y = "CUSUM statistic",
    linetype = NULL
  ) +
  theme_bw(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# -----------------------------
# Plot 2: X-bar / Individuals Chart
# -----------------------------
p2 <- ggplot(xbar_df, aes(x = n, y = Z)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.7) +
  geom_hline(yintercept = ucl_xbar, linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = lcl_xbar, linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  geom_vline(xintercept = tau, linetype = "dotted", linewidth = 0.9) +
  annotate(
    "text",
    x = tau + 3,
    y = ucl_xbar + 0.8,
    label = "variance increases",
    hjust = 0,
    size = 4.6
  ) +
  annotate(
    "text",
    x = 8,
    y = ucl_xbar + 0.8,
    label = "X-bar / Individuals chart: UCL = 3, LCL = -3",
    hjust = 0,
    size = 4.6
  ) +
  labs(
    title = "X-bar / Individuals Shewhart Chart",
    x = "Sample number",
    y = expression(Z[n])
  ) +
  theme_bw(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold")
  )

# -----------------------------
# Combine plots
# -----------------------------
p_final <- p1 / p2 +
  plot_annotation(
    title = "Comparison under Variance Shift: Mean-CUSUM vs X-bar Chart",
    subtitle = expression(mu~"unchanged, but"~sigma~"increases after sample 60")
  )

ggsave(
  filename = "figs/ch4_example48_cusum_vs_xbar_variance_shift.png",
  plot = p_final,
  width = 9.5,
  height = 8,
  dpi = 300
)

cat("Variance shift point tau =", tau, "\n")
cat("CUSUM first signal time =", cusum_signal_time, "\n")
cat("X-bar / Individuals first signal time =", xbar_signal_time, "\n")
cat("Figure saved to figs/ch4_example48_cusum_vs_xbar_variance_shift.png\n")