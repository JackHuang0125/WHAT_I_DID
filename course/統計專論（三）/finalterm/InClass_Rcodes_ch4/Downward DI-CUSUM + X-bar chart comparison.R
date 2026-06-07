# ============================================================
# Example 4.2-like simulation
# Downward DI-CUSUM + X-bar chart comparison
# Save combined and separated charts into figs/
# Save outputs into outputs/
# ============================================================

rm(list = ls())

setwd("E:/統計品質管制/Week12")

set.seed(42)

# -----------------------------
# folders
# -----------------------------
dir.create("figs", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# packages
# -----------------------------
packages <- c("ggplot2", "patchwork")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

# -----------------------------
# 1. settings
# -----------------------------
m <- 5

n_ic <- 10
n_oc <- 10
n_total <- n_ic + n_oc

mu0 <- 0
sigma <- 1

delta <- 0.5
tau <- 11

k <- delta / 2
h <- -5.597

# Downward shift
mu1 <- mu0 - delta

sigma_xbar <- sigma / sqrt(m)

xbar_ucl <- mu0 + 3 * sigma_xbar
xbar_cl  <- mu0
xbar_lcl <- mu0 - 3 * sigma_xbar

# -----------------------------
# 2. generate data
# -----------------------------
x_ic <- matrix(
  rnorm(n_ic * m, mean = mu0, sd = sigma),
  nrow = n_ic
)

x_oc <- matrix(
  rnorm(n_oc * m, mean = mu1, sd = sigma),
  nrow = n_oc
)

x_batch <- rbind(x_ic, x_oc)

xbar <- rowMeans(x_batch)

z <- (xbar - mu0) / sigma_xbar

# -----------------------------
# 3. downward DI-CUSUM
# C_n^- = min{0, C_{n-1}^- + Z_n + k}
# -----------------------------
Cminus <- numeric(n_total)

for (i in 1:n_total) {
  
  prev <- ifelse(i == 1, 0, Cminus[i - 1])
  
  Cminus[i] <- min(
    0,
    prev + z[i] + k
  )
}

signal_index <- which(Cminus < h)

signal_time <- ifelse(
  length(signal_index) == 0,
  NA,
  signal_index[1]
)

xbar_signal <- (xbar > xbar_ucl) | (xbar < xbar_lcl)

# -----------------------------
# 4. output table
# -----------------------------
result_table <- data.frame(
  n = 1:n_total,
  phase = c(rep("IC", n_ic), rep("Shift", n_oc)),
  xbar = round(xbar, 4),
  Z_n = round(z, 4),
  Z_n_plus_k = round(z + k, 4),
  Cminus = round(Cminus, 4),
  CUSUM_signal = Cminus < h,
  Xbar_signal = xbar_signal
)

print(result_table)

write.csv(
  result_table,
  "outputs/example42_downward.csv",
  row.names = FALSE
)

# -----------------------------
# 5. summary
# -----------------------------
sink("outputs/example42_downward_summary.txt")

cat("Example 4.2-like simulation: Downward DI-CUSUM vs X-bar chart\n\n")
cat("m =", m, "\n")
cat("n_ic =", n_ic, "\n")
cat("n_oc =", n_oc, "\n")
cat("tau =", tau, "\n")
cat("mu0 =", mu0, "\n")
cat("mu1 = mu0 - delta =", mu1, "\n")
cat("sigma =", sigma, "\n")
cat("sigma_xbar =", round(sigma_xbar, 4), "\n")
cat("delta =", delta, "\n")
cat("k = delta / 2 =", k, "\n")
cat("h =", h, "\n\n")

cat("Downward DI-CUSUM signal time =", signal_time, "\n")

if (!is.na(signal_time)) {
  cat("Detection delay =", signal_time - tau, "\n")
}

cat("\nX-bar chart limits\n")
cat("LCL =", round(xbar_lcl, 4), "\n")
cat("CL  =", round(xbar_cl, 4), "\n")
cat("UCL =", round(xbar_ucl, 4), "\n")
cat(
  "X-bar signal time =",
  ifelse(any(xbar_signal), which(xbar_signal)[1], NA),
  "\n"
)

sink()

# -----------------------------
# 6. plot data
# -----------------------------
plot_df <- result_table

plot_df$CUSUM_label <- ifelse(
  plot_df$CUSUM_signal,
  as.character(plot_df$n),
  ""
)

plot_df$Xbar_label <- ifelse(
  plot_df$Xbar_signal,
  as.character(plot_df$n),
  ""
)

cusum_exceed_df <- subset(plot_df, CUSUM_signal)
xbar_exceed_df  <- subset(plot_df, Xbar_signal)

# -----------------------------
# 7. downward CUSUM plot
# -----------------------------
ymin_cusum <- min(c(plot_df$Cminus, h)) * 1.15
ymax_cusum <- max(c(plot_df$Cminus, 0)) + 0.35

p_cusum <- ggplot(plot_df, aes(x = n, y = Cminus)) +
  
  geom_line(linewidth = 0.8) +
  
  geom_point(
    aes(shape = phase),
    size = 2.7
  ) +
  
  geom_point(
    data = cusum_exceed_df,
    aes(x = n, y = Cminus),
    color = "red",
    size = 3.8
  ) +
  
  geom_text(
    data = cusum_exceed_df,
    aes(x = n, y = Cminus, label = CUSUM_label),
    color = "red",
    vjust = 1.5,
    size = 4.2,
    fontface = "bold"
  ) +
  
  geom_hline(
    yintercept = h,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    linewidth = 0.5
  ) +
  
  geom_vline(
    xintercept = tau,
    linetype = "dotted",
    linewidth = 0.8
  ) +
  
  annotate(
    "text",
    x = tau + 1,
    y = -2,
    label = "tau==11",
    parse = TRUE,
    size = 4.5
  ) +
  
  annotate(
    "text",
    x = n_total - 2.3,
    y = h - 0.35,
    label = "h==-5.597",
    parse = TRUE,
    size = 4.5
  ) +
  
  scale_shape_manual(
    values = c("IC" = 16, "Shift" = 17),
    labels = c("IC" = "IC", "Shift" = "Shift")
  ) +
  
  scale_x_continuous(breaks = 1:n_total) +
  
  coord_cartesian(
    ylim = c(ymin_cusum, ymax_cusum),
    clip = "off"
  ) +
  
  labs(
    title = "Downward DI-CUSUM Chart",
    subtitle = expression(
      C[n]^"-" == min(0, C[n - 1]^"-" + Z[n] + k)
    ),
    x = "Time point",
    y = expression(C[n]^"-"),
    shape = NULL
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(10, 18, 10, 10)
  )

# -----------------------------
# 8. X-bar chart plot
# -----------------------------
ymin_xbar <- min(c(plot_df$xbar, xbar_lcl)) - 0.15
ymax_xbar <- max(c(plot_df$xbar, xbar_ucl)) + 0.15

p_xbar <- ggplot(plot_df, aes(x = n, y = xbar)) +
  
  geom_line(linewidth = 0.8) +
  
  geom_point(
    aes(shape = phase),
    size = 2.7
  ) +
  
  geom_point(
    data = xbar_exceed_df,
    aes(x = n, y = xbar),
    color = "red",
    size = 3.8
  ) +
  
  geom_text(
    data = xbar_exceed_df,
    aes(x = n, y = xbar, label = Xbar_label),
    color = "red",
    vjust = -0.8,
    size = 4.2,
    fontface = "bold"
  ) +
  
  geom_hline(
    yintercept = xbar_ucl,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  geom_hline(
    yintercept = xbar_cl,
    linetype = "solid",
    linewidth = 0.6
  ) +
  
  geom_hline(
    yintercept = xbar_lcl,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  geom_vline(
    xintercept = tau,
    linetype = "dotted",
    linewidth = 0.8
  ) +
  
  annotate(
    "text",
    x = n_total - 2,
    y = xbar_ucl + 0.08,
    label = "UCL",
    size = 4.2
  ) +
  
  annotate(
    "text",
    x = n_total - 2,
    y = xbar_lcl - 0.08,
    label = "LCL",
    size = 4.2
  ) +
  
  scale_shape_manual(
    values = c("IC" = 16, "Shift" = 17),
    labels = c("IC" = "IC", "Shift" = "Shift")
  ) +
  
  scale_x_continuous(breaks = 1:n_total) +
  
  coord_cartesian(
    ylim = c(ymin_xbar, ymax_xbar),
    clip = "off"
  ) +
  
  labs(
    title = expression(bar(X)~"Chart"),
    subtitle = expression(
      UCL == mu[0] + 3 * sigma / sqrt(m)
      ~","~
        LCL == mu[0] - 3 * sigma / sqrt(m)
    ),
    x = "Time point",
    y = expression(bar(X)[n]),
    shape = NULL
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(10, 18, 10, 10)
  )

# -----------------------------
# 9. combined plot
# -----------------------------
p_compare <- p_xbar / p_cusum +
  plot_layout(heights = c(1, 1.08))

print(p_compare)

# -----------------------------
# 10. save plots
# -----------------------------

# Combined comparison plot
ggsave(
  "figs/ch4_downward_xbar_vs_DIcusum.png",
  p_compare,
  width = 8.2,
  height = 7.2,
  dpi = 300
)

# Separated X-bar chart
ggsave(
  "figs/ch4_downward_xbar_chart.png",
  p_xbar,
  width = 8.0,
  height = 4.6,
  dpi = 300
)

# Separated downward DI-CUSUM chart
ggsave(
  "figs/ch4_downward_DIcusum_chart.png",
  p_cusum,
  width = 8.0,
  height = 4.6,
  dpi = 300
)

cat(
  "\nFiles saved:\n",
  "figs/ch4_downward_xbar_vs_DIcusum.png\n",
  "figs/ch4_downward_xbar_chart.png\n",
  "figs/ch4_downward_DIcusum_chart.png\n",
  "outputs/example42_downward.csv\n",
  "outputs/example42_downward_summary.txt\n"
)