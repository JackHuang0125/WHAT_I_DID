# ============================================================
# Joint Monitoring Simulation
# CUSUM-Mean and CUSUM-V
# Save figs/ and outputs/
# ============================================================

rm(list = ls())

setwd("E:/統計品質管制/Week12")

set.seed(509)

dir.create("figs", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

packages <- c("ggplot2", "patchwork")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# -----------------------------
# 1. Simulation settings
# -----------------------------
N <- 120
tau1 <- 41
tau2 <- 81

mu0 <- 0
sigma0 <- 1

# Phase 1: IC
# Phase 2: mean shift
# Phase 3: variance shift
mu_shift <- 0.75
sigma_shift <- 1.75

mu_vec <- c(
  rep(mu0, tau1 - 1),
  rep(mu_shift, tau2 - tau1),
  rep(mu0, N - tau2 + 1)
)

sigma_vec <- c(
  rep(sigma0, tau1 - 1),
  rep(sigma0, tau2 - tau1),
  rep(sigma_shift, N - tau2 + 1)
)

x <- rnorm(N, mean = mu_vec, sd = sigma_vec)

z <- (x - mu0) / sigma0

# -----------------------------
# 2. CUSUM-Mean settings
# -----------------------------
k_M <- 0.5
h_M <- 4.095   # approximately ARL0 = 370 for k = 0.5

C_M_plus <- numeric(N)
C_M_minus <- numeric(N)

for (i in 1:N) {
  
  prev_plus <- ifelse(i == 1, 0, C_M_plus[i - 1])
  prev_minus <- ifelse(i == 1, 0, C_M_minus[i - 1])
  
  C_M_plus[i] <- max(
    0,
    prev_plus + z[i] - k_M
  )
  
  C_M_minus[i] <- min(
    0,
    prev_minus + z[i] + k_M
  )
}

signal_M_plus <- C_M_plus > h_M
signal_M_minus <- C_M_minus < -h_M

signal_time_M_plus <- ifelse(
  any(signal_M_plus),
  which(signal_M_plus)[1],
  NA
)

signal_time_M_minus <- ifelse(
  any(signal_M_minus),
  which(signal_M_minus)[1],
  NA
)

# -----------------------------
# 3. CUSUM-V settings
# -----------------------------
k_V_up <- 1.848
k_V_down <- 0.462

h_V_up <- 7.416
h_V_down <- -2.446

C_V_plus <- numeric(N)
C_V_minus <- numeric(N)

for (i in 1:N) {
  
  prev_plus <- ifelse(i == 1, 0, C_V_plus[i - 1])
  prev_minus <- ifelse(i == 1, 0, C_V_minus[i - 1])
  
  C_V_plus[i] <- max(
    0,
    prev_plus + z[i]^2 - k_V_up
  )
  
  C_V_minus[i] <- min(
    0,
    prev_minus + z[i]^2 - k_V_down
  )
}

signal_V_plus <- C_V_plus > h_V_up
signal_V_minus <- C_V_minus < h_V_down

signal_time_V_plus <- ifelse(
  any(signal_V_plus),
  which(signal_V_plus)[1],
  NA
)

signal_time_V_minus <- ifelse(
  any(signal_V_minus),
  which(signal_V_minus)[1],
  NA
)

# -----------------------------
# 4. Output table
# -----------------------------
result_table <- data.frame(
  n = 1:N,
  phase = factor(
    c(
      rep("IC", tau1 - 1),
      rep("Mean Shift", tau2 - tau1),
      rep("Variance Shift", N - tau2 + 1)
    ),
    levels = c("IC", "Mean Shift", "Variance Shift")
  ),
  mu = mu_vec,
  sigma = sigma_vec,
  x = round(x, 4),
  z = round(z, 4),
  z_square = round(z^2, 4),
  C_M_plus = round(C_M_plus, 4),
  C_M_minus = round(C_M_minus, 4),
  C_V_plus = round(C_V_plus, 4),
  C_V_minus = round(C_V_minus, 4),
  signal_M_plus = signal_M_plus,
  signal_M_minus = signal_M_minus,
  signal_V_plus = signal_V_plus,
  signal_V_minus = signal_V_minus
)

write.csv(
  result_table,
  "outputs/ch4_joint_monitoring_cusum_M_V_data.csv",
  row.names = FALSE
)

sink("outputs/ch4_joint_monitoring_cusum_M_V_summary.txt")

cat("Joint monitoring by CUSUM-Mean and CUSUM-V\n\n")
cat("N =", N, "\n")
cat("tau1 =", tau1, "\n")
cat("tau2 =", tau2, "\n")
cat("mu0 =", mu0, "\n")
cat("sigma0 =", sigma0, "\n")
cat("mean shift =", mu_shift, "\n")
cat("variance shift sigma =", sigma_shift, "\n\n")

cat("CUSUM-Mean\n")
cat("k_M =", k_M, "\n")
cat("h_M =", h_M, "\n")
cat("signal_time_M_plus =", signal_time_M_plus, "\n")
cat("signal_time_M_minus =", signal_time_M_minus, "\n\n")

cat("CUSUM-V\n")
cat("k_V_up =", k_V_up, "\n")
cat("h_V_up =", h_V_up, "\n")
cat("signal_time_V_plus =", signal_time_V_plus, "\n")
cat("k_V_down =", k_V_down, "\n")
cat("h_V_down =", h_V_down, "\n")
cat("signal_time_V_minus =", signal_time_V_minus, "\n")

sink()

# -----------------------------
# 5. Plotting data
# -----------------------------
plot_df <- result_table

M_plus_df <- subset(plot_df, signal_M_plus)
M_minus_df <- subset(plot_df, signal_M_minus)
V_plus_df <- subset(plot_df, signal_V_plus)
V_minus_df <- subset(plot_df, signal_V_minus)

M_plus_df$label <- ""
M_minus_df$label <- ""
V_plus_df$label <- ""
V_minus_df$label <- ""

if (!is.na(signal_time_M_plus)) {
  M_plus_df$label[M_plus_df$n == signal_time_M_plus] <-
    as.character(signal_time_M_plus)
}

if (!is.na(signal_time_M_minus)) {
  M_minus_df$label[M_minus_df$n == signal_time_M_minus] <-
    as.character(signal_time_M_minus)
}

if (!is.na(signal_time_V_plus)) {
  V_plus_df$label[V_plus_df$n == signal_time_V_plus] <-
    as.character(signal_time_V_plus)
}

if (!is.na(signal_time_V_minus)) {
  V_minus_df$label[V_minus_df$n == signal_time_V_minus] <-
    as.character(signal_time_V_minus)
}

# -----------------------------
# 6. Raw data plot
# -----------------------------
p_x <- ggplot(plot_df, aes(x = n, y = x)) +
  
  geom_line(color = "gray35", linewidth = 0.6) +
  
  geom_point(
    aes(shape = phase),
    size = 2.1
  ) +
  
  geom_vline(
    xintercept = tau1,
    linetype = "dotted",
    linewidth = 0.8,
    color = "gray35"
  ) +
  
  geom_vline(
    xintercept = tau2,
    linetype = "dotted",
    linewidth = 0.8,
    color = "gray35"
  ) +
  
  geom_hline(
    yintercept = 0,
    linewidth = 0.5,
    color = "gray25"
  ) +
  
  annotate(
    "text",
    x = tau1 + 10,
    y = max(plot_df$x) * 0.92,
    label = paste0("mu==", mu_shift),
    parse = TRUE,
    size = 4.0
  ) +
  
  annotate(
    "text",
    x = tau2 + 10,
    y = max(plot_df$x) * 0.92,
    label = paste0("sigma==", sigma_shift),
    parse = TRUE,
    size = 4.0
  ) +
  
  scale_shape_manual(
    values = c(
      "IC" = 16,
      "Mean Shift" = 17,
      "Variance Shift" = 15
    )
  ) +
  
  labs(
    title = "Simulated Observations",
    subtitle = expression(
      N == 120~","~
        tau[1] == 41~","~
        tau[2] == 81
    ),
    x = NULL,
    y = expression(X[n]),
    shape = NULL
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# -----------------------------
# 7. CUSUM-Mean plot
# -----------------------------
M_long <- rbind(
  data.frame(
    n = plot_df$n,
    phase = plot_df$phase,
    statistic = "CMplus",
    value = plot_df$C_M_plus
  ),
  data.frame(
    n = plot_df$n,
    phase = plot_df$phase,
    statistic = "CMminus",
    value = plot_df$C_M_minus
  )
)

M_long$statistic <- factor(
  M_long$statistic,
  levels = c("CMplus", "CMminus")
)

ymin_M <- min(c(plot_df$C_M_minus, -h_M)) * 1.20
ymax_M <- max(c(plot_df$C_M_plus, h_M)) * 1.18

p_M <- ggplot(M_long, aes(x = n, y = value, color = statistic)) +
  
  geom_line(linewidth = 0.9) +
  
  geom_vline(xintercept = tau1, linetype = "dotted", linewidth = 0.8, color = "gray35") +
  geom_vline(xintercept = tau2, linetype = "dotted", linewidth = 0.8, color = "gray35") +
  
  geom_hline(yintercept = h_M, linetype = "dashed", linewidth = 0.8, color = "gray25") +
  geom_hline(yintercept = -h_M, linetype = "dashed", linewidth = 0.8, color = "gray25") +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "gray25") +
  
  geom_point(aes(shape = phase), size = 1.9) +
  
  geom_point(
    data = M_plus_df,
    aes(x = n, y = C_M_plus),
    inherit.aes = FALSE,
    shape = 21,
    fill = "red",
    color = "black",
    stroke = 0.9,
    size = 4.0
  ) +
  
  geom_text(
    data = M_plus_df,
    aes(x = n, y = C_M_plus, label = label),
    inherit.aes = FALSE,
    color = "red4",
    fontface = "bold",
    vjust = -0.8,
    size = 3.8
  ) +
  
  geom_point(
    data = M_minus_df,
    aes(x = n, y = C_M_minus),
    inherit.aes = FALSE,
    shape = 21,
    fill = "red",
    color = "black",
    stroke = 0.9,
    size = 4.0
  ) +
  
  geom_text(
    data = M_minus_df,
    aes(x = n, y = C_M_minus, label = label),
    inherit.aes = FALSE,
    color = "red4",
    fontface = "bold",
    vjust = 1.4,
    size = 3.8
  ) +
  
  annotate(
    "text",
    x = 18,
    y = ymax_M * 0.88,
    label = "CUSUM-M",
    fontface = "bold",
    size = 4.3
  ) +
  
  scale_color_manual(
    values = c(
      "CMplus" = "black",
      "CMminus" = "blue"
    ),
    labels = c(
      "CMplus" = expression(C[M,n]^"+"),
      "CMminus" = expression(C[M,n]^"-")
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "IC" = 16,
      "Mean Shift" = 17,
      "Variance Shift" = 15
    )
  ) +
  
  coord_cartesian(ylim = c(ymin_M, ymax_M), clip = "off") +
  
  labs(
    x = NULL,
    y = "CUSUM-Mean",
    color = NULL,
    shape = NULL
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    legend.position = "bottom"
  )

# -----------------------------
# 8. CUSUM-V plot
# -----------------------------
V_long <- rbind(
  data.frame(
    n = plot_df$n,
    phase = plot_df$phase,
    statistic = "CVplus",
    value = plot_df$C_V_plus
  ),
  data.frame(
    n = plot_df$n,
    phase = plot_df$phase,
    statistic = "CVminus",
    value = plot_df$C_V_minus
  )
)

V_long$statistic <- factor(
  V_long$statistic,
  levels = c("CVplus", "CVminus")
)

ymin_V <- min(c(plot_df$C_V_minus, h_V_down)) * 1.20
ymax_V <- max(c(plot_df$C_V_plus, h_V_up)) * 1.18

p_V <- ggplot(V_long, aes(x = n, y = value, color = statistic)) +
  
  geom_line(linewidth = 0.9) +
  
  geom_vline(xintercept = tau1, linetype = "dotted", linewidth = 0.8, color = "gray35") +
  geom_vline(xintercept = tau2, linetype = "dotted", linewidth = 0.8, color = "gray35") +
  
  geom_hline(yintercept = h_V_up, linetype = "dashed", linewidth = 0.8, color = "gray25") +
  geom_hline(yintercept = h_V_down, linetype = "dashed", linewidth = 0.8, color = "gray25") +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "gray25") +
  
  geom_point(aes(shape = phase), size = 1.9) +
  
  geom_point(
    data = V_plus_df,
    aes(x = n, y = C_V_plus),
    inherit.aes = FALSE,
    shape = 21,
    fill = "red",
    color = "black",
    stroke = 0.9,
    size = 4.0
  ) +
  
  geom_text(
    data = V_plus_df,
    aes(x = n, y = C_V_plus, label = label),
    inherit.aes = FALSE,
    color = "red4",
    fontface = "bold",
    vjust = -0.8,
    size = 3.8
  ) +
  
  geom_point(
    data = V_minus_df,
    aes(x = n, y = C_V_minus),
    inherit.aes = FALSE,
    shape = 21,
    fill = "red",
    color = "black",
    stroke = 0.9,
    size = 4.0
  ) +
  
  geom_text(
    data = V_minus_df,
    aes(x = n, y = C_V_minus, label = label),
    inherit.aes = FALSE,
    color = "red4",
    fontface = "bold",
    vjust = 1.4,
    size = 3.8
  ) +
  
  annotate(
    "text",
    x = 18,
    y = ymax_V * 0.88,
    label = "CUSUM-V",
    fontface = "bold",
    size = 4.3
  ) +
  
  scale_color_manual(
    values = c(
      "CVplus" = "black",
      "CVminus" = "blue"
    ),
    labels = c(
      "CVplus" = expression(C[V,n]^"+"),
      "CVminus" = expression(C[V,n]^"-")
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "IC" = 16,
      "Mean Shift" = 17,
      "Variance Shift" = 15
    )
  ) +
  
  coord_cartesian(ylim = c(ymin_V, ymax_V), clip = "off") +
  
  labs(
    x = "Time point",
    y = "CUSUM-V",
    color = NULL,
    shape = NULL
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    legend.position = "bottom"
  )

# -----------------------------
# 9. Combined figure
# -----------------------------
p_joint <- p_x / p_M / p_V +
  plot_layout(heights = c(0.95, 1, 1))

print(p_joint)

# -----------------------------
# 10. Save figures
# -----------------------------
ggsave(
  "figs/ch4_joint_monitoring_cusum_M_V.png",
  p_joint,
  width = 8.8,
  height = 8.4,
  dpi = 300
)

ggsave(
  "figs/ch4_joint_monitoring_cusum_M_only.png",
  p_M,
  width = 8.4,
  height = 4.6,
  dpi = 300
)

ggsave(
  "figs/ch4_joint_monitoring_cusum_V_only.png",
  p_V,
  width = 8.4,
  height = 4.6,
  dpi = 300
)

ggsave(
  "figs/ch4_joint_monitoring_data.png",
  p_x,
  width = 8.4,
  height = 4.4,
  dpi = 300
)

cat(
  "\nFiles saved:\n",
  "figs/ch4_joint_monitoring_cusum_M_V.png\n",
  "figs/ch4_joint_monitoring_cusum_M_only.png\n",
  "figs/ch4_joint_monitoring_cusum_V_only.png\n",
  "figs/ch4_joint_monitoring_data.png\n",
  "outputs/ch4_joint_monitoring_cusum_M_V_data.csv\n",
  "outputs/ch4_joint_monitoring_cusum_M_V_summary.txt\n"
)