# ============================================================
# Example 4.9-like simulation
# Dedicated Variance CUSUM
# Plot refined according to current R settings
# C+ line: black
# C- line: blue
# All OOC points: red
# Only first OOC n is labelled
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
# 1. settings
# -----------------------------
N <- 120
tau_up <- 41
tau_down <- 81

mu0 <- 0
sigma0 <- 1

sigma_up <- 1.25
sigma_down <- 0.75

k_up <- 1.848
k_down <- 0.462

# h values calibrated for ARL0 approximately 370
h_up <- 7.416
h_down <- -2.446
ARL0_target <- 370

# -----------------------------
# 2. generate data
# -----------------------------
sigma_vec <- c(
  rep(sigma0, tau_up - 1),
  rep(sigma_up, tau_down - tau_up),
  rep(sigma_down, N - tau_down + 1)
)

x <- rnorm(N, mean = mu0, sd = sigma_vec)
z <- (x - mu0) / sigma0

# -----------------------------
# 3. variance CUSUM statistics
# -----------------------------
Cplus_var <- numeric(N)
Cminus_var <- numeric(N)

for (i in 1:N) {
  
  prev_plus <- ifelse(i == 1, 0, Cplus_var[i - 1])
  prev_minus <- ifelse(i == 1, 0, Cminus_var[i - 1])
  
  Cplus_var[i] <- max(
    0,
    prev_plus + z[i]^2 - k_up
  )
  
  Cminus_var[i] <- min(
    0,
    prev_minus + z[i]^2 - k_down
  )
}

signal_up <- Cplus_var > h_up
signal_down <- Cminus_var < h_down

signal_time_up <- ifelse(any(signal_up), which(signal_up)[1], NA)
signal_time_down <- ifelse(any(signal_down), which(signal_down)[1], NA)

# -----------------------------
# 4. output table
# -----------------------------
result_table <- data.frame(
  n = 1:N,
  phase = factor(
    c(
      rep("IC", tau_up - 1),
      rep("Variance Up", tau_down - tau_up),
      rep("Variance Down", N - tau_down + 1)
    ),
    levels = c("IC", "Variance Up", "Variance Down")
  ),
  sigma = sigma_vec,
  x = round(x, 4),
  z = round(z, 4),
  z_square = round(z^2, 4),
  z_square_minus_k_up = round(z^2 - k_up, 4),
  z_square_minus_k_down = round(z^2 - k_down, 4),
  Cplus_var = round(Cplus_var, 4),
  Cminus_var = round(Cminus_var, 4),
  signal_up = signal_up,
  signal_down = signal_down
)

write.csv(
  result_table,
  "outputs/ch4_example49_dedicated_variance_cusum_data.csv",
  row.names = FALSE
)

# -----------------------------
# 5. summary
# -----------------------------
sink("outputs/ch4_example49_dedicated_variance_cusum_summary.txt")

cat("Dedicated Variance CUSUM simulation\n\n")
cat("N =", N, "\n")
cat("mu0 =", mu0, "\n")
cat("sigma0 =", sigma0, "\n")
cat("sigma_up =", sigma_up, "\n")
cat("sigma_down =", sigma_down, "\n")
cat("tau_up =", tau_up, "\n")
cat("tau_down =", tau_down, "\n")
cat("ARL0 target ≈", ARL0_target, "\n\n")

cat("Upward variance CUSUM\n")
cat("k_up =", k_up, "\n")
cat("h_up =", h_up, "\n")
cat("signal_time_up =", signal_time_up, "\n")

if (!is.na(signal_time_up)) {
  cat("upward detection delay =", signal_time_up - tau_up, "\n")
}

cat("\nDownward variance CUSUM\n")
cat("k_down =", k_down, "\n")
cat("h_down =", h_down, "\n")
cat("signal_time_down =", signal_time_down, "\n")

if (!is.na(signal_time_down)) {
  cat("downward detection delay =", signal_time_down - tau_down, "\n")
}

sink()

# -----------------------------
# 6. plotting data
# -----------------------------
plot_df <- result_table

up_signal_df <- subset(plot_df, signal_up)
down_signal_df <- subset(plot_df, signal_down)

up_signal_df$up_label <- ""
down_signal_df$down_label <- ""

if (!is.na(signal_time_up)) {
  up_signal_df$up_label[
    up_signal_df$n == signal_time_up
  ] <- as.character(signal_time_up)
}

if (!is.na(signal_time_down)) {
  down_signal_df$down_label[
    down_signal_df$n == signal_time_down
  ] <- as.character(signal_time_down)
}

# -----------------------------
# 7. raw data plot
# -----------------------------
p_x <- ggplot(plot_df, aes(x = n, y = x)) +
  
  geom_line(linewidth = 0.6, color = "gray35") +
  
  geom_point(
    aes(shape = phase),
    size = 2.2
  ) +
  
  geom_vline(
    xintercept = tau_up,
    linetype = "dotted",
    linewidth = 0.8,
    color = "gray35"
  ) +
  
  geom_vline(
    xintercept = tau_down,
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
    x = tau_up + 9,
    y = max(plot_df$x) * 0.92,
    label = paste0("sigma==", sigma_up),
    parse = TRUE,
    size = 4.2
  ) +
  
  annotate(
    "text",
    x = tau_down + 9,
    y = max(plot_df$x) * 0.92,
    label = paste0("sigma==", sigma_down),
    parse = TRUE,
    size = 4.2
  ) +
  
  scale_shape_manual(
    values = c(
      "IC" = 16,
      "Variance Up" = 17,
      "Variance Down" = 15
    )
  ) +
  
  labs(
    title = "Simulated Observations",
    subtitle = expression(mu == 0~", variance changes only"),
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
# 8. dedicated variance CUSUM plot
# -----------------------------
cusum_long <- rbind(
  data.frame(
    n = plot_df$n,
    phase = plot_df$phase,
    statistic = "Cplus",
    value = plot_df$Cplus_var
  ),
  data.frame(
    n = plot_df$n,
    phase = plot_df$phase,
    statistic = "Cminus",
    value = plot_df$Cminus_var
  )
)

cusum_long$statistic <- factor(
  cusum_long$statistic,
  levels = c("Cplus", "Cminus")
)

ymin_cusum <- min(c(plot_df$Cminus_var, h_down)) * 1.20
ymax_cusum <- max(c(plot_df$Cplus_var, h_up)) * 1.18

p_cusum <- ggplot(
  cusum_long,
  aes(x = n, y = value, color = statistic)
) +
  
  geom_line(linewidth = 0.95) +
  
  geom_vline(
    xintercept = tau_up,
    linetype = "dotted",
    linewidth = 0.8,
    color = "gray35"
  ) +
  
  geom_vline(
    xintercept = tau_down,
    linetype = "dotted",
    linewidth = 0.8,
    color = "gray35"
  ) +
  
  geom_hline(
    yintercept = h_up,
    linetype = "dashed",
    linewidth = 0.8,
    color = "gray25"
  ) +
  
  geom_hline(
    yintercept = h_down,
    linetype = "dashed",
    linewidth = 0.8,
    color = "gray25"
  ) +
  
  geom_hline(
    yintercept = 0,
    linewidth = 0.5,
    color = "gray25"
  ) +
  
  geom_point(
    aes(shape = phase),
    size = 2.0
  ) +
  
  # all OOC points are red
  geom_point(
    data = up_signal_df,
    aes(x = n, y = Cplus_var),
    inherit.aes = FALSE,
    shape = 21,
    fill = "red",
    color = "black",
    stroke = 1,
    size = 4.3
  ) +
  
  geom_text(
    data = up_signal_df,
    aes(x = n, y = Cplus_var, label = up_label),
    inherit.aes = FALSE,
    color = "red4",
    fontface = "bold",
    vjust = -0.8,
    size = 4.0
  ) +
  
  geom_point(
    data = down_signal_df,
    aes(x = n, y = Cminus_var),
    inherit.aes = FALSE,
    shape = 21,
    fill = "red",
    color = "black",
    stroke = 1,
    size = 4.3
  ) +
  
  geom_text(
    data = down_signal_df,
    aes(x = n, y = Cminus_var, label = down_label),
    inherit.aes = FALSE,
    color = "red4",
    fontface = "bold",
    vjust = 1.4,
    size = 4.0
  ) +
  
  annotate(
    "text",
    x = tau_up + 9,
    y = h_up + 1.05,
    label = paste0("h[U]==", h_up),
    parse = TRUE,
    size = 4.1
  ) +
  
  annotate(
    "text",
    x = tau_down + 9,
    y = h_down - 0.65,
    label = paste0("h[L]==", h_down),
    parse = TRUE,
    size = 4.1
  ) +
  
  annotate(
    "text",
    x = 20,
    y = ymax_cusum * 0.88,
    label = paste0("ARL[0]%~~%", ARL0_target),
    parse = TRUE,
    size = 4.3
  ) +
  
  annotate(
    "text",
    x = tau_up + 8,
    y = ymax_cusum * 0.22,
    label = paste0("sigma%up%", sigma_up),
    parse = TRUE,
    size = 4.2
  ) +
  
  annotate(
    "text",
    x = tau_down + 8,
    y = ymin_cusum * 0.35,
    label = paste0("sigma%down%", sigma_down),
    parse = TRUE,
    size = 4.2
  ) +
  
  scale_color_manual(
    values = c(
      "Cplus" = "black",
      "Cminus" = "blue"
    ),
    labels = c(
      "Cplus" = expression(C[n]^"+"),
      "Cminus" = expression(C[n]^"-")
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "IC" = 16,
      "Variance Up" = 17,
      "Variance Down" = 15
    )
  ) +
  
  coord_cartesian(
    ylim = c(ymin_cusum, ymax_cusum),
    clip = "off"
  ) +
  
  labs(
    title = "Dedicated Variance CUSUM",
    subtitle = expression(
      C[n]^"+" == max(0,C[n-1]^"+"+Z[n]^2-k^"+")
      ~","~
        C[n]^"-" == min(0,C[n-1]^"-"+Z[n]^2-k^"-")
    ),
    x = "Time point",
    y = "Variance CUSUM statistic",
    shape = NULL,
    color = NULL
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
p_compare <- p_x / p_cusum +
  plot_layout(heights = c(1, 1.15))

print(p_compare)

# -----------------------------
# 10. save figures
# -----------------------------
ggsave(
  "figs/ch4_example49_dedicated_variance_cusum.png",
  p_compare,
  width = 8.6,
  height = 7.2,
  dpi = 300
)

ggsave(
  "figs/ch4_example49_dedicated_variance_cusum_only.png",
  p_cusum,
  width = 8.2,
  height = 4.9,
  dpi = 300
)

ggsave(
  "figs/ch4_example49_variance_shift_data.png",
  p_x,
  width = 8.2,
  height = 4.6,
  dpi = 300
)

cat(
  "\nFiles saved:\n",
  "figs/ch4_example49_dedicated_variance_cusum.png\n",
  "figs/ch4_example49_dedicated_variance_cusum_only.png\n",
  "figs/ch4_example49_variance_shift_data.png\n",
  "outputs/ch4_example49_dedicated_variance_cusum_data.csv\n",
  "outputs/ch4_example49_dedicated_variance_cusum_summary.txt\n"
)