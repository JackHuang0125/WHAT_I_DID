# ============================================================
# Chapter 4.4.2 Discrete CUSUM
# Binomial and Poisson examples
# pi1 = 0.125, lambda1 = 12.5
# Designed shift point tau = 11 marked in all PNGs
# ============================================================

rm(list = ls())

setwd("E:/統計品質管制/Week12")

set.seed(4412)

dir.create("figs", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

packages <- c("ggplot2", "patchwork")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# ============================================================
# Helper functions
# ============================================================

first_signal <- function(signal_vec) {
  if (any(signal_vec, na.rm = TRUE)) {
    which(signal_vec)[1]
  } else {
    NA_integer_
  }
}

safe_delay <- function(signal_time, tau) {
  if (is.na(signal_time)) NA_integer_ else signal_time - tau
}

add_shift_marker <- function(p, tau, ymax, label_y = 1.06) {
  p +
    geom_vline(
      xintercept = tau,
      linetype = "dotted",
      linewidth = 0.9,
      color = "gray25"
    ) +
    annotate(
      "text",
      x = tau + 0.6,
      y = ymax * label_y,
      label = paste0("designed shift: n = ", tau),
      hjust = 0,
      size = 3.5,
      fontface = "bold"
    )
}

# ============================================================
# 1. Binomial CUSUM
# pi0 = 0.10 -> pi1 = 0.125
# shift point tau = 11
# ============================================================

m_bin <- 100
pi0 <- 0.10
pi1 <- 0.125
tau_bin <- 11

k_bin <- -m_bin * log((1 - pi1) / (1 - pi0)) /
  log((pi1 / (1 - pi1)) / (pi0 / (1 - pi0)))

h_bin <- 12.0

x_bin <- c(
  rbinom(tau_bin - 1, size = m_bin, prob = pi0),
  rbinom(20, size = m_bin, prob = pi1)
)

n_bin <- length(x_bin)

C_bin <- numeric(n_bin)

for (i in seq_len(n_bin)) {
  prev <- if (i == 1) 0 else C_bin[i - 1]
  C_bin[i] <- max(0, prev + x_bin[i] - k_bin)
}

signal_bin <- C_bin > h_bin
first_signal_bin <- first_signal(signal_bin)

df_bin <- data.frame(
  n = seq_len(n_bin),
  X = x_bin,
  C = C_bin,
  phase = ifelse(seq_len(n_bin) < tau_bin, "IC", "Shift"),
  signal_cusum = signal_bin
)

write.csv(
  df_bin,
  "outputs/ch4_binomial_cusum_data.csv",
  row.names = FALSE
)

p_bin <- ggplot(df_bin, aes(x = n, y = C)) +
  geom_line(linewidth = 0.85, color = "black") +
  geom_point(aes(shape = phase), size = 2.4, color = "black") +
  geom_hline(
    yintercept = h_bin,
    linetype = "dashed",
    linewidth = 0.8,
    color = "gray30"
  ) +
  geom_point(
    data = subset(df_bin, signal_cusum),
    aes(x = n, y = C),
    shape = 21,
    fill = "red",
    color = "black",
    stroke = 0.9,
    size = 4.2
  ) +
  {
    if (!is.na(first_signal_bin)) {
      annotate(
        "text",
        x = first_signal_bin,
        y = df_bin$C[first_signal_bin] + 1.4,
        label = paste0("first signal: n = ", first_signal_bin),
        color = "red4",
        fontface = "bold",
        size = 4
      )
    }
  } +
  annotate(
    "text",
    x = 5.0,
    y = max(c(df_bin$C, h_bin)) * 0.88,
    label = paste0(
      "m==", m_bin,
      "*','~~pi[0]==", pi0,
      "*','~~pi[1]==", pi1
    ),
    parse = TRUE,
    size = 3.8
  ) +
  annotate(
    "text",
    x = 5.0,
    y = max(c(df_bin$C, h_bin)) * 0.76,
    label = paste0(
      "k^'+'==", round(k_bin, 3),
      "*','~~h^'+'==", h_bin
    ),
    parse = TRUE,
    size = 3.8
  ) +
  scale_shape_manual(values = c("IC" = 16, "Shift" = 17)) +
  scale_x_continuous(breaks = seq(0, n_bin, by = 5)) +
  labs(
    title = "Binomial CUSUM",
    subtitle = expression(pi[0] == 0.10 ~ " to " ~ pi[1] == 0.125),
    x = "Time point",
    y = expression(C[n]^"+"),
    shape = NULL
  ) +
  coord_cartesian(
    ylim = c(0, max(c(df_bin$C, h_bin)) * 1.18),
    clip = "off"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    plot.margin = margin(8, 16, 8, 8)
  )

p_bin <- add_shift_marker(
  p_bin,
  tau = tau_bin,
  ymax = max(c(df_bin$C, h_bin)),
  label_y = 1.08
)

ggsave(
  "figs/ch4_binomial_cusum_example.png",
  p_bin,
  width = 6.4,
  height = 4.8,
  dpi = 300
)

# ============================================================
# 2. Poisson CUSUM
# lambda0 = 10 -> lambda1 = 20
# shift point tau = 11
# ============================================================
set.seed(611211106)


lambda0 <- 10
lambda1 <- 20
tau_pois <- 11

k_pois <- (lambda1 - lambda0) /
  (log(lambda1) - log(lambda0))

h_pois <- 14.0

x_pois <- c(
  rpois(tau_pois - 1, lambda = lambda0),
  rpois(20, lambda = lambda1)
)

n_pois <- length(x_pois)

C_pois <- numeric(n_pois)

for (i in seq_len(n_pois)) {
  prev <- if (i == 1) 0 else C_pois[i - 1]
  C_pois[i] <- max(0, prev + x_pois[i] - k_pois)
}

signal_pois <- C_pois > h_pois
first_signal_pois <- first_signal(signal_pois)

df_pois <- data.frame(
  n = seq_len(n_pois),
  X = x_pois,
  C = C_pois,
  phase = ifelse(seq_len(n_pois) < tau_pois, "IC", "Shift"),
  signal_cusum = signal_pois
)

write.csv(
  df_pois,
  "outputs/ch4_poisson_cusum_data.csv",
  row.names = FALSE
)

p_pois <- ggplot(df_pois, aes(x = n, y = C)) +
  geom_line(linewidth = 0.85, color = "black") +
  geom_point(aes(shape = phase), size = 2.4, color = "black") +
  geom_hline(
    yintercept = h_pois,
    linetype = "dashed",
    linewidth = 0.8,
    color = "gray30"
  ) +
  geom_point(
    data = subset(df_pois, signal_cusum),
    aes(x = n, y = C),
    shape = 21,
    fill = "red",
    color = "black",
    stroke = 0.9,
    size = 4.2
  ) +
  {
    if (!is.na(first_signal_pois)) {
      annotate(
        "text",
        x = first_signal_pois,
        y = df_pois$C[first_signal_pois] + 2.0,
        label = paste0("first signal: n = ", first_signal_pois),
        color = "red4",
        fontface = "bold",
        size = 4
      )
    }
  } +
  annotate(
    "text",
    x = 6.8,
    y = max(c(df_pois$C, h_pois)) * 0.88,
    label = paste0(
      "lambda[0]==", lambda0,
      "*','~~lambda[1]==", lambda1
    ),
    parse = TRUE,
    size = 3.8
  ) +
  annotate(
    "text",
    x = 6.8,
    y = max(c(df_pois$C, h_pois)) * 0.76,
    label = paste0(
      "k^'+'==", round(k_pois, 3),
      "*','~~h^'+'==", h_pois
    ),
    parse = TRUE,
    size = 3.8
  ) +
  scale_shape_manual(values = c("IC" = 16, "Shift" = 17)) +
  scale_x_continuous(breaks = seq(0, n_pois, by = 5)) +
  labs(
    title = "Poisson CUSUM",
    subtitle = expression(lambda[0] == 10 ~ " to " ~ lambda[1] == 12.5),
    x = "Time point",
    y = expression(C[n]^"+"),
    shape = NULL
  ) +
  coord_cartesian(
    ylim = c(0, max(c(df_pois$C, h_pois)) * 1.18),
    clip = "off"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    plot.margin = margin(8, 16, 8, 8)
  )

p_pois <- add_shift_marker(
  p_pois,
  tau = tau_pois,
  ymax = max(c(df_pois$C, h_pois)),
  label_y = 1.08
)

ggsave(
  "figs/ch4_poisson_cusum_example.png",
  p_pois,
  width = 6.4,
  height = 4.8,
  dpi = 300
)

# ============================================================
# 3. Individual count charts
# ============================================================

# -----------------------------
# Binomial individual chart
# -----------------------------
df_bin$center <- m_bin * pi0
df_bin$ucl <- qbinom(0.9973, size = m_bin, prob = pi0)
df_bin$lcl <- qbinom(0.0027, size = m_bin, prob = pi0)

df_bin$signal_ind <- (df_bin$X > df_bin$ucl) |
  (df_bin$X < df_bin$lcl)

first_signal_bin_ind <- first_signal(df_bin$signal_ind)

p_bin_ind <- ggplot(df_bin, aes(x = n, y = X)) +
  geom_line(linewidth = 0.85, color = "black") +
  geom_point(aes(shape = phase), size = 2.4, color = "black") +
  geom_hline(
    yintercept = df_bin$center[1],
    linewidth = 0.75,
    color = "gray30"
  ) +
  geom_hline(
    yintercept = df_bin$ucl[1],
    linetype = "dashed",
    linewidth = 0.8,
    color = "gray30"
  ) +
  geom_hline(
    yintercept = df_bin$lcl[1],
    linetype = "dashed",
    linewidth = 0.8,
    color = "gray30"
  ) +
  geom_point(
    data = subset(df_bin, signal_ind),
    aes(x = n, y = X),
    shape = 21,
    fill = "red",
    color = "black",
    stroke = 0.9,
    size = 4.2
  ) +
  scale_shape_manual(values = c("IC" = 16, "Shift" = 17)) +
  scale_x_continuous(breaks = seq(0, n_bin, by = 5)) +
  labs(
    title = "Binomial Individual Chart",
    subtitle = expression(X[n] %~% Binomial(100, 0.10)),
    x = "Time point",
    y = expression(X[n]),
    shape = NULL
  ) +
  coord_cartesian(
    ylim = c(
      max(0, min(c(df_bin$X, df_bin$lcl)) - 2),
      max(c(df_bin$X, df_bin$ucl)) * 1.12
    ),
    clip = "off"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    plot.margin = margin(8, 16, 8, 8)
  )

p_bin_ind <- add_shift_marker(
  p_bin_ind,
  tau = tau_bin,
  ymax = max(c(df_bin$X, df_bin$ucl)),
  label_y = 1.06
)

ggsave(
  "figs/ch4_binomial_individual.png",
  p_bin_ind,
  width = 6.4,
  height = 4.8,
  dpi = 300
)

# -----------------------------
# Poisson individual chart
# -----------------------------
df_pois$center <- lambda0
df_pois$ucl <- qpois(0.9973, lambda = lambda0)
df_pois$lcl <- qpois(0.0027, lambda = lambda0)

df_pois$signal_ind <- (df_pois$X > df_pois$ucl) |
  (df_pois$X < df_pois$lcl)

first_signal_pois_ind <- first_signal(df_pois$signal_ind)

p_pois_ind <- ggplot(df_pois, aes(x = n, y = X)) +
  geom_line(linewidth = 0.85, color = "black") +
  geom_point(aes(shape = phase), size = 2.4, color = "black") +
  geom_hline(
    yintercept = df_pois$center[1],
    linewidth = 0.75,
    color = "gray30"
  ) +
  geom_hline(
    yintercept = df_pois$ucl[1],
    linetype = "dashed",
    linewidth = 0.8,
    color = "gray30"
  ) +
  geom_hline(
    yintercept = df_pois$lcl[1],
    linetype = "dashed",
    linewidth = 0.8,
    color = "gray30"
  ) +
  geom_point(
    data = subset(df_pois, signal_ind),
    aes(x = n, y = X),
    shape = 21,
    fill = "red",
    color = "black",
    stroke = 0.9,
    size = 4.2
  ) +
  scale_shape_manual(values = c("IC" = 16, "Shift" = 17)) +
  scale_x_continuous(breaks = seq(0, n_pois, by = 5)) +
  labs(
    title = "Poisson Individual Chart",
    subtitle = expression(X[n] %~% Poisson(10)),
    x = "Time point",
    y = expression(X[n]),
    shape = NULL
  ) +
  coord_cartesian(
    ylim = c(
      max(0, min(c(df_pois$X, df_pois$lcl)) - 2),
      max(c(df_pois$X, df_pois$ucl)) * 1.12
    ),
    clip = "off"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    plot.margin = margin(8, 16, 8, 8)
  )

p_pois_ind <- add_shift_marker(
  p_pois_ind,
  tau = tau_pois,
  ymax = max(c(df_pois$X, df_pois$ucl)),
  label_y = 1.06
)

ggsave(
  "figs/ch4_poisson_individual.png",
  p_pois_ind,
  width = 6.4,
  height = 4.8,
  dpi = 300
)

# ============================================================
# 4. Comparison table
# ============================================================

comparison_table <- data.frame(
  Distribution = c("Binomial", "Poisson"),
  IC_Value = c(pi0, lambda0),
  OC_Value = c(pi1, lambda1),
  Shift_Point = c(tau_bin, tau_pois),
  Individual_First_Signal = c(first_signal_bin_ind, first_signal_pois_ind),
  CUSUM_First_Signal = c(first_signal_bin, first_signal_pois),
  Individual_Delay = c(
    safe_delay(first_signal_bin_ind, tau_bin),
    safe_delay(first_signal_pois_ind, tau_pois)
  ),
  CUSUM_Delay = c(
    safe_delay(first_signal_bin, tau_bin),
    safe_delay(first_signal_pois, tau_pois)
  ),
  Individual_Total_Signals = c(
    sum(df_bin$signal_ind, na.rm = TRUE),
    sum(df_pois$signal_ind, na.rm = TRUE)
  ),
  CUSUM_Total_Signals = c(
    sum(df_bin$signal_cusum, na.rm = TRUE),
    sum(df_pois$signal_cusum, na.rm = TRUE)
  )
)

write.csv(
  comparison_table,
  "outputs/ch4_discrete_comparison.csv",
  row.names = FALSE
)

print(comparison_table)

# ============================================================
# 5. Combined figures
# ============================================================

p_cusum_only <- p_bin + p_pois +
  plot_annotation(
    title = "Discrete CUSUM Examples",
    subtitle = "Designed shift point is marked by dotted vertical line"
  )

ggsave(
  "figs/ch4_discrete_cusum_combined.png",
  p_cusum_only,
  width = 11.2,
  height = 5.0,
  dpi = 300
)

p_ind_only <- p_bin_ind + p_pois_ind +
  plot_annotation(
    title = "Individual Count Charts",
    subtitle = "Designed shift point is marked by dotted vertical line"
  )

ggsave(
  "figs/ch4_discrete_individual_combined.png",
  p_ind_only,
  width = 11.2,
  height = 5.0,
  dpi = 300
)

p_compare <- (p_bin_ind + p_bin) / (p_pois_ind + p_pois) +
  plot_annotation(
    title = "Individual Charts vs. Discrete CUSUM",
    subtitle = paste(
      "Individual charts react to single counts;",
      "CUSUM charts accumulate persistent evidence."
    )
  )

ggsave(
  "figs/ch4_discrete_comparison.png",
  p_compare,
  width = 11.2,
  height = 8.8,
  dpi = 300
)

# ============================================================
# 6. Output summaries
# ============================================================

sink("outputs/ch4_discrete_cusum_summary.txt")

cat("Discrete CUSUM Examples\n")
cat("============================================================\n\n")

cat("Binomial CUSUM\n")
cat("------------------------------------------------------------\n")
cat("m =", m_bin, "\n")
cat("pi0 =", pi0, "\n")
cat("pi1 =", pi1, "\n")
cat("Designed shift point =", tau_bin, "\n")
cat("k+ =", round(k_bin, 6), "\n")
cat("h+ =", h_bin, "\n")
cat("First CUSUM signal =", first_signal_bin, "\n")
cat("First individual-chart signal =", first_signal_bin_ind, "\n\n")

cat("Poisson CUSUM\n")
cat("------------------------------------------------------------\n")
cat("lambda0 =", lambda0, "\n")
cat("lambda1 =", lambda1, "\n")
cat("Designed shift point =", tau_pois, "\n")
cat("k+ =", round(k_pois, 6), "\n")
cat("h+ =", h_pois, "\n")
cat("First CUSUM signal =", first_signal_pois, "\n")
cat("First individual-chart signal =", first_signal_pois_ind, "\n\n")

cat("Comparison table\n")
cat("------------------------------------------------------------\n")
print(comparison_table)

cat("\nTeaching interpretation:\n")
cat("1. The designed shift point is tau = 11 in both examples.\n")
cat("2. Individual charts detect only single extreme observations.\n")
cat("3. CUSUM charts accumulate weak evidence after the shift.\n")
cat("4. Smaller shifts make the CUSUM advantage clearer.\n\n")

cat("Files saved:\n")
cat("figs/ch4_binomial_cusum_example.png\n")
cat("figs/ch4_poisson_cusum_example.png\n")
cat("figs/ch4_binomial_individual.png\n")
cat("figs/ch4_poisson_individual.png\n")
cat("figs/ch4_discrete_cusum_combined.png\n")
cat("figs/ch4_discrete_individual_combined.png\n")
cat("figs/ch4_discrete_comparison.png\n")
cat("outputs/ch4_binomial_cusum_data.csv\n")
cat("outputs/ch4_poisson_cusum_data.csv\n")
cat("outputs/ch4_discrete_comparison.csv\n")
cat("outputs/ch4_discrete_cusum_summary.txt\n")

sink()

cat("\nFiles saved successfully.\n")