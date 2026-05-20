# ============================================================
# Compare X-bar Chart, V-mask CUSUM, and DI-CUSUM
# Highlight ALL OOC points in red
# Label only the FIRST point of each continuous OOC run
# Save PNG to figs/ and summary CSV to outputs/
# ============================================================

rm(list = ls())

setwd("E:/統計品質管制/Week12")

packages <- c("ggplot2", "dplyr", "tidyr", "patchwork")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

dir.create("figs", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

set.seed(20260511)

# ------------------------------------------------------------
# 1. Simulation settings
# ------------------------------------------------------------

N  <- 80
m  <- 5

mu0   <- 0
sigma <- 1

tau   <- 45
delta <- 0.8

mu <- ifelse(1:N < tau, mu0, mu0 + delta * sigma)

X <- matrix(
  rnorm(N * m, mean = rep(mu, each = m), sd = sigma),
  nrow = N,
  byrow = TRUE
)

xbar <- rowMeans(X)

dat <- data.frame(
  time = 1:N,
  xbar = xbar,
  mu = mu
)

# ------------------------------------------------------------
# 2. X-bar chart
# ------------------------------------------------------------

center <- mu0
se_xbar <- sigma / sqrt(m)

UCL <- center + 3 * se_xbar
LCL <- center - 3 * se_xbar

dat <- dat %>%
  mutate(
    xbar_signal = xbar > UCL | xbar < LCL
  )

# ------------------------------------------------------------
# 3. Standardized subgroup means
# ------------------------------------------------------------

Z <- (xbar - mu0) / se_xbar
dat$Z <- Z

# ------------------------------------------------------------
# 4. V-mask CUSUM
# ------------------------------------------------------------

C <- cumsum(Z)
dat$C <- C

k <- delta / 2
h <- 5

v_signal <- rep(FALSE, N)

for (n in 2:N) {
  i <- 1:(n - 1)
  
  lower_arm <- C[n] - h - k * (n - i)
  upper_arm <- C[n] + h + k * (n - i)
  
  if (any(C[i] < lower_arm | C[i] > upper_arm)) {
    v_signal[n] <- TRUE
  }
}

dat$v_signal <- v_signal

# ------------------------------------------------------------
# 5. DI-CUSUM
# ------------------------------------------------------------

Cplus  <- numeric(N)
Cminus <- numeric(N)

for (i in 2:N) {
  Cplus[i]  <- max(0, Cplus[i - 1] + Z[i] - k)
  Cminus[i] <- min(0, Cminus[i - 1] + Z[i] + k)
}

h_DI <- 5

dat$Cplus  <- Cplus
dat$Cminus <- Cminus

dat$di_signal_up   <- Cplus > h_DI
dat$di_signal_down <- Cminus < -h_DI
dat$di_signal      <- dat$di_signal_up | dat$di_signal_down

# ------------------------------------------------------------
# 6. First signal function
# ------------------------------------------------------------

first_signal <- function(signal_vec) {
  idx <- which(signal_vec)[1]
  if (is.na(idx)) return(NA_integer_)
  return(idx)
}

xbar_first  <- first_signal(dat$xbar_signal)
vmask_first <- first_signal(dat$v_signal)
di_first    <- first_signal(dat$di_signal)

# ------------------------------------------------------------
# 7. OOC data
# ------------------------------------------------------------

xbar_ooc <- dat %>%
  filter(xbar_signal)

vmask_ooc <- dat %>%
  filter(v_signal)

di_ooc <- dat %>%
  filter(di_signal) %>%
  mutate(
    di_y = ifelse(di_signal_up, Cplus, Cminus)
  )

# ------------------------------------------------------------
# 8. Label only the first point of each continuous OOC run
# ------------------------------------------------------------

get_run_first <- function(ooc_data) {
  if (nrow(ooc_data) == 0) return(ooc_data)
  
  ooc_data %>%
    arrange(time) %>%
    mutate(
      new_run = c(TRUE, diff(time) > 1),
      run_id = cumsum(new_run)
    ) %>%
    group_by(run_id) %>%
    slice(1) %>%
    ungroup()
}

xbar_ooc_label  <- get_run_first(xbar_ooc)
vmask_ooc_label <- get_run_first(vmask_ooc)
di_ooc_label    <- get_run_first(di_ooc)

# ------------------------------------------------------------
# 9. V-mask data drawn at first V-mask signal
# ------------------------------------------------------------

first_v_signal <- ifelse(is.na(vmask_first), N, vmask_first)

v_mask_dat <- data.frame(
  time = 1:first_v_signal,
  lower_arm =
    C[first_v_signal] -
    h -
    k * (first_v_signal - 1:first_v_signal),
  upper_arm =
    C[first_v_signal] +
    h +
    k * (first_v_signal - 1:first_v_signal)
)

# ------------------------------------------------------------
# 10. Summary table
# ------------------------------------------------------------

summary_table <- data.frame(
  Chart = c("X-bar chart", "V-mask CUSUM", "DI-CUSUM"),
  First_signal_time = c(xbar_first, vmask_first, di_first),
  True_change_point = tau,
  Detection_delay = c(
    xbar_first - tau,
    vmask_first - tau,
    di_first - tau
  ),
  Number_of_OOC_points = c(
    sum(dat$xbar_signal),
    sum(dat$v_signal),
    sum(dat$di_signal)
  ),
  Number_of_OOC_runs = c(
    nrow(xbar_ooc_label),
    nrow(vmask_ooc_label),
    nrow(di_ooc_label)
  )
)

print(summary_table)

write.csv(
  summary_table,
  file = "outputs/ch4_compare_xbar_vmask_dicusum_summary.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 11. Plot: X-bar chart
# ------------------------------------------------------------

p1 <- ggplot(dat, aes(x = time, y = xbar)) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 1.8) +
  geom_hline(yintercept = center, linewidth = 0.8) +
  geom_hline(yintercept = UCL, linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = LCL, linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = tau, linetype = "dotted", linewidth = 0.9) +
  geom_point(
    data = xbar_ooc,
    aes(x = time, y = xbar),
    color = "red",
    size = 4
  ) +
  geom_label(
    data = xbar_ooc_label,
    aes(x = time, y = xbar, label = time),
    fill = "white",
    color = "red",
    fontface = "bold",
    size = 4,
    label.size = 0,
    nudge_y = 0.35
  ) +
  labs(
    title = expression(bar(X) * "-chart"),
    subtitle = paste0(
      "First signal at n = ",
      ifelse(is.na(xbar_first), "No signal", xbar_first),
      "; OOC points = ", sum(dat$xbar_signal),
      "; OOC runs = ", nrow(xbar_ooc_label)
    ),
    x = "Subgroup",
    y = expression(bar(X))
  ) +
  theme_bw(base_size = 13)

# ------------------------------------------------------------
# 12. Plot: V-mask CUSUM
# ------------------------------------------------------------

p2 <- ggplot(dat, aes(x = time, y = C)) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 1.8) +
  geom_line(
    data = v_mask_dat,
    aes(x = time, y = lower_arm),
    linetype = "dashed",
    linewidth = 0.9,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = v_mask_dat,
    aes(x = time, y = upper_arm),
    linetype = "dashed",
    linewidth = 0.9,
    inherit.aes = FALSE
  ) +
  geom_vline(xintercept = tau, linetype = "dotted", linewidth = 0.9) +
  geom_point(
    data = vmask_ooc,
    aes(x = time, y = C),
    color = "red",
    size = 4
  ) +
  geom_label(
    data = vmask_ooc_label,
    aes(x = time, y = C, label = time),
    fill = "white",
    color = "red",
    fontface = "bold",
    size = 4,
    label.size = 0,
    nudge_y = 1.2
  ) +
  labs(
    title = "V-mask CUSUM",
    subtitle = paste0(
      "First signal at n = ",
      ifelse(is.na(vmask_first), "No signal", vmask_first),
      "; OOC points = ", sum(dat$v_signal),
      "; OOC runs = ", nrow(vmask_ooc_label),
      "; k = ", k,
      ", h = ", h
    ),
    x = "Subgroup",
    y = expression(C[n])
  ) +
  theme_bw(base_size = 13)

# ------------------------------------------------------------
# 13. Plot: DI-CUSUM
# ------------------------------------------------------------

di_dat <- dat %>%
  select(time, Cplus, Cminus) %>%
  pivot_longer(
    cols = c(Cplus, Cminus),
    names_to = "CUSUM",
    values_to = "value"
  )

p3 <- ggplot(di_dat, aes(x = time, y = value, linetype = CUSUM)) +
  geom_line(linewidth = 0.85) +
  geom_hline(yintercept = h_DI, linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = -h_DI, linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = tau, linetype = "dotted", linewidth = 0.9) +
  geom_point(
    data = di_ooc,
    aes(x = time, y = di_y),
    color = "red",
    size = 4,
    inherit.aes = FALSE
  ) +
  geom_label(
    data = di_ooc_label,
    aes(x = time, y = di_y, label = time),
    fill = "white",
    color = "red",
    fontface = "bold",
    size = 4,
    label.size = 0,
    nudge_y = 1.2,
    inherit.aes = FALSE
  ) +
  scale_linetype_manual(
    values = c(
      "Cplus" = "solid",
      "Cminus" = "dotted"
    ),
    labels = c(
      "Cplus" = expression(C[n]^"+"),
      "Cminus" = expression(C[n]^"-")
    )
  ) +
  labs(
    title = "DI-CUSUM Chart",
    subtitle = paste0(
      "First signal at n = ",
      ifelse(is.na(di_first), "No signal", di_first),
      "; OOC points = ", sum(dat$di_signal),
      "; OOC runs = ", nrow(di_ooc_label),
      "; k = ", k,
      ", h = ", h_DI
    ),
    x = "Subgroup",
    y = "CUSUM statistic",
    linetype = NULL
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom"
  )

# ------------------------------------------------------------
# 14. Combined plot
# ------------------------------------------------------------

p_all <-
  (p1 / p2 / p3) +
  plot_annotation(
    title =
      expression(
        "Comparison of " *
          bar(X) *
          "-chart, V-mask CUSUM, and DI-CUSUM"
      ),
    subtitle =
      paste0(
        "Simulated mean shift: ",
        "mu0 = ", mu0,
        ", sigma = ", sigma,
        ", subgroup size m = ", m,
        ", shift time tau = ", tau,
        ", shift size delta = ", delta
      )
  )

print(p_all)

# ------------------------------------------------------------
# 15. Save figures
# ------------------------------------------------------------

ggsave(
  filename = "figs/ch4_compare_xbar_vmask_dicusum_ooc_runs.png",
  plot = p_all,
  width = 10,
  height = 14,
  dpi = 300
)

ggsave(
  filename = "figs/ch4_xbar_chart_ooc_runs.png",
  plot = p1,
  width = 9,
  height = 4.8,
  dpi = 300
)

ggsave(
  filename = "figs/ch4_vmask_cusum_ooc_runs.png",
  plot = p2,
  width = 9,
  height = 4.8,
  dpi = 300
)

ggsave(
  filename = "figs/ch4_di_cusum_ooc_runs.png",
  plot = p3,
  width = 9,
  height = 4.8,
  dpi = 300
)

cat("\nSaved PNG files:\n")
cat("figs/ch4_compare_xbar_vmask_dicusum_ooc_runs.png\n")
cat("figs/ch4_xbar_chart_ooc_runs.png\n")
cat("figs/ch4_vmask_cusum_ooc_runs.png\n")
cat("figs/ch4_di_cusum_ooc_runs.png\n\n")

cat("Saved summary table:\n")
cat("outputs/ch4_compare_xbar_vmask_dicusum_summary.csv\n")