# ============================================================
# Chapter 4: Univariate CUSUM Charts
# Complete R Code for Teaching in Class
# Author: Prof. Hong-Ji Yang
# Purpose:
#   Generate teaching examples, figures, and output tables
#   for Chapter 4: Univariate CUSUM Charts
#
# Notes:
#   1. This version fixes all plotmath syntax errors such as
#      expression(C[n]^+ == ...).
#   2. Use C[n]^"+" and C[n]^"-" in plotmath expressions.
#   3. Figures are saved to figs/.
#   4. CSV outputs are saved to outputs/.
# ============================================================

rm(list = ls())

setwd("E:/統計品質管制/Week12")

# ------------------------- Packages -------------------------
packages <- c("ggplot2", "patchwork", "dplyr", "tidyr")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

# Optional package for accurate CUSUM ARL computation.
# The code below does not require spc, but it will load spc if available.
if (!requireNamespace("spc", quietly = TRUE)) {
  message("Package 'spc' is not installed. Simulation and approximation functions will be used.")
} else {
  library(spc)
}

# ------------------------- Output folders -------------------
dir.create("figs", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

set.seed(20260509)

# ============================================================
# Utility functions
# ============================================================

standard_theme <- function(base_size = 15) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank()
    )
}

save_plot <- function(p, filename, width = 8.5, height = 5.2, dpi = 320) {
  ggsave(
    filename = file.path("figs", filename),
    plot = p,
    width = width,
    height = height,
    dpi = dpi
  )
}

first_signal <- function(signal_vec) {
  idx <- which(signal_vec)
  if (length(idx) == 0) return(NA_integer_)
  idx[1]
}

# ============================================================
# Core CUSUM functions
# ============================================================

# Upward one-sided CUSUM for mean shifts
cusum_mean_up <- function(x, mu0 = 0, sigma0 = 1, k = 0.5, h = 5,
                          standardize = TRUE) {
  z <- if (standardize) (x - mu0) / sigma0 else x - mu0
  n <- length(z)
  cp <- numeric(n)
  signal <- rep(FALSE, n)

  for (i in seq_len(n)) {
    previous <- ifelse(i == 1, 0, cp[i - 1])
    cp[i] <- max(0, previous + z[i] - k)
    signal[i] <- cp[i] > h
  }

  data.frame(
    n = seq_len(n),
    x = x,
    z = z,
    Cplus = cp,
    signal = signal
  )
}

# Downward one-sided CUSUM for mean shifts
cusum_mean_down <- function(x, mu0 = 0, sigma0 = 1, k = 0.5, h = 5,
                            standardize = TRUE) {
  z <- if (standardize) (x - mu0) / sigma0 else x - mu0
  n <- length(z)
  cm <- numeric(n)
  signal <- rep(FALSE, n)

  for (i in seq_len(n)) {
    previous <- ifelse(i == 1, 0, cm[i - 1])
    cm[i] <- min(0, previous + z[i] + k)
    signal[i] <- cm[i] < -h
  }

  data.frame(
    n = seq_len(n),
    x = x,
    z = z,
    Cminus = cm,
    signal = signal
  )
}

# Raw cumulative sum C_n = sum(X_i - mu0)
raw_cusum <- function(x, mu0 = 0) {
  data.frame(
    n = seq_along(x),
    C = cumsum(x - mu0)
  )
}

# Siegmund approximation for one-sided mean CUSUM ARL0
arl0_siegmund <- function(k, h) {
  if (abs(k) < 1e-12) return(NA_real_)
  a <- h + 1.166
  (exp(2 * k * a) - 2 * k * a - 1) / (2 * k^2)
}

# ARL relationship under N(mu0 + delta, lambda^2 sigma^2)
arl1_from_arl0_approx <- function(k, h, delta = 0, lambda = 1, sigma = 1) {
  k_star <- (k - delta) / (lambda * sigma)
  h_star <- h / (lambda * sigma)

  if (abs(k_star) < 1e-12) return(NA_real_)
  arl0_siegmund(k_star, h_star)
}

# Monte Carlo ARL for upward mean CUSUM
simulate_arl_mean_up <- function(k = 0.5, h = 3.5, mu = 0, sd = 1,
                                 M = 5000, Nmax = 10000) {
  rl <- rep(NA_integer_, M)

  for (j in seq_len(M)) {
    cval <- 0

    for (n in seq_len(Nmax)) {
      x <- rnorm(1, mean = mu, sd = sd)
      cval <- max(0, cval + x - k)

      if (cval > h) {
        rl[j] <- n
        break
      }
    }
  }

  mean(rl, na.rm = TRUE)
}

# Bisection search for h using Monte Carlo ARL
search_h_mean_up <- function(target_arl0 = 200, k = 0.5,
                             hL = 0, hU = 15,
                             tol = 2, M = 2000,
                             max_iter = 30) {
  history <- data.frame(iter = integer(), h = numeric(), arl = numeric())

  for (iter in seq_len(max_iter)) {
    h <- (hL + hU) / 2
    arl <- simulate_arl_mean_up(k = k, h = h, M = M)

    history <- rbind(
      history,
      data.frame(iter = iter, h = h, arl = arl)
    )

    if (abs(arl - target_arl0) <= tol) break

    if (arl > target_arl0) {
      hU <- h
    } else {
      hL <- h
    }
  }

  list(h = h, arl = arl, history = history)
}

# ============================================================
# Variance CUSUM functions
# ============================================================

# Variance CUSUM for individual observations
cusum_var_individual <- function(x, mu0 = 0, sigma0 = 1, sigma1 = 2,
                                 direction = c("up", "down"), h = 7.416) {
  direction <- match.arg(direction)

  y <- ((x - mu0) / sigma0)^2

  k_var <- 2 * log(sigma0 / sigma1) /
    ((sigma0 / sigma1)^2 - 1)

  n <- length(x)
  C <- numeric(n)
  signal <- rep(FALSE, n)

  if (direction == "up") {
    for (i in seq_len(n)) {
      previous <- ifelse(i == 1, 0, C[i - 1])
      C[i] <- max(0, previous + y[i] - k_var)
      signal[i] <- C[i] > h
    }
  } else {
    for (i in seq_len(n)) {
      previous <- ifelse(i == 1, 0, C[i - 1])
      C[i] <- min(0, previous + y[i] - k_var)
      signal[i] <- C[i] < h
    }
  }

  data.frame(
    n = seq_len(n),
    x = x,
    y = y,
    C = C,
    signal = signal,
    k = k_var
  )
}

# Variance CUSUM for batch data using sample variances
cusum_var_batch_up <- function(s2, sigma0 = 1, sigma1 = 2, h = 2.533) {
  k_var <- 2 * log(sigma1 / sigma0) * sigma0^2 * sigma1^2 /
    (sigma1^2 - sigma0^2)

  C <- numeric(length(s2))
  signal <- rep(FALSE, length(s2))

  for (i in seq_along(s2)) {
    previous <- ifelse(i == 1, 0, C[i - 1])
    C[i] <- max(0, previous + s2[i] - k_var)
    signal[i] <- C[i] > h
  }

  data.frame(
    n = seq_along(s2),
    s2 = s2,
    Cplus = C,
    signal = signal,
    k = k_var
  )
}

# ============================================================
# Discrete CUSUM functions
# ============================================================

# Binomial CUSUM for upward shift in nonconforming proportion
cusum_binomial_up <- function(x, m, pi0, pi1, h = 4) {
  k <- -m * log((1 - pi1) / (1 - pi0)) /
    log((pi1 / (1 - pi1)) / (pi0 / (1 - pi0)))

  C <- numeric(length(x))
  signal <- rep(FALSE, length(x))

  for (i in seq_along(x)) {
    previous <- ifelse(i == 1, 0, C[i - 1])
    C[i] <- max(0, previous + x[i] - k)
    signal[i] <- C[i] > h
  }

  data.frame(
    n = seq_along(x),
    x = x,
    Cplus = C,
    signal = signal,
    k = k
  )
}

# Poisson CUSUM for upward shift in rate
cusum_poisson_up <- function(x, lambda0, lambda1, h = 5) {
  k <- (lambda1 - lambda0) / (log(lambda1) - log(lambda0))

  C <- numeric(length(x))
  signal <- rep(FALSE, length(x))

  for (i in seq_along(x)) {
    previous <- ifelse(i == 1, 0, C[i - 1])
    C[i] <- max(0, previous + x[i] - k)
    signal[i] <- C[i] > h
  }

  data.frame(
    n = seq_along(x),
    x = x,
    Cplus = C,
    signal = signal,
    k = k
  )
}

# ============================================================
# Self-starting and adaptive CUSUM functions
# ============================================================

# Self-starting CUSUM transformation
self_starting_z <- function(x) {
  n <- length(x)

  z <- rep(NA_real_, n)
  xbar_prev <- rep(NA_real_, n)
  s_prev <- rep(NA_real_, n)

  for (i in 3:n) {
    xb <- mean(x[1:(i - 1)])
    ss <- sd(x[1:(i - 1)])

    tval <- (x[i] - xb) / ss

    z[i] <- qnorm(
      pt(
        sqrt((i - 1) / i) * tval,
        df = i - 2
      )
    )

    xbar_prev[i] <- xb
    s_prev[i] <- ss
  }

  data.frame(
    n = seq_len(n),
    x = x,
    xbar_prev = xbar_prev,
    s_prev = s_prev,
    z = z
  )
}

cusum_self_starting_up <- function(x, k = 0.25, h = 5.597) {
  dat <- self_starting_z(x)

  C <- numeric(length(x))
  signal <- rep(FALSE, length(x))

  for (i in 3:length(x)) {
    C[i] <- max(0, C[i - 1] + dat$z[i] - k)
    signal[i] <- C[i] > h
  }

  dat$CplusSS <- C
  dat$signal <- signal
  dat
}

# Adaptive CUSUM control limit approximation
h_adaptive <- function(delta_hat, ARL0 = 200) {
  d <- pmax(delta_hat, 1e-6)
  log(1 + 2 * d^2 * ARL0 + 2.332 * d) / (2 * d) - 1.166
}

cusum_adaptive_up <- function(x_std, delta_min = 0, lambda = 0.1, ARL0 = 200) {
  n <- length(x_std)

  delta_hat <- numeric(n)
  C <- numeric(n)
  hval <- numeric(n)
  signal <- rep(FALSE, n)

  delta_hat[1] <- max(
    delta_min,
    lambda * x_std[1] + (1 - lambda) * delta_min
  )

  hval[1] <- h_adaptive(delta_hat[1], ARL0)

  C[1] <- max(
    0,
    (x_std[1] - delta_hat[1] / 2) / hval[1]
  )

  signal[1] <- C[1] > 1

  for (i in 2:n) {
    delta_hat[i] <- max(
      delta_min,
      lambda * x_std[i] + (1 - lambda) * delta_hat[i - 1]
    )

    hval[i] <- h_adaptive(delta_hat[i], ARL0)

    C[i] <- max(
      0,
      C[i - 1] + (x_std[i] - delta_hat[i] / 2) / hval[i]
    )

    signal[i] <- C[i] > 1
  }

  data.frame(
    n = seq_len(n),
    x_std = x_std,
    delta_hat = delta_hat,
    h = hval,
    CplusA = C,
    signal = signal
  )
}

# ============================================================
# Example 4.1 / 4.2
# Shewhart chart vs raw CUSUM vs DI CUSUM
# ============================================================

m <- 5

x_batches <- matrix(
  c(
    rnorm(10 * m, 0, 1),
    rnorm(10 * m, 0.2, 1)
  ),
  ncol = m,
  byrow = TRUE
)

xbar <- rowMeans(x_batches)
time <- seq_along(xbar)

shewhart_df <- data.frame(
  n = time,
  xbar = xbar,
  phase = ifelse(time <= 10, "IC", "Shift")
)

UCL <- 3 / sqrt(m)
LCL <- -3 / sqrt(m)

p_shewhart <- ggplot(
  shewhart_df,
  aes(n, xbar, linetype = phase, shape = phase)
) +
  geom_line() +
  geom_point(size = 2.8) +
  geom_hline(
    yintercept = c(LCL, 0, UCL),
    linetype = c("dashed", "solid", "dashed")
  ) +
  annotate("text", x = 20.5, y = UCL, label = "UCL", hjust = 0) +
  annotate("text", x = 20.5, y = 0, label = "CL", hjust = 0) +
  annotate("text", x = 20.5, y = LCL, label = "LCL", hjust = 0) +
  labs(
    title = "Shewhart X-bar Chart: Small Shift Is Hard to Detect",
    subtitle = "First 10 samples: N(0,1); last 10 samples: N(0.2,1)",
    x = "Time point",
    y = expression(bar(X)[n])
  ) +
  coord_cartesian(xlim = c(1, 22)) +
  standard_theme()

save_plot(p_shewhart, "ch4_example41_shewhart_xbar.png")

raw_df <- raw_cusum(xbar, mu0 = 0)
raw_df$phase <- ifelse(raw_df$n <= 10, "IC", "Shift")

p_raw_cusum <- ggplot(
  raw_df,
  aes(n, C, linetype = phase, shape = phase)
) +
  geom_line() +
  geom_point(size = 2.8) +
  geom_vline(xintercept = 11, linetype = "dotted") +
  annotate(
    "text",
    x = 11.2,
    y = max(raw_df$C),
    label = "Shift starts",
    hjust = 0
  ) +
  labs(
    title = expression(C[n] == sum(X[i] - mu[0])),
    subtitle = "Raw accumulation reveals persistent directional drift",
    x = "Time point",
    y = expression(C[n])
  ) +
  standard_theme()

save_plot(p_raw_cusum, "ch4_example42_raw_cusum.png")

# DI CUSUM uses standardized sample mean.
# Target k = 0.25; ARL0 = 200 gives h = 5.597.
zbar <- xbar / (1 / sqrt(m))

di_df <- cusum_mean_up(
  zbar,
  mu0 = 0,
  sigma0 = 1,
  k = 0.25,
  h = 5.597
)

di_df$phase <- ifelse(di_df$n <= 10, "IC", "Shift")

p_di <- ggplot(
  di_df,
  aes(n, Cplus, linetype = phase, shape = phase)
) +
  geom_line() +
  geom_point(size = 2.8) +
  geom_hline(yintercept = 5.597, linetype = "dashed") +
  annotate("text", x = 20.5, y = 5.597, label = "h = 5.597", hjust = 0) +
  labs(
    title = "Decision-Interval CUSUM for Upward Mean Shift",
    subtitle = expression(
      C[n]^"+" == max(0, C[n-1]^"+" + Z[n] - k)
    ),
    x = "Time point",
    y = expression(C[n]^"+")
  ) +
  coord_cartesian(xlim = c(1, 22)) +
  standard_theme()

save_plot(p_di, "ch4_example42_DI_cusum.png")

# ============================================================
# ARL0 curve: effect of h and k
# ============================================================

h_grid <- seq(1, 4, by = 0.25)
k_grid <- c(0.1, 0.25, 0.5, 0.75)

arl_grid <- expand.grid(h = h_grid, k = k_grid)

arl_grid$ARL0_approx <- mapply(
  arl0_siegmund,
  arl_grid$k,
  arl_grid$h
)

p_arl0 <- ggplot(
  arl_grid,
  aes(h, ARL0_approx, group = factor(k), linetype = factor(k))
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_log10() +
  labs(
    title = expression(paste("Effect of ", h, " and ", k, " on ", ARL[0])),
    subtitle = "Larger h or k gives fewer false alarms and larger IC ARL",
    x = "Decision interval h",
    y = expression(ARL[0]),
    linetype = "k"
  ) +
  standard_theme()

save_plot(p_arl0, "ch4_arl0_vs_h_k.png")

write.csv(
  arl_grid,
  "outputs/ch4_arl0_grid_siegmund.csv",
  row.names = FALSE
)

# ============================================================
# ARL1 curve: target shift delta = 2k
# ============================================================

delta_grid <- seq(0, 2, by = 0.05)

arl1_df <- bind_rows(
  data.frame(delta = delta_grid, k = "0.25", h = 5.597),
  data.frame(delta = delta_grid, k = "0.50", h = 3.502),
  data.frame(delta = delta_grid, k = "0.75", h = 2.481)
)

arl1_df$k_num <- as.numeric(arl1_df$k)

arl1_df$ARL1_approx <- mapply(
  arl1_from_arl0_approx,
  k = arl1_df$k_num,
  h = arl1_df$h,
  delta = arl1_df$delta
)

arl1_df$ARL1_approx <- pmax(arl1_df$ARL1_approx, 1)

p_arl1 <- ggplot(
  arl1_df,
  aes(delta, ARL1_approx, group = k, linetype = k)
) +
  geom_line(linewidth = 1) +
  scale_y_log10() +
  labs(
    title = expression(paste("OC ", ARL[1], " and Target Shift Selection")),
    subtitle = expression(paste("CUSUM is most efficient near target shift ", delta == 2 * k)),
    x = expression("Mean shift size " * delta),
    y = expression(ARL[1]),
    linetype = "k"
  ) +
  standard_theme()

save_plot(p_arl1, "ch4_arl1_vs_delta.png")

write.csv(
  arl1_df,
  "outputs/ch4_arl1_delta_grid.csv",
  row.names = FALSE
)

# ============================================================
# Example 4.8: Mean CUSUM under variance shifts
# ============================================================

x_var_up <- c(
  rnorm(50, 0, 1),
  rnorm(50, 0, 2)
)

x_var_down <- c(
  rnorm(50, 0, 1),
  rnorm(50, 0, 0.5)
)

mean_cusum_upvar <- cusum_mean_up(
  x_var_up,
  k = 0.25,
  h = 5.597
)

mean_cusum_downvar <- cusum_mean_up(
  x_var_down,
  k = 0.25,
  h = 5.597
)

p_var_data_up <- ggplot(
  data.frame(n = 1:100, x = x_var_up),
  aes(n, x)
) +
  geom_line() +
  geom_point(size = 1.9) +
  geom_vline(xintercept = 51, linetype = "dotted") +
  labs(
    title = expression(paste("Data: Variance Shift ", sigma, ": 1 to 2")),
    x = "n",
    y = expression(X[n])
  ) +
  standard_theme()

p_var_cusum_up <- ggplot(
  mean_cusum_upvar,
  aes(n, Cplus)
) +
  geom_line() +
  geom_point(size = 1.9) +
  geom_hline(yintercept = 5.597, linetype = "dashed") +
  labs(
    title = "Mean-CUSUM Reacts to Upward Variance Shift",
    x = "n",
    y = expression(C[n]^"+")
  ) +
  standard_theme()

p_var_data_down <- ggplot(
  data.frame(n = 1:100, x = x_var_down),
  aes(n, x)
) +
  geom_line() +
  geom_point(size = 1.9) +
  geom_vline(xintercept = 51, linetype = "dotted") +
  labs(
    title = expression(paste("Data: Variance Shift ", sigma, ": 1 to 0.5")),
    x = "n",
    y = expression(X[n])
  ) +
  standard_theme()

p_var_cusum_down <- ggplot(
  mean_cusum_downvar,
  aes(n, Cplus)
) +
  geom_line() +
  geom_point(size = 1.9) +
  geom_hline(yintercept = 5.597, linetype = "dashed") +
  labs(
    title = "Mean-CUSUM Becomes Less Variable",
    x = "n",
    y = expression(C[n]^"+")
  ) +
  standard_theme()

p_example48 <- (p_var_data_up + p_var_cusum_up) /
  (p_var_data_down + p_var_cusum_down)

save_plot(
  p_example48,
  "ch4_example48_mean_cusum_variance_shift.png",
  width = 11,
  height = 8
)

# ============================================================
# Dedicated variance CUSUM
# ============================================================

var_cusum_up <- cusum_var_individual(
  x_var_up,
  sigma0 = 1,
  sigma1 = 2,
  direction = "up",
  h = 7.416
)

var_cusum_down <- cusum_var_individual(
  x_var_down,
  sigma0 = 1,
  sigma1 = 0.5,
  direction = "down",
  h = -2.446
)

p_var_cusum_ded_up <- ggplot(
  var_cusum_up,
  aes(n, C)
) +
  geom_line() +
  geom_point(size = 1.9) +
  geom_hline(yintercept = 7.416, linetype = "dashed") +
  geom_vline(xintercept = 51, linetype = "dotted") +
  labs(
    title = expression(paste("Dedicated Upward Variance CUSUM: ", sigma[0] == 1, ", ", sigma[1] == 2)),
    subtitle = paste0("k+ = ", round(unique(var_cusum_up$k), 3), ", hU = 7.416"),
    x = "n",
    y = expression(C[n]^"+")
  ) +
  standard_theme()

p_var_cusum_ded_down <- ggplot(
  var_cusum_down,
  aes(n, C)
) +
  geom_line() +
  geom_point(size = 1.9) +
  geom_hline(yintercept = -2.446, linetype = "dashed") +
  geom_vline(xintercept = 51, linetype = "dotted") +
  labs(
    title = expression(paste("Dedicated Downward Variance CUSUM: ", sigma[0] == 1, ", ", sigma[1] == 0.5)),
    subtitle = paste0("k- = ", round(unique(var_cusum_down$k), 3), ", hL = -2.446"),
    x = "n",
    y = expression(C[n]^"-")
  ) +
  standard_theme()

p_example49 <- p_var_cusum_ded_up + p_var_cusum_ded_down

save_plot(
  p_example49,
  "ch4_example49_dedicated_variance_cusum.png",
  width = 11,
  height = 5.5
)

# ============================================================
# Joint monitoring: CUSUM-M + CUSUM-V for batch data
# ============================================================

m_batch <- 5

# Scenario A: mean shift from 0 to 1
batch_mean_shift <- matrix(
  c(
    rnorm(10 * m_batch, 0, 1),
    rnorm(10 * m_batch, 1, 1)
  ),
  ncol = m_batch,
  byrow = TRUE
)

xbar_A <- rowMeans(batch_mean_shift)
s2_A <- apply(batch_mean_shift, 1, var)

CM_A <- cusum_mean_up(
  xbar_A,
  mu0 = 0,
  sigma0 = 1 / sqrt(m_batch),
  k = 0.5,
  h = 0.881
)

CV_A <- cusum_var_batch_up(
  s2_A,
  sigma0 = 1,
  sigma1 = 2,
  h = 2.533
)

# Scenario B: variance shift from 1 to 4
batch_var_shift <- matrix(
  c(
    rnorm(10 * m_batch, 0, 1),
    rnorm(10 * m_batch, 0, 2)
  ),
  ncol = m_batch,
  byrow = TRUE
)

xbar_B <- rowMeans(batch_var_shift)
s2_B <- apply(batch_var_shift, 1, var)

CM_B <- cusum_mean_up(
  xbar_B,
  mu0 = 0,
  sigma0 = 1 / sqrt(m_batch),
  k = 0.5,
  h = 0.881
)

CV_B <- cusum_var_batch_up(
  s2_B,
  sigma0 = 1,
  sigma1 = 2,
  h = 2.533
)

p_joint_CM_A <- ggplot(CM_A, aes(n, Cplus)) +
  geom_line() +
  geom_point(size = 2.2) +
  geom_hline(yintercept = 0.881, linetype = "dashed") +
  geom_vline(xintercept = 11, linetype = "dotted") +
  labs(
    title = "Scenario A: CUSUM-M Detects Mean Shift",
    x = "n",
    y = expression(C[M,n]^"+")
  ) +
  standard_theme()

p_joint_CV_A <- ggplot(CV_A, aes(n, Cplus)) +
  geom_line() +
  geom_point(size = 2.2) +
  geom_hline(yintercept = 2.533, linetype = "dashed") +
  geom_vline(xintercept = 11, linetype = "dotted") +
  labs(
    title = "Scenario A: CUSUM-V No Variance Signal",
    x = "n",
    y = expression(C[V,n]^"+")
  ) +
  standard_theme()

p_joint_CM_B <- ggplot(CM_B, aes(n, Cplus)) +
  geom_line() +
  geom_point(size = 2.2) +
  geom_hline(yintercept = 0.881, linetype = "dashed") +
  geom_vline(xintercept = 11, linetype = "dotted") +
  labs(
    title = "Scenario B: CUSUM-M No Mean Signal",
    x = "n",
    y = expression(C[M,n]^"+")
  ) +
  standard_theme()

p_joint_CV_B <- ggplot(CV_B, aes(n, Cplus)) +
  geom_line() +
  geom_point(size = 2.2) +
  geom_hline(yintercept = 2.533, linetype = "dashed") +
  geom_vline(xintercept = 11, linetype = "dotted") +
  labs(
    title = "Scenario B: CUSUM-V Detects Variance Shift",
    x = "n",
    y = expression(C[V,n]^"+")
  ) +
  standard_theme()

p_joint <- (p_joint_CM_A + p_joint_CV_A) /
  (p_joint_CM_B + p_joint_CV_B)

save_plot(
  p_joint,
  "ch4_joint_monitoring_cusum_M_V.png",
  width = 11,
  height = 8
)

# ============================================================
# Discrete CUSUM examples: Binomial and Poisson
# ============================================================

# Binomial example:
# m = 100, pi0 = 0.10, pi1 = 0.20
x_binom <- c(
  8, 17, 10, 8, 11,
  14, 8, 16, 8, 13,
  21, 15, 20, 18, 17
)

binom_df <- cusum_binomial_up(
  x_binom,
  m = 100,
  pi0 = 0.1,
  pi1 = 0.2,
  h = 4
)

p_binom <- ggplot(binom_df, aes(n, Cplus)) +
  geom_line() +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 4, linetype = "dashed") +
  labs(
    title = "Binomial CUSUM for Nonconforming Counts",
    subtitle = paste0(
      "m = 100, pi0 = 0.10, pi1 = 0.20, k+ = ",
      round(unique(binom_df$k), 3)
    ),
    x = "Sample number",
    y = expression(C[n]^"+")
  ) +
  standard_theme()

save_plot(p_binom, "ch4_binomial_cusum_example.png")

# Poisson example:
# Junk emails, lambda0 = 10, lambda1 = 20
x_pois <- c(
  14, 10, 6, 10, 9, 7, 17, 9, 13, 10,
  22, 21, 15, 25, 27, 26, 20, 26, 26, 17,
  23, 16, 18, 22, 15, 23, 19, 15, 18, 13
)

pois_df <- cusum_poisson_up(
  x_pois,
  lambda0 = 10,
  lambda1 = 20,
  h = 8
)

p_pois <- ggplot(pois_df, aes(n, Cplus)) +
  geom_line() +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 8, linetype = "dashed") +
  labs(
    title = "Poisson CUSUM for Defect / Event Counts",
    subtitle = paste0(
      "lambda0 = 10, lambda1 = 20, k+ = ",
      round(unique(pois_df$k), 3)
    ),
    x = "Day",
    y = expression(C[n]^"+")
  ) +
  standard_theme()

save_plot(p_pois, "ch4_poisson_cusum_example.png")

# ============================================================
# Self-starting CUSUM example
# ============================================================

x_ss <- c(
  rnorm(10, 0, 1),
  rnorm(30, 0.5, 1)
)

ss_df <- cusum_self_starting_up(
  x_ss,
  k = 0.25,
  h = 5.597
)

known_df <- cusum_mean_up(
  x_ss,
  mu0 = 0,
  sigma0 = 1,
  k = 0.25,
  h = 5.597
)

ss_plot_df <- data.frame(
  n = rep(seq_along(x_ss), 2),
  C = c(ss_df$CplusSS, known_df$Cplus),
  Chart = rep(
    c("Self-starting", "Known IC parameters"),
    each = length(x_ss)
  )
)

p_ss <- ggplot(
  ss_plot_df,
  aes(n, C, linetype = Chart)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  geom_hline(yintercept = 5.597, linetype = "dashed") +
  geom_vline(xintercept = 11, linetype = "dotted") +
  labs(
    title = "Self-Starting CUSUM vs Conventional CUSUM",
    subtitle = "Self-starting chart estimates IC parameters recursively",
    x = "n",
    y = expression(C[n]^"+")
  ) +
  standard_theme()

save_plot(p_ss, "ch4_self_starting_cusum.png")

write.csv(
  ss_df,
  "outputs/ch4_self_starting_values.csv",
  row.names = FALSE
)

# ============================================================
# Adaptive CUSUM example
# ============================================================

x_ex421 <- c(
  8.9, 9.4, 10.8, 8.5, 9.3,
  8.2, 9.2, 10.9, 8.5, 7.9,
  10.0, 12.0, 9.1, 8.3, 11.6,
  9.8, 9.9, 12.6, 11.0, 8.8,
  13.8, 14.4, 12.4, 17.0, 14.2,
  15.1, 12.3, 16.6, 14.1, 16.4
)

x_std <- (x_ex421 - 10) / 1.5

ad1 <- cusum_adaptive_up(
  x_std,
  delta_min = 0,
  lambda = 0.1,
  ARL0 = 200
)

ad2 <- cusum_adaptive_up(
  x_std,
  delta_min = 0,
  lambda = 0.5,
  ARL0 = 200
)

ad3 <- cusum_adaptive_up(
  x_std,
  delta_min = 2.0,
  lambda = 0.1,
  ARL0 = 200
)

ad_plot_df <- bind_rows(
  mutate(ad1, Setting = "delta_min = 0, lambda = 0.1"),
  mutate(ad2, Setting = "delta_min = 0, lambda = 0.5"),
  mutate(ad3, Setting = "delta_min = 2, lambda = 0.1")
)

p_ad <- ggplot(
  ad_plot_df,
  aes(n, CplusA, linetype = Setting)
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    title = "Adaptive CUSUM for Unknown Mean Shift Size",
    subtitle = expression(C[n,A]^"+" > 1 ~ "gives a signal"),
    x = "n",
    y = expression(C[n,A]^"+")
  ) +
  standard_theme()

save_plot(p_ad, "ch4_adaptive_cusum.png")

write.csv(
  ad_plot_df,
  "outputs/ch4_adaptive_cusum_values.csv",
  row.names = FALSE
)

# ============================================================
# Formula summary table for slides
# ============================================================

formula_table <- data.frame(
  Chart = c(
    "Mean CUSUM upward",
    "Mean CUSUM downward",
    "Variance CUSUM upward",
    "Binomial CUSUM upward",
    "Poisson CUSUM upward"
  ),
  Statistic = c(
    "Cplus[n] = max(0, Cplus[n-1] + (X[n]-mu0)/sigma0 - k)",
    "Cminus[n] = min(0, Cminus[n-1] + (X[n]-mu0)/sigma0 + k)",
    "Cplus[n] = max(0, Cplus[n-1] + ((X[n]-mu0)/sigma0)^2 - kplus)",
    "Cplus[n] = max(0, Cplus[n-1] + X[n] - kplus)",
    "Cplus[n] = max(0, Cplus[n-1] + X[n] - kplus)"
  ),
  Signal = c(
    "Cplus[n] > h",
    "Cminus[n] < -h",
    "Cplus[n] > hU",
    "Cplus[n] > hplus",
    "Cplus[n] > hplus"
  )
)

write.csv(
  formula_table,
  "outputs/ch4_cusum_formula_summary.csv",
  row.names = FALSE
)

# ============================================================
# Summary of first signal locations
# ============================================================

signal_summary <- data.frame(
  Example = c(
    "DI CUSUM upward mean",
    "Mean CUSUM under upward variance shift",
    "Dedicated upward variance CUSUM",
    "Dedicated downward variance CUSUM",
    "Binomial CUSUM",
    "Poisson CUSUM",
    "Self-starting CUSUM",
    "Adaptive CUSUM: delta_min=0, lambda=0.1",
    "Adaptive CUSUM: delta_min=0, lambda=0.5",
    "Adaptive CUSUM: delta_min=2, lambda=0.1"
  ),
  First_signal = c(
    first_signal(di_df$signal),
    first_signal(mean_cusum_upvar$signal),
    first_signal(var_cusum_up$signal),
    first_signal(var_cusum_down$signal),
    first_signal(binom_df$signal),
    first_signal(pois_df$signal),
    first_signal(ss_df$signal),
    first_signal(ad1$signal),
    first_signal(ad2$signal),
    first_signal(ad3$signal)
  )
)

write.csv(
  signal_summary,
  "outputs/ch4_cusum_signal_summary.csv",
  row.names = FALSE
)

# ============================================================
# Print quick summary
# ============================================================

cat("\n============================================================\n")
cat("Chapter 4 CUSUM teaching R code completed.\n")
cat("Figures saved in: figs/\n")
cat("Tables saved in: outputs/\n")
cat("Key correction: use expression(C[n]^\"+\") instead of expression(C[n]^+).\n")
cat("============================================================\n\n")

print(signal_summary)
