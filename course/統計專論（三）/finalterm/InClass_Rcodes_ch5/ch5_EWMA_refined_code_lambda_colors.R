############################################################
# Chapter 5: Univariate EWMA Charts
# Complete R companion code for Beamer slides
#
# Output folders:
#   figs/    : all PNG figures used in the slides
#   outputs/ : numerical summaries and tables
############################################################

rm(list = ls())
set.seed(611211106)
# Set your working directory if needed.
setwd("D:/統計品質管制/Week14")

#-----------------------------------------------------------
# 0. Folders
#-----------------------------------------------------------
fig_dir <- "figs"
out_dir <- "outputs"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

#-----------------------------------------------------------
# 1. Basic EWMA functions
#-----------------------------------------------------------
x <- c(
  13, 12, 10, 10, 9, 7, 11, 12, 7, 10,
  11, 10, 11, 10, 9,
  18, 14, 16, 18, 12, 13, 17, 18, 15, 18,
  15, 14, 18, 14, 16
)
n = length(x)
mu0 = 10
sigma = 2
ewma_stat <- function(x, lambda, e0 = mu0) {
  n <- length(x)
  e <- numeric(n)
  prev <- e0
  for (i in seq_len(n)) {
    e[i] <- lambda * x[i] + (1 - lambda) * prev
    prev <- e[i]
  }
  e
}

mean_ewma_limits <- function(n, mu0, sigma, lambda, rho) {
  se <- sigma * sqrt(
    lambda / (2 - lambda) *
      (1 - (1 - lambda)^(2 * seq_len(n)))
  )
  data.frame(n = seq_len(n), LCL = mu0 - rho * se, CL = mu0, UCL = mu0 + rho * se)
}

variance_ewma_stat_individual <- function(x, mu0 = 0, sigma0 = 1, lambda = 0.1) {
  y <- ((x - mu0) / sigma0)^2
  ewma_stat(y, lambda = lambda, e0 = 1)
}

variance_ewma_limits_individual <- function(n, lambda = 0.1, rhoU = 2.595,
                                             rhoL = 1.580, time_varying = FALSE) {
  if (time_varying) {
    se <- sqrt(2 * lambda / (2 - lambda) * (1 - (1 - lambda)^(2 * seq_len(n))))
  } else {
    se <- rep(sqrt(2 * lambda / (2 - lambda)), n)
  }
  data.frame(n = seq_len(n), LCL = 1 - rhoL * se, CL = 1, UCL = 1 + rhoU * se)
}

first_signal <- function(E, LCL, UCL) {
  id <- which(E < LCL | E > UCL)
  
  if (length(id) == 0) {
    return(NA)
  } else {
    return(id[1])
  }
}

setting_table <- data.frame(
  case = c("(i)", "(ii)", "(iii)", "(iv)"),
  ARL0 = c(200, 200, 500, 500),
  lambda = c(0.1, 0.5, 0.1, 0.5),
  rho = c(2.454, 2.777, 2.814, 3.071)
)

png(file.path(fig_dir, "ch5_exercise_5_4_EWMA_2x2.png"), width = 1600, height = 900, res = 180)

par(mfrow = c(2, 2),
    mar = c(4.2, 4.5, 3.2, 1.2))

result_summary <- data.frame()

for (j in 1:nrow(setting_table)) {
  
  lambda <- setting_table$lambda[j]
  rho <- setting_table$rho[j]
  
  E <- ewma_stat(x, lambda = lambda, e0 = mu0)
  limits <- mean_ewma_limits(n, mu0, sigma, lambda, rho)
  
  signal <- first_signal(E, limits$LCL, limits$UCL)
  
  temp <- data.frame(
    case = setting_table$case[j],
    ARL0 = setting_table$ARL0[j],
    lambda = lambda,
    rho = rho,
    first_signal = signal
  )
  
  result_summary <- rbind(result_summary, temp)
  
  plot(
    1:n, E,
    type = "b",
    pch = 16,
    lwd = 2,
    ylim = range(c(E, limits$LCL, limits$UCL)),
    xlab = "n",
    ylab = expression(E[n]),
    main = paste0(
      setting_table$case[j],
      "EWMA Chart: ARL0 = ",
      setting_table$ARL0[j],
      ", lambda = ",
      lambda
    )
  )
  
  lines(1:n, limits$UCL, col = "red", lwd = 2, lty = 2)
  lines(1:n, limits$LCL, col = "red", lwd = 2, lty = 2)
  abline(h = mu0, lty = 3, lwd = 2)
  
  if (!is.na(signal)) {
    points(signal, E[signal], pch = 21, bg = "orange", cex = 1.8)
    text(signal, E[signal], labels = paste0("n=", signal),
         pos = 4, cex = 0.8)
  }
}

dev.off()

result_summary

# Highlight all out-of-control points with transparent red fill
add_ooc_points <- function(x, y, LCL = NULL, UCL = NULL, cex = 0.95) {
  if (is.null(LCL)) LCL <- rep(-Inf, length(y))
  if (is.null(UCL)) UCL <- rep( Inf, length(y))
  if (length(LCL) == 1) LCL <- rep(LCL, length(y))
  if (length(UCL) == 1) UCL <- rep(UCL, length(y))
  idx <- which(y < LCL | y > UCL)
  if (length(idx) > 0) {
    points(
      x[idx], y[idx],
      pch = 21,
      bg  = rgb(1, 0, 0, 0.45),
      col = "red",
      lwd = 1.6,
      cex = cex
    )
  }
  invisible(idx)
}

# One-sided upward CUSUM used only for the comparison figure
cusum_up <- function(x, k = 0.25) {
  cplus <- numeric(length(x))
  for (i in seq_along(x)) {
    cplus[i] <- max(0, ifelse(i == 1, 0, cplus[i - 1]) + x[i] - k)
  }
  cplus
}

#-----------------------------------------------------------
# 2. Figure: EWMA weights, n = 100
#-----------------------------------------------------------
png(file.path(fig_dir, "ch5_weight_decay.png"), width = 1600, height = 900, res = 180)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
n <- 20

for (lambda in c(1, 0.5, 0.2, 0.05)) {
  i <- 1:n
  w <- lambda * (1 - lambda)^(n - i)
  plot(i, w, type = "h", lwd = 2, xlab = "i", ylab = expression(w[i]),
       main = bquote(lambda == .(lambda)))
  points(i, w, pch = 16, cex = 0.45)
}

weight_df <- data.frame(
  i = 1:n,
  Observation = paste0("X_", 1:n)
)

for (lambda in c(1, 0.5, 0.2, 0.05)) {
  w <- lambda * (1 - lambda)^(n - weight_df$i)
  
  weight_df[[paste0("lambda_", lambda)]] <- w
}

weight_df

write.csv(weight_df, "exercise_5_1_EWMA_weights.csv", row.names = FALSE)

dev.off()
#-----------------------------------------------------------
# 3. Figure: lambda comparison on same data stream
#    Refined: all EWMA trajectories using lines
#-----------------------------------------------------------

x_lam <- c(rnorm(50, 0, 1), rnorm(50, 0.6, 1))
n_lam <- seq_along(x_lam)

lambda_vals <- c(0.05, 0.10, 0.20, 0.50)

lambda_cols <- c(
  "0.05" = "royalblue3",
  "0.10" = "darkgreen",
  "0.20" = "orange3",
  "0.50" = "purple3"
)

png(file.path(fig_dir, "ch5_lambda_comparison.png"),
    width = 1600, height = 900, res = 180)

par(mar = c(4.6, 4.8, 3.2, 1.2))

plot(
  n_lam, x_lam,
  type = "l",
  lwd = 1.2,
  col = rgb(0, 0, 0, 0.25),
  xlab = "n",
  ylab = "EWMA statistic",
  ylim = c(-1.5, 1.5),
  main = expression("Effect of " * lambda * " on EWMA smoothing")
)

grid(col = "gray85", lty = 3)

abline(v = 51, col = "gray35", lty = 3, lwd = 1.8)
text(51, 1.42, labels = "mean shift", pos = 4, cex = 0.85, col = "gray25")

abline(h = 0, col = "gray35", lty = 2, lwd = 1.5)

for (lambda in lambda_vals) {
  key <- sprintf("%.2f", lambda)
  
  lines(
    n_lam,
    ewma_stat(x_lam, lambda = lambda, e0 = 0),
    col = lambda_cols[key],
    lwd = 3.0,
    lty = 1
  )
}

legend(
  "topleft",
  legend = c(
    expression(lambda == 0.05),
    expression(lambda == 0.10),
    expression(lambda == 0.20),
    expression(lambda == 0.50),
    "raw data",
    "center line"
  ),
  col = c(lambda_cols, rgb(0, 0, 0, 0.25), "gray35"),
  lty = c(rep(1, length(lambda_vals)), 1, 2),
  lwd = c(rep(3.0, length(lambda_vals)), 1.2, 1.5),
  bty = "n",
  cex = 0.9
)

dev.off()

#-----------------------------------------------------------
# 4. Figure: variance convergence in Eq. (5.2)
#-----------------------------------------------------------
png(file.path(fig_dir, "ch5_variance_convergence.png"),
    width = 1600, height = 900, res = 180)

n_grid <- 1:30
lambda_vals <- c(0.05, 0.10, 0.20)

# Different colors for teaching clarity
line_cols <- c("blue", "darkgreen", "red")
line_ltys <- c(1, 2, 3)
point_pchs <- c(16, 17, 15)

plot(NULL,
     xlim = c(1, 30),
     ylim = c(0, 0.13),
     xlab = "n",
     ylab = expression(Var(E[n])),
     main = expression("Time-varying variance of EWMA: " *
                         Var(E[n]) ==
                         frac(lambda, 2-lambda) *
                         (1-(1-lambda)^(2*n)) * sigma^2))

grid(col = "gray85", lty = 3)

for (j in seq_along(lambda_vals)) {
  lambda <- lambda_vals[j]
  
  v <- lambda / (2 - lambda) *
    (1 - (1 - lambda)^(2 * n_grid))
  
  v_inf <- lambda / (2 - lambda)
  
  lines(n_grid, v,
        type = "l",
        pch = point_pchs[j],
        col = line_cols[j],
        lwd = 2.5,
        lty = line_ltys[j],
        cex = 0.75)
  
  abline(h = v_inf,
         col = line_cols[j],
         lty = 4,
         lwd = 1.5)
  
  text(30, v_inf,
       labels = bquote(frac(lambda, 2-lambda) == .(round(v_inf, 3))),
       col = line_cols[j],
       pos = 3,
       cex = 0.75)
}

legend("bottomright",
       legend = c(
         expression(lambda == 0.05),
         expression(lambda == 0.10),
         expression(lambda == 0.20),
         "asymptotic variance"
       ),
       col = c(line_cols, "gray30"),
       lty = c(line_ltys, 4),
       pch = c(point_pchs, NA),
       lwd = c(rep(2.5, 3), 1.5),
       bty = "n",
       cex = 0.9)

dev.off()

#-----------------------------------------------------------
# 5. Example 5.1: Mean EWMA chart + X chart comparison
#-----------------------------------------------------------
x51 <- c(6.15, 11.36, 10.66, 9.16, 11.26, 7.45, 10.20, 13.20, 6.74, 11.19,
         9.63, 5.43, 10.20, 8.60, 10.22, 14.36, 10.85, 13.70, 10.67, 14.40,
         17.80, 12.22, 12.29, 12.72, 11.85, 12.91, 11.65, 14.17, 11.99, 12.48)

mu0 <- 10
sigma <- 2
lambda <- 0.1
rho <- 2.703

# EWMA statistic and limits
E51 <- ewma_stat(x51, lambda = lambda, e0 = mu0)
lim51 <- mean_ewma_limits(length(x51), mu0, sigma, lambda, rho,
                          time_varying = TRUE)
sig51 <- first_signal(E51, lim51$LCL, lim51$UCL)

# Shewhart X chart limits
x_UCL <- mu0 + 3 * sigma
x_LCL <- mu0 - 3 * sigma
sigX <- first_signal(x51, x_LCL, x_UCL)

# Two-panel figure: X chart + EWMA chart
png(file.path(fig_dir, "ch5_example51_xbar_ewma.png"),
    width = 1600, height = 900, res = 180)

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 3, 1))

# X chart
plot(seq_along(x51), x51,
     type = "l", pch = 16, lwd = 2,
     xlab = "n", ylab = expression(X[n]),
     ylim = range(c(x51, x_LCL, x_UCL)),
     main = "Shewhart X Chart")

abline(h = mu0, lty = 3, lwd = 2, col = "gray30")
abline(h = c(x_LCL, x_UCL), lty = 2, lwd = 2, col = "red")

if (!is.na(sigX)) {
  points(sigX, x51[sigX], pch = 21, 
         col = "red", cex = 1.8, lwd = 2)
}

legend("topleft",
       legend = c("observations", "3-sigma limits", "center line",
                  paste("first signal =", sigX)),
       lty = c(1, 2, 3, NA),
       pch = c(16, NA, NA, 21),
       col = c("black", "red", "gray30", "red"),
       lwd = c(2, 2, 2, NA),
       bty = "n", cex = 0.85)

# EWMA chart
plot(seq_along(x51), E51,
     type = "l", pch = 16, lwd = 2,
     xlab = "n", ylab = expression(E[n]),
     ylim = range(c(E51, lim51$LCL, lim51$UCL)),
     main = "EWMA Chart")

lines(lim51$n, lim51$UCL, lty = 2, lwd = 2, col = "red")
lines(lim51$n, lim51$LCL, lty = 2, lwd = 2, col = "red")
abline(h = mu0, lty = 3, lwd = 2, col = "gray30")

add_ooc_points(
  seq_along(E51), E51,
  LCL = lim51$LCL, UCL = lim51$UCL,
  cex = 1.15
)

legend("topleft",
       legend = c("EWMA statistic", "time-varying limits", "center line",
                  paste("first signal =", sig51)),
       lty = c(1, 2, 3, NA),
       pch = c(16, NA, NA, 21),
       col = c("black", "red", "gray30", "red"),
       lwd = c(2, 2, 2, NA),
       bty = "n", cex = 0.85)

dev.off()

#-----------------------------------------------------------
# 6. ARL0 against rho and lambda by Monte Carlo simulation
#-----------------------------------------------------------
sim_arl_mean <- function(lambda, rho, delta = 0,
                         nrep = 2000,
                         maxn = 10000) {
  
  U <- rho * sqrt(lambda / (2 - lambda))
  L <- -U
  
  rl <- integer(nrep)
  
  for (r in seq_len(nrep)) {
    
    e <- 0
    
    for (i in seq_len(maxn)) {
      
      x <- rnorm(1, mean = delta, sd = 1)
      
      e <- lambda * x + (1 - lambda) * e
      
      if (e < L || e > U) {
        rl[r] <- i
        break
      }
    }
    
    if (rl[r] == 0)
      rl[r] <- maxn
  }
  
  mean(rl)
}

rho_grid <- seq(1, 3, by = 0.5)
lambda_grid <- c(0.05, 0.10, 0.20, 0.30)

arl0_table <- expand.grid(
  lambda = lambda_grid,
  rho = rho_grid
)

arl0_table$ARL0 <- NA_real_

#-----------------------------------------------------------
# Monte Carlo estimation
#-----------------------------------------------------------
for (j in seq_len(nrow(arl0_table))) {
  
  arl0_table$ARL0[j] <-
    sim_arl_mean(
      lambda = arl0_table$lambda[j],
      rho    = arl0_table$rho[j],
      delta  = 0,
      nrep   = 800
    )
}

write.csv(
  arl0_table,
  file.path(out_dir, "ch5_arl0_vs_rho_lambda.csv"),
  row.names = FALSE
)

#-----------------------------------------------------------
# Teaching-friendly plot
#-----------------------------------------------------------
png(
  file.path(fig_dir, "ch5_arl0_vs_rho_lambda.png"),
  width = 1600,
  height = 900,
  res = 180
)

# Colors and symbols
line_cols <- c(
  "blue",
  "darkgreen",
  "red",
  "purple"
)

line_ltys <- c(1, 2, 3, 4)

point_pchs <- c(16, 17, 15, 18)

plot(
  NULL,
  xlim = range(rho_grid),
  ylim = c(1, max(arl0_table$ARL0, na.rm = TRUE)),
  log  = "y",
  
  xlab = expression(rho),
  ylab = expression(ARL[0]),
  
  main = expression(
    ARL[0] ~ "versus" ~ rho ~ "for different" ~ lambda
  )
)

grid(
  col = "gray85",
  lty = 3
)

for (j in seq_along(lambda_grid)) {
  
  lambda_val <- lambda_grid[j]
  
  tmp <- arl0_table[
    arl0_table$lambda == lambda_val,
  ]
  
  lines(
    tmp$rho,
    tmp$ARL0,
    
    type = "l",
    
    col = line_cols[j],
    lty = line_ltys[j],
    lwd = 3,
    
    pch = point_pchs[j],
    cex = 1.1
  )
}

legend(
  "topleft",
  
  legend = c(
    expression(lambda == 0.05),
    expression(lambda == 0.10),
    expression(lambda == 0.20),
    expression(lambda == 0.30)
  ),
  
  col = line_cols,
  lty = line_ltys,
  pch = point_pchs,
  
  lwd = 3,
  pt.cex = 1.2,
  
  bty = "n",
  cex = 1.0
)

dev.off()

#-----------------------------------------------------------
# 7. ARL1 curves using table-based rho values from Chapter 5
#-----------------------------------------------------------
library(spc)

ARL0_target <- 200

lambda_vals <- c(0.01, 0.3, 0.75)
delta_vals <- seq(0, 3, by = 0.1)

rho_vals <- numeric(length(lambda_vals))

for (j in seq_along(lambda_vals)) {
  rho_vals[j] <- xewma.crit(
    l = lambda_vals[j],
    L0 = ARL0_target,
    mu = 0,
    sided = "two"
  )
}

rho_table <- data.frame(
  lambda = lambda_vals,
  rho = rho_vals
)

rho_table

arl1_table <- expand.grid(
  lambda = lambda_vals,
  delta = delta_vals
)

arl1_table <- merge(
  arl1_table,
  rho_table,
  by = "lambda"
)

arl1_table$ARL1 <- mapply(
  function(lambda, rho, delta) {
    xewma.arl(
      l = lambda,
      c = rho,
      mu = delta,
      sided = "two"
    )
  },
  arl1_table$lambda,
  arl1_table$rho,
  arl1_table$delta
)

arl1_table

png(
  filename = "ch5_exercise_5_8_arl1_curves.png",
  width = 1600,
  height = 900,
  res = 180
)

plot(
  NULL,
  xlim = range(delta_vals),
  ylim = range(arl1_table$ARL1),
  log = "y",
  xlab = expression(delta),
  ylab = expression(ARL[1]),
  main = expression(ARL[1] ~ "curves for different" ~ lambda)
)

grid(col = "gray85", lty = 3)

line_lty <- c(1, 2, 3)
point_pch <- c(16, 17, 15)

for (j in seq_along(lambda_vals)) {
  
  lambda <- lambda_vals[j]
  tmp <- arl1_table[arl1_table$lambda == lambda, ]
  tmp <- tmp[order(tmp$delta), ]
  
  lines(
    tmp$delta,
    tmp$ARL1,
    type = "b",
    lty = line_lty[j],
    pch = point_pch[j],
    lwd = 2
  )
}

legend(
  "topright",
  legend = c(
    expression(lambda == 0.01),
    expression(lambda == 0.30),
    expression(lambda == 0.75)
  ),
  lty = line_lty,
  pch = point_pch,
  lwd = 2,
  bty = "n"
)

dev.off()

arl1_table$rho <-
  unname(
    rho_for_arl200[
      as.character(arl1_table$lambda)
    ]
  )

arl1_table$ARL <- NA_real_

#-----------------------------------------------------------
# Monte Carlo estimation
#-----------------------------------------------------------
for (j in seq_len(nrow(arl1_table))) {
  
  arl1_table$ARL[j] <-
    sim_arl_mean(
      lambda = arl1_table$lambda[j],
      rho    = arl1_table$rho[j],
      delta  = arl1_table$delta[j],
      nrep   = 800
    )
}

write.csv(
  arl1_table,
  file.path(out_dir, "ch5_arl1_vs_delta.csv"),
  row.names = FALSE
)

#-----------------------------------------------------------
# Refined teaching-quality figure
#-----------------------------------------------------------
png(
  file.path(fig_dir, "ch5_arl1_vs_delta.png"),
  width = 1600,
  height = 900,
  res = 180
)

# Teaching-friendly colors
line_cols <- c(
  "blue",
  "darkgreen",
  "red"
)

line_ltys <- c(1, 2, 3)

point_pchs <- c(16, 17, 15)

plot(
  NULL,
  
  xlim = range(delta_grid),
  ylim = c(1, 220),
  
  log = "y",
  
  xlab = expression(delta),
  ylab = expression(ARL[1]),
  
  main = expression(
    ARL[1] ~ "curves for different" ~ lambda
  )
)

grid(
  col = "gray85",
  lty = 3
)

for (j in seq_along(c(0.05, 0.1, 0.2))) {
  
  lambda_val <- c(0.05, 0.1, 0.2)[j]
  
  tmp <- arl1_table[
    arl1_table$lambda == lambda_val,
  ]
  
  lines(
    tmp$delta,
    tmp$ARL,
    
    type = "l",
    
    col = line_cols[j],
    lty = line_ltys[j],
    lwd = 3,
    
    pch = point_pchs[j],
    cex = 1.1
  )
}

legend(
  "topright",
  
  legend = c(
    expression(lambda == 0.05),
    expression(lambda == 0.10),
    expression(lambda == 0.20)
  ),
  
  col = line_cols,
  lty = line_ltys,
  pch = point_pchs,
  
  lwd = 3,
  pt.cex = 1.2,
  
  bty = "n",
  cex = 1.0
)

dev.off()

#-----------------------------------------------------------
# 8. Separate CUSUM and EWMA figures
#    Individual observations vs batch means
#-----------------------------------------------------------


#===========================================================
# Simulation setting
#===========================================================
m <- 5

x_ind <- c(
  rnorm(50, 0, 1),
  rnorm(50, 0.5, 1)
)

xbatch <- colMeans(
  matrix(x_ind, nrow = m)
) * sqrt(m)

tau_ind   <- 51
tau_batch <- ceiling(tau_ind / m)

#===========================================================
# CUSUM setting
#===========================================================
k_cusum <- 0.25
h_cusum <- 5.597

C_ind <- cusum_up(x_ind, k = k_cusum)
C_batch <- cusum_up(xbatch, k = k_cusum)

sig_C_ind <- first_signal(C_ind, UCL = h_cusum)
sig_C_batch <- first_signal(C_batch, UCL = h_cusum)

#===========================================================
# EWMA setting
#===========================================================
lambda_ewma <- 0.1
rho_ewma <- 2.454

E_ind <- ewma_stat(x_ind, lambda = lambda_ewma, e0 = 0)
E_batch <- ewma_stat(xbatch, lambda = lambda_ewma, e0 = 0)

U_ewma <- rho_ewma * sqrt(lambda_ewma / (2 - lambda_ewma))
L_ewma <- -U_ewma

sig_E_ind <- first_signal(E_ind, LCL = L_ewma, UCL = U_ewma)
sig_E_batch <- first_signal(E_batch, LCL = L_ewma, UCL = U_ewma)

#===========================================================
# Helper functions
#===========================================================
add_shift_line <- function(v) {
  abline(v = v, lty = 3, lwd = 2, col = "gray40")
  
  usr <- par("usr")
  text(
    x = v,
    y = usr[4],
    labels = "shift",
    pos = 2,
    cex = 0.75,
    col = "gray30"
  )
}

highlight_signal <- function(sig, y) {
  if (!is.na(sig)) {
    points(
      sig, y[sig],
      pch = 21,
      bg = "yellow",
      col = "red",
      cex = 1.8,
      lwd = 2
    )
  }
}

label_limit <- function(y, label) {
  usr <- par("usr")
  old_xpd <- par("xpd")
  par(xpd = NA)
  
  text(
    x = usr[2] + 0.02 * diff(usr[1:2]),
    y = y,
    labels = label,
    pos = 4,
    cex = 0.82,
    col = "red"
  )
  
  par(xpd = old_xpd)
}

#===========================================================
# (A) CUSUM comparison figure
#===========================================================
png(
  file.path(fig_dir, "ch5_cusum_individual_vs_batch.png"),
  width = 1600,
  height = 850,
  res = 180
)

par(
  mfrow = c(1, 2),
  mar = c(4.2, 4.6, 3.0, 4.2),
  oma = c(0, 0, 1.0, 0)
)

#-----------------------------------------------------------
# CUSUM: individual observations
#-----------------------------------------------------------
plot(
  C_ind,
  type = "l",
  pch = 16,
  cex = 0.55,
  lwd = 2,
  xlab = "Observation index",
  ylab = expression(C[n]^"+"),
  ylim = c(0, max(C_ind, h_cusum) * 1.18),
  main = "CUSUM: Individual Observations"
)

grid(col = "gray85", lty = 3)

abline(h = 0, lty = 3, lwd = 2, col = "gray35")
abline(h = h_cusum, lty = 2, lwd = 2.5, col = "red")

add_shift_line(tau_ind)
highlight_signal(sig_C_ind, C_ind)

label_limit(
  y = h_cusum,
  label = bquote(UCL == .(round(h_cusum, 3)))
)

legend(
  "bottomright",
  legend = c("CUSUM statistic", "UCL", "OOC points"),
  lty = c(1, 2, NA),
  pch = c(16, NA, 21),
  pt.bg = c(NA, NA, rgb(1, 0, 0, 0.45)),
  col = c("black", "red", "red"),
  lwd = c(2, 2.5, NA),
  bty = "n",
  cex = 0.82
)

#-----------------------------------------------------------
# CUSUM: batch means
#-----------------------------------------------------------
plot(
  C_batch,
  type = "l",
  pch = 16,
  cex = 0.70,
  lwd = 2,
  xlab = "Batch index",
  ylab = expression(C[n]^"+"),
  ylim = c(0, max(C_batch, h_cusum) * 1.18),
  main = "CUSUM: Batch Means"
)

grid(col = "gray85", lty = 3)

abline(h = 0, lty = 3, lwd = 2, col = "gray35")
abline(h = h_cusum, lty = 2, lwd = 2.5, col = "red")

add_shift_line(tau_batch)
highlight_signal(sig_C_batch, C_batch)

label_limit(
  y = h_cusum,
  label = bquote(UCL == .(round(h_cusum, 3)))
)

legend(
  "bottomright",
  legend = c("CUSUM statistic", "UCL", "OOC points"),
  lty = c(1, 2, NA),
  pch = c(16, NA, 21),
  pt.bg = c(NA, NA, rgb(1, 0, 0, 0.45)),
  col = c("black", "red", "red"),
  lwd = c(2, 2.5, NA),
  bty = "n",
  cex = 0.82
)

mtext(
  "CUSUM: Individual Observations versus Batch Means",
  outer = TRUE,
  cex = 1.1,
  font = 2
)

dev.off()

#===========================================================
# (B) EWMA comparison figure
#===========================================================
png(
  file.path(fig_dir, "ch5_ewma_individual_vs_batch.png"),
  width = 1600,
  height = 850,
  res = 180
)

par(
  mfrow = c(1, 2),
  mar = c(4.2, 4.6, 3.0, 4.2),
  oma = c(0, 0, 1.0, 0)
)

#-----------------------------------------------------------
# EWMA: individual observations
#-----------------------------------------------------------
ylim_E_ind <- range(c(E_ind, L_ewma, U_ewma))
ylim_E_ind <- ylim_E_ind + c(-0.12, 0.12) * diff(ylim_E_ind)

plot(
  E_ind,
  type = "l",
  pch = 16,
  cex = 0.55,
  lwd = 2,
  xlab = "Observation index",
  ylab = expression(E[n]),
  ylim = ylim_E_ind,
  main = "EWMA: Individual Observations"
)

grid(col = "gray85", lty = 3)

abline(h = 0, lty = 3, lwd = 2, col = "gray35")
abline(h = c(L_ewma, U_ewma), lty = 2, lwd = 2.5, col = "red")

add_shift_line(tau_ind)
highlight_signal(sig_E_ind, E_ind)

label_limit(
  y = U_ewma,
  label = bquote(UCL == .(round(U_ewma, 3)))
)

label_limit(
  y = L_ewma,
  label = bquote(LCL == .(round(L_ewma, 3)))
)

legend(
  "bottomright",
  legend = c("EWMA statistic", "UCL/LCL", "OOC points"),
  lty = c(1, 2, NA),
  pch = c(16, NA, 21),
  pt.bg = c(NA, NA, rgb(1, 0, 0, 0.45)),
  col = c("black", "red", "red"),
  lwd = c(2, 2.5, NA),
  bty = "n",
  cex = 0.82
)

#-----------------------------------------------------------
# EWMA: batch means
#-----------------------------------------------------------
ylim_E_batch <- range(c(E_batch, L_ewma, U_ewma))
ylim_E_batch <- ylim_E_batch + c(-0.12, 0.12) * diff(ylim_E_batch)

plot(
  E_batch,
  type = "l",
  pch = 16,
  cex = 0.70,
  lwd = 2,
  xlab = "Batch index",
  ylab = expression(E[n]),
  ylim = ylim_E_batch,
  main = "EWMA: Batch Means"
)

grid(col = "gray85", lty = 3)

abline(h = 0, lty = 3, lwd = 2, col = "gray35")
abline(h = c(L_ewma, U_ewma), lty = 2, lwd = 2.5, col = "red")

add_shift_line(tau_batch)
highlight_signal(sig_E_batch, E_batch)

label_limit(
  y = U_ewma,
  label = bquote(UCL == .(round(U_ewma, 3)))
)

label_limit(
  y = L_ewma,
  label = bquote(LCL == .(round(L_ewma, 3)))
)

legend(
  "bottomright",
  legend = c("EWMA statistic", "UCL/LCL", "OOC points"),
  lty = c(1, 2, NA),
  pch = c(16, NA, 21),
  pt.bg = c(NA, NA, rgb(1, 0, 0, 0.45)),
  col = c("black", "red", "red"),
  lwd = c(2, 2.5, NA),
  bty = "n",
  cex = 0.82
)

mtext(
  "EWMA: Individual Observations versus Batch Means",
  outer = TRUE,
  cex = 1.1,
  font = 2
)

dev.off()
#-----------------------------------------------------------
# 9. Mean EWMA under variance shifts
#    Refined figure: connected data + red control limits
#-----------------------------------------------------------

n0 <- 50
n1 <- 50
tau <- n0 + 1
N  <- n0 + n1

lambda_mean <- 0.20
rho_mean    <- 2.635
mu0         <- 0
sigma0      <- 1

# Variance-shift scenarios: same mean, different variance after tau
x_upvar <- c(
  rnorm(n0, mean = mu0, sd = sigma0),
  rnorm(n1, mean = mu0, sd = 2)
)

x_downvar <- c(
  rnorm(n0, mean = mu0, sd = sigma0),
  rnorm(n1, mean = mu0, sd = 0.5)
)

# Mean EWMA statistics
E_upvar   <- ewma_stat(x_upvar,   lambda = lambda_mean, e0 = mu0)
E_downvar <- ewma_stat(x_downvar, lambda = lambda_mean, e0 = mu0)

# Steady-state EWMA limits for the mean chart
U_mean <- rho_mean * sigma0 * sqrt(lambda_mean / (2 - lambda_mean))
L_mean <- -U_mean

# Shewhart limits for raw individual observations
U_x <- mu0 + 3 * sigma0
L_x <- mu0 - 3 * sigma0

# First EWMA signals
sig_upvar   <- first_signal(E_upvar,   LCL = L_mean, UCL = U_mean)
sig_downvar <- first_signal(E_downvar, LCL = L_mean, UCL = U_mean)

# Helper functions for consistent teaching-style graphics
add_grid <- function() {
  grid(col = "gray85", lty = 3)
}

add_shift_line <- function(tau) {
  abline(v = tau, lty = 3, lwd = 2, col = "gray35")
  mtext("variance shift", side = 3, at = tau, line = 0.15,
        cex = 0.70, col = "gray25")
}

add_limit_labels <- function(U, L, x_pos = N) {
  text(x_pos, U, labels = paste0("UCL = ", round(U, 3)),
       pos = 3, col = "red", cex = 0.78)
  text(x_pos, L, labels = paste0("LCL = ", round(L, 3)),
       pos = 1, col = "red", cex = 0.78)
}

highlight_first_signal <- function(sig, y) {
  if (!is.na(sig)) {
    points(sig, y[sig], pch = 21,
           bg = rgb(1, 0, 0, 0.45),
           col = "red", cex = 1.65, lwd = 2.2)
  }
}

png(
  file.path(fig_dir, "ch5_mean_ewma_variance_shift.png"),
  width = 1600,
  height = 1100,
  res = 180
)

par(
  mfrow = c(2, 2),
  mar = c(4.2, 4.4, 3.0, 1.2),
  oma = c(0.5, 0.5, 2.2, 0.5)
)

#-------------------------
# Upward variance shift: raw data
#-------------------------
ylim_x_up <- range(c(x_upvar, L_x, U_x))
plot(
  seq_len(N), x_upvar,
  type = "l",
  pch = 16,
  cex = 0.55,
  lwd = 1.8,
  xlab = "n",
  ylab = expression(X[n]),
  ylim = ylim_x_up,
  main = expression("Upward variance shift: " * sigma * ": 1 " %->% " 2")
)
add_grid()
abline(h = mu0, lty = 3, lwd = 2, col = "gray35")
abline(h = c(L_x, U_x), lty = 2, lwd = 2.5, col = "red")
add_shift_line(tau)
add_limit_labels(U_x, L_x)
add_ooc_points(seq_len(N), x_upvar, LCL = L_x, UCL = U_x, cex = 1.0)
legend(
  "topleft",
  legend = c("connected observations", "3-sigma limits", "center line", "OOC points"),
  lty = c(1, 2, 3, NA),
  pch = c(16, NA, NA, 21),
  pt.bg = c(NA, NA, NA, rgb(1, 0, 0, 0.45)),
  col = c("black", "red", "gray35", "red"),
  lwd = c(1.8, 2.5, 2, NA),
  bty = "n",
  cex = 0.78
)

#-------------------------
# Upward variance shift: mean EWMA
#-------------------------
ylim_E_up <- range(c(E_upvar, L_mean, U_mean))
plot(
  seq_len(N), E_upvar,
  type = "l",
  pch = 16,
  cex = 0.55,
  lwd = 1.8,
  xlab = "n",
  ylab = expression(E[n]),
  ylim = ylim_E_up,
  main = "Mean EWMA under upward variance shift"
)
add_grid()
abline(h = mu0, lty = 3, lwd = 2, col = "gray35")
abline(h = c(L_mean, U_mean), lty = 2, lwd = 2.5, col = "red")
add_shift_line(tau)
add_limit_labels(U_mean, L_mean)
add_ooc_points(seq_len(N), E_upvar, LCL = L_mean, UCL = U_mean, cex = 1.0)
legend(
  "topleft",
  legend = c("connected EWMA statistic", "EWMA limits", "OOC points"),
  lty = c(1, 2, NA),
  pch = c(16, NA, 21),
  pt.bg = c(NA, NA, rgb(1, 0, 0, 0.45)),
  col = c("black", "red", "red"),
  lwd = c(1.8, 2.5, NA),
  bty = "n",
  cex = 0.78
)

#-------------------------
# Downward variance shift: raw data
#-------------------------
ylim_x_down <- range(c(x_downvar, L_x, U_x))
plot(
  seq_len(N), x_downvar,
  type = "l",
  pch = 16,
  cex = 0.55,
  lwd = 1.8,
  xlab = "n",
  ylab = expression(X[n]),
  ylim = ylim_x_down,
  main = expression("Downward variance shift: " * sigma * ": 1 " %->% " 0.5")
)
add_grid()
abline(h = mu0, lty = 3, lwd = 2, col = "gray35")
abline(h = c(L_x, U_x), lty = 2, lwd = 2.5, col = "red")
add_shift_line(tau)
add_limit_labels(U_x, L_x)
add_ooc_points(seq_len(N), x_downvar, LCL = L_x, UCL = U_x, cex = 1.0)
legend(
  "topleft",
  legend = c("connected observations", "3-sigma limits", "center line", "OOC points"),
  lty = c(1, 2, 3, NA),
  pch = c(16, NA, NA, 21),
  pt.bg = c(NA, NA, NA, rgb(1, 0, 0, 0.45)),
  col = c("black", "red", "gray35", "red"),
  lwd = c(1.8, 2.5, 2, NA),
  bty = "n",
  cex = 0.78
)

#-------------------------
# Downward variance shift: mean EWMA
#-------------------------
ylim_E_down <- range(c(E_downvar, L_mean, U_mean))
plot(
  seq_len(N), E_downvar,
  type = "l",
  pch = 16,
  cex = 0.55,
  lwd = 1.8,
  xlab = "n",
  ylab = expression(E[n]),
  ylim = ylim_E_down,
  main = "Mean EWMA under downward variance shift"
)
add_grid()
abline(h = mu0, lty = 3, lwd = 2, col = "gray35")
abline(h = c(L_mean, U_mean), lty = 2, lwd = 2.5, col = "red")
add_shift_line(tau)
add_limit_labels(U_mean, L_mean)
add_ooc_points(seq_len(N), E_downvar, LCL = L_mean, UCL = U_mean, cex = 1.0)
legend(
  "topleft",
  legend = c("connected EWMA statistic", "EWMA limits", "OOC points"),
  lty = c(1, 2, NA),
  pch = c(16, NA, 21),
  pt.bg = c(NA, NA, rgb(1, 0, 0, 0.45)),
  col = c("black", "red", "red"),
  lwd = c(1.8, 2.5, NA),
  bty = "n",
  cex = 0.78
)

mtext(
  "Mean EWMA Chart under Variance Shifts",
  outer = TRUE,
  cex = 1.15,
  font = 2
)

dev.off()

#-----------------------------------------------------------
# 10. Dedicated variance EWMA, individual data
#     Add corresponding Shewhart variance charts
#-----------------------------------------------------------

# Standardized squared observations for variance monitoring
Y_up <- ((x_upvar - mu0) / sigma0)^2
Y_down <- ((x_downvar - mu0) / sigma0)^2

# Dedicated variance EWMA statistics
lambda_var <- 0.10
Ev_up <- variance_ewma_stat_individual(
  x_upvar,
  mu0 = mu0,
  sigma0 = sigma0,
  lambda = lambda_var
)

Ev_down <- variance_ewma_stat_individual(
  x_downvar,
  mu0 = mu0,
  sigma0 = sigma0,
  lambda = lambda_var
)

# EWMA variance-chart limits
limV <- variance_ewma_limits_individual(
  n = N,
  lambda = lambda_var,
  rhoU = 2.595,
  rhoL = 1.580,
  time_varying = FALSE
)

# Shewhart variance-chart limits for Y_n ~ chi-square(1)
# Two-sided 3-sigma-equivalent probability: alpha = 0.0027
alpha_shewhart <- 0.0027
Y_LCL <- qchisq(alpha_shewhart / 2, df = 1)
Y_CL  <- 1
Y_UCL <- qchisq(1 - alpha_shewhart / 2, df = 1)

# First signals
sigY_up   <- first_signal(Y_up,   LCL = Y_LCL,       UCL = Y_UCL)
sigY_down <- first_signal(Y_down, LCL = Y_LCL,       UCL = Y_UCL)
sigV_up   <- first_signal(Ev_up,                 UCL = limV$UCL)
sigV_down <- first_signal(Ev_down, LCL = limV$LCL)

# Teaching-style helper functions for this figure
add_var_shift_line <- function(tau) {
  abline(v = tau, lty = 3, lwd = 2, col = "gray35")
  mtext("variance shift", side = 3, at = tau, line = 0.15,
        cex = 0.68, col = "gray25")
}

highlight_first_ooc <- function(sig, y) {
  if (!is.na(sig)) {
    points(
      sig, y[sig],
      pch = 21,
      bg = rgb(1, 0, 0, 0.45),
      col = "red",
      cex = 1.65,
      lwd = 2.2
    )
  }
}

add_right_label <- function(y, label, col = "red", pos = 4) {
  usr <- par("usr")
  old_xpd <- par("xpd")
  par(xpd = NA)
  text(
    x = usr[2] + 0.012 * diff(usr[1:2]),
    y = y,
    labels = label,
    pos = pos,
    col = col,
    cex = 0.70
  )
  par(xpd = old_xpd)
}

png(
  file.path(fig_dir, "ch5_variance_ewma_dedicated.png"),
  width = 1600,
  height = 1100,
  res = 180
)

par(
  mfrow = c(2, 2),
  mar = c(4.2, 4.6, 3.0, 4.5),
  oma = c(0.3, 0.3, 2.2, 0.3)
)

#-------------------------
# Upward variance shift: Shewhart variance chart
#-------------------------
ylim_Y_up <- range(c(Y_up, Y_LCL, Y_UCL))
ylim_Y_up <- ylim_Y_up + c(-0.05, 0.12) * diff(ylim_Y_up)

plot(
  seq_len(N), Y_up,
  type = "l",
  pch = 16,
  cex = 0.52,
  lwd = 1.8,
  xlab = "n",
  ylab = expression(Y[n] == ((X[n]-mu[0])/sigma[0])^2),
  ylim = ylim_Y_up,
  main = expression("Shewhart variance chart: " * sigma * ": 1 " %->% " 2")
)

grid(col = "gray85", lty = 3)
abline(h = Y_CL,  lty = 3, lwd = 2.0, col = "gray35")
abline(h = c(Y_LCL, Y_UCL), lty = 2, lwd = 2.5, col = "red")
add_var_shift_line(tau)
add_ooc_points(seq_len(N), Y_up, LCL = Y_LCL, UCL = Y_UCL, cex = 1.0)
add_right_label(Y_UCL, paste0("UCL = ", round(Y_UCL, 2)))
add_right_label(Y_LCL, paste0("LCL = ", round(Y_LCL, 3)))

legend(
  "topleft",
  legend = c("connected Y[n]", "Shewhart limits", "center line", "OOC points"),
  lty = c(1, 2, 3, NA),
  pch = c(16, NA, NA, 21),
  pt.bg = c(NA, NA, NA, rgb(1, 0, 0, 0.45)),
  col = c("black", "red", "gray35", "red"),
  lwd = c(1.8, 2.5, 2.0, NA),
  bty = "n",
  cex = 0.72
)

#-------------------------
# Downward variance shift: Shewhart variance chart
#-------------------------
ylim_Y_down <- range(c(Y_down, Y_LCL, Y_UCL))
ylim_Y_down <- ylim_Y_down + c(-0.05, 0.12) * diff(ylim_Y_down)

plot(
  seq_len(N), Y_down,
  type = "l",
  pch = 16,
  cex = 0.52,
  lwd = 1.8,
  xlab = "n",
  ylab = expression(Y[n] == ((X[n]-mu[0])/sigma[0])^2),
  ylim = ylim_Y_down,
  main = expression("Shewhart variance chart: " * sigma * ": 1 " %->% " 0.5")
)

grid(col = "gray85", lty = 3)
abline(h = Y_CL,  lty = 3, lwd = 2.0, col = "gray35")
abline(h = c(Y_LCL, Y_UCL), lty = 2, lwd = 2.5, col = "red")
add_var_shift_line(tau)
add_ooc_points(seq_len(N), Y_down, LCL = Y_LCL, UCL = Y_UCL, cex = 1.0)
add_right_label(Y_UCL, paste0("UCL = ", round(Y_UCL, 2)))
add_right_label(Y_LCL, paste0("LCL = ", round(Y_LCL, 3)))

legend(
  "topleft",
  legend = c("connected Y[n]", "Shewhart limits", "center line", "OOC points"),
  lty = c(1, 2, 3, NA),
  pch = c(16, NA, NA, 21),
  pt.bg = c(NA, NA, NA, rgb(1, 0, 0, 0.45)),
  col = c("black", "red", "gray35", "red"),
  lwd = c(1.8, 2.5, 2.0, NA),
  bty = "n",
  cex = 0.72
)

#-------------------------
# Upward variance shift: dedicated EWMA variance chart
#-------------------------
ylim_Ev_up <- range(c(Ev_up, limV$LCL, limV$UCL))
ylim_Ev_up <- ylim_Ev_up + c(-0.08, 0.12) * diff(ylim_Ev_up)

plot(
  seq_len(N), Ev_up,
  type = "l",
  pch = 16,
  cex = 0.52,
  lwd = 1.8,
  xlab = "n",
  ylab = expression(E[n]),
  ylim = ylim_Ev_up,
  main = "Dedicated EWMA variance chart: upward shift"
)

grid(col = "gray85", lty = 3)
abline(h = 1, lty = 3, lwd = 2.0, col = "gray35")
abline(h = c(limV$LCL[1], limV$UCL[1]), lty = 2, lwd = 2.5, col = "red")
add_var_shift_line(tau)
add_ooc_points(seq_len(N), Ev_up, LCL = limV$LCL, UCL = limV$UCL, cex = 1.0)
add_right_label(limV$UCL[1], paste0("UCL = ", round(limV$UCL[1], 3)))
add_right_label(limV$LCL[1], paste0("LCL = ", round(limV$LCL[1], 3)))

legend(
  "topleft",
  legend = c("connected EWMA", "EWMA limits", "center line", "OOC points"),
  lty = c(1, 2, 3, NA),
  pch = c(16, NA, NA, 21),
  pt.bg = c(NA, NA, NA, rgb(1, 0, 0, 0.45)),
  col = c("black", "red", "gray35", "red"),
  lwd = c(1.8, 2.5, 2.0, NA),
  bty = "n",
  cex = 0.72
)

#-------------------------
# Downward variance shift: dedicated EWMA variance chart
#-------------------------
ylim_Ev_down <- range(c(Ev_down, limV$LCL, limV$UCL))
ylim_Ev_down <- ylim_Ev_down + c(-0.08, 0.12) * diff(ylim_Ev_down)

plot(
  seq_len(N), Ev_down,
  type = "l",
  pch = 16,
  cex = 0.52,
  lwd = 1.8,
  xlab = "n",
  ylab = expression(E[n]),
  ylim = ylim_Ev_down,
  main = "Dedicated EWMA variance chart: downward shift"
)

grid(col = "gray85", lty = 3)
abline(h = 1, lty = 3, lwd = 2.0, col = "gray35")
abline(h = c(limV$LCL[1], limV$UCL[1]), lty = 2, lwd = 2.5, col = "red")
add_var_shift_line(tau)
add_ooc_points(seq_len(N), Ev_down, LCL = limV$LCL, UCL = limV$UCL, cex = 1.0)
add_right_label(limV$UCL[1], paste0("UCL = ", round(limV$UCL[1], 3)))
add_right_label(limV$LCL[1], paste0("LCL = ", round(limV$LCL[1], 3)))

legend(
  "topleft",
  legend = c("connected EWMA", "EWMA limits", "center line", "OOC points"),
  lty = c(1, 2, 3, NA),
  pch = c(16, NA, NA, 21),
  pt.bg = c(NA, NA, NA, rgb(1, 0, 0, 0.45)),
  col = c("black", "red", "gray35", "red"),
  lwd = c(1.8, 2.5, 2.0, NA),
  bty = "n",
  cex = 0.72
)

mtext(
  "Shewhart Variance Chart versus Dedicated Variance EWMA",
  outer = TRUE,
  cex = 1.15,
  font = 2
)

dev.off()

#-----------------------------------------------------------
# 11. Joint monitoring by sample mean and sample variance EWMA
#-----------------------------------------------------------
sim_batch_case <- function(mu1, sigma1, n0 = 50, n1 = 50, m = 5) {
  x0 <- matrix(rnorm(n0 * m, 0, 1), nrow = n0, ncol = m)
  x1 <- matrix(rnorm(n1 * m, mu1, sigma1), nrow = n1, ncol = m)
  x <- rbind(x0, x1)
  data.frame(n = seq_len(nrow(x)), xbar = rowMeans(x), s2 = apply(x, 1, var))
}

cases <- list("N(1,1)" = c(1, 1), "N(0,2^2)" = c(0, 2),
              "N(1,2^2)" = c(1, 2), "N(0,0.5^2)" = c(0, 0.5))

png(file.path(fig_dir, "ch5_joint_ewma.png"), width = 1600, height = 1200, res = 180)
par(mfrow = c(4, 3), mar = c(3.4, 3.7, 2.3, 0.9), oma = c(0, 0, 1.8, 0))

lambda_joint <- 0.1
m_joint <- 5
rho_M <- 2.731
rho_V <- 2.836

# Shewhart limits for the subgroup mean under X ~ N(0,1)
U_Xbar <- 3 / sqrt(m_joint)
L_Xbar <- -U_Xbar

# EWMA mean limits
U_M <- rho_M * sqrt(lambda_joint / (2 - lambda_joint)) / sqrt(m_joint)
L_M <- -U_M

# EWMA variance limits for S_n^2, where Var(S_n^2)=2/(m-1) under sigma_0^2=1
sd_EV <- sqrt(2 * lambda_joint / ((2 - lambda_joint) * (m_joint - 1)))
U_V <- 1 + rho_V * sd_EV
L_V <- max(0, 1 - rho_V * sd_EV)

for (nm in names(cases)) {
  dat <- sim_batch_case(cases[[nm]][1], cases[[nm]][2], m = m_joint)
  EM <- ewma_stat(dat$xbar, lambda = lambda_joint, e0 = 0)
  EV <- ewma_stat(dat$s2, lambda = lambda_joint, e0 = 1)

  #-------------------------------------------------------
  # Shewhart chart for subgroup means
  #-------------------------------------------------------
  ylim_xbar <- range(dat$xbar, L_Xbar, U_Xbar, 0)
  plot(
    dat$n, dat$xbar,
    type = "o", pch = 16, cex = 0.35, lwd = 1.4,
    xlab = "n", ylab = expression(bar(X)[n]),
    main = paste0(nm, ": Shewhart mean chart"),
    ylim = ylim_xbar
  )
  abline(h = c(L_Xbar, U_Xbar), col = "red", lty = 2, lwd = 2.2)
  abline(h = 0, col = "gray35", lty = 3, lwd = 1.6)
  abline(v = 51, col = "gray35", lty = 3, lwd = 1.6)
  add_ooc_points(dat$n, dat$xbar, LCL = L_Xbar, UCL = U_Xbar, cex = 0.95)
  legend(
    "topleft",
    legend = c("connected data", "control limits", "center line", "OOC points"),
    col = c("black", "red", "gray35", "red"),
    lty = c(1, 2, 3, NA), lwd = c(1.4, 2.2, 1.6, NA),
    pch = c(16, NA, NA, 21), pt.bg = c(NA, NA, NA, rgb(1, 0, 0, 0.45)), pt.cex = 0.8,
    bty = "n", cex = 0.64
  )

  #-------------------------------------------------------
  # EWMA chart for subgroup means
  #-------------------------------------------------------
  ylim_EM <- range(EM, L_M, U_M, 0)
  plot(
    dat$n, EM,
    type = "o", pch = 16, cex = 0.35, lwd = 1.4,
    xlab = "n", ylab = expression(E[n,M]),
    main = "Mean EWMA chart",
    ylim = ylim_EM
  )
  abline(h = c(L_M, U_M), col = "red", lty = 2, lwd = 2.2)
  abline(h = 0, col = "gray35", lty = 3, lwd = 1.6)
  abline(v = 51, col = "gray35", lty = 3, lwd = 1.6)
  add_ooc_points(dat$n, EM, LCL = L_M, UCL = U_M, cex = 0.95)
  legend(
    "topleft",
    legend = c("connected EWMA", "control limits", "center line", "OOC points"),
    col = c("black", "red", "gray35", "red"),
    lty = c(1, 2, 3, NA), lwd = c(1.4, 2.2, 1.6, NA),
    pch = c(16, NA, NA, 21), pt.bg = c(NA, NA, NA, rgb(1, 0, 0, 0.45)), pt.cex = 0.8,
    bty = "n", cex = 0.64
  )

  #-------------------------------------------------------
  # EWMA chart for subgroup variances
  #-------------------------------------------------------
  ylim_EV <- range(EV, L_V, U_V, 1)
  plot(
    dat$n, EV,
    type = "o", pch = 16, cex = 0.35, lwd = 1.4,
    xlab = "n", ylab = expression(E[n,V]),
    main = "Variance EWMA chart",
    ylim = ylim_EV
  )
  abline(h = c(L_V, U_V), col = "red", lty = 2, lwd = 2.2)
  abline(h = 1, col = "gray35", lty = 3, lwd = 1.6)
  abline(v = 51, col = "gray35", lty = 3, lwd = 1.6)
  add_ooc_points(dat$n, EV, LCL = L_V, UCL = U_V, cex = 0.95)
  legend(
    "topleft",
    legend = c("connected EWMA", "control limits", "center line", "OOC points"),
    col = c("black", "red", "gray35", "red"),
    lty = c(1, 2, 3, NA), lwd = c(1.4, 2.2, 1.6, NA),
    pch = c(16, NA, NA, 21), pt.bg = c(NA, NA, NA, rgb(1, 0, 0, 0.45)), pt.cex = 0.8,
    bty = "n", cex = 0.64
  )
}

mtext(
  "Joint Monitoring by Shewhart Mean Chart, Mean EWMA, and Variance EWMA",
  outer = TRUE,
  cex = 1.15,
  font = 2
)

dev.off()

#-----------------------------------------------------------
# 12. Self-starting EWMA
#-----------------------------------------------------------
self_start_z <- function(x) {
  z <- rep(NA_real_, length(x))
  for (n in 3:length(x)) {
    xbar_prev <- mean(x[1:(n - 1)])
    s_prev <- sd(x[1:(n - 1)])
    tval <- sqrt((n - 1) / n) * (x[n] - xbar_prev) / s_prev
    z[n] <- qt(pt(tval, df = n - 2), df = Inf)
  }
  z
}

x_ss <- c(rnorm(10, 0, 1), rnorm(30, 0.5, 1))
z_ss <- self_start_z(x_ss)
E_ss <- ewma_stat(ifelse(is.na(z_ss), 0, z_ss), lambda = 0.05, e0 = 0)
E_conv <- ewma_stat(x_ss, lambda = 0.05, e0 = 0)
U_ss <- 2.216 * sqrt(0.05 / 1.95)

png(file.path(fig_dir, "ch5_self_starting_ewma.png"), width = 1600, height = 900, res = 180)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(x_ss, type = "l", pch = 16, cex = 0.65, xlab = "n", ylab = expression(X[n]), main = "Data stream"); abline(v = 11, lty = 3)
plot(E_ss, type = "l", lwd = 2, xlab = "n", ylab = "EWMA statistic", main = "Self-starting versus conventional")
lines(E_conv, lty = 3, lwd = 2)
abline(h = c(-U_ss, U_ss), lty = 2, lwd = 2)
abline(v = 11, lty = 3)
legend("topleft", legend = c("self-starting", "conventional", "limits"), lty = c(1, 3, 2), lwd = 2, bty = "n")
dev.off()

#-----------------------------------------------------------
# 13. Adaptive EWMA score functions
#-----------------------------------------------------------
x <- c(
  0.0, -2.3, 0.6, -1.0, -0.2, 0.3, 0.5, -0.4, 0.3, -0.5,
  -0.9, -0.5, 1.0, -0.9, 1.5, 1.4, 0.0, 2.2, 2.7, 0.7,
  -0.4, -0.8, -0.1, 0.0, 0.3, -1.0, 2.5, 1.3, 0.3, -0.3
)

n <- length(x)
mu0 <- 0

eta1 <- function(e, lambda, u) {
  ifelse(e < -u, e + (1 - lambda) * u,
         ifelse(e > u, e - (1 - lambda) * u, lambda * e))
}
eta2 <- function(e, lambda , u) {
  ifelse(abs(e) <= u, e * (1 - (1 - lambda) * (1 - (e / u)^2)^2), e)
}

e <- seq(-10, 10, length.out = 1001)
png(file.path(fig_dir, "ch5_aewma_score_functions.png"), width = 1600, height = 1100, res = 180)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(NULL, xlim = range(e), ylim = range(e), xlab = expression(e[n]), ylab = expression(eta[1](e[n])), main = expression(eta[1]))
for (u in c(2, 4, 6)) lines(e, eta1(e, u = u), lwd = 2, lty = match(u, c(2, 4, 6)))
abline(a = 0, b = 1, lty = 3); abline(a = 0, b = 0.1, lty = 2); legend("topleft", legend = paste0("u=", c(2, 4, 6)), lty = 1:3, lwd = 2, bty = "n")
plot(NULL, xlim = range(e), ylim = c(0, 1), xlab = expression(e[n]), ylab = expression(w[1](e[n])), main = expression(w[1] == eta[1]/e))
for (u in c(2, 4, 6)) lines(e, eta1(e, u = u) / ifelse(e == 0, NA, e), lwd = 2, lty = match(u, c(2, 4, 6)))
abline(h = c(0.1, 1), lty = c(2, 3))
plot(NULL, xlim = range(e), ylim = range(e), xlab = expression(e[n]), ylab = expression(eta[2](e[n])), main = expression(eta[2]))
for (u in c(2, 4, 6)) lines(e, eta2(e, u = u), lwd = 2, lty = match(u, c(2, 4, 6)))
abline(a = 0, b = 1, lty = 3); abline(a = 0, b = 0.1, lty = 2)
plot(NULL, xlim = range(e), ylim = c(0, 1), xlab = expression(e[n]), ylab = expression(w[2](e[n])), main = expression(w[2] == eta[2]/e))
for (u in c(2, 4, 6)) lines(e, eta2(e, u = u) / ifelse(e == 0, NA, e), lwd = 2, lty = match(u, c(2, 4, 6)))
abline(h = c(0.1, 1), lty = c(2, 3))
dev.off()

#-----------------------------------------------------------
# 14. AEWMA comparison: small and large shifts
#-----------------------------------------------------------
aewma_stat <- function(x, lambda, u, eta_fun, a0 = 0) {
  A <- numeric(length(x))
  prev <- a0
  for (i in seq_along(x)) {
    err <- x[i] - prev
    A[i] <- prev + eta_fun(err, lambda = lambda, u = u)
    prev <- A[i]
  }
  A
}

first_signal <- function(A, h, mu0 = 0) {
  id <- which(abs(A - mu0) > h)
  
  if (length(id) == 0) {
    return(NA)
  } else {
    return(id[1])
  }
}

# ------------------------------------------------------------
# Exercise 5.20: Adaptive EWMA chart
# ------------------------------------------------------------

x <- c(
  0.0, -2.3, 0.6, -1.0, -0.2, 0.3, 0.5, -0.4, 0.3, -0.5,
  -0.9, -0.5, 1.0, -0.9, 1.5, 1.4, 0.0, 2.2, 2.7, 0.7,
  -0.4, -0.8, -0.1, 0.0, 0.3, -1.0, 2.5, 1.3, 0.3, -0.3
)

n <- length(x)
mu0 <- 0

# ------------------------------------------------------------
# Score functions
# ------------------------------------------------------------

eta1 <- function(e, lambda, u) {
  ifelse(
    e < -u,
    e + (1 - lambda) * u,
    ifelse(
      e > u,
      e - (1 - lambda) * u,
      lambda * e
    )
  )
}

eta2 <- function(e, lambda, u) {
  ifelse(
    abs(e) <= u,
    e * (1 - (1 - lambda) * (1 - (e / u)^2)^2),
    e
  )
}

# ------------------------------------------------------------
# Adaptive EWMA statistic
# ------------------------------------------------------------

aewma_stat <- function(x, lambda, u, eta_fun, a0 = 0) {
  
  A <- numeric(length(x))
  prev <- a0
  
  for (i in seq_along(x)) {
    e <- x[i] - prev
    A[i] <- prev + eta_fun(e, lambda = lambda, u = u)
    prev <- A[i]
  }
  
  return(A)
}

first_signal <- function(A, h, mu0 = 0) {
  id <- which(abs(A - mu0) > h)
  
  if (length(id) == 0) {
    return(NA)
  } else {
    return(id[1])
  }
}

# ------------------------------------------------------------
# Parameter settings from Table 5.6
# ------------------------------------------------------------

setting_table <- data.frame(
  case = c("(i)", "(ii)", "(iii)", "(iv)", "(v)", "(vi)"),
  eta = c("eta1", "eta1", "eta1", "eta2", "eta2", "eta2"),
  mu1 = c(0.25, 1.00, 0.25, 0.25, 1.00, 0.25),
  mu2 = c(4, 4, 6, 4, 4, 6),
  h = c(0.1471, 0.7874, 0.1515, 0.3542, 0.7697, 0.1430),
  lambda = c(0.0162, 0.1813, 0.0207, 0.0188, 0.1359, 0.0168),
  u = c(2.7459, 2.5752, 4.2537, 12.6145, 10.6114, 48.7509)
)

result_summary <- data.frame()
aewma_list <- list()

for (j in 1:nrow(setting_table)) {
  
  eta_name <- setting_table$eta[j]
  
  eta_fun <- if (eta_name == "eta1") {
    eta1
  } else {
    eta2
  }
  
  A <- aewma_stat(
    x = x,
    lambda = setting_table$lambda[j],
    u = setting_table$u[j],
    eta_fun = eta_fun,
    a0 = mu0
  )
  
  signal <- first_signal(
    A = A,
    h = setting_table$h[j],
    mu0 = mu0
  )
  
  aewma_list[[setting_table$case[j]]] <- A
  
  result_summary <- rbind(
    result_summary,
    data.frame(
      case = setting_table$case[j],
      eta = eta_name,
      mu1 = setting_table$mu1[j],
      mu2 = setting_table$mu2[j],
      h = setting_table$h[j],
      lambda = setting_table$lambda[j],
      u = setting_table$u[j],
      first_signal = signal
    )
  )
}

result_summary

png(
  filename = "figs/ch5_exercise_5_20_AEWMA_3x2.png",
  width = 1600,
  height = 1800,
  res = 180
)

par(mfrow = c(3, 2),
    mar = c(4.2, 4.5, 3.2, 1.2))

for (j in 1:nrow(setting_table)) {
  
  case_now <- setting_table$case[j]
  A <- aewma_list[[case_now]]
  h <- setting_table$h[j]
  signal <- result_summary$first_signal[j]
  
  plot(
    1:n,
    A,
    type = "b",
    pch = 16,
    lwd = 2,
    xlab = "n",
    ylab = expression(A[n]),
    ylim = range(c(A, -h, h)),
    main = paste0(
      case_now,
      ": ",
      setting_table$eta[j],
      ", (mu1, mu2)=(",
      setting_table$mu1[j],
      ", ",
      setting_table$mu2[j],
      ")"
    )
  )
  
  abline(h = c(-h, h), col = "red", lty = 2, lwd = 2)
  abline(h = 0, lty = 3, lwd = 2)
  
  if (!is.na(signal)) {
    points(signal, A[signal], pch = 21, bg = "orange", cex = 1.8)
    text(signal, A[signal], labels = paste0("n=", signal),
         pos = 4, cex = 0.8)
  }
}

dev.off()

x_small <- c(rnorm(50, 0, 1), rnorm(50, 0.5, 1))
x_large <- c(rnorm(50, 0, 1), rnorm(50, 2, 1))
E_small_slow <- ewma_stat(x_small, lambda = 0.01, e0 = 0)
E_small_fast <- ewma_stat(x_small, lambda = 0.5, e0 = 0)
A_small <- aewma_stat(x_small)
E_large_slow <- ewma_stat(x_large, lambda = 0.01, e0 = 0)
E_large_fast <- ewma_stat(x_large, lambda = 0.5, e0 = 0)
A_large <- aewma_stat(x_large)
U_slow <- 1.973 * sqrt(0.01 / 1.99)
U_fast <- 3.071 * sqrt(0.5 / 1.5)
h_a <- 0.4306

png(file.path(fig_dir, "ch5_aewma_comparison.png"), width = 1600, height = 1200, res = 180)
par(mfrow = c(4, 2), mar = c(3.2, 3.8, 2.2, 1))

plot(x_small, type = "l", pch = 16, cex = 0.45,
     xlab = "n", ylab = expression(X[n]), main = "Small shift: delta=0.5")
abline(v = 51, lty = 3)

plot(x_large, type = "l", pch = 16, cex = 0.45,
     xlab = "n", ylab = expression(X[n]), main = "Large shift: delta=2.0")
abline(v = 51, lty = 3)

plot(E_small_slow, type = "l", lwd = 2,
     xlab = "n", ylab = expression(E[n]), main = "EWMA lambda=0.01")
abline(h = c(-U_slow, U_slow), col = "red", lty = 2, lwd = 2)
abline(v = 51, lty = 3)
add_ooc_points(seq_along(E_small_slow), E_small_slow, LCL = -U_slow, UCL = U_slow, cex = 1.0)

plot(E_large_slow, type = "l", lwd = 2,
     xlab = "n", ylab = expression(E[n]), main = "EWMA lambda=0.01")
abline(h = c(-U_slow, U_slow), col = "red", lty = 2, lwd = 2)
abline(v = 51, lty = 3)
add_ooc_points(seq_along(E_large_slow), E_large_slow, LCL = -U_slow, UCL = U_slow, cex = 1.0)

plot(E_small_fast, type = "l", lwd = 2,
     xlab = "n", ylab = expression(E[n]), main = "EWMA lambda=0.5")
abline(h = c(-U_fast, U_fast), col = "red", lty = 2, lwd = 2)
abline(v = 51, lty = 3)
add_ooc_points(seq_along(E_small_fast), E_small_fast, LCL = -U_fast, UCL = U_fast, cex = 1.0)

plot(E_large_fast, type = "l", lwd = 2,
     xlab = "n", ylab = expression(E[n]), main = "EWMA lambda=0.5")
abline(h = c(-U_fast, U_fast), col = "red", lty = 2, lwd = 2)
abline(v = 51, lty = 3)
add_ooc_points(seq_along(E_large_fast), E_large_fast, LCL = -U_fast, UCL = U_fast, cex = 1.0)

plot(A_small, type = "l", lwd = 2,
     xlab = "n", ylab = expression(A[n]), main = "AEWMA eta1")
abline(h = c(-h_a, h_a), col = "red", lty = 2, lwd = 2)
abline(v = 51, lty = 3)
add_ooc_points(seq_along(A_small), A_small, LCL = -h_a, UCL = h_a, cex = 1.0)

plot(A_large, type = "l", lwd = 2,
     xlab = "n", ylab = expression(A[n]), main = "AEWMA eta1")
abline(h = c(-h_a, h_a), col = "red", lty = 2, lwd = 2)
abline(v = 51, lty = 3)
add_ooc_points(seq_along(A_large), A_large, LCL = -h_a, UCL = h_a, cex = 1.0)

dev.off()

#-----------------------------------------------------------
# 15. Numerical summary for slides
#-----------------------------------------------------------
summary_table <- data.frame(
  item = c("Example 5.1 mean EWMA", "Upward variance EWMA", "Downward variance EWMA"),
  first_signal = c(sig51, sigV_up, sigV_down)
)
write.csv(summary_table, file.path(out_dir, "ch5_example_signal_summary.csv"), row.names = FALSE)

rho_table_mean <- data.frame(
  ARL0 = c(50, 100, 200, 300, 370, 400, 500, 1000),
  lambda_0.05 = c(1.520, 1.879, 2.216, 2.399, 2.490, 2.523, 2.615, 2.884),
  lambda_0.10 = c(1.811, 2.148, 2.454, 2.619, 2.701, 2.731, 2.814, 3.059),
  lambda_0.20 = c(2.054, 2.360, 2.635, 2.785, 2.859, 2.886, 2.962, 3.187),
  lambda_0.50 = c(2.268, 2.534, 2.777, 2.911, 2.978, 3.002, 3.071, 3.277)
)
write.csv(rho_table_mean, file.path(out_dir, "ch5_mean_ewma_rho_table.csv"), row.names = FALSE)




#-----------------------------------------------------------
# 16. EWMA with correlated observations: AR(1) examples
#     Connected data trajectories using type = "o"
#-----------------------------------------------------------

#===========================================================
# Folders
#===========================================================
fig_dir <- "figs"
out_dir <- "outputs"

if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

#===========================================================
# Basic functions
#===========================================================
simulate_ar1 <- function(n, mu0 = 0, phi = 0.5, sigma_x = 1) {
  
  sigma_e <- sigma_x * sqrt(1 - phi^2)
  
  x <- numeric(n)
  x[1] <- rnorm(1, mean = mu0, sd = sigma_x)
  
  for (i in 2:n) {
    x[i] <- mu0 + phi * (x[i - 1] - mu0) + rnorm(1, 0, sigma_e)
  }
  
  x
}

ewma_stat <- function(x, lambda, e0 = 0) {
  
  e <- numeric(length(x))
  prev <- e0
  
  for (i in seq_along(x)) {
    e[i] <- lambda * x[i] + (1 - lambda) * prev
    prev <- e[i]
  }
  
  e
}

first_signal <- function(stat, LCL, UCL) {
  
  idx <- which(stat < LCL | stat > UCL)
  
  if (length(idx) == 0) {
    NA_integer_
  } else {
    idx[1]
  }
}

ewma_sd_ar1 <- function(lambda, phi, sigma_x = 1, M = 500) {
  
  s <- 1:M
  
  ratio <- lambda / (2 - lambda) *
    (1 + 2 * sum((phi^s) * ((1 - lambda)^s)))
  
  sigma_x * sqrt(ratio)
}

#===========================================================
# Settings
#===========================================================
n <- 150
mu0 <- 0
sigma_x <- 1

lambda <- 0.10
rho <- 2.454

phi_values <- c(
  "negative_phi" = -0.5,
  "independent"  =  0.0,
  "positive_phi" =  0.5
)

U_ind <- rho * sqrt(lambda / (2 - lambda)) * sigma_x
L_ind <- -U_ind

#===========================================================
# Simulate example paths
#===========================================================
example_list <- list()

for (nm in names(phi_values)) {
  
  phi <- phi_values[nm]
  
  x <- simulate_ar1(
    n = n,
    mu0 = mu0,
    phi = phi,
    sigma_x = sigma_x
  )
  
  e <- ewma_stat(
    x,
    lambda = lambda,
    e0 = mu0
  )
  
  sd_adj <- ewma_sd_ar1(
    lambda = lambda,
    phi = phi,
    sigma_x = sigma_x
  )
  
  U_adj <- rho * sd_adj
  L_adj <- -U_adj
  
  example_list[[nm]] <- data.frame(
    n = seq_len(n),
    phi = phi,
    X = x,
    E = e,
    L_ind = L_ind,
    U_ind = U_ind,
    L_adj = L_adj,
    U_adj = U_adj
  )
}

example_dat <- do.call(rbind, example_list)

write.csv(
  example_dat,
  file.path(out_dir, "ch5_correlated_ewma_example_data.csv"),
  row.names = FALSE
)

#===========================================================
# Figure 1: EWMA paths using usual independence limits
#===========================================================
png(
  file.path(fig_dir, "ch5_ewma_correlated_observations.png"),
  width = 1600,
  height = 1000,
  res = 180
)

par(
  mfrow = c(3, 1),
  mar = c(3.5, 4.5, 2.6, 1.0),
  oma = c(0, 0, 2.0, 0)
)

for (nm in names(example_list)) {
  
  dat <- example_list[[nm]]
  phi <- unique(dat$phi)
  
  ylim_now <- range(c(dat$E, dat$L_ind, dat$U_ind))
  ylim_now <- ylim_now + c(-0.15, 0.15) * diff(ylim_now)
  
  plot(
    dat$n,
    dat$E,
    type = "l",
    pch = 16,
    cex = 0.45,
    lwd = 1.8,
    xlab = "n",
    ylab = expression(E[n]),
    ylim = ylim_now,
    main = bquote("AR(1) EWMA with usual limits: " ~ phi == .(phi))
  )
  
  grid(col = "gray85", lty = 3)
  
  abline(h = 0, lty = 3, lwd = 2, col = "gray35")
  abline(h = c(L_ind, U_ind), lty = 2, lwd = 2.4, col = "red")
  
  sig <- first_signal(dat$E, LCL = L_ind, UCL = U_ind)
  
  if (!is.na(sig)) {
    points(
      sig,
      dat$E[sig],
      pch = 21,
      bg = "yellow",
      col = "red",
      cex = 1.6,
      lwd = 2
    )
  }
  
  legend(
    "topright",
    legend = c("EWMA statistic", "usual limits", "OOC points"),
    lty = c(1, 2, NA),
    pch = c(16, NA, 21),
    pt.bg = c(NA, NA, rgb(1, 0, 0, 0.45)),
    col = c("black", "red", "red"),
    lwd = c(1.8, 2.4, NA),
    bty = "n",
    cex = 0.85
  )
}

mtext(
  "Effect of Correlation on EWMA with Usual Independence Limits",
  outer = TRUE,
  cex = 1.15,
  font = 2
)

dev.off()

#===========================================================
# Figure 2: Usual limits versus correlation-adjusted limits
#===========================================================
png(
  file.path(fig_dir, "ch5_ewma_adjusted_limits_ar1.png"),
  width = 1600,
  height = 1000,
  res = 180
)

par(
  mfrow = c(3, 1),
  mar = c(3.5, 4.5, 2.6, 1.0),
  oma = c(0, 0, 2.0, 0)
)

for (nm in names(example_list)) {
  
  dat <- example_list[[nm]]
  phi <- unique(dat$phi)
  
  ylim_now <- range(c(dat$E, dat$L_adj, dat$U_adj, dat$L_ind, dat$U_ind))
  ylim_now <- ylim_now + c(-0.15, 0.15) * diff(ylim_now)
  
  plot(
    dat$n,
    dat$E,
    type = "o",
    pch = 16,
    cex = 0.45,
    lwd = 1.8,
    xlab = "n",
    ylab = expression(E[n]),
    ylim = ylim_now,
    main = bquote("Usual versus adjusted limits: " ~ phi == .(phi))
  )
  
  grid(col = "gray85", lty = 3)
  
  abline(h = 0, lty = 3, lwd = 2, col = "gray35")
  abline(h = c(L_ind, U_ind), lty = 2, lwd = 2.2, col = "red")
  abline(h = c(dat$L_adj[1], dat$U_adj[1]), lty = 4, lwd = 2.2, col = "blue")
  add_ooc_points(dat$n, dat$E, LCL = L_ind, UCL = U_ind, cex = 1.05)

  legend(
    "topright",
    legend = c("EWMA statistic", "usual limits", "adjusted limits"),
    lty = c(1, 2, 4),
    pch = c(16, NA, NA),
    col = c("black", "red", "blue"),
    lwd = c(1.8, 2.2, 2.2),
    bty = "n",
    cex = 0.85
  )
}

mtext(
  "EWMA Control Limits under AR(1) Correlation",
  outer = TRUE,
  cex = 1.15,
  font = 2
)

dev.off()

#===========================================================
# ARL0 simulation under AR(1) dependence
#===========================================================
sim_arl0_ar1 <- function(phi,
                         lambda = 0.1,
                         rho = 2.454,
                         nrep = 1000,
                         maxn = 10000,
                         adjusted = FALSE) {
  
  sigma_x <- 1
  
  if (adjusted) {
    U <- rho * ewma_sd_ar1(
      lambda = lambda,
      phi = phi,
      sigma_x = sigma_x
    )
  } else {
    U <- rho * sqrt(lambda / (2 - lambda)) * sigma_x
  }
  
  L <- -U
  
  rl <- integer(nrep)
  
  for (r in seq_len(nrep)) {
    
    x_prev <- rnorm(1, 0, sigma_x)
    e_prev <- 0
    sigma_e <- sigma_x * sqrt(1 - phi^2)
    
    for (i in seq_len(maxn)) {
      
      x_now <- phi * x_prev + rnorm(1, 0, sigma_e)
      e_now <- lambda * x_now + (1 - lambda) * e_prev
      
      if (e_now < L || e_now > U) {
        rl[r] <- i
        break
      }
      
      x_prev <- x_now
      e_prev <- e_now
    }
    
    if (rl[r] == 0) {
      rl[r] <- maxn
    }
  }
  
  mean(rl)
}

phi_grid <- c(-0.7, -0.5, -0.3, 0, 0.3, 0.5, 0.7)

arl_corr_table <- data.frame(
  phi = phi_grid,
  ARL0_usual_limits = NA_real_,
  ARL0_adjusted_limits = NA_real_
)

for (j in seq_along(phi_grid)) {
  
  arl_corr_table$ARL0_usual_limits[j] <-
    sim_arl0_ar1(
      phi = phi_grid[j],
      lambda = lambda,
      rho = rho,
      nrep = 1000,
      adjusted = FALSE
    )
  
  arl_corr_table$ARL0_adjusted_limits[j] <-
    sim_arl0_ar1(
      phi = phi_grid[j],
      lambda = lambda,
      rho = rho,
      nrep = 1000,
      adjusted = TRUE
    )
}

write.csv(
  arl_corr_table,
  file.path(out_dir, "ch5_arl0_ar1_correlation.csv"),
  row.names = FALSE
)

install.packages("spc")
library(spc)

setting_table <- data.frame(
  case = c("(i)", "(ii)", "(iii)", "(iv)"),
  lambda = c(0.1, 0.1, 0.5, 0.5),
  rho = c(1, 2, 1, 2)
)

setting_table$ARL0 <- mapply(
  function(lambda, rho) {
    xewma.arl(
      l = lambda,
      c = rho,
      mu = 0,
      sided = "two"
    )
  },
  setting_table$lambda,
  setting_table$rho
)

setting_table

#===========================================================
# Figure 3: ARL0 under correlation
#===========================================================
png(
  file.path(fig_dir, "ch5_arl0_ar1_correlation.png"),
  width = 1600,
  height = 900,
  res = 180
)

plot(
  arl_corr_table$phi,
  arl_corr_table$ARL0_usual_limits,
  type = "o",
  pch = 16,
  cex = 1.0,
  lwd = 2.5,
  xlab = expression(phi),
  ylab = expression(ARL[0]),
  ylim = range(arl_corr_table[, -1], na.rm = TRUE),
  main = expression(ARL[0] ~ "of EWMA under AR(1) correlation")
)

grid(col = "gray85", lty = 3)

lines(
  arl_corr_table$phi,
  arl_corr_table$ARL0_adjusted_limits,
  type = "o",
  pch = 17,
  cex = 1.0,
  lwd = 2.5,
  lty = 2
)

abline(
  h = 200,
  lty = 3,
  lwd = 2,
  col = "gray35"
)

legend(
  "topright",
  legend = c("usual limits", "adjusted limits", "target ARL0 = 200"),
  lty = c(1, 2, 3),
  pch = c(16, 17, NA),
  col = c("black", "black", "gray35"),
  lwd = c(2.5, 2.5, 2),
  bty = "n"
)

dev.off()



# ============================================================
# Self-starting EWMA example
# Output: figs/ch5_self_starting_ewma.png
# ============================================================


# -----------------------------
# 1. Simulation setting
# -----------------------------
n  <- 80
tau <- 40

mu0 <- 10
sigma0 <- 2
mu1 <- 11.2

lambda <- 0.10
rho <- 2.70

x <- c(
  rnorm(tau, mean = mu0, sd = sigma0),
  rnorm(n - tau, mean = mu1, sd = sigma0)
)







message("Done. Figures saved in figs/ and numerical summaries saved in outputs/.")
