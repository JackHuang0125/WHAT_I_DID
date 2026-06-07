# ============================================================
# Self-starting Q chart + Self-starting EWMA chart
#
# Teaching style:
#   Q chart    -> type = "b"
#   EWMA chart -> type = "l"
#
# Save outputs in:
#   figs/
#   outputs/
# ============================================================

rm(list = ls())

setwd("D:/統計品質管制/Week14")

set.seed(2026)

# ------------------------------------------------------------
# 1. Create folders
# ------------------------------------------------------------
dir.create("figs", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

# ------------------------------------------------------------
# 2. Simulation setting
# ------------------------------------------------------------
n <- 80
tau <- 40

mu0 <- 10
sigma0 <- 2
mu1 <- 11.2

lambda <- 0.10
rho <- 2.70

# ------------------------------------------------------------
# 3. Generate process data
# ------------------------------------------------------------
x <- c(
  rnorm(tau, mean = mu0, sd = sigma0),
  rnorm(n - tau, mean = mu1, sd = sigma0)
)

# ------------------------------------------------------------
# 4. Self-starting Q statistics
# ------------------------------------------------------------
Q <- rep(NA_real_, n)

# Self-starting EWMA statistics
E_SS <- rep(NA_real_, n)

E_SS[2] <- 0

for (i in 3:n) {
  
  xbar_prev <- mean(x[1:(i - 1)])
  s_prev <- sd(x[1:(i - 1)])
  
  T_i <- (x[i] - xbar_prev) / s_prev
  
  Q[i] <- qnorm(
    pt(
      sqrt((i - 1) / i) * T_i,
      df = i - 2
    )
  )
  
  E_SS[i] <- lambda * Q[i] +
    (1 - lambda) * E_SS[i - 1]
}

# ------------------------------------------------------------
# 5. Q-chart limits
# ------------------------------------------------------------
Q_UCL <- 3
Q_CL  <- 0
Q_LCL <- -3

Q_signal_id <- which(Q > Q_UCL | Q < Q_LCL)

Q_first_signal <- ifelse(
  length(Q_signal_id) > 0,
  Q_signal_id[1],
  NA
)

# ------------------------------------------------------------
# 6. EWMA limits
# ------------------------------------------------------------
sigma_E <- sqrt(lambda / (2 - lambda))

EWMA_UCL <- rho * sigma_E
EWMA_CL  <- 0
EWMA_LCL <- -EWMA_UCL

EWMA_signal_id <- which(
  E_SS > EWMA_UCL | E_SS < EWMA_LCL
)

EWMA_first_signal <- ifelse(
  length(EWMA_signal_id) > 0,
  EWMA_signal_id[1],
  NA
)

# ============================================================
# 7. Self-starting Q chart
# ============================================================
png(
  filename = "figs/ch5_self_starting_Q_chart.png",
  width = 1600,
  height = 900,
  res = 180
)

par(mar = c(4.8, 5.0, 3.0, 1.5))

plot(
  1:n,
  Q,
  type = "b",          # Teaching style for Q chart
  pch = 19,
  cex = 0.65,
  lwd = 1.5,
  xlab = "Sample index",
  ylab = expression(Q[n]),
  main = "Self-Starting Q Chart",
  ylim = range(c(Q, Q_UCL, Q_LCL), na.rm = TRUE) +
    c(-0.35, 0.35)
)

abline(h = Q_UCL, col = "red", lwd = 2)
abline(h = Q_LCL, col = "red", lwd = 2)
abline(h = Q_CL, lty = 2, lwd = 1.5)

abline(v = tau, col = "gray40", lty = 3, lwd = 2)

text(
  tau + 2,
  max(Q, na.rm = TRUE),
  labels = "Mean shift starts",
  pos = 4,
  cex = 0.85
)

if (!is.na(Q_first_signal)) {
  
  points(
    Q_first_signal,
    Q[Q_first_signal],
    pch = 21,
    bg = "orange",
    col = "black",
    cex = 1.6,
    lwd = 1.2
  )
  
  text(
    Q_first_signal,
    Q[Q_first_signal],
    labels = " First signal",
    pos = 4,
    cex = 0.85
  )
}

legend(
  "topleft",
  legend = c(
    expression(Q[n]),
    "3-sigma limits",
    "Center line",
    "Mean shift"
  ),
  col = c("black", "red", "black", "gray40"),
  lty = c(1, 1, 2, 3),
  pch = c(19, NA, NA, NA),
  lwd = c(1.5, 2, 1.5, 2),
  bty = "n",
  cex = 0.85
)

dev.off()

# ============================================================
# 8. Self-starting EWMA chart
# ============================================================
png(
  filename = "figs/ch5_self_starting_ewma.png",
  width = 1600,
  height = 900,
  res = 180
)

par(mar = c(4.8, 5.0, 3.0, 1.5))

plot(
  1:n,
  E_SS,
  type = "l",          # Teaching style for EWMA
  lwd = 2,
  xlab = "Sample index",
  ylab = expression(E[n,SS]),
  main = "Self-Starting EWMA Chart",
  ylim = range(c(E_SS, EWMA_UCL, EWMA_LCL), na.rm = TRUE) +
    c(-0.25, 0.25)
)

abline(h = EWMA_UCL, col = "red", lwd = 2)
abline(h = EWMA_LCL, col = "red", lwd = 2)
abline(h = EWMA_CL, lty = 2, lwd = 1.5)

abline(v = tau, col = "gray40", lty = 3, lwd = 2)

text(
  tau + 2,
  max(E_SS, na.rm = TRUE),
  labels = "Mean shift starts",
  pos = 4,
  cex = 0.85
)

if (!is.na(EWMA_first_signal)) {
  
  points(
    EWMA_first_signal,
    E_SS[EWMA_first_signal],
    pch = 21,
    bg = "orange",
    col = "black",
    cex = 1.6,
    lwd = 1.2
  )
  
  text(
    EWMA_first_signal,
    E_SS[EWMA_first_signal],
    labels = " First signal",
    pos = 4,
    cex = 0.85
  )
}

legend(
  "topleft",
  legend = c(
    "EWMA statistic",
    "Control limits",
    "Center line",
    "Mean shift"
  ),
  col = c("black", "red", "black", "gray40"),
  lty = c(1, 1, 2, 3),
  lwd = c(2, 2, 1.5, 2),
  bty = "n",
  cex = 0.85
)

dev.off()

# ============================================================
# 9. Combined comparison figure
# ============================================================
png(
  filename = "figs/ch5_Q_vs_EWMA.png",
  width = 1600,
  height = 1200,
  res = 180
)

par(
  mfrow = c(2, 1),
  mar = c(4.2, 5.0, 3.0, 1.5)
)

# ---------------- Q chart ----------------
plot(
  1:n,
  Q,
  type = "b",
  pch = 19,
  cex = 0.60,
  lwd = 1.4,
  xlab = "Sample index",
  ylab = expression(Q[n]),
  main = "Self-Starting Q Chart",
  ylim = range(c(Q, Q_UCL, Q_LCL), na.rm = TRUE) +
    c(-0.35, 0.35)
)

abline(h = Q_UCL, col = "red", lwd = 2)
abline(h = Q_LCL, col = "red", lwd = 2)
abline(h = Q_CL, lty = 2, lwd = 1.5)

abline(v = tau, col = "gray40", lty = 3, lwd = 2)

if (!is.na(Q_first_signal)) {
  
  points(
    Q_first_signal,
    Q[Q_first_signal],
    pch = 21,
    bg = "orange",
    col = "black",
    cex = 1.5
  )
}

# ---------------- EWMA chart ----------------
plot(
  1:n,
  E_SS,
  type = "l",
  lwd = 2,
  xlab = "Sample index",
  ylab = expression(E[n,SS]),
  main = "Self-Starting EWMA Chart",
  ylim = range(c(E_SS, EWMA_UCL, EWMA_LCL), na.rm = TRUE) +
    c(-0.25, 0.25)
)

abline(h = EWMA_UCL, col = "red", lwd = 2)
abline(h = EWMA_LCL, col = "red", lwd = 2)
abline(h = EWMA_CL, lty = 2, lwd = 1.5)

abline(v = tau, col = "gray40", lty = 3, lwd = 2)

if (!is.na(EWMA_first_signal)) {
  
  points(
    EWMA_first_signal,
    E_SS[EWMA_first_signal],
    pch = 21,
    bg = "orange",
    col = "black",
    cex = 1.5
  )
}

dev.off()

# ============================================================
# 10. Save results
# ============================================================
result_table <- data.frame(
  n = 1:n,
  X = round(x, 4),
  Q = round(Q, 4),
  EWMA_SS = round(E_SS, 4),
  Q_signal = ifelse(
    Q > Q_UCL | Q < Q_LCL,
    "Yes",
    "No"
  ),
  EWMA_signal = ifelse(
    E_SS > EWMA_UCL | E_SS < EWMA_LCL,
    "Yes",
    "No"
  )
)

write.csv(
  result_table,
  file = "outputs/ch5_self_starting_results.csv",
  row.names = FALSE
)

summary_table <- data.frame(
  Chart = c("Q chart", "EWMA"),
  First_signal = c(
    Q_first_signal,
    EWMA_first_signal
  )
)

write.csv(
  summary_table,
  file = "outputs/ch5_self_starting_summary.csv",
  row.names = FALSE
)

# ============================================================
# 11. Console output
# ============================================================
cat("====================================\n")
cat("Self-starting monitoring charts\n")
cat("====================================\n\n")

cat("Figures saved in figs/\n")
cat("- ch5_self_starting_Q_chart.png\n")
cat("- ch5_self_starting_ewma.png\n")
cat("- ch5_Q_vs_EWMA.png\n\n")

cat("Tables saved in outputs/\n")
cat("- ch5_self_starting_results.csv\n")
cat("- ch5_self_starting_summary.csv\n\n")

cat("First Q-chart signal    :", Q_first_signal, "\n")
cat("First EWMA signal       :", EWMA_first_signal, "\n")