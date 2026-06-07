############################################################
# Normal vs Exact p Chart
# alpha = 0.0027
# Consistent notation:
#   Exact upper quantile count: k_U = min{k : F(k) >= 1-alpha/2}
#   UCL = k_U / m
#   Signal rule: X > k_U
############################################################

rm(list = ls())

setwd("E:/統計品質管制/Week11")

dir.create("figs", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

#----------------------------------------------------------
# 1. Setting
#----------------------------------------------------------
m     <- 25
pi0   <- 0.05
alpha <- 0.0027
z     <- qnorm(1 - alpha / 2)

x   <- 0:m
p_x <- x / m

# Teaching monitoring data
X_obs <- c(0, 1, 2, 1, 3, 0, 1, 2, 0, 4,
           1, 0, 2, 1, 3, 0, 1, 5, 2, 1)

i     <- seq_along(X_obs)
p_obs <- X_obs / m

#----------------------------------------------------------
# 2. Normal approximation limits
#----------------------------------------------------------
L_norm_raw <- pi0 - z * sqrt(pi0 * (1 - pi0) / m)
U_norm_raw <- pi0 + z * sqrt(pi0 * (1 - pi0) / m)

L_norm <- max(0, L_norm_raw)
U_norm <- min(1, U_norm_raw)

# Signal rule: p < LCL or p > UCL
alpha_norm_actual <-
  pbinom(floor(m * L_norm), size = m, prob = pi0) +
  (1 - pbinom(floor(m * U_norm), size = m, prob = pi0))

#----------------------------------------------------------
# 3. Exact binomial limits
#----------------------------------------------------------
q_L <- alpha / 2
q_U <- 1 - alpha / 2

# Lower exact quantile count:
# largest k such that F(k) <= alpha/2.
# If none exists, use LCL = 0 and lower signal probability = 0.
lower_candidates <- x[pbinom(x, size = m, prob = pi0) <= q_L]

if (length(lower_candidates) == 0) {
  k_L <- 0
  L_exact <- 0
  alpha_L_exact <- 0
} else {
  k_L <- max(lower_candidates)
  L_exact <- k_L / m
  alpha_L_exact <- pbinom(k_L, size = m, prob = pi0)
}

# Upper exact quantile count:
# smallest k such that F(k) >= 1-alpha/2.
# UCL = k_U/m and signal if X > k_U.
k_U <- min(x[pbinom(x, size = m, prob = pi0) >= q_U])

U_exact <- k_U / m
alpha_U_exact <- 1 - pbinom(k_U, size = m, prob = pi0)

alpha_exact_actual <- alpha_L_exact + alpha_U_exact

#----------------------------------------------------------
# 4. Summary tables
#----------------------------------------------------------
summary_table <- data.frame(
  Method = c("Normal approximation", "Exact binomial"),
  LCL = c(L_norm, L_exact),
  UCL = c(U_norm, U_exact),
  Lower_signal_probability = c(pbinom(floor(m * L_norm), m, pi0),
                               alpha_L_exact),
  Upper_signal_probability = c(1 - pbinom(floor(m * U_norm), m, pi0),
                               alpha_U_exact),
  Actual_alpha = c(alpha_norm_actual, alpha_exact_actual)
)

quantile_check <- data.frame(
  k = x,
  p = x / m,
  CDF = pbinom(x, size = m, prob = pi0),
  Upper_tail_signal_prob = 1 - pbinom(x, size = m, prob = pi0)
)

print(summary_table)

cat("\nExact upper quantile check:\n")
cat("q_U =", q_U, "\n")
cat("F(k_U - 1) =", pbinom(k_U - 1, m, pi0), "\n")
cat("F(k_U)     =", pbinom(k_U, m, pi0), "\n")
cat("k_U =", k_U, ", U_exact =", U_exact, "\n")
cat("Signal rule: X >", k_U, "\n")

write.csv(summary_table,
          "outputs/normal_vs_exact_p_chart_summary.csv",
          row.names = FALSE)

write.csv(quantile_check,
          "outputs/binomial_cdf_check.csv",
          row.names = FALSE)

#==========================================================
# Plot 1: Limit construction
#==========================================================

png("figs/normal_vs_exact_limits_construction.png",
    width = 1600, height = 900, res = 220)

par(
  mar = c(5.5, 5.3, 4.2, 1.2),
  cex.lab  = 1.20,
  cex.axis = 1.00,
  cex.main = 1.35
)

pmf <- dbinom(x, size = m, prob = pi0)

plot(p_x, pmf,
     type = "h",
     lwd = 2.8,
     xlab = expression(p == X/m),
     ylab = "Binomial probability",
     main = "Normal vs Exact Limits: Construction",
     xlim = c(0, 0.35),
     ylim = c(0, max(pmf) * 1.22))

points(p_x, pmf, pch = 19, cex = 0.9)

abline(v = pi0, lwd = 2.8, lty = 1)
abline(v = c(L_norm, U_norm), lwd = 2.8, lty = 2)
abline(v = c(L_exact, U_exact), lwd = 2.8, lty = 3)

legend("topright",
       legend = c(
         expression(pi[0] == 0.05),
         paste0("Normal: [", round(L_norm, 3), ", ", round(U_norm, 3), "]"),
         paste0("Exact: [", round(L_exact, 3), ", ", round(U_exact, 3), "]")
       ),
       lty = c(1, 2, 3),
       lwd = 2.8,
       cex = 0.85,
       bty = "n")

mtext(
  paste0("Exact upper quantile: k_U = ", k_U,
         ", UCL = ", round(U_exact, 3),
         "; signal if X > ", k_U),
  side = 1, line = 4.3, cex = 0.85
)

dev.off()

#==========================================================
# Plot 2: SPC p chart comparison
#==========================================================

png("figs/spc_p_chart_normal_vs_exact_limits.png",
    width = 1600, height = 900, res = 220)

par(
  mar = c(5.7, 5.3, 4.2, 1.2),
  cex.lab  = 1.20,
  cex.axis = 1.00,
  cex.main = 1.35
)

ylim_spc <- c(0, max(p_obs, U_norm, U_exact) * 1.22)

plot(i, p_obs,
     type = "b",
     pch = 19,
     lwd = 2.6,
     cex = 1.0,
     ylim = ylim_spc,
     xlab = "Sample index i",
     ylab = expression(p[i] == X[i]/m),
     main = "SPC p Chart: Normal vs Exact Limits")

# Center line
abline(h = pi0, lwd = 2.8, lty = 1)

# Limits
abline(h = c(L_norm, U_norm), lwd = 2.8, lty = 2)
abline(h = c(L_exact, U_exact), lwd = 2.8, lty = 3)

# Signals
out_norm  <- which(p_obs < L_norm | p_obs > U_norm)
out_exact <- which(p_obs < L_exact | p_obs > U_exact)

if (length(out_norm) > 0) {
  points(i[out_norm], p_obs[out_norm],
         pch = 21, cex = 1.5, lwd = 2.2)
}

if (length(out_exact) > 0) {
  points(i[out_exact], p_obs[out_exact],
         pch = 4, cex = 1.5, lwd = 2.2)
}

legend("topleft",
       legend = c(
         paste0("Normal limits: [", round(L_norm, 3), ", ", round(U_norm, 3), "]"),
         paste0("Exact limits: [", round(L_exact, 3), ", ", round(U_exact, 3), "]")
       ),
       lty = c(2, 3),
       lwd = c(2.8, 2.8),
       cex = 0.85,
       bty = "n")

mtext(
  paste0("Actual alpha: Normal = ",
         round(alpha_norm_actual, 5),
         "   |   Exact = ",
         round(alpha_exact_actual, 5)),
  side = 1, line = 4.5, cex = 0.82
)

dev.off()

############################################################
# End
############################################################