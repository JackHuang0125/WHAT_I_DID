############################################################
# Normal vs Exact p Chart (SPC standard: alpha = 0.0027)
############################################################

rm(list = ls())

setwd("E:/統計品質管制/Week11")

dir.create("figs", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

#----------------------------------------------------------
# 1. Setting (SPC standard)
#----------------------------------------------------------
m     <- 25
pi0   <- 0.05
alpha <- 0.0027     # ⭐ 3-sigma rule
z     <- qnorm(1 - alpha / 2)  # ≈ 3

x   <- 0:m
p_x <- x / m

# Teaching monitoring data
X_obs <- c(0, 1, 2, 1, 3, 0, 1, 2, 0, 4,
           1, 0, 2, 1, 3, 0, 1, 5, 2, 1)

i     <- seq_along(X_obs)
p_obs <- X_obs / m

#----------------------------------------------------------
# 2. Normal approximation limits (3σ)
#----------------------------------------------------------
L_norm <- max(0, pi0 - z * sqrt(pi0 * (1 - pi0) / m))
U_norm <- min(1, pi0 + z * sqrt(pi0 * (1 - pi0) / m))

alpha_norm_actual <-
  pbinom(floor(m * L_norm), size = m, prob = pi0) +
  (1 - pbinom(ceiling(m * U_norm) - 1, size = m, prob = pi0))

#----------------------------------------------------------
# 3. Exact binomial limits (robust version)
#----------------------------------------------------------
# ⭐ 關鍵修正：確保一定找到解
lower_candidates <- x[pbinom(x, m, pi0) <= alpha / 2]
a_L <- if (length(lower_candidates) == 0) 0 else max(lower_candidates)

upper_candidates <- x[(1 - pbinom(x - 1, m, pi0)) <= alpha / 2]
a_U <- if (length(upper_candidates) == 0) m else min(upper_candidates)

L_exact <- a_L / m
U_exact <- a_U / m

alpha_exact_actual <-
  pbinom(a_L, size = m, prob = pi0) +
  (1 - pbinom(a_U - 1, size = m, prob = pi0))

#----------------------------------------------------------
# 4. Summary table
#----------------------------------------------------------
summary_table <- data.frame(
  Method = c("Normal approximation", "Exact binomial"),
  LCL = c(L_norm, L_exact),
  UCL = c(U_norm, U_exact),
  Actual_alpha = c(alpha_norm_actual, alpha_exact_actual)
)

print(summary_table)

write.csv(summary_table,
          "outputs/normal_vs_exact_p_chart_summary_alpha0027.csv",
          row.names = FALSE)

#==========================================================
# Plot 1: Limit construction
#==========================================================

png("figs/normal_vs_exact_limits_construction_alpha0027.png",
    width = 1600, height = 900, res = 220)

par(mar = c(5.5,5.3,4.2,1.2),
    cex.lab=1.2, cex.axis=1.0, cex.main=1.35)

pmf <- dbinom(x, m, pi0)

plot(p_x, pmf,
     type = "h",
     lwd = 2.8,
     xlab = expression(p == X/m),
     ylab = "Binomial probability",
     main = "Normal vs Exact Limits (alpha = 0.0027)",
     xlim = c(0, 0.35),
     ylim = c(0, max(pmf)*1.22))

points(p_x, pmf, pch = 19, cex = 0.9)

abline(v = pi0, lwd = 2.8)
abline(v = c(L_norm, U_norm), lwd = 2.8, lty = 2)
abline(v = c(L_exact, U_exact), lwd = 2.8, lty = 3)

legend("topright",
       legend = c(
         expression(pi == 0.05),
         paste0("Normal: [", round(L_norm,3), ", ", round(U_norm,3), "]"),
         paste0("Exact: [", round(L_exact,3), ", ", round(U_exact,3), "]")
       ),
       lty = c(1,2,3),
       lwd = 2.8,
       cex = 0.85,
       bty = "n")

dev.off()

#==========================================================
# Plot 2: SPC chart
#==========================================================

png("figs/spc_p_chart_alpha0027.png",
    width = 1600, height = 900, res = 220)

par(mar = c(5.7,5.3,4.2,1.2),
    cex.lab=1.2, cex.axis=1.0, cex.main=1.35)

ylim_spc <- c(0, max(p_obs, U_norm, U_exact)*1.22)

plot(i, p_obs,
     type = "b", pch = 19, lwd = 2.6, cex = 1.0,
     ylim = ylim_spc,
     xlab = "Sample index i",
     ylab = expression(p[i]),
     main = "SPC p Chart (alpha = 0.0027)")

abline(h = c(L_norm, U_norm), lwd = 2.8, lty = 2)
abline(h = c(L_exact, U_exact), lwd = 2.8, lty = 3)

# signals
out_norm  <- which(p_obs < L_norm | p_obs > U_norm)
out_exact <- which(p_obs < L_exact | p_obs > U_exact)

points(i[out_norm],  p_obs[out_norm],  pch = 21, cex = 1.4, lwd = 2)
points(i[out_exact], p_obs[out_exact], pch = 4,  cex = 1.4, lwd = 2)

legend("topleft",
       legend = c("Normal limits", "Exact limits"),
       lty = c(2,3), lwd = 2.8,
       cex = 0.9, bty = "n")

mtext(
  paste0("alpha_e: Normal = ",
         round(alpha_norm_actual,5),
         " | Exact = ",
         round(alpha_exact_actual,5)),
  side = 1, line = 4.5, cex = 0.85
)

dev.off()