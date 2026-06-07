############################################################
# Section 3.3 Shewhart Charts for Categorical Variables
# Refined PNG version: readable but not overflowing
############################################################

rm(list = ls())

setwd("E:/統計品質管制/Week11")

dir.create("figs", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

#----------------------------------------------------------
# Global settings
#----------------------------------------------------------
z <- 3

png_w   <- 1500
png_h   <- 950
png_res <- 220

theme_spc <- function() {
  par(
    mar = c(5.0, 5.2, 4.0, 1.5),
    cex.lab  = 1.35,
    cex.axis = 1.15,
    cex.main = 1.45
  )
}

add_spc_legend <- function(location = "topright", U, C, L, obs_label = "Observed") {
  legend(location,
         legend = c(
           obs_label,
           paste0("UCL = ", round(U, 3)),
           paste0("CL = ", round(C, 3)),
           paste0("LCL = ", round(L, 3))
         ),
         lty = c(1, 2, 1, 2),
         pch = c(19, NA, NA, NA),
         lwd = c(2.5, 2.5, 2.5, 2.5),
         cex = 0.95,
         bty = "n")
}

#==========================================================
# Example 3.4: p chart
#==========================================================

X <- c(7, 3, 10, 1, 8, 5, 4, 9, 3, 9,
       5, 7, 2, 10, 4, 6, 9, 3, 11, 5)

m <- 50
n <- length(X)
i <- 1:n

p <- X / m
pbar <- mean(p)

U_p <- pbar + z * sqrt(pbar * (1 - pbar) / m)
C_p <- pbar
L_p <- pbar - z * sqrt(pbar * (1 - pbar) / m)
L_p_adj <- max(0, L_p)

p_summary <- data.frame(
  n = n,
  m = m,
  pbar = pbar,
  mpbar = m * pbar,
  m_1_minus_pbar = m * (1 - pbar),
  LCL_raw = L_p,
  LCL_adjusted = L_p_adj,
  CL = C_p,
  UCL = U_p
)

write.csv(p_summary, "outputs/example_3_4_p_chart_summary.csv",
          row.names = FALSE)

png("figs/example_3_4_p_chart.png",
    width = png_w, height = png_h, res = png_res)

theme_spc()

plot(i, p,
     type = "b", pch = 19, lwd = 2.5, cex = 1.1,
     ylim = c(0, max(U_p, p) * 1.18),
     xlab = "Sample index i",
     ylab = expression(p[i]),
     main = "Example 3.4: p Chart")

abline(h = U_p, lwd = 2.5, lty = 2)
abline(h = C_p, lwd = 2.5, lty = 1)
abline(h = L_p_adj, lwd = 2.5, lty = 2)

add_spc_legend("topright", U_p, C_p, L_p_adj, obs_label = expression(p[i]))

dev.off()

#==========================================================
# mp chart: frequency version of Example 3.4
#==========================================================

Xbar <- mean(X)

U_mp <- Xbar + z * sqrt(m * pbar * (1 - pbar))
C_mp <- Xbar
L_mp <- Xbar - z * sqrt(m * pbar * (1 - pbar))
L_mp_adj <- max(0, L_mp)

mp_summary <- data.frame(
  n = n,
  m = m,
  Xbar = Xbar,
  LCL_raw = L_mp,
  LCL_adjusted = L_mp_adj,
  CL = C_mp,
  UCL = U_mp
)

write.csv(mp_summary, "outputs/mp_chart_summary.csv",
          row.names = FALSE)

png("figs/mp_chart_example_3_4_frequency_version.png",
    width = png_w, height = png_h, res = png_res)

theme_spc()

plot(i, X,
     type = "b", pch = 19, lwd = 2.5, cex = 1.1,
     ylim = c(0, max(U_mp, X) * 1.18),
     xlab = "Sample index i",
     ylab = expression(X[i]),
     main = "mp Chart: Frequency Version of Example 3.4")

abline(h = U_mp, lwd = 2.5, lty = 2)
abline(h = C_mp, lwd = 2.5, lty = 1)
abline(h = L_mp_adj, lwd = 2.5, lty = 2)

add_spc_legend("topright", U_mp, C_mp, L_mp_adj, obs_label = expression(X[i]))

dev.off()

#==========================================================
# Example 3.5: Actual Type I error probability alpha_e
#==========================================================

actual_alpha_p_chart <- function(m, pi, alpha) {
  z_alpha <- qnorm(1 - alpha / 2)
  
  L <- pi - z_alpha * sqrt(pi * (1 - pi) / m)
  U <- pi + z_alpha * sqrt(pi * (1 - pi) / m)
  
  x_lower <- floor(m * L)
  x_upper <- ceiling(m * U)
  
  prob_lower <- ifelse(x_lower >= 0,
                       pbinom(x_lower, size = m, prob = pi),
                       0)
  
  prob_upper <- ifelse(x_upper <= m,
                       1 - pbinom(x_upper - 1, size = m, prob = pi),
                       0)
  
  prob_lower + prob_upper
}

pi_grid <- seq(0.01, 0.99, by = 0.01)

cases <- expand.grid(
  m = c(25, 50),
  alpha = c(0.01, 0.001)
)

alpha_results <- do.call(rbind, lapply(seq_len(nrow(cases)), function(k) {
  m_k <- cases$m[k]
  alpha_k <- cases$alpha[k]
  
  data.frame(
    m = m_k,
    alpha = alpha_k,
    pi = pi_grid,
    alpha_e = sapply(pi_grid, function(p0) {
      actual_alpha_p_chart(m = m_k, pi = p0, alpha = alpha_k)
    })
  )
}))

write.csv(alpha_results, "outputs/example_3_5_actual_alpha.csv",
          row.names = FALSE)

png("figs/example_3_5_actual_alpha.png",
    width = 1600, height = 1200, res = png_res)

par(
  mfrow = c(2, 2),
  mar = c(4.3, 4.6, 3.2, 1.2),
  cex.lab  = 1.15,
  cex.axis = 1.00,
  cex.main = 1.20
)

for (k in seq_len(nrow(cases))) {
  dat <- subset(alpha_results,
                m == cases$m[k] & alpha == cases$alpha[k])
  
  plot(dat$pi, dat$alpha_e,
       type = "l", lwd = 2.4,
       xlab = expression(pi),
       ylab = expression(alpha[e]),
       main = paste0("m = ", cases$m[k],
                     ", alpha = ", cases$alpha[k]),
       ylim = c(0, max(dat$alpha_e, cases$alpha[k]) * 1.25))
  
  abline(h = cases$alpha[k], lty = 2, lwd = 2.4)
  
  legend("top",
         legend = c(expression(alpha[e]), "Nominal alpha"),
         lty = c(1, 2),
         lwd = 2.4,
         cex = 0.85,
         bty = "n")
}

par(mfrow = c(1, 1))
dev.off()

#==========================================================
# Exact p chart limits for small samples
#==========================================================

exact_p_limits <- function(m, pi, alpha = 0.0027) {
  
  probs <- dbinom(0:m, size = m, prob = pi)
  cdf <- pbinom(0:m, size = m, prob = pi)
  
  upper_tail <- sapply(0:m, function(a) {
    sum(probs[(a + 1):(m + 1)])
  })
  
  lower_candidates <- which(cdf <= alpha / 2) - 1
  upper_candidates <- which(upper_tail <= alpha / 2) - 1
  
  a_L <- ifelse(length(lower_candidates) == 0,
                NA, max(lower_candidates))
  
  a_U <- ifelse(length(upper_candidates) == 0,
                NA, min(upper_candidates))
  
  L_star <- ifelse(is.na(a_L), 0, a_L / m)
  U_star <- ifelse(is.na(a_U), 1, a_U / m)
  
  alpha_e <- pbinom(floor(m * L_star) - 1, size = m, prob = pi) +
    (1 - pbinom(ceiling(m * U_star), size = m, prob = pi))
  
  data.frame(
    m = m,
    pi = pi,
    alpha = alpha,
    L_star = L_star,
    U_star = U_star,
    alpha_e = alpha_e
  )
}

exact_example <- exact_p_limits(m = 25, pi = 0.05, alpha = 0.01)

write.csv(exact_example, "outputs/exact_p_chart_small_sample_example.csv",
          row.names = FALSE)

#==========================================================
# Example 3.6: c chart
#==========================================================

c <- c(2, 7, 4, 3, 9, 2, 5, 2, 6, 1, 8, 3, 5, 10, 2)
n_c <- length(c)
i_c <- 1:n_c

cbar <- mean(c)

U_c <- cbar + z * sqrt(cbar)
C_c <- cbar
L_c <- cbar - z * sqrt(cbar)
L_c_adj <- max(0, L_c)

c_summary <- data.frame(
  n = n_c,
  cbar = cbar,
  LCL_raw = L_c,
  LCL_adjusted = L_c_adj,
  CL = C_c,
  UCL = U_c
)

write.csv(c_summary, "outputs/example_3_6_c_chart_summary.csv",
          row.names = FALSE)

png("figs/example_3_6_c_chart.png",
    width = png_w, height = png_h, res = png_res)

theme_spc()

plot(i_c, c,
     type = "b", pch = 19, lwd = 2.5, cex = 1.1,
     ylim = c(0, max(U_c, c) * 1.18),
     xlab = "Inspection unit i",
     ylab = expression(c[i]),
     main = "Example 3.6: c Chart")

abline(h = U_c, lwd = 2.5, lty = 2)
abline(h = C_c, lwd = 2.5, lty = 1)
abline(h = L_c_adj, lwd = 2.5, lty = 2)

add_spc_legend("topright", U_c, C_c, L_c_adj, obs_label = expression(c[i]))

dev.off()

#==========================================================
# u chart: unequal inspection sizes
#==========================================================

c_u <- c(3, 5, 2, 7, 4, 6, 3, 8, 5, 4)
m_u <- c(10, 12, 8, 15, 10, 14, 9, 16, 13, 11)

u_i <- c_u / m_u
ubar <- sum(c_u) / sum(m_u)

U_u <- ubar + z * sqrt(ubar / m_u)
C_u <- rep(ubar, length(u_i))
L_u <- ubar - z * sqrt(ubar / m_u)
L_u_adj <- pmax(0, L_u)

u_summary <- data.frame(
  i = seq_along(c_u),
  c_i = c_u,
  m_i = m_u,
  u_i = u_i,
  LCL = L_u_adj,
  CL = C_u,
  UCL = U_u
)

write.csv(u_summary, "outputs/u_chart_teaching_example.csv",
          row.names = FALSE)

png("figs/u_chart_teaching_example.png",
    width = png_w, height = png_h, res = png_res)

theme_spc()

plot(seq_along(u_i), u_i,
     type = "b", pch = 19, lwd = 2.5, cex = 1.1,
     ylim = c(0, max(U_u, u_i) * 1.18),
     xlab = "Inspection unit i",
     ylab = expression(u[i] == c[i] / m[i]),
     main = "u Chart: Unequal Inspection Sizes")

lines(seq_along(u_i), U_u, lwd = 2.5, lty = 2)
lines(seq_along(u_i), C_u, lwd = 2.5, lty = 1)
lines(seq_along(u_i), L_u_adj, lwd = 2.5, lty = 2)

legend("topright",
       legend = c(expression(u[i]), expression(UCL[i]), "CL", expression(LCL[i])),
       lty = c(1, 2, 1, 2),
       pch = c(19, NA, NA, NA),
       lwd = 2.5,
       cex = 0.95,
       bty = "n")

dev.off()

#==========================================================
# D chart: weighted defect teaching example
#==========================================================

defect_data <- data.frame(
  unit = 1:12,
  minor = c(3, 5, 2, 4, 6, 3, 4, 5, 2, 6, 3, 4),
  moderate = c(1, 2, 0, 1, 2, 1, 1, 3, 0, 2, 1, 1),
  serious = c(0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0)
)

w <- c(minor = 1, moderate = 3, serious = 8)

D_i <- with(defect_data,
            w["minor"] * minor +
              w["moderate"] * moderate +
              w["serious"] * serious)

Dbar <- mean(D_i)

lambda_minor <- mean(defect_data$minor)
lambda_moderate <- mean(defect_data$moderate)
lambda_serious <- mean(defect_data$serious)

Var_D <- w["minor"]^2 * lambda_minor +
  w["moderate"]^2 * lambda_moderate +
  w["serious"]^2 * lambda_serious

U_D <- Dbar + z * sqrt(Var_D)
C_D <- Dbar
L_D <- Dbar - z * sqrt(Var_D)
L_D_adj <- max(0, L_D)

D_summary <- data.frame(
  Dbar = Dbar,
  Var_D = Var_D,
  LCL_raw = L_D,
  LCL_adjusted = L_D_adj,
  CL = C_D,
  UCL = U_D
)

D_output <- cbind(defect_data, D_i = D_i)

write.csv(D_output, "outputs/D_chart_weighted_defect_data.csv",
          row.names = FALSE)

write.csv(D_summary, "outputs/D_chart_summary.csv",
          row.names = FALSE)

png("figs/D_chart_weighted_defects.png",
    width = png_w, height = png_h, res = png_res)

theme_spc()

plot(defect_data$unit, D_i,
     type = "b", pch = 19, lwd = 2.5, cex = 1.1,
     ylim = c(0, max(U_D, D_i) * 1.18),
     xlab = "Inspection unit i",
     ylab = expression(D[i]),
     main = "D Chart: Weighted Defects")

abline(h = U_D, lwd = 2.5, lty = 2)
abline(h = C_D, lwd = 2.5, lty = 1)
abline(h = L_D_adj, lwd = 2.5, lty = 2)

add_spc_legend("topright", U_D, C_D, L_D_adj, obs_label = expression(D[i]))

dev.off()

############################################################
# End
############################################################