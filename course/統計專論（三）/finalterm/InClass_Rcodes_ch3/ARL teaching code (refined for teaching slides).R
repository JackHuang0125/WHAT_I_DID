############################################################
# ARL teaching code (refined for teaching slides)
############################################################

rm(list = ls())

setwd("E:/統計品質管制/Week10")

#----------------------------------------------------------
# 0. Create folders
#----------------------------------------------------------
dir.create("figs", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

#----------------------------------------------------------
# 1. Basic setting
#----------------------------------------------------------

alpha <- 0.0027
z <- qnorm(1 - alpha / 2)

m_vec <- c(5, 10, 20)
k_grid <- seq(0, 3, by = 0.01)

#----------------------------------------------------------
# 2. Functions
#----------------------------------------------------------

beta_fun <- function(k, m, alpha = 0.0027) {
  z <- qnorm(1 - alpha / 2)
  pnorm(-k * sqrt(m) + z) - pnorm(-k * sqrt(m) - z)
}

arl1_fun <- function(k, m, alpha = 0.0027) {
  beta <- beta_fun(k, m, alpha)
  1 / (1 - beta)
}

sd_rl1_fun <- function(k, m, alpha = 0.0027) {
  beta <- beta_fun(k, m, alpha)
  sqrt(beta) / (1 - beta)
}

overall_far <- function(alpha, n) {
  1 - (1 - alpha)^n
}

#----------------------------------------------------------
# 3. ARL0
#----------------------------------------------------------

ARL0 <- 1 / alpha
SD_RL0 <- sqrt(1 - alpha) / alpha

cat("ARL0 =", round(ARL0, 2), "\n")

#----------------------------------------------------------
# 4. Summary table → save CSV
#----------------------------------------------------------

k_selected <- c(0, 0.5, 1, 1.5, 2, 2.5, 3)

summary_table <- expand.grid(k = k_selected, m = m_vec)
summary_table$beta <- mapply(beta_fun, summary_table$k, summary_table$m)
summary_table$ARL1 <- mapply(arl1_fun, summary_table$k, summary_table$m)

summary_table <- round(summary_table, 4)

write.csv(summary_table, "outputs/arl_summary_table.csv", row.names = FALSE)

#----------------------------------------------------------
# Common plotting style (IMPORTANT)
#----------------------------------------------------------

par(
  cex.main = 1.8,
  cex.lab  = 1.6,
  cex.axis = 1.4,
  cex.sub  = 1.4
)

#----------------------------------------------------------
# 5. Beta plot
#----------------------------------------------------------

png("figs/ARL_beta_vs_shift.png", width = 2000, height = 1400, res = 250)

plot(
  k_grid,
  beta_fun(k_grid, m_vec[1]),
  type = "l",
  lwd = 3,
  ylim = c(0, 1),
  xlab = expression("Shift size " * k),
  ylab = expression(beta == P("no signal")),
  main = expression(beta * " vs shift size")
)

for (m in m_vec[-1]) {
  lines(k_grid, beta_fun(k_grid, m), lwd = 3, lty = which(m_vec == m))
}

legend(
  "topright",
  legend = paste0("m = ", m_vec),
  lwd = 3,
  cex = 1.4,
  bty = "n"
)

grid()
dev.off()

#----------------------------------------------------------
# 6. ARL1 plot
#----------------------------------------------------------

png("figs/ARL1_vs_shift.png", width = 2000, height = 1400, res = 250)

plot(
  k_grid,
  arl1_fun(k_grid, m_vec[1]),
  type = "l",
  lwd = 3,
  ylim = c(0, 40),
  xlab = "Shift size k",
  ylab = expression(ARL[1]),
  main = expression(ARL[1] * " vs shift size")
)

for (m in m_vec[-1]) {
  lines(k_grid, arl1_fun(k_grid, m), lwd = 3, lty = which(m_vec == m))
}

legend(
  "topright",
  legend = paste0("m = ", m_vec),
  lwd = 3,
  cex = 1.4,
  bty = "n"
)

grid()
dev.off()

#----------------------------------------------------------
# 7. SD of run length
#----------------------------------------------------------

png("figs/SD_RL1_vs_shift.png", width = 2000, height = 1400, res = 250)

plot(
  k_grid,
  sd_rl1_fun(k_grid, m_vec[1]),
  type = "l",
  lwd = 3,
  ylim = c(0, 40),
  xlab = "Shift size k",
  ylab = "SD of run length",
  main = "SD of run length vs shift size"
)

for (m in m_vec[-1]) {
  lines(k_grid, sd_rl1_fun(k_grid, m), lwd = 3, lty = which(m_vec == m))
}

legend(
  "topright",
  legend = paste0("m = ", m_vec),
  lwd = 3,
  cex = 1.4,
  bty = "n"
)

grid()
dev.off()

#----------------------------------------------------------
# 8. Overall FAR table + plot
#----------------------------------------------------------

n_vec <- c(1, 2, 5, 10, 20, 50, 100)
alpha_vec <- c(0.001, 0.0027, 0.005)

far_table <- expand.grid(n = n_vec, alpha = alpha_vec)
far_table$overall_FAR <- overall_far(far_table$alpha, far_table$n)

write.csv(far_table, "outputs/overall_far_table.csv", row.names = FALSE)

png("figs/overall_false_alarm_probability.png", width = 2000, height = 1400, res = 250)

plot(
  n_vec,
  overall_far(alpha_vec[1], n_vec),
  type = "l",
  lwd = 3,
  ylim = c(0, 0.5),
  xlab = "Number of samples (n)",
  ylab = expression(alpha[e]),
  main = "Overall false alarm probability"
)

for (a in alpha_vec[-1]) {
  lines(n_vec, overall_far(a, n_vec), lwd = 3, lty = which(alpha_vec == a))
}

legend(
  "topleft",
  legend = paste0("alpha = ", alpha_vec),
  lwd = 3,
  cex = 1.4,
  bty = "n"
)

grid()
dev.off()