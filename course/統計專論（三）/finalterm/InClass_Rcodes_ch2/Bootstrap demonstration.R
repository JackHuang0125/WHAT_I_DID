# =========================================================
# Bootstrap demonstration: sampling distribution of median
# =========================================================

rm(list = ls())

set.seed(2026)

setwd("E:/統計品質管制/Week4")

# -----------------------------
# 1. Observed sample
# -----------------------------
# Use a right-skewed sample so bootstrap motivation is clear
x <- c(2.1, 2.4, 2.8, 3.0, 3.1, 3.5, 3.7, 4.2, 4.8, 5.1,
       5.5, 6.0, 6.4, 7.2, 8.5)

n <- length(x)

# Parameter of interest: sample median
theta_hat <- median(x)

cat("=====================================\n")
cat("Observed data\n")
cat("=====================================\n")
print(x)
cat("\nSample size n =", n, "\n")
cat("Observed sample median =", theta_hat, "\n\n")

# -----------------------------
# 2. Bootstrap algorithm
# -----------------------------
B <- 10000
boot_median <- numeric(B)

for (b in 1:B) {
  x_star <- sample(x, size = n, replace = TRUE)
  boot_median[b] <- median(x_star)
}

# -----------------------------
# 3. Bootstrap summaries
# -----------------------------
boot_se <- sd(boot_median)
ci_perc <- quantile(boot_median, probs = c(0.025, 0.975))

cat("=====================================\n")
cat("Bootstrap results\n")
cat("=====================================\n")
cat("Bootstrap SE of median =", round(boot_se, 4), "\n")
cat("95% percentile bootstrap CI = (",
    round(ci_perc[1], 3), ", ", round(ci_perc[2], 3), ")\n", sep = "")
cat("\n")

# -----------------------------
# 4. Simple comparison:
#    normal-style CI using bootstrap SE
# -----------------------------
z <- qnorm(0.975)
ci_normal <- c(theta_hat - z * boot_se, theta_hat + z * boot_se)

cat("95% normal-approx bootstrap CI = (",
    round(ci_normal[1], 3), ", ", round(ci_normal[2], 3), ")\n", sep = "")
cat("\n")

# -----------------------------
# 5. Plot observed data
# -----------------------------
png("bootstrap_data_plot.png", width = 1000, height = 500, res = 140)

par(mar = c(4.2, 4.2, 3, 1))
plot(x, rep(1, n),
     pch = 19,
     xlab = "Observed data",
     ylab = "",
     yaxt = "n",
     main = "Observed Sample")
abline(v = theta_hat, lty = 2, lwd = 2)
text(theta_hat, 1.05,
     labels = paste("median =", theta_hat),
     pos = 4)

dev.off()

# -----------------------------
# 6. Plot bootstrap distribution
# -----------------------------
png("bootstrap_median_hist.png", width = 1000, height = 700, res = 140)

par(mar = c(4.5, 4.5, 3.2, 1.2))
hist(boot_median,
     breaks = 25,
     main = "Bootstrap Distribution of the Sample Median",
     xlab = expression(hat(theta)^"*"),
     border = "white")

abline(v = theta_hat, lwd = 2, lty = 2)
abline(v = ci_perc, lwd = 2, lty = 3)

legend("topright",
       legend = c("Observed median", "95% percentile CI"),
       lwd = c(2, 2),
       lty = c(2, 3),
       bty = "n")

dev.off()

# -----------------------------
# 7. Show one bootstrap sample
# -----------------------------
set.seed(99)
x_star_demo <- sample(x, size = n, replace = TRUE)

cat("=====================================\n")
cat("One bootstrap sample (for illustration)\n")
cat("=====================================\n")
print(x_star_demo)
cat("Median of this bootstrap sample =", median(x_star_demo), "\n")