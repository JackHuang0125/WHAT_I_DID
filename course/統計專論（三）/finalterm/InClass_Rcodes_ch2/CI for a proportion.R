# =========================================================
# Coverage comparison for Wald / Wilson / Agresti-Coull / Jeffreys
# n = 20
# =========================================================

rm(list = ls())

setwd("E:/統計品質管制/Week4")

alpha <- 0.05
z <- qnorm(1 - alpha/2)
n <- 20

cov_fig_file <- file.path(getwd(), "coverage_compare_n20.png")

# -----------------------------
# CI function for a given X, n
# -----------------------------
get_ci <- function(X, n, alpha = 0.05) {
  phat <- X / n
  z <- qnorm(1 - alpha/2)
  
  # Wald
  wald_se   <- sqrt(phat * (1 - phat) / n)
  wald_low  <- phat - z * wald_se
  wald_high <- phat + z * wald_se
  
  # Wilson
  wilson_den <- 1 + z^2 / n
  wilson_ctr <- (phat + z^2 / (2*n)) / wilson_den
  wilson_hw  <- (z / wilson_den) *
    sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2))
  wilson_low  <- wilson_ctr - wilson_hw
  wilson_high <- wilson_ctr + wilson_hw
  
  # Agresti-Coull
  n_tilde <- n + z^2
  p_tilde <- (X + z^2 / 2) / n_tilde
  ac_se   <- sqrt(p_tilde * (1 - p_tilde) / n_tilde)
  ac_low  <- p_tilde - z * ac_se
  ac_high <- p_tilde + z * ac_se
  
  # Jeffreys
  jeff_low  <- qbeta(alpha/2,     X + 0.5, n - X + 0.5)
  jeff_high <- qbeta(1 - alpha/2, X + 0.5, n - X + 0.5)
  
  c(wald_low, wald_high,
    wilson_low, wilson_high,
    ac_low, ac_high,
    jeff_low, jeff_high)
}

# -----------------------------
# Exact coverage for each true p
# -----------------------------
coverage_one_p <- function(p, n, alpha = 0.05) {
  xs <- 0:n
  probs <- dbinom(xs, size = n, prob = p)
  
  cover_wald <- 0
  cover_wilson <- 0
  cover_ac <- 0
  cover_jeff <- 0
  
  for (x in xs) {
    ci <- get_ci(x, n, alpha)
    
    wald_low  <- ci[1]
    wald_high <- ci[2]
    wil_low   <- ci[3]
    wil_high  <- ci[4]
    ac_low    <- ci[5]
    ac_high   <- ci[6]
    jeff_low  <- ci[7]
    jeff_high <- ci[8]
    
    px <- dbinom(x, size = n, prob = p)
    
    if (wald_low <= p && p <= wald_high) cover_wald   <- cover_wald + px
    if (wil_low  <= p && p <= wil_high)  cover_wilson <- cover_wilson + px
    if (ac_low   <= p && p <= ac_high)   cover_ac     <- cover_ac + px
    if (jeff_low <= p && p <= jeff_high) cover_jeff   <- cover_jeff + px
  }
  
  c(cover_wald, cover_wilson, cover_ac, cover_jeff)
}

# grid of true p
p_grid <- seq(0.01, 0.99, by = 0.01)

cov_mat <- t(sapply(p_grid, coverage_one_p, n = n, alpha = alpha))
colnames(cov_mat) <- c("Wald", "Wilson", "Agresti-Coull", "Jeffreys")

coverage_df <- data.frame(
  p = p_grid,
  Wald = cov_mat[, "Wald"],
  Wilson = cov_mat[, "Wilson"],
  Agresti_Coull = cov_mat[, "Agresti-Coull"],
  Jeffreys = cov_mat[, "Jeffreys"]
)

print(head(coverage_df, 10))
cat("\n")
cat("Minimum coverage by method:\n")
cat(sprintf("Wald            : %.3f\n", min(coverage_df$Wald)))
cat(sprintf("Wilson          : %.3f\n", min(coverage_df$Wilson)))
cat(sprintf("Agresti-Coull   : %.3f\n", min(coverage_df$Agresti_Coull)))
cat(sprintf("Jeffreys        : %.3f\n", min(coverage_df$Jeffreys)))

# -----------------------------
# Plot
# -----------------------------
png(filename = cov_fig_file, width = 1200, height = 800, res = 150)

par(mar = c(4.5, 4.8, 3.2, 1.5))

plot(coverage_df$p, coverage_df$Wald,
     type = "l", lwd = 3, lty = 1,
     ylim = c(0.80, 1.00),
     xlab = "True proportion p",
     ylab = "Coverage probability",
     main = "Exact Coverage Comparison of 95% CI Methods (n = 20)")

lines(coverage_df$p, coverage_df$Wilson, lwd = 3, lty = 2)
lines(coverage_df$p, coverage_df$Agresti_Coull, lwd = 3, lty = 3)
lines(coverage_df$p, coverage_df$Jeffreys, lwd = 3, lty = 4)

abline(h = 0.95, lwd = 2, lty = 5)

legend("bottom",
       legend = c("Wald", "Wilson", "Agresti-Coull", "Jeffreys", "Nominal 0.95"),
       lwd = c(3,3,3,3,2),
       lty = c(1,2,3,4,5),
       cex = 0.95,
       horiz = FALSE,
       bty = "n")

dev.off()

cat("\nCoverage figure saved to:\n")
cat(cov_fig_file, "\n")