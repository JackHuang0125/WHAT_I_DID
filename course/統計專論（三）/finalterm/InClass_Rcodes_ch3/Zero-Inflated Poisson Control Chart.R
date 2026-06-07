############################################################
# Zero-Inflated Poisson Control Chart
# Simulate ZIP data -> Estimate parameters -> Construct UCL
############################################################

rm(list = ls())

setwd("E:/統計品質管制/Week11")

#----------------------------------------------------------
# 0. Create folders
#----------------------------------------------------------

if (!dir.exists("figs")) {
  dir.create("figs")
}

if (!dir.exists("outputs")) {
  dir.create("outputs")
}

#----------------------------------------------------------
# 1. True ZIP parameters for simulation
#----------------------------------------------------------

p_true <- 0.40
lambda_true <- 0.50

alpha <- 0.0027
target_prob <- 1 - alpha

m <- 25
set.seed(123)

#----------------------------------------------------------
# 2. Simulate ZIP observations
#----------------------------------------------------------
# With probability p_true, generate a structural zero.
# With probability 1 - p_true, generate a Poisson(lambda_true) count.

structural_zero <- rbinom(
  n = m,
  size = 1,
  prob = p_true
)

poisson_count <- rpois(
  n = m,
  lambda = lambda_true
)

count <- ifelse(
  structural_zero == 1,
  0,
  poisson_count
)

df <- data.frame(
  Unit = 1:m,
  Structural_Zero = structural_zero,
  Poisson_Count = poisson_count,
  Count = count
)

print(df)

write.csv(
  df,
  file = "outputs/zip_raw_data.csv",
  row.names = FALSE
)

#----------------------------------------------------------
# 3. Estimate ZIP parameters from simulated data
#----------------------------------------------------------

zero_prop <- mean(count == 0)
sample_mean <- mean(count)

#----------------------------------------------------------
# Moment estimation:
# E(C) = (1 - p) lambda
# P(C = 0) = p + (1 - p) exp(-lambda)
#
# Solve for lambda first:
# zero_prop = 1 - sample_mean/lambda + (sample_mean/lambda)*exp(-lambda)
# Then p = 1 - sample_mean/lambda
#----------------------------------------------------------

estimate_zip_mom <- function(count_data) {
  
  zero_prop <- mean(count_data == 0)
  sample_mean <- mean(count_data)
  
  if (sample_mean == 0) {
    stop("All observations are zero. ZIP parameters cannot be estimated by this moment method.")
  }
  
  f_lambda <- function(lambda) {
    1 - sample_mean / lambda +
      (sample_mean / lambda) * exp(-lambda) -
      zero_prop
  }
  
  lower <- sample_mean + 1e-6
  upper <- 100
  
  lambda_hat <- uniroot(
    f = f_lambda,
    interval = c(lower, upper)
  )$root
  
  p_hat <- 1 - sample_mean / lambda_hat
  
  list(
    p_hat = p_hat,
    lambda_hat = lambda_hat,
    zero_prop = zero_prop,
    sample_mean = sample_mean
  )
}

fit <- estimate_zip_mom(count)

p_hat <- fit$p_hat
lambda_hat <- fit$lambda_hat

cat("\nEstimated ZIP parameters:\n")
cat("Observed zero proportion =", round(fit$zero_prop, 4), "\n")
cat("Sample mean =", round(fit$sample_mean, 4), "\n")
cat("p_hat =", round(p_hat, 4), "\n")
cat("lambda_hat =", round(lambda_hat, 4), "\n")

#----------------------------------------------------------
# 4. Define ZIP pmf and cdf
#----------------------------------------------------------

dzip <- function(x, p, lambda) {
  
  ifelse(
    x == 0,
    p + (1 - p) * dpois(0, lambda),
    (1 - p) * dpois(x, lambda)
  )
}

pzip <- function(q, p, lambda) {
  
  sapply(
    q,
    function(k) {
      sum(dzip(0:k, p, lambda))
    }
  )
}

#----------------------------------------------------------
# 5. Construct exact UCL using estimated parameters
#----------------------------------------------------------

k_grid <- 0:50

cdf_values <- pzip(
  q = k_grid,
  p = p_hat,
  lambda = lambda_hat
)

UCL <- min(
  k_grid[cdf_values >= target_prob]
)

LCL <- 0

CL <- (1 - p_hat) * lambda_hat

cat("\nControl limits based on estimated ZIP model:\n")
cat("LCL =", LCL, "\n")
cat("CL =", round(CL, 4), "\n")
cat("UCL =", UCL, "\n")

#----------------------------------------------------------
# 6. Save summary and CDF table
#----------------------------------------------------------

summary_df <- data.frame(
  m = m,
  p_true = p_true,
  lambda_true = lambda_true,
  observed_zero_proportion = fit$zero_prop,
  sample_mean = fit$sample_mean,
  p_hat = p_hat,
  lambda_hat = lambda_hat,
  LCL = LCL,
  CL = CL,
  UCL = UCL,
  alpha = alpha,
  target_prob = target_prob
)

write.csv(
  summary_df,
  file = "outputs/zip_summary.csv",
  row.names = FALSE
)

cdf_df <- data.frame(
  Count = k_grid,
  ZIP_CDF = round(cdf_values, 6)
)

write.csv(
  cdf_df,
  file = "outputs/zip_cdf_table.csv",
  row.names = FALSE
)

print(summary_df)
print(head(cdf_df, 10))

#----------------------------------------------------------
# 7. Plot ZIP control chart
#----------------------------------------------------------

png(
  filename = "figs/zip_chart_example.png",
  width = 2200,
  height = 1400,
  res = 220
)

par(
  mar = c(5, 5, 4, 5),
  cex.axis = 1.4,
  cex.lab = 1.6,
  cex.main = 1.8
)

plot(
  x = df$Unit,
  y = df$Count,
  type = "b",
  pch = 19,
  lwd = 2,
  xlab = "Inspection Unit",
  ylab = expression(C[i]),
  main = "ZIP Control Chart",
  ylim = c(
    0,
    max(UCL, df$Count) + 1
  )
)

abline(h = CL,  lwd = 2)
abline(h = UCL, lwd = 2, lty = 2)
abline(h = LCL, lwd = 2, lty = 2)

x_pos <- m * 0.78

text(
  x = x_pos,
  y = UCL + 0.2,
  labels = paste0("UCL = ", UCL),
  pos = 4,
  cex = 1.2
)

text(
  x = x_pos,
  y = CL + 0.2,
  labels = paste0("CL = ", round(CL, 2)),
  pos = 4,
  cex = 1.2
)

text(
  x = x_pos,
  y = LCL + 0.2,
  labels = paste0("LCL = ", LCL),
  pos = 4,
  cex = 1.2
)

legend(
  "topleft",
  inset = c(0.03, 0.03),
  legend = c(
    expression(C[i]),
    "CL",
    "UCL/LCL"
  ),
  lty = c(1, 1, 2),
  pch = c(19, NA, NA),
  lwd = c(2, 2, 2),
  bty = "n",
  cex = 1.2
)

dev.off()

#----------------------------------------------------------
# 8. Final message
#----------------------------------------------------------

cat("\nFiles created:\n")
cat("figs/zip_chart_example.png\n")
cat("outputs/zip_raw_data.csv\n")
cat("outputs/zip_summary.csv\n")
cat("outputs/zip_cdf_table.csv\n")
cat("\n")