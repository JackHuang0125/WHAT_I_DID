############################################################
# Goodness-of-Fit Tests Comparison
# True distribution: Exponential(1)
# H0: Normal(mu, sigma^2)
############################################################

rm(list = ls())
set.seed(123)

setwd("E:/統計品質管制/Week6")

############################################################
# 0. Create folders
############################################################
if (!dir.exists("figs")) dir.create("figs")
if (!dir.exists("output")) dir.create("output")

############################################################
# 1. Generate data
############################################################
n <- 100
x <- rexp(n, rate = 1)   # right-skewed data

############################################################
# 2. Estimate Normal parameters
############################################################
mu_hat <- mean(x)
sigma_hat <- sd(x)

############################################################
# 3. Kolmogorov-Smirnov test
# Note:
# This uses estimated mean and sd, so strictly speaking
# this is not the classical fully specified KS setting.
############################################################
ks_result <- ks.test(x, "pnorm", mean = mu_hat, sd = sigma_hat)

############################################################
# 4. Pearson Chi-square test (grouped)
############################################################
k <- 6

# Equal-probability grid under fitted Normal
pgrid <- seq(0, 1, length.out = k + 1)

# Avoid -Inf and +Inf from qnorm(0), qnorm(1)
eps <- 1e-6
pgrid[1] <- eps
pgrid[length(pgrid)] <- 1 - eps

breaks <- qnorm(pgrid, mean = mu_hat, sd = sigma_hat)

# Extend endpoints so all observations are included in hist()
breaks[1] <- min(x) - 1e-8
breaks[length(breaks)] <- max(x) + 1e-8

# Observed counts
obs <- hist(x, breaks = breaks, plot = FALSE)$counts

# Expected probabilities under fitted Normal
prob <- diff(pnorm(breaks, mean = mu_hat, sd = sigma_hat))

# Normalize for numerical stability
prob <- prob / sum(prob)

# Expected counts
exp_counts <- n * prob

# Pearson statistic
X2 <- sum((obs - exp_counts)^2 / exp_counts)

# Degrees of freedom:
# number of bins - 1 - number of estimated parameters
df <- k - 1 - 2
pval_chi <- 1 - pchisq(X2, df = df)

############################################################
# 5. Likelihood Ratio Test (G^2)
############################################################
# Handle 0 * log(0 / E) safely by only summing over obs > 0
idx <- obs > 0
G2 <- 2 * sum(obs[idx] * log(obs[idx] / exp_counts[idx]))
pval_g <- 1 - pchisq(G2, df = df)

############################################################
# 6. Print results
############################################################
cat("===== Goodness-of-Fit Results =====\n\n")

cat("Sample size n =", n, "\n\n")

cat("Fitted Normal parameters:\n")
cat("mu_hat    =", round(mu_hat, 4), "\n")
cat("sigma_hat =", round(sigma_hat, 4), "\n\n")

cat("Kolmogorov-Smirnov Test:\n")
cat("D =", round(as.numeric(ks_result$statistic), 4),
    ", p-value =", signif(ks_result$p.value, 4), "\n\n")

cat("Pearson Chi-square Test:\n")
cat("X^2 =", round(X2, 4),
    ", df =", df,
    ", p-value =", signif(pval_chi, 4), "\n\n")

cat("Likelihood Ratio Test:\n")
cat("G^2 =", round(G2, 4),
    ", df =", df,
    ", p-value =", signif(pval_g, 4), "\n\n")

cat("Observed counts:\n")
print(obs)

cat("\nExpected counts:\n")
print(round(exp_counts, 2))

############################################################
# 7. Collect results into a table
############################################################
result_tab <- data.frame(
  Test = c("Kolmogorov-Smirnov", "Pearson Chi-square", "Likelihood Ratio"),
  Statistic = c(as.numeric(ks_result$statistic), X2, G2),
  DF = c(NA, df, df),
  P_value = c(ks_result$p.value, pval_chi, pval_g),
  Decision = c(
    ifelse(ks_result$p.value < 0.05, "Reject H0", "Do not reject"),
    ifelse(pval_chi < 0.05, "Reject H0", "Do not reject"),
    ifelse(pval_g < 0.05, "Reject H0", "Do not reject")
  ),
  stringsAsFactors = FALSE
)

print(result_tab)

############################################################
# 8. Save CSV outputs
############################################################
write.csv(result_tab,
          file = "output/gof_results.csv",
          row.names = FALSE)

obs_exp_tab <- data.frame(
  Bin = paste0("Bin ", 1:k),
  Observed = obs,
  Expected = round(exp_counts, 4)
)

write.csv(obs_exp_tab,
          file = "output/gof_observed_expected.csv",
          row.names = FALSE)

############################################################
# 9. Helper function: save LaTeX tabular only
############################################################
latex_tabular_only <- function(df, file,
                               align = NULL,
                               digits_stat = 4,
                               digits_p = 4) {
  if (is.null(align)) {
    align <- paste(rep("l", ncol(df)), collapse = "")
  }
  
  con <- file(file, "w")
  
  writeLines(sprintf("\\begin{tabular}{%s}", align), con)
  writeLines("\\toprule", con)
  
  header <- paste(colnames(df), collapse = " & ")
  writeLines(paste0(header, " \\\\"), con)
  writeLines("\\midrule", con)
  
  for (i in 1:nrow(df)) {
    row_vals <- vector("character", ncol(df))
    
    for (j in 1:ncol(df)) {
      val <- df[i, j]
      
      if (is.numeric(val)) {
        colname <- colnames(df)[j]
        if (colname %in% c("Statistic")) {
          row_vals[j] <- sprintf(paste0("%.", digits_stat, "f"), val)
        } else if (colname %in% c("P_value")) {
          row_vals[j] <- sprintf(paste0("%.", digits_p, "g"), val)
        } else if (colname %in% c("DF")) {
          row_vals[j] <- ifelse(is.na(val), "", as.character(val))
        } else {
          row_vals[j] <- as.character(val)
        }
      } else {
        row_vals[j] <- as.character(val)
      }
    }
    
    line <- paste(row_vals, collapse = " & ")
    writeLines(paste0(line, " \\\\"), con)
  }
  
  writeLines("\\bottomrule", con)
  writeLines("\\end{tabular}", con)
  
  close(con)
}

############################################################
# 10. Save LaTeX tables
############################################################
latex_tabular_only(
  result_tab,
  file = "output/tab_gof_results.tex",
  align = "lcccc"
)

latex_tabular_only(
  obs_exp_tab,
  file = "output/tab_gof_observed_expected.tex",
  align = "lcc"
)

############################################################
# 11. Visualization
############################################################
png("figs/gof_comparison.png", width = 1600, height = 900, res = 150)

par(mfrow = c(1, 2), mar = c(4.8, 4.8, 3.2, 1.2))

#----------------------------------
# Left panel: Histogram + fitted Normal density
#----------------------------------
hist(x,
     probability = TRUE,
     breaks = 12,
     main = "Histogram vs Fitted Normal Density",
     xlab = "x",
     cex.main = 1.45,
     cex.lab = 1.25,
     cex.axis = 1.15)

curve(dnorm(x, mean = mu_hat, sd = sigma_hat),
      add = TRUE, lwd = 3)

legend("topright",
       legend = c("Histogram", "Fitted Normal density"),
       lwd = c(10, 3),
       cex = 1.05,
       bty = "n")

#----------------------------------
# Right panel: ECDF vs fitted Normal CDF
#----------------------------------
plot(ecdf(x),
     main = "ECDF vs Fitted Normal CDF",
     xlab = "x",
     ylab = "F(x)",
     lwd = 2,
     verticals = TRUE,
     do.points = FALSE,
     cex.main = 1.45,
     cex.lab = 1.25,
     cex.axis = 1.15)

curve(pnorm(x, mean = mu_hat, sd = sigma_hat),
      add = TRUE, col = "red", lwd = 3)

legend("bottomright",
       legend = c("ECDF", "Fitted Normal CDF"),
       lwd = c(2, 3),
       col = c("black", "red"),
       cex = 1.05,
       bty = "n")

dev.off()

############################################################
# 12. Optional: one more figure for grouped counts
############################################################
png("figs/gof_grouped_counts.png", width = 1200, height = 900, res = 150)

bp <- barplot(rbind(obs, exp_counts),
              beside = TRUE,
              names.arg = paste0("B", 1:k),
              main = "Observed vs Expected Counts",
              ylab = "Count",
              xlab = "Bin",
              cex.main = 1.45,
              cex.lab = 1.25,
              cex.axis = 1.15,
              cex.names = 1.15)

legend("topright",
       legend = c("Observed", "Expected"),
       fill = c("gray40", "gray75"),
       cex = 1.1,
       bty = "n")

dev.off()

############################################################
# 13. End message
############################################################
cat("\nFiles saved:\n")
cat("  figs/gof_comparison.png\n")
cat("  figs/gof_grouped_counts.png\n")
cat("  output/gof_results.csv\n")
cat("  output/gof_observed_expected.csv\n")
cat("  output/tab_gof_results.tex\n")
cat("  output/tab_gof_observed_expected.tex\n")