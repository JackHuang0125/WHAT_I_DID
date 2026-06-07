
setwd("E:/統計品質管制/Week6")
############################################################
# 0. Setup: folders
############################################################
if (!dir.exists("figs")) dir.create("figs")
if (!dir.exists("outputs")) dir.create("outputs")

############################################################
# 1. Simulate noisy non-linear data
############################################################
set.seed(42)
x <- seq(0, 4 * pi, length.out = 150)
y <- sin(x) + rnorm(150, sd = 0.4)

############################################################
# 2. Fit smoothers
############################################################
# Nadaraya-Watson
fit_nw <- ksmooth(x, y,
                  kernel = "normal",
                  bandwidth = 1.5)

# Loess
fit_loess <- loess(y ~ x, span = 0.25)
y_loess_hat <- predict(fit_loess, newdata = x)

############################################################
# 3. Save numerical results to outputs/
############################################################
results <- data.frame(
  x = x,
  y = y,
  true_fx = sin(x),
  nw_y = fit_nw$y,
  loess_y = y_loess_hat
)

write.csv(results,
          file = "outputs/nonparametric_regression_results.csv",
          row.names = FALSE)

############################################################
# 4. Save plot to figs/
############################################################
png("figs/nonparametric_regression.png",
    width = 1600, height = 900, res = 150)

par(cex = 1.3, mar = c(4, 4, 2, 1))

plot(x, y,
     col = "gray40", pch = 16,
     xlab = "x", ylab = "y",
     main = "Nonparametric Regression Smoothing")

lines(x, sin(x),
      col = "black", lty = 2, lwd = 2)

lines(fit_nw$x, fit_nw$y,
      col = "blue", lwd = 3)

lines(x, y_loess_hat,
      col = "red", lwd = 3)

legend("topright",
       legend = c("True f(x)",
                  "Nadaraya-Watson",
                  "Local Linear (Loess)"),
       col = c("black", "blue", "red"),
       lty = c(2, 1, 1),
       lwd = c(2, 3, 3),
       bty = "n",
       cex = 1.1)

dev.off()

############################################################
# 5. Export LaTeX table to outputs/
############################################################
latex_tabular_only <- function(df, file) {
  writeLines(c(
    "\\begin{tabular}{rrrrr}",
    "\\toprule",
    "x & y & true f(x) & NW & Loess \\\\",
    "\\midrule"
  ), con = file)
  
  write.table(df[1:10, ],
              file = file,
              append = TRUE,
              sep = " & ",
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE,
              eol = " \\\\\n")
  
  cat("\\bottomrule\n\\end{tabular}\n",
      file = file,
      append = TRUE)
}

latex_tabular_only(results,
                   "outputs/nonparametric_regression_table.tex")