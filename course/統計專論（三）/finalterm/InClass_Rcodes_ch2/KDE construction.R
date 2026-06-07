############################################################
# KDE construction:
# 1. side-by-side KDE comparison
# 2. each Gaussian bump saved separately
# 3. all bumps + KDE saved together
############################################################

rm(list = ls())

setwd("E:/統計品質管制/Week6")

#-----------------------------------------------------------
# Output folders
#-----------------------------------------------------------
dir.create("figs", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

#-----------------------------------------------------------
# Data
#-----------------------------------------------------------
x <- c(2, 3, 5, 6, 8)

# Main target bandwidths
h_vals <- c(1, 0.5)

# Evaluation grid
x_grid <- seq(0, 10, length.out = 2000)

#-----------------------------------------------------------
# Manual Gaussian KDE
#-----------------------------------------------------------
kde_manual <- function(x, x_grid, h) {
  n <- length(x)
  sapply(x_grid, function(t) {
    mean(dnorm((t - x) / h)) / h
  })
}

#-----------------------------------------------------------
# Individual bump:
# bump_i(x) = (1/h) K((x - X_i)/h)
#-----------------------------------------------------------
bump_manual <- function(xi, x_grid, h) {
  dnorm((x_grid - xi) / h) / h
}

#-----------------------------------------------------------
# Compute KDEs
#-----------------------------------------------------------
f_h1  <- kde_manual(x, x_grid, h = 1)
f_h05 <- kde_manual(x, x_grid, h = 0.5)

#-----------------------------------------------------------
# 1. Side-by-side comparison for h = 1 and h = 0.5
#    (same y-axis for fair comparison)
#-----------------------------------------------------------
y_max_compare <- max(c(f_h1, f_h05)) * 1.08

png("figs/kde_h_comparison.png",
    width = 1600, height = 900, res = 150)

par(mfrow = c(1, 2),
    mar = c(4.8, 4.8, 3.2, 1.2),
    cex.lab = 1.6,
    cex.axis = 1.35,
    cex.main = 1.45,
    lwd = 3)

# Left panel: h = 1
plot(x_grid, f_h1, type = "l",
     ylim = c(0, y_max_compare),
     main = "Bandwidth h = 1",
     xlab = "x", ylab = "Density")
points(x, rep(0, length(x)), pch = 19, cex = 1.5)
abline(v = 5, lty = 2, lwd = 2)
mtext("More smoothing", side = 3, line = 0.2, cex = 1.0)

# Right panel: h = 0.5
plot(x_grid, f_h05, type = "l",
     ylim = c(0, y_max_compare),
     main = "Bandwidth h = 0.5",
     xlab = "x", ylab = "Density")
points(x, rep(0, length(x)), pch = 19, cex = 1.5)
abline(v = 5, lty = 2, lwd = 2)
mtext("Less smoothing", side = 3, line = 0.2, cex = 1.0)

dev.off()

#-----------------------------------------------------------
# 2. Construct all bumps for h = 0.5
#-----------------------------------------------------------
h <- 0.5
n <- length(x)

bumps_h05 <- sapply(x, function(xi) bump_manual(xi, x_grid, h))
colnames(bumps_h05) <- paste0("bump_X", x)

kde_from_bumps_h05 <- rowMeans(bumps_h05)

#-----------------------------------------------------------
# Save numerical results
#-----------------------------------------------------------
out_df <- data.frame(
  x_grid = x_grid,
  bumps_h05,
  kde_h05 = kde_from_bumps_h05
)

write.csv(out_df,
          file = "output/kde_h05_bumps.csv",
          row.names = FALSE)

#-----------------------------------------------------------
# 3. Plot all bumps + KDE together
#-----------------------------------------------------------
# Use base default colors for clean teaching display
png("figs/kde_h05_bumps_all.png",
    width = 1600, height = 900, res = 150)

par(mar = c(4.8, 4.8, 3.2, 1.5),
    cex.lab = 1.6,
    cex.axis = 1.35,
    cex.main = 1.45,
    lwd = 3)

y_max_all <- max(c(bumps_h05, kde_from_bumps_h05)) * 1.08

plot(x_grid, bumps_h05[, 1], type = "l",
     lty = 2,
     ylim = c(0, y_max_all),
     main = "Gaussian bumps and their average (h = 0.5)",
     xlab = "x", ylab = "Density")

if (n >= 2) {
  for (j in 2:n) {
    lines(x_grid, bumps_h05[, j], lty = 2)
  }
}

lines(x_grid, kde_from_bumps_h05, lwd = 4)

points(x, rep(0, n), pch = 19, cex = 1.4)

legend("topright",
       legend = c(paste0("Bump at X = ", x), "KDE (average)"),
       lty = c(rep(2, n), 1),
       lwd = c(rep(2, n), 4),
       cex = 1.0,
       bty = "n")

dev.off()

#-----------------------------------------------------------
# 4. Save each bump separately
#-----------------------------------------------------------
y_max_bump <- max(bumps_h05) * 1.08

for (j in seq_along(x)) {
  
  file_name <- paste0("figs/kde_h05_bump_", j, ".png")
  
  png(file_name,
      width = 1400, height = 900, res = 150)
  
  par(mar = c(4.8, 4.8, 3.2, 1.2),
      cex.lab = 1.6,
      cex.axis = 1.35,
      cex.main = 1.45,
      lwd = 3)
  
  plot(x_grid, bumps_h05[, j], type = "l",
       ylim = c(0, y_max_bump),
       main = paste0("Single Gaussian bump centered at X = ", x[j],
                     " (h = 0.5)"),
       xlab = "x", ylab = "Density")
  
  abline(v = x[j], lty = 2, lwd = 2)
  points(x[j], 0, pch = 19, cex = 1.5)
  
  dev.off()
}

#-----------------------------------------------------------
# 5. Optional: one figure with 5 separate bump panels
#-----------------------------------------------------------
png("figs/kde_h05_bumps_panel.png",
    width = 1800, height = 900, res = 150)

par(mfrow = c(2, 3),
    mar = c(4.2, 4.2, 2.4, 1.0),
    cex.lab = 1.25,
    cex.axis = 1.1,
    cex.main = 1.15,
    lwd = 2.5)

for (j in seq_along(x)) {
  plot(x_grid, bumps_h05[, j], type = "l",
       ylim = c(0, y_max_bump),
       main = paste0("Bump at X = ", x[j]),
       xlab = "x", ylab = "Density")
  abline(v = x[j], lty = 2, lwd = 1.8)
  points(x[j], 0, pch = 19, cex = 1.2)
}

# last panel: average KDE
plot(x_grid, kde_from_bumps_h05, type = "l",
     ylim = c(0, max(kde_from_bumps_h05) * 1.08),
     main = "Average = KDE",
     xlab = "x", ylab = "Density")
points(x, rep(0, n), pch = 19, cex = 1.2)

dev.off()

cat("All figures and CSV files have been written to:\n")
cat("  figs/\n")
cat("  output/\n")



############################################################
# 6. Construct all bumps for h = 1  (NEW)
############################################################
h <- 1
n <- length(x)

bumps_h1 <- sapply(x, function(xi) bump_manual(xi, x_grid, h))
colnames(bumps_h1) <- paste0("bump_X", x)

kde_from_bumps_h1 <- rowMeans(bumps_h1)

#-----------------------------------------------------------
# Save numerical results
#-----------------------------------------------------------
out_df_h1 <- data.frame(
  x_grid = x_grid,
  bumps_h1,
  kde_h1 = kde_from_bumps_h1
)

write.csv(out_df_h1,
          file = "output/kde_h1_bumps.csv",
          row.names = FALSE)

#-----------------------------------------------------------
# 7. Plot all bumps + KDE (h = 1)
#-----------------------------------------------------------
png("figs/kde_h1_bumps_all.png",
    width = 1600, height = 900, res = 150)

par(mar = c(4.8, 4.8, 3.2, 1.5),
    cex.lab = 1.6,
    cex.axis = 1.35,
    cex.main = 1.45,
    lwd = 3)

y_max_all_h1 <- max(c(bumps_h1, kde_from_bumps_h1)) * 1.08

plot(x_grid, bumps_h1[,1], type = "l",
     lty = 2,
     ylim = c(0, y_max_all_h1),
     main = "Gaussian bumps and KDE (h = 1)",
     xlab = "x", ylab = "Density")

if (n >= 2) {
  for (j in 2:n) {
    lines(x_grid, bumps_h1[, j], lty = 2)
  }
}

lines(x_grid, kde_from_bumps_h1, lwd = 4)

points(x, rep(0, n), pch = 19, cex = 1.4)

legend("topright",
       legend = c(paste0("Bump at X = ", x), "KDE (average)"),
       lty = c(rep(2, n), 1),
       lwd = c(rep(2, n), 4),
       cex = 1.0,
       bty = "n")

dev.off()

#-----------------------------------------------------------
# 8. Save each bump separately (h = 1)
#-----------------------------------------------------------
y_max_bump_h1 <- max(bumps_h1) * 1.08

for (j in seq_along(x)) {
  
  file_name <- paste0("figs/kde_h1_bump_", j, ".png")
  
  png(file_name,
      width = 1400, height = 900, res = 150)
  
  par(mar = c(4.8, 4.8, 3.2, 1.2),
      cex.lab = 1.6,
      cex.axis = 1.35,
      cex.main = 1.45,
      lwd = 3)
  
  plot(x_grid, bumps_h1[, j], type = "l",
       ylim = c(0, y_max_bump_h1),
       main = paste0("Single bump at X = ", x[j], " (h = 1)"),
       xlab = "x", ylab = "Density")
  
  abline(v = x[j], lty = 2, lwd = 2)
  points(x[j], 0, pch = 19, cex = 1.5)
  
  dev.off()
}

#-----------------------------------------------------------
# 9. Panel plot (h = 1)
#-----------------------------------------------------------
png("figs/kde_h1_bumps_panel.png",
    width = 1800, height = 900, res = 150)

par(mfrow = c(2,3),
    mar = c(4.2, 4.2, 2.4, 1.0),
    cex.lab = 1.25,
    cex.axis = 1.1,
    cex.main = 1.15,
    lwd = 2.5)

for (j in seq_along(x)) {
  plot(x_grid, bumps_h1[, j], type = "l",
       ylim = c(0, y_max_bump_h1),
       main = paste0("Bump at X = ", x[j]),
       xlab = "x", ylab = "Density")
  abline(v = x[j], lty = 2, lwd = 1.8)
  points(x[j], 0, pch = 19, cex = 1.2)
}

# KDE panel
plot(x_grid, kde_from_bumps_h1, type = "l",
     ylim = c(0, max(kde_from_bumps_h1)*1.08),
     main = "Average = KDE (h = 1)",
     xlab = "x", ylab = "Density")
points(x, rep(0, n), pch = 19, cex = 1.2)

dev.off()


