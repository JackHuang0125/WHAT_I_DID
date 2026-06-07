############################################################
# KDE teaching figures
# 1) KDE with different bandwidths
# 2) Histogram + KDE overlay
# 3) Histogram + multiple KDE curves
# 4) Rule-of-thumb bandwidth comparison
#
# Output files:
#   figs/kde_bandwidth.png
#   figs/hist_kde_overlay.png
#   figs/hist_kde_multi.png
#   figs/kde_rot_compare.png
############################################################

rm(list = ls())
set.seed(123)

setwd("E:/統計品質管制/Week6")

############################################################
# 0. Settings
############################################################
out_dir <- "figs"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Example data: slightly non-normal mixture to make KDE shape clearer
n <- 200
x <- c(rnorm(140, mean = 0,   sd = 1.0),
       rnorm(60,  mean = 2.5, sd = 0.6))

#-----------------------------
# Helper: save png
#-----------------------------
save_png <- function(filename, expr, width = 1600, height = 900, res = 150) {
  png(filename, width = width, height = height, res = res)
  expr
  dev.off()
}

#-----------------------------
# Helper: graphical settings
#-----------------------------
par_settings <- function() {
  par(
    mar = c(4.8, 5.0, 2.6, 1.2),
    cex.lab = 1.45,
    cex.axis = 1.25,
    cex.main = 1.35,
    lwd = 2
  )
}

############################################################
# 1. Rule-of-thumb bandwidths
############################################################
s_x   <- sd(x)
iqr_x <- IQR(x)

# Silverman:
# 0.9 * min(s, IQR/1.34) * n^(-1/5)
h_silverman <- 0.9 * min(s_x, iqr_x / 1.34) * n^(-1/5)

# Scott:
# 1.06 * s * n^(-1/5)
h_scott <- 1.06 * s_x * n^(-1/5)

# Normal reference:
# proportional to sigma * n^(-1/5)
# for Gaussian kernel this is the same constant as Scott
h_normal_ref <- ((4 / 3)^(1 / 5)) * s_x * n^(-1/5)

# Built-in R bandwidths for reference
h_bw_nrd0 <- bw.nrd0(x)  # Silverman-type default
h_bw_nrd  <- bw.nrd(x)   # Scott / normal-reference style

# Console summary
cat("====================================================\n")
cat("Rule-of-thumb bandwidths for KDE\n")
cat("====================================================\n")
cat(sprintf("Sample size n               : %d\n", n))
cat(sprintf("Standard deviation s        : %.4f\n", s_x))
cat(sprintf("Interquartile range IQR     : %.4f\n", iqr_x))
cat(sprintf("Silverman (manual)          : %.4f\n", h_silverman))
cat(sprintf("Scott (manual)              : %.4f\n", h_scott))
cat(sprintf("Normal reference (manual)   : %.4f\n", h_normal_ref))
cat(sprintf("bw.nrd0(x)                  : %.4f\n", h_bw_nrd0))
cat(sprintf("bw.nrd(x)                   : %.4f\n", h_bw_nrd))
cat("====================================================\n\n")

# Compact comparison table for console
bw_table <- data.frame(
  Method = c("Silverman", "Scott", "Normal reference"),
  Formula = c(
    "0.9 * min(s, IQR/1.34) * n^(-1/5)",
    "1.06 * s * n^(-1/5)",
    "(4/3)^(1/5) * sigma * n^(-1/5)"
  ),
  When_to_use = c(
    "Robust, default",
    "Near-normal data",
    "Theory (AMISE)"
  ),
  Bandwidth = c(h_silverman, h_scott, h_normal_ref),
  stringsAsFactors = FALSE
)

print(bw_table, row.names = FALSE)

############################################################
# 2. KDE with different bandwidths
############################################################
h_values <- c(0.20, 0.50, 1.00)

save_png(file.path(out_dir, "kde_bandwidth.png"), {
  par_settings()
  
  dens_list <- lapply(h_values, function(h) density(x, bw = h))
  ymax <- max(sapply(dens_list, function(d) max(d$y)))
  
  plot(
    dens_list[[1]],
    main = "KDE with Different Bandwidths",
    xlab = "x",
    ylab = "Density",
    ylim = c(0, ymax * 1.08),
    lty = 1
  )
  
  for (i in 2:length(dens_list)) {
    lines(dens_list[[i]], lty = i)
  }
  
  rug(x)
  legend(
    "topright",
    legend = paste("h =", format(h_values, nsmall = 2)),
    lty = seq_along(h_values),
    lwd = 3,
    cex = 1.20,
    bty = "n"
  )
})

############################################################
# 3. Histogram + one KDE overlay
############################################################
save_png(file.path(out_dir, "hist_kde_overlay.png"), {
  par_settings()
  
  hist(
    x,
    probability = TRUE,
    breaks = "FD",
    main = "Histogram vs KDE",
    xlab = "x",
    ylab = "Density",
    col = "gray85",
    border = "white",
    ylim = c(0, 0.35)
  )
  
  d_mid <- density(x, bw = 0.50)
  lines(d_mid, lwd = 3)
  
  rug(x)
  legend(
    "topright",
    legend = c("Histogram", "KDE (h = 0.50)"),
    lwd = c(8, 3),
    col = c("gray60", "black"),
    cex = 1.20,
    bty = "n"
  )
})

############################################################
# 4. Histogram + multiple KDE curves
############################################################
save_png(file.path(out_dir, "hist_kde_multi.png"), {
  par_settings()
  
  hist(
    x,
    probability = TRUE,
    breaks = "FD",
    main = "Histogram and KDEs with Different Bandwidths",
    xlab = "x",
    ylab = "Density",
    col = "gray88",
    border = "white",
    ylim = c(0, 0.35)
  )
  
  for (i in seq_along(h_values)) {
    lines(density(x, bw = h_values[i]), lwd = 3, lty = i)
  }
  
  rug(x)
  legend(
    "topright",
    legend = paste("KDE: h =", format(h_values, nsmall = 2)),
    lwd = 3,
    lty = seq_along(h_values),
    cex = 1.20,
    bty = "n"
  )
})

############################################################
# 5. Rule-of-thumb bandwidth comparison figure
############################################################
rot_values <- c(h_silverman, h_scott, h_normal_ref)
rot_labels <- c(
  paste0("Silverman: h = ", format(h_silverman, digits = 3, nsmall = 3)),
  paste0("Scott: h = ", format(h_scott, digits = 3, nsmall = 3)),
  paste0("Normal ref.: h = ", format(h_normal_ref, digits = 3, nsmall = 3))
)

save_png(file.path(out_dir, "kde_rot_compare.png"), {
  par_settings()
  
  dens_rot <- lapply(rot_values, function(h) density(x, bw = h))
  ymax <- max(sapply(dens_rot, function(d) max(d$y)))
  
  plot(
    dens_rot[[1]],
    main = "KDE with Rule-of-Thumb Bandwidths",
    xlab = "x",
    ylab = "Density",
    ylim = c(0, ymax * 1.08),
    lty = 1
  )
  
  for (i in 2:length(dens_rot)) {
    lines(dens_rot[[i]], lty = i)
  }
  
  rug(x)
  legend(
    "topright",
    legend = rot_labels,
    lty = seq_along(rot_values),
    lwd = 3,
    cex = 1.05,
    bty = "n"
  )
  
  mtext(
    "Silverman: robust/default   |   Scott: near-normal   |   Normal ref.: AMISE theory",
    side = 3, line = 0.2, cex = 0.95
  )
})

############################################################
# 6. Optional: export numerical summary to CSV
############################################################
write.csv(
  bw_table,
  file = file.path(out_dir, "kde_bandwidth_rules_summary.csv"),
  row.names = FALSE
)

############################################################
# 7. Print output paths
############################################################
cat("Files created:\n")
cat(file.path(out_dir, "kde_bandwidth.png"), "\n")
cat(file.path(out_dir, "hist_kde_overlay.png"), "\n")
cat(file.path(out_dir, "hist_kde_multi.png"), "\n")
cat(file.path(out_dir, "kde_rot_compare.png"), "\n")
cat(file.path(out_dir, "kde_bandwidth_rules_summary.csv"), "\n")