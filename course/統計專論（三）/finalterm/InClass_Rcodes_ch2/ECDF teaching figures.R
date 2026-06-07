############################################################
# Section 2.8 — ECDF teaching figures
# Generate:
#   1. figs/ecdf_vs_true_overlay.png
#   2. figs/ecdf_ties_zoom.png
#
# Purpose:
#   - Compare empirical CDF with a true CDF
#   - Show how ties create larger jumps of size k/n
############################################################

rm(list = ls())

setwd("E:/統計品質管制/Week6")

############################################################
# 0. Create output folder
############################################################
if (!dir.exists("figs")) dir.create("figs", recursive = TRUE)

############################################################
# 1. Global graphical settings
############################################################
img_width  <- 1600
img_height <- 900
img_res    <- 150

axis_cex <- 1.35
lab_cex  <- 1.55
main_cex <- 1.55
legend_cex <- 1.20
line_wd  <- 3

############################################################
# 2. Example 2.8 data
############################################################
x_example <- c(2.3, 1.5, 3.4, 1.5, 0.9, 1.7, 1.5, 1.7, 2.1, 0.7)
n_example <- length(x_example)
x_sorted  <- sort(x_example)

cat("Ordered observations:\n")
print(x_sorted)

############################################################
# 3. Figure 1: Empirical CDF vs True CDF (overlay)
#
# Here we generate a sample from N(0,1) so that the true cdf
# is known and can be overlaid by pnorm(x).
############################################################
set.seed(123)
x_norm <- rnorm(50, mean = 0, sd = 1)

png(
  filename = "figs/ecdf_vs_true_overlay.png",
  width    = img_width,
  height   = img_height,
  res      = img_res
)

par(
  mar = c(5.2, 5.2, 2.0, 1.5),
  mgp = c(3.0, 0.9, 0)
)

# Plot ECDF first
plot(
  ecdf(x_norm),
  verticals = TRUE,
  do.points = FALSE,
  lwd       = line_wd,
  col       = "blue",
  main      = "",
  xlab      = "x",
  ylab      = "CDF",
  cex.axis  = axis_cex,
  cex.lab   = lab_cex
)

# Overlay true normal cdf
curve(
  pnorm(x, mean = 0, sd = 1),
  from = min(x_norm) - 1,
  to   = max(x_norm) + 1,
  add  = TRUE,
  lwd  = line_wd,
  col  = "red"
)

legend(
  "bottomright",
  legend = c("Empirical CDF", "True CDF"),
  col    = c("blue", "red"),
  lwd    = c(line_wd, line_wd),
  bty    = "n",
  cex    = legend_cex
)

mtext(
  "Empirical CDF vs True CDF",
  side = 3,
  line = 0.2,
  cex  = main_cex,
  font = 2
)

dev.off()

############################################################
# 4. Figure 2: Ties -> jump size = k/n
#
# Use Example 2.8 data directly.
# Repeated values:
#   1.5 appears 3 times  -> jump = 3/10
#   1.7 appears 2 times  -> jump = 2/10
############################################################

# Helper values for annotation
tab_vals <- table(x_example)
k_15 <- as.numeric(tab_vals["1.5"])
k_17 <- as.numeric(tab_vals["1.7"])

# ECDF values immediately before and at the repeated values
Fn_before_15 <- sum(x_example < 1.5) / n_example
Fn_at_15     <- sum(x_example <= 1.5) / n_example

Fn_before_17 <- sum(x_example < 1.7) / n_example
Fn_at_17     <- sum(x_example <= 1.7) / n_example

png(
  filename = "figs/ecdf_ties_zoom.png",
  width    = img_width,
  height   = img_height,
  res      = img_res
)

par(
  mar = c(5.2, 5.2, 2.0, 1.5),
  mgp = c(3.0, 0.9, 0)
)

plot(
  ecdf(x_example),
  verticals = TRUE,
  do.points = FALSE,
  lwd       = line_wd,
  col       = "blue",
  main      = "",
  xlab      = "x",
  ylab      = expression(F[n](x)),
  xlim      = c(0.6, 3.5),
  ylim      = c(0, 1.05),
  cex.axis  = axis_cex,
  cex.lab   = lab_cex
)

mtext(
  "Effect of Ties on the Empirical CDF",
  side = 3,
  line = 0.2,
  cex  = main_cex,
  font = 2
)

# Reference lines at repeated values
abline(v = 1.5, col = "red",    lwd = 2, lty = 2)
abline(v = 1.7, col = "purple", lwd = 2, lty = 2)

# Highlight jump segments
segments(1.5, Fn_before_15, 1.5, Fn_at_15, lwd = 5, col = "red")
segments(1.7, Fn_before_17, 1.7, Fn_at_17, lwd = 5, col = "purple")

# Add arrows showing jump sizes
arrows(
  x0 = 1.43, y0 = Fn_before_15,
  x1 = 1.43, y1 = Fn_at_15,
  length = 0.08, angle = 20, code = 3,
  lwd = 2, col = "red"
)

arrows(
  x0 = 1.77, y0 = Fn_before_17,
  x1 = 1.77, y1 = Fn_at_17,
  length = 0.08, angle = 20, code = 3,
  lwd = 2, col = "purple"
)

# Text annotations
text(
  x = 1.52, y = (Fn_before_15 + Fn_at_15) / 2,
  labels = expression("jump =" ~ 3/10),
  pos = 4, cex = 1.25, col = "red"
)

text(
  x = 1.72, y = (Fn_before_17 + Fn_at_17) / 2,
  labels = expression("jump =" ~ 2/10),
  pos = 4, cex = 1.25, col = "purple"
)

# Mark repeated x-values on axis region
points(c(1.5, 1.7), c(0, 0), pch = 16, cex = 1.3, col = c("red", "purple"))

legend(
  "bottomright",
  legend = c("ECDF", "Repeated value: 1.5", "Repeated value: 1.7"),
  col    = c("blue", "red", "purple"),
  lwd    = c(line_wd, 2, 2),
  lty    = c(1, 2, 2),
  bty    = "n",
  cex    = legend_cex
)

dev.off()

############################################################
# 5. Optional: print useful quantities in console
############################################################
cat("\nRepeated-value summary:\n")
print(tab_vals)

cat("\nJump at x = 1.5:\n")
cat("Before 1.5 =", Fn_before_15, "\n")
cat("At 1.5     =", Fn_at_15, "\n")
cat("Jump size  =", Fn_at_15 - Fn_before_15, "=", k_15, "/", n_example, "\n")

cat("\nJump at x = 1.7:\n")
cat("Before 1.7 =", Fn_before_17, "\n")
cat("At 1.7     =", Fn_at_17, "\n")
cat("Jump size  =", Fn_at_17 - Fn_before_17, "=", k_17, "/", n_example, "\n")

cat("\nDone.\nFiles created:\n")
cat(" - figs/ecdf_vs_true_overlay.png\n")
cat(" - figs/ecdf_ties_zoom.png\n")