############################################################
# Example 3.1 / 3.2: Xbar-R chart and Xbar-s chart
# Phase I: n = 20, m = 5
# Phase II: next 4 subgroups
# Goal:
#   - Xbar-R chart for mean / range monitoring
#   - Xbar-s chart for mean / sd monitoring
#   - comparison PNG for R chart and s chart in one figure
# Save figures to figs/ and outputs to outputs/
############################################################

rm(list = ls())
set.seed(123)

# Optional: set your working directory
setwd("E:/統計品質管制/Week7")

############################
# 0. Create folders
############################
dir.create("figs", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

############################
# 1. Scenario
############################
n1 <- 20              # Phase I subgroups
n2 <- 4               # Phase II subgroups
m  <- 5               # subgroup size
z  <- 3               # 3-sigma limits

mu0   <- 79.533       # in-control mean
sigma <- 3.76         # in-control SD
shift <- 7.0          # mean shift at subgroup 24

# Constants for m = 5
d1 <- 2.326
d2 <- 0.864
d3 <- gamma(m / 2) / gamma((m - 1) / 2) * sqrt(2 / (m - 1))

############################
# 2. Simulate raw data
############################
# Phase I: all in control
X_phase1 <- matrix(
  rnorm(n1 * m, mean = mu0, sd = sigma),
  nrow = n1, ncol = m, byrow = TRUE
)

# Phase II:
# subgroups 21-23 still in control
X_phase2 <- matrix(
  rnorm(n2 * m, mean = mu0, sd = sigma),
  nrow = n2, ncol = m, byrow = TRUE
)

# subgroup 24 has a mean shift
X_phase2[n2, ] <- rnorm(m, mean = mu0 + shift, sd = sigma)

# combine all raw data
X_all <- rbind(X_phase1, X_phase2)

############################
# 3. Subgroup summaries
############################
xbar_phase1 <- rowMeans(X_phase1)
R_phase1 <- apply(X_phase1, 1, function(x) max(x) - min(x))
s_phase1 <- apply(X_phase1, 1, sd)

xbar_phase2 <- rowMeans(X_phase2)
R_phase2 <- apply(X_phase2, 1, function(x) max(x) - min(x))
s_phase2 <- apply(X_phase2, 1, sd)

xbar_all <- c(xbar_phase1, xbar_phase2)
R_all    <- c(R_phase1, R_phase2)
s_all    <- c(s_phase1, s_phase2)

subgroup <- 1:(n1 + n2)

############################
# 4. Construct limits from Phase I
############################
xbarbar <- mean(xbar_phase1)
Rbar    <- mean(R_phase1)
sbar    <- mean(s_phase1)

# ----- Xbar-R chart -----
A2 <- z / (d1 * sqrt(m))
D3 <- max(0, 1 - z * d2 / d1)
D4 <- 1 + z * d2 / d1

UCL_xbar_R <- xbarbar + A2 * Rbar
CL_xbar_R  <- xbarbar
LCL_xbar_R <- xbarbar - A2 * Rbar

UCL_R <- D4 * Rbar
CL_R  <- Rbar
LCL_R <- D3 * Rbar

# ----- Xbar-s chart -----
A3 <- z / (d3 * sqrt(m))
Bs3 <- max(0, 1 - z * sqrt(1 - d3^2) / d3)
Bs4 <- 1 + z * sqrt(1 - d3^2) / d3

UCL_xbar_s <- xbarbar + A3 * sbar
CL_xbar_s  <- xbarbar
LCL_xbar_s <- xbarbar - A3 * sbar

UCL_s <- Bs4 * sbar
CL_s  <- sbar
LCL_s <- Bs3 * sbar

############################
# 5. Signal indicators
############################
xbar_R_signal <- (xbar_all < LCL_xbar_R | xbar_all > UCL_xbar_R)
R_signal      <- (R_all    < LCL_R      | R_all    > UCL_R)

xbar_s_signal <- (xbar_all < LCL_xbar_s | xbar_all > UCL_xbar_s)
s_signal      <- (s_all    < LCL_s      | s_all    > UCL_s)

############################
# 6. Save outputs
############################
limits <- data.frame(
  Chart = c("Xbar_R", "R", "Xbar_s", "s"),
  LCL   = c(LCL_xbar_R, LCL_R, LCL_xbar_s, LCL_s),
  CL    = c(CL_xbar_R,  CL_R,  CL_xbar_s,  CL_s),
  UCL   = c(UCL_xbar_R, UCL_R, UCL_xbar_s, UCL_s)
)

summary_data <- data.frame(
  Subgroup = subgroup,
  Phase = c(rep("Phase I", n1), rep("Phase II", n2)),
  Xbar = round(xbar_all, 3),
  R = round(R_all, 3),
  s = round(s_all, 3),
  Xbar_R_signal = xbar_R_signal,
  R_signal = R_signal,
  Xbar_s_signal = xbar_s_signal,
  s_signal = s_signal
)

raw_data <- data.frame(
  Subgroup = rep(subgroup, each = m),
  Phase = rep(c(rep("Phase I", n1), rep("Phase II", n2)), each = m),
  Obs = rep(1:m, times = n1 + n2),
  Value = as.vector(t(X_all))
)

write.csv(limits, "outputs/example32_control_limits_all.csv", row.names = FALSE)
write.csv(summary_data, "outputs/example32_subgroup_summaries.csv", row.names = FALSE)
write.csv(raw_data, "outputs/example32_raw_data.csv", row.names = FALSE)

############################
# 7. Print key results
############################
cat("===== Phase I Estimates =====\n")
cat("xbarbar =", round(xbarbar, 3), "\n")
cat("Rbar    =", round(Rbar, 3), "\n")
cat("sbar    =", round(sbar, 3), "\n")
cat("d3(5)   =", round(d3, 3), "\n\n")

cat("===== Xbar-R Chart Limits =====\n")
cat("LCL =", round(LCL_xbar_R, 3), "\n")
cat("CL  =", round(CL_xbar_R, 3), "\n")
cat("UCL =", round(UCL_xbar_R, 3), "\n\n")

cat("===== R Chart Limits =====\n")
cat("LCL =", round(LCL_R, 3), "\n")
cat("CL  =", round(CL_R, 3), "\n")
cat("UCL =", round(UCL_R, 3), "\n\n")

cat("===== Xbar-s Chart Limits =====\n")
cat("LCL =", round(LCL_xbar_s, 3), "\n")
cat("CL  =", round(CL_xbar_s, 3), "\n")
cat("UCL =", round(UCL_xbar_s, 3), "\n\n")

cat("===== s Chart Limits =====\n")
cat("LCL =", round(LCL_s, 3), "\n")
cat("CL  =", round(CL_s, 3), "\n")
cat("UCL =", round(UCL_s, 3), "\n\n")

cat("===== Signals =====\n")
cat("Xbar-R chart signal subgroups:", which(xbar_R_signal), "\n")
cat("R chart signal subgroups     :", which(R_signal), "\n")
cat("Xbar-s chart signal subgroups:", which(xbar_s_signal), "\n")
cat("s chart signal subgroups     :", which(s_signal), "\n")

############################
# 8. Save Xbar-R chart PNG
############################
png("figs/example31_xbar_chart_using_R.png", width = 900, height = 600)

par(cex = 1.2)

plot(
  subgroup, xbar_all, type = "b", pch = 19,
  ylim = range(c(LCL_xbar_R, xbar_all, UCL_xbar_R)),
  xlab = "Subgroup",
  ylab = expression(bar(X)[i]),
  main = expression(bar(X) * " Chart using " * bar(R)),
  cex.main = 1.5,
  cex.lab  = 1.3,
  cex.axis = 1.2
)

abline(h = UCL_xbar_R, col = "red",  lwd = 2, lty = 2)
abline(h = CL_xbar_R,  col = "blue", lwd = 2)
abline(h = LCL_xbar_R, col = "red",  lwd = 2, lty = 2)
abline(v = n1 + 0.5, col = "darkgreen", lwd = 2, lty = 3)

idx_xr <- which(xbar_R_signal)
if(length(idx_xr) > 0){
  points(idx_xr, xbar_all[idx_xr], col = "red", pch = 19, cex = 1.5)
  text(idx_xr, xbar_all[idx_xr], labels = idx_xr, pos = 3, col = "red", cex = 1.1)
}

legend(
  "topleft",
  legend = c(
    "Data",
    paste0("CL = ", round(CL_xbar_R, 3)),
    paste0("UCL = ", round(UCL_xbar_R, 3)),
    paste0("LCL = ", round(LCL_xbar_R, 3)),
    "Phase I / II split"
  ),
  col    = c("black", "blue", "red", "red", "darkgreen"),
  lty    = c(1, 1, 2, 2, 3),
  lwd    = c(1, 2, 2, 2, 2),
  pch    = c(19, NA, NA, NA, NA),
  cex    = 1.0,
  pt.cex = 1.2,
  bty    = "n"
)

dev.off()

############################
# 9. Save Xbar-s chart PNG
############################
png("figs/example32_xbar_chart_using_s.png", width = 900, height = 600)

par(cex = 1.2)

plot(
  subgroup, xbar_all, type = "b", pch = 19,
  ylim = range(c(LCL_xbar_s, xbar_all, UCL_xbar_s)),
  xlab = "Subgroup",
  ylab = expression(bar(X)[i]),
  main = expression(bar(X) * " Chart using " * bar(s)),
  cex.main = 1.5,
  cex.lab  = 1.3,
  cex.axis = 1.2
)

abline(h = UCL_xbar_s, col = "red",  lwd = 2, lty = 2)
abline(h = CL_xbar_s,  col = "blue", lwd = 2)
abline(h = LCL_xbar_s, col = "red",  lwd = 2, lty = 2)
abline(v = n1 + 0.5, col = "darkgreen", lwd = 2, lty = 3)

idx_xs <- which(xbar_s_signal)
if(length(idx_xs) > 0){
  points(idx_xs, xbar_all[idx_xs], col = "red", pch = 19, cex = 1.5)
  text(idx_xs, xbar_all[idx_xs], labels = idx_xs, pos = 3, col = "red", cex = 1.1)
}

legend(
  "topleft",
  legend = c(
    "Data",
    paste0("CL = ", round(CL_xbar_s, 3)),
    paste0("UCL = ", round(UCL_xbar_s, 3)),
    paste0("LCL = ", round(LCL_xbar_s, 3)),
    "Phase I / II split"
  ),
  col    = c("black", "blue", "red", "red", "darkgreen"),
  lty    = c(1, 1, 2, 2, 3),
  lwd    = c(1, 2, 2, 2, 2),
  pch    = c(19, NA, NA, NA, NA),
  cex    = 1.0,
  pt.cex = 1.2,
  bty    = "n"
)

dev.off()

############################
# 10. Save comparison PNG: R chart and s chart
############################
png("figs/example32_R_vs_s_comparison.png", width = 1400, height = 650)

par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3.5, 1.5), cex = 1.1)

# ----- Left: R chart -----
plot(
  subgroup, R_all, type = "b", pch = 19,
  ylim = range(c(LCL_R, R_all, UCL_R)),
  xlab = "Subgroup",
  ylab = expression(R[i]),
  main = "R Chart",
  cex.main = 1.4,
  cex.lab  = 1.2,
  cex.axis = 1.1
)

abline(h = UCL_R, col = "red",  lwd = 2, lty = 2)
abline(h = CL_R,  col = "blue", lwd = 2)
abline(h = LCL_R, col = "red",  lwd = 2, lty = 2)
abline(v = n1 + 0.5, col = "darkgreen", lwd = 2, lty = 3)

idx_r <- which(R_signal)
if(length(idx_r) > 0){
  points(idx_r, R_all[idx_r], col = "red", pch = 19, cex = 1.5)
  text(idx_r, R_all[idx_r], labels = idx_r, pos = 3, col = "red", cex = 1.0)
}

legend(
  "topleft",
  legend = c(
    "Data",
    paste0("CL = ", round(CL_R, 3)),
    paste0("UCL = ", round(UCL_R, 3)),
    paste0("LCL = ", round(LCL_R, 3)),
    "Phase I / II split"
  ),
  col    = c("black", "blue", "red", "red", "darkgreen"),
  lty    = c(1, 1, 2, 2, 3),
  lwd    = c(1, 2, 2, 2, 2),
  pch    = c(19, NA, NA, NA, NA),
  cex    = 0.95,
  pt.cex = 1.1,
  bty    = "n"
)

# ----- Right: s chart -----
plot(
  subgroup, s_all, type = "b", pch = 19,
  ylim = range(c(LCL_s, s_all, UCL_s)),
  xlab = "Subgroup",
  ylab = expression(s[i]),
  main = "s Chart",
  cex.main = 1.4,
  cex.lab  = 1.2,
  cex.axis = 1.1
)

abline(h = UCL_s, col = "red",  lwd = 2, lty = 2)
abline(h = CL_s,  col = "blue", lwd = 2)
abline(h = LCL_s, col = "red",  lwd = 2, lty = 2)
abline(v = n1 + 0.5, col = "darkgreen", lwd = 2, lty = 3)

idx_s <- which(s_signal)
if(length(idx_s) > 0){
  points(idx_s, s_all[idx_s], col = "red", pch = 19, cex = 1.5)
  text(idx_s, s_all[idx_s], labels = idx_s, pos = 3, col = "red", cex = 1.0)
}

legend(
  "topleft",
  legend = c(
    "Data",
    paste0("CL = ", round(CL_s, 3)),
    paste0("UCL = ", round(UCL_s, 3)),
    paste0("LCL = ", round(LCL_s, 3)),
    "Phase I / II split"
  ),
  col    = c("black", "blue", "red", "red", "darkgreen"),
  lty    = c(1, 1, 2, 2, 3),
  lwd    = c(1, 2, 2, 2, 2),
  pch    = c(19, NA, NA, NA, NA),
  cex    = 0.95,
  pt.cex = 1.1,
  bty    = "n"
)

par(mfrow = c(1, 1))
dev.off()

############################
# 11. Optional: save single s chart PNG
############################
png("figs/example32_s_chart.png", width = 900, height = 600)

par(cex = 1.2)

plot(
  subgroup, s_all, type = "b", pch = 19,
  ylim = range(c(LCL_s, s_all, UCL_s)),
  xlab = "Subgroup",
  ylab = expression(s[i]),
  main = "s Chart",
  cex.main = 1.5,
  cex.lab  = 1.3,
  cex.axis = 1.2
)

abline(h = UCL_s, col = "red",  lwd = 2, lty = 2)
abline(h = CL_s,  col = "blue", lwd = 2)
abline(h = LCL_s, col = "red",  lwd = 2, lty = 2)
abline(v = n1 + 0.5, col = "darkgreen", lwd = 2, lty = 3)

if(length(idx_s) > 0){
  points(idx_s, s_all[idx_s], col = "red", pch = 19, cex = 1.5)
  text(idx_s, s_all[idx_s], labels = idx_s, pos = 3, col = "red", cex = 1.1)
}

legend(
  "topleft",
  legend = c(
    "Data",
    paste0("CL = ", round(CL_s, 3)),
    paste0("UCL = ", round(UCL_s, 3)),
    paste0("LCL = ", round(LCL_s, 3)),
    "Phase I / II split"
  ),
  col    = c("black", "blue", "red", "red", "darkgreen"),
  lty    = c(1, 1, 2, 2, 3),
  lwd    = c(1, 2, 2, 2, 2),
  pch    = c(19, NA, NA, NA, NA),
  cex    = 1.0,
  pt.cex = 1.2,
  bty    = "n"
)

dev.off()

############################
# 12. Console summary
############################
cat("\nDone.\n")
cat("Saved files:\n")
cat(" - outputs/example32_control_limits_all.csv\n")
cat(" - outputs/example32_subgroup_summaries.csv\n")
cat(" - outputs/example32_raw_data.csv\n")
cat(" - figs/example31_xbar_chart_using_R.png\n")
cat(" - figs/example32_xbar_chart_using_s.png\n")
cat(" - figs/example32_R_vs_s_comparison.png\n")
cat(" - figs/example32_s_chart.png\n")