############################################################
# D Chart: Numerical Example (Teaching Version)
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
# 1. Defect types
#----------------------------------------------------------

defect_type <- c(
  "Minor",
  "Major",
  "Critical"
)

w <- c(
  1,
  5,
  10
)

lambda <- c(
  3.0,   # Minor
  1.0,   # Major
  0.2    # Critical
)

#----------------------------------------------------------
# 2. Theoretical mean and variance
#----------------------------------------------------------

mu_D <- sum(
  w * lambda
)

var_D <- sum(
  w^2 * lambda
)

sd_D <- sqrt(
  var_D
)

CL <- mu_D

UCL <- mu_D + 3 * sd_D

LCL <- max(
  0,
  mu_D - 3 * sd_D
)

#----------------------------------------------------------
# 3. Save theoretical summary
#----------------------------------------------------------

summary_df <- data.frame(
  Mean = mu_D,
  Variance = var_D,
  SD = sd_D,
  LCL = LCL,
  CL = CL,
  UCL = UCL
)

print(summary_df)

write.csv(
  summary_df,
  file = "outputs/d_chart_summary.csv",
  row.names = FALSE
)

#----------------------------------------------------------
# 4. Simulate Phase II observations
#----------------------------------------------------------

set.seed(123)

m <- 25

c_minor <- rpois(
  m,
  lambda[1]
)

c_major <- rpois(
  m,
  lambda[2]
)

c_critical <- rpois(
  m,
  lambda[3]
)

D <- w[1] * c_minor +
  w[2] * c_major +
  w[3] * c_critical

df <- data.frame(
  Unit = 1:m,
  Minor = c_minor,
  Major = c_major,
  Critical = c_critical,
  D = D
)

print(df)

#----------------------------------------------------------
# 5. Save raw data
#----------------------------------------------------------

write.csv(
  df,
  file = "outputs/d_chart_raw_data.csv",
  row.names = FALSE
)

#----------------------------------------------------------
# 6. Plot D chart
#----------------------------------------------------------

png(
  filename = "figs/d_chart_numeric_example.png",
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
  y = df$D,
  
  type = "b",
  pch = 19,
  lwd = 2,
  
  xlab = "Inspection Unit",
  ylab = expression(D[i]),
  
  main = "D Chart: Weighted Defect Counts",
  
  ylim = c(
    0,
    max(UCL, df$D) * 1.15
  )
)

#----------------------------------------------------------
# Control limits
#----------------------------------------------------------

abline(
  h = CL,
  lwd = 2
)

abline(
  h = UCL,
  lwd = 2,
  lty = 2
)

abline(
  h = LCL,
  lwd = 2,
  lty = 2
)

#----------------------------------------------------------
# Right-side labels
#----------------------------------------------------------

x_label <- m * 0.78

text(
  x = x_label,
  y = UCL + 1,
  labels = sprintf("UCL = %.2f", UCL),
  pos = 4,
  cex = 1.25
)

text(
  x = x_label,
  y = CL + 1,
  labels = sprintf("CL = %.2f", CL),
  pos = 4,
  cex = 1.25
)

text(
  x = x_label,
  y = LCL + 1,
  labels = sprintf("LCL = %.0f", LCL),
  pos = 4,
  cex = 1.25
)

#----------------------------------------------------------
# Legend (upper-left)
#----------------------------------------------------------

legend(
  "topleft",
  
  inset = c(0.03, 0.03),
  
  legend = c(
    expression(D[i]),
    "CL",
    "UCL / LCL"
  ),
  
  lty = c(
    1,
    1,
    2
  ),
  
  pch = c(
    19,
    NA,
    NA
  ),
  
  lwd = c(
    2,
    2,
    2
  ),
  
  bty = "n",
  
  cex = 1.3
)

dev.off()

#----------------------------------------------------------
# Final message
#----------------------------------------------------------

cat("\n")
cat("Files created successfully:\n")
cat("figs/d_chart_numeric_example.png\n")
cat("outputs/d_chart_summary.csv\n")
cat("outputs/d_chart_raw_data.csv\n")
cat("\n")