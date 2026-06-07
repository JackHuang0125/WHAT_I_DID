############################################################
# u-chart: textile inspection example
# defects per square meter
############################################################

rm(list = ls())

setwd("E:/統計品質管制/Week11")

#----------------------------------------------------------
# 0. folders
#----------------------------------------------------------
if (!dir.exists("figs")) dir.create("figs")
if (!dir.exists("outputs")) dir.create("outputs")

fig_dir <- file.path(getwd(), "figs")
out_dir <- file.path(getwd(), "outputs")

#----------------------------------------------------------
# 1. real teaching example
# textile inspection
#----------------------------------------------------------
n <- 20

# inspected area (m^2)
m <- c(
  28,32,35,30,40,
  38,25,45,33,42,
  27,36,48,29,39,
  24,44,34,50,31
)

# observed defects
count <- c(
  71,81,90,74,104,
  97,63,112,79,108,
  66,93,122,73,98,
  58,109,85,126,77
)

#----------------------------------------------------------
# 2. u statistics
#----------------------------------------------------------
u <- count / m

u_bar <- sum(count) / sum(m)

#----------------------------------------------------------
# 3. control limits
#----------------------------------------------------------
alpha <- 0.0027
z <- qnorm(1-alpha/2)

ucl <- u_bar + z*sqrt(u_bar/m)

lcl <- u_bar - z*sqrt(u_bar/m)

lcl[lcl < 0] <- 0

#----------------------------------------------------------
# 4. save data
#----------------------------------------------------------
result <- data.frame(
  roll = 1:n,
  area = m,
  defects = count,
  u = round(u,3),
  LCL = round(lcl,3),
  CL = round(u_bar,3),
  UCL = round(ucl,3)
)

write.csv(
  result,
  file.path(
    out_dir,
    "u_chart_fabric_data.csv"
  ),
  row.names = FALSE
)

#----------------------------------------------------------
# 5. plot
#----------------------------------------------------------
png(
  file.path(
    fig_dir,
    "u_chart_fabric_example.png"
  ),
  width = 1800,
  height = 1200,
  res = 220
)

par(
  mar = c(5,5,3,2),
  cex.axis = 1.5,
  cex.lab = 1.6,
  cex.main = 1.7
)

plot(
  1:n,
  u,
  type = "b",
  pch = 19,
  lwd = 2,
  ylim = c(0,max(ucl)*1.08),
  xlab = "Roll index i",
  ylab = expression(u[i]),
  main = "u Chart: Fabric Inspection"
)

lines(1:n,ucl,lty=2,lwd=2)
lines(1:n,lcl,lty=2,lwd=2)

abline(
  h = u_bar,
  lwd = 2
)

text(
  n-1,
  u_bar+0.08,
  labels = bquote(CL==.(round(u_bar,3))),
  cex = 1.3
)

text(
  n-1,
  ucl[n-1]+0.08,
  labels = expression(UCL[i]),
  cex = 1.3
)

text(
  n-1,
  lcl[n-1]+0.08,
  labels = expression(LCL[i]),
  cex = 1.3
)

dev.off()

cat("u-bar =", round(u_bar,3), "\n")