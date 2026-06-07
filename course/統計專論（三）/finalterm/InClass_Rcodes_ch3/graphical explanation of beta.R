############################################################
# graphical explanation of beta
############################################################

rm(list = ls())

setwd("E:/統計品質管制/Week10")

dir.create("figs", showWarnings = FALSE)

mu0 <- 0
sigma <- 1
m <- 5
k <- 1
alpha <- 0.0027
z <- qnorm(1 - alpha / 2)

mu1 <- mu0 + k * sigma
se <- sigma / sqrt(m)

LCL <- mu0 - z * se
UCL <- mu0 + z * se

x <- seq(mu0 - 4 * se, mu1 + 4.2 * se, length.out = 2500)

dens_ic <- dnorm(x, mean = mu0, sd = se)
dens_oc <- dnorm(x, mean = mu1, sd = se)

beta_region <- x >= LCL & x <= UCL
beta_value <- pnorm(UCL, mean = mu1, sd = se) -
  pnorm(LCL, mean = mu1, sd = se)

png("figs/beta_graphical_explanation.png",
    width = 2200, height = 1400, res = 250)

par(
  mar = c(5, 5, 4.2, 2),
  cex.main = 1.9,
  cex.lab  = 1.55,
  cex.axis = 1.25
)

plot(
  x, dens_ic,
  type = "l",
  lwd = 3,
  ylim = c(0, max(dens_ic, dens_oc) * 1.28),
  xlab = expression(bar(X)),
  ylab = "Density",
  main = expression("Graphical interpretation of " * beta)
)

lines(x, dens_oc, lwd = 3, lty = 2)

polygon(
  c(LCL, x[beta_region], UCL),
  c(0, dens_oc[beta_region], 0),
  density = 25,
  angle = 45
)

abline(v = LCL, lwd = 2, lty = 3)
abline(v = UCL, lwd = 2, lty = 3)
abline(v = mu0, lwd = 2)
abline(v = mu1, lwd = 2, lty = 2)

# Labels
text(LCL - 0.12, 0.11, "LCL", cex = 1.25)
text(UCL + 0.16, 0.11, "UCL", cex = 1.25)

text(mu0, max(dens_ic) * 1.07, expression(mu[0]), cex = 1.35)
text(mu1 + 0.08, max(dens_oc) * 1.07, expression(mu[1]), cex = 1.35)

# beta in mathematical notation, placed in open area
text(
  x = 0.03,
  y = max(dens_ic) * 0.58,
  labels = bquote(beta == .(round(beta_value, 3))),
  cex = 1.45
)

# Legend in upper-right empty area
legend(
  "topright",
  legend = c("IC distribution", "Shifted OC distribution"),
  lwd = 3,
  lty = c(1, 2),
  bty = "n",
  cex = 1.15,
  inset = c(0.02, 0.02)
)

grid()
dev.off()