############################################################
# Geometric run length histogram (IC case)
############################################################

rm(list = ls())

setwd("E:/統計品質管制/Week10")

dir.create("figs", showWarnings = FALSE)

set.seed(123)

alpha <- 0.0027
Nsim  <- 5000

# simulate run length (Geom with support 1,2,...)
RL <- rgeom(Nsim, prob = alpha) + 1

# theoretical values
ARL <- 1 / alpha
MRL <- ceiling(log(0.5) / log(1 - alpha))

png("figs/geom_RL_hist.png",
    width = 2000, height = 1400, res = 250)

par(
  mar = c(5,5,4,2),
  cex.main = 1.8,
  cex.lab  = 1.5,
  cex.axis = 1.2
)

# histogram
h <- hist(
  RL,
  breaks = 50,
  probability = TRUE,
  main = "Run Length Distribution (IC)",
  xlab = "Run Length (RL)",
  ylab = "Density",
  border = "black"
)

# theoretical curve
r <- 1:max(RL)
lines(r, dgeom(r-1, prob = alpha), lwd = 3)

# y-position for annotations (avoid overlap)
ypos <- max(h$density) * 0.75

# ARL line
abline(v = ARL, lwd = 2, lty = 2)

text(ARL, ypos,
     labels = expression(ARL[0] == 1/alpha),
     pos = 4,
     cex = 1.2)

# MRL line
abline(v = MRL, col = "red", lwd = 2, lty = 3)

text(MRL, ypos * 0.9,
     labels = expression(MRL[0]),
     pos = 2,
     col = "red",
     cex = 1.2)

# legend
legend("topright",
       legend = c("Theoretical PMF", "ARL", "MRL"),
       lty = c(1,2,3),
       lwd = c(3,2,2),
       col = c("black", "black", "red"),
       bty = "n",
       cex = 1.2)

grid()

dev.off()