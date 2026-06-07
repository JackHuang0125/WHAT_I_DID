# =========================================================
# Chapter 2, Subsection 2
# Acceptance Sampling with Hypergeometric and Binomial Models
# Related R codes for teaching / homework / demonstration
# =========================================================

rm(list = ls())

# =========================================================
# Output folder for figures
# =========================================================
root_dir <- "E:/統計品質管制/Week3"
fig_dir  <- file.path(root_dir, "figs")

if (!dir.exists(root_dir)) {
  stop("Root directory does not exist: ", root_dir)
}
if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

# File names
png_oc_binom_10_1   <- file.path(fig_dir, "oc_curve_binomial_n10_c1.png")
png_oc_binom_50_3   <- file.path(fig_dir, "oc_curve_binomial_n50_c3.png")
png_oc_hyper_50_1   <- file.path(fig_dir, "oc_curve_hyper_N500_n50_c1.png")
png_zero_compare    <- file.path(fig_dir, "zero_acceptance_comparison_n5_c0.png")

# ---------------------------------------------------------
# 0. Utility functions
# ---------------------------------------------------------

# Exact acceptance probability under Hypergeometric model
Pa_hyper <- function(N, M, n, c) {
  # X = number of defectives found in sample
  # phyper(q, m, n, k) = P(X <= q), where:
  #   m = number of "successes" in population = M defectives
  #   n = number of failures in population   = N - M conforming
  #   k = sample size = n
  phyper(q = c, m = M, n = N - M, k = n)
}

# Approximate acceptance probability under Binomial model
Pa_binom <- function(n, p, c) {
  pbinom(q = c, size = n, prob = p)
}

# Rare-defect Poisson approximation
Pa_pois <- function(n, p, c) {
  lambda <- n * p
  ppois(q = c, lambda = lambda)
}

# Print a compact summary
print_plan_summary <- function(model_name, Pa_value) {
  cat(sprintf("%-18s Pa = %.6f\n", model_name, Pa_value))
}


# ---------------------------------------------------------
# 1. Hypergeometric model for finite-lot inspection
# Handout context: exact model for finite lot, sampling without replacement
# ---------------------------------------------------------

N <- 200   # lot size
M <- 12    # defectives in lot
n <- 15    # sample size

# support of X
x_support <- 0:min(n, M)
x_support

# pmf of X
pmf_hyper <- dhyper(x = x_support, m = M, n = N - M, k = n)
data.frame(x = x_support, pmf = pmf_hyper)

# Example probabilities from Problem 5
P_X_eq_2 <- dhyper(x = 2, m = M, n = N - M, k = n)
P_X_eq_0 <- dhyper(x = 0, m = M, n = N - M, k = n)

cat("Problem 5:\n")
cat("P(X = 2) =", round(P_X_eq_2, 6), "\n")
cat("P(X = 0) =", round(P_X_eq_0, 6), "\n\n")

# Mean and variance
p <- M / N
EX_hyper  <- n * p
Var_hyper <- n * p * (1 - p) * (N - n) / (N - 1)

cat("Hypergeometric mean     =", round(EX_hyper, 6), "\n")
cat("Hypergeometric variance =", round(Var_hyper, 6), "\n\n")

# ---------------------------------------------------------
# 2. Acceptance probability under a hypergeometric sampling plan
# Handout context: accept lot if X <= c
# ---------------------------------------------------------

N <- 100
M <- 5
n <- 10
c <- 0

Pa_exact <- Pa_hyper(N = N, M = M, n = n, c = c)

cat("Hypergeometric acceptance probability example:\n")
cat(sprintf("N=%d, M=%d, n=%d, c=%d => Pa = %.6f\n\n", N, M, n, c, Pa_exact))

# To display the full cdf / pmf table
x_tab <- 0:min(n, M)
tab_hyper <- data.frame(
  x   = x_tab,
  pmf = dhyper(x_tab, m = M, n = N - M, k = n),
  cdf = phyper(x_tab, m = M, n = N - M, k = n)
)
tab_hyper

# ---------------------------------------------------------
# 3. Binomial approximation to Hypergeometric
# Handout context: when n/N < 0.10, use Binomial(n, p), p = M/N
# ---------------------------------------------------------

N <- 5000
M <- 250
n <- 20
p <- M / N
c <- 1

sampling_fraction <- n / N
cat("Sampling fraction n/N =", sampling_fraction, "\n")

Pa_exact  <- Pa_hyper(N = N, M = M, n = n, c = c)
Pa_approx <- Pa_binom(n = n, p = p, c = c)

cat("Hypergeometric vs Binomial approximation:\n")
print_plan_summary("Hypergeometric", Pa_exact)
print_plan_summary("Binomial",       Pa_approx)
cat(sprintf("Absolute difference = %.8f\n\n", abs(Pa_exact - Pa_approx)))

# Problem 6 numerical part
n <- 20
p <- 0.05
P_le_1_binom <- pbinom(q = 1, size = n, prob = p)

cat("Problem 6:\n")
cat("Binomial approximation P(X <= 1) =", round(P_le_1_binom, 6), "\n\n")

# Compare exact vs approximate for a matching finite lot
N <- 1000
M <- round(N * p)
Pa_exact_6  <- Pa_hyper(N = N, M = M, n = n, c = 1)
Pa_approx_6 <- Pa_binom(n = n, p = p, c = 1)

cat("Problem 6 comparison (illustrative exact vs approximate):\n")
print_plan_summary("Hypergeometric", Pa_exact_6)
print_plan_summary("Binomial",       Pa_approx_6)
cat("\n")

# ---------------------------------------------------------
# 4. Acceptance probability under Binomial model
# Handout context: Pa(p) = P(X <= c) = pbinom(c, n, p)
# ---------------------------------------------------------

n <- 10
c <- 1
p <- 0.02

Pa_7 <- Pa_binom(n = n, p = p, c = c)

cat("Problem 7:\n")
cat(sprintf("(n,c)=(%d,%d), p=%.3f => Pa = %.6f\n\n", n, c, p, Pa_7))

# Show effect of c on strictness
c_grid <- 0:4
Pa_by_c <- data.frame(
  c  = c_grid,
  Pa = pbinom(q = c_grid, size = n, prob = p)
)
Pa_by_c

# ---------------------------------------------------------
# 5. AQL-based design under Binomial model
# Handout context: choose plan so that Pa(AQL) >= 0.95
# ---------------------------------------------------------

# Given n, find smallest c such that Pa(p) >= target
find_c_given_n <- function(n, p, target = 0.95) {
  for (c in 0:n) {
    Pa <- pbinom(q = c, size = n, prob = p)
    if (Pa >= target) {
      return(data.frame(n = n, c = c, Pa = Pa))
    }
  }
  return(NULL)
}

# Example from handout: p = 0.02, n = 50
find_c_given_n(n = 50, p = 0.02, target = 0.95)

# Search over n to find a small feasible plan
find_plan_binom <- function(p, target = 0.95, n_max = 200) {
  out <- list()
  idx <- 1
  for (n in 1:n_max) {
    ans <- find_c_given_n(n = n, p = p, target = target)
    if (!is.null(ans)) {
      out[[idx]] <- ans
      idx <- idx + 1
    }
  }
  do.call(rbind, out)
}

plans_binom_2pct <- find_plan_binom(p = 0.02, target = 0.95, n_max = 80)
head(plans_binom_2pct, 12)

# AQL reference values from the handout pages
AQL_grid <- c(0.005, 0.010, 0.020, 0.030, 0.050, 0.100, 0.150)

# One simple search strategy:
# for each AQL, find the first n and smallest c such that Pa >= 0.95
recommend_plan_binom <- function(p, target = 0.95, n_max = 200) {
  for (n in 1:n_max) {
    for (c in 0:n) {
      Pa <- pbinom(q = c, size = n, prob = p)
      if (Pa >= target) {
        return(data.frame(AQL = p, n = n, c = c, Pa = Pa))
      }
    }
  }
  NULL
}

rec_binom <- do.call(rbind, lapply(AQL_grid, recommend_plan_binom, target = 0.95, n_max = 200))
rec_binom

# ---------------------------------------------------------
# 6. Zero-acceptance plan (c = 0)
# Handout context: choose smallest n such that Pa(p) >= 0.95
# ---------------------------------------------------------

# Under Binomial: Pa = (1 - p)^n
Pa_zero_binom <- function(n, p) {
  (1 - p)^n
}

# Under Poisson approximation: Pa ~ exp(-np)
Pa_zero_pois <- function(n, p) {
  exp(-n * p)
}

# Under Hypergeometric: Pa = choose(N-M, n) / choose(N, n)
Pa_zero_hyper <- function(N, M, n) {
  dhyper(x = 0, m = M, n = N - M, k = n)
}

# Find largest n satisfying Pa >= target for c=0
# (This matches the handout's example p=0.01 gives n=5, because n=6 drops below 0.95.)
find_zero_accept_n <- function(p, target = 0.95, n_max = 1000) {
  good <- which((1 - p)^(1:n_max) >= target)
  if (length(good) == 0) return(NA_integer_)
  max(good)
}

p <- 0.01
n_zero <- find_zero_accept_n(p = p, target = 0.95, n_max = 200)

cat("Problem 8 / zero-acceptance:\n")
cat("Largest n such that Pa >= 0.95 =", n_zero, "\n")
cat("Check Pa =", round(Pa_zero_binom(n_zero, p), 6), "\n")
cat("Next n gives Pa =", round(Pa_zero_binom(n_zero + 1, p), 6), "\n\n")

# Compare the three models at handout example N=500, p=0.01, M=5, n=5
N <- 500
p <- 0.01
M <- N * p
n <- 5

Pa_pois_0  <- Pa_zero_pois(n = n, p = p)
Pa_binom_0 <- Pa_zero_binom(n = n, p = p)
Pa_hyper_0 <- Pa_zero_hyper(N = N, M = M, n = n)

cat("Zero-acceptance example (p=0.01, N=500, M=5, n=5):\n")
print_plan_summary("Poisson",        Pa_pois_0)
print_plan_summary("Binomial",       Pa_binom_0)
print_plan_summary("Hypergeometric", Pa_hyper_0)
cat("\n")

# Limitation table for increasing defect rate
p_grid <- c(0.01, 0.05, 0.10, 0.20)
n <- 5
zero_accept_table <- data.frame(
  p  = p_grid,
  n  = n,
  Pa = (1 - p_grid)^n
)
zero_accept_table

# ---------------------------------------------------------
# 7. Determining (n, c) for c >= 1
# Handout context: choose smallest c for given n and p so that Pa >= 0.95
# ---------------------------------------------------------

p <- 0.02
n <- 50
target <- 0.95

nc_table <- data.frame(
  c  = 0:n,
  Pa = pbinom(q = 0:n, size = n, prob = p)
)

head(nc_table, 10)

# first c satisfying Pa >= 0.95
best_row <- nc_table[which(nc_table$Pa >= target)[1], ]
best_row

# More general search over both n and c
find_min_plan_binom <- function(p, target = 0.95, n_max = 200) {
  candidates <- list()
  idx <- 1
  for (n in 1:n_max) {
    for (c in 0:n) {
      Pa <- pbinom(c, size = n, prob = p)
      if (Pa >= target) {
        candidates[[idx]] <- data.frame(n = n, c = c, Pa = Pa)
        idx <- idx + 1
        break
      }
    }
  }
  out <- do.call(rbind, candidates)
  # one reasonable criterion: smallest n, then smallest c
  out[order(out$n, out$c), ][1, ]
}

find_min_plan_binom(p = 0.02, target = 0.95, n_max = 100)

# ---------------------------------------------------------
# 8. Exact hypergeometric plan design for finite lot
# Handout context: N matters explicitly
# ---------------------------------------------------------

find_c_given_n_hyper <- function(N, M, n, target = 0.95) {
  max_c <- min(n, M)
  for (c in 0:max_c) {
    Pa <- phyper(q = c, m = M, n = N - M, k = n)
    if (Pa >= target) {
      return(data.frame(N = N, M = M, n = n, c = c, Pa = Pa))
    }
  }
  NULL
}

find_plan_hyper <- function(N, p, target = 0.95, n_max = 200) {
  M <- round(N * p)
  out <- list()
  idx <- 1
  for (n in 1:n_max) {
    ans <- find_c_given_n_hyper(N = N, M = M, n = n, target = target)
    if (!is.null(ans)) {
      out[[idx]] <- ans
      idx <- idx + 1
    }
  }
  do.call(rbind, out)
}

# Example from handout: N = 500, p = 0.02 => M = 10
plans_hyper_2pct <- find_plan_hyper(N = 500, p = 0.02, target = 0.95, n_max = 80)
head(plans_hyper_2pct, 12)

# ---------------------------------------------------------
# 9. Problem 9: exact vs approximate acceptance probability
# ---------------------------------------------------------

N <- 500
p <- 0.02
M <- N * p
n <- 50
c <- 1

Pa_exact_9  <- Pa_hyper(N = N, M = M, n = n, c = c)
Pa_binom_9  <- Pa_binom(n = n, p = p, c = c)

cat("Problem 9:\n")
print_plan_summary("Hypergeometric", Pa_exact_9)
print_plan_summary("Binomial",       Pa_binom_9)
cat(sprintf("Absolute difference = %.8f\n\n", abs(Pa_exact_9 - Pa_binom_9)))

# ---------------------------------------------------------
# 10. Operating Characteristic (OC) curve
# Very useful for teaching acceptance sampling plans
# ---------------------------------------------------------

# Binomial OC curve for a fixed plan
oc_curve_binom <- function(n, c, p_grid) {
  data.frame(
    p  = p_grid,
    Pa = pbinom(q = c, size = n, prob = p_grid)
  )
}

# Hypergeometric OC curve for a fixed finite lot
oc_curve_hyper <- function(N, n, c, p_grid) {
  M_grid <- round(N * p_grid)
  Pa_vals <- mapply(function(Mi) {
    phyper(q = c, m = Mi, n = N - Mi, k = n)
  }, M_grid)
  data.frame(
    p  = p_grid,
    M  = M_grid,
    Pa = Pa_vals
  )
}

p_grid <- seq(0, 0.20, by = 0.001)

oc_binom_1 <- oc_curve_binom(n = 10, c = 1, p_grid = p_grid)
oc_binom_2 <- oc_curve_binom(n = 50, c = 3, p_grid = p_grid)
oc_hyper_1 <- oc_curve_hyper(N = 500, n = 50, c = 1, p_grid = p_grid)

# -----------------------------
# Save: Binomial OC (n=10, c=1)
# -----------------------------
png(filename = png_oc_binom_10_1, width = 1800, height = 1200, res = 200)
plot(oc_binom_1$p, oc_binom_1$Pa, type = "l", lwd = 3,
     xlab = "Defect rate p", ylab = "Acceptance probability Pa",
     main = "OC Curve: Binomial Plan (n=10, c=1)")
abline(h = 0.95, lty = 2, lwd = 2)
grid()
dev.off()

# -----------------------------
# Save: Binomial OC (n=50, c=3)
# -----------------------------
png(filename = png_oc_binom_50_3, width = 1800, height = 1200, res = 200)
plot(oc_binom_2$p, oc_binom_2$Pa, type = "l", lwd = 3,
     xlab = "Defect rate p", ylab = "Acceptance probability Pa",
     main = "OC Curve: Binomial Plan (n=50, c=3)")
abline(h = 0.95, lty = 2, lwd = 2)
grid()
dev.off()

# --------------------------------------
# Save: Hypergeometric OC (N=500, n=50, c=1)
# --------------------------------------
png(filename = png_oc_hyper_50_1, width = 1800, height = 1200, res = 200)
plot(oc_hyper_1$p, oc_hyper_1$Pa, type = "l", lwd = 3,
     xlab = "Defect rate p", ylab = "Acceptance probability Pa",
     main = "OC Curve: Hypergeometric Plan (N=500, n=50, c=1)")
abline(h = 0.95, lty = 2, lwd = 2)
grid()
dev.off()

# ---------------------------------------------------------
# 11. Compare exact / binomial / Poisson on same graph for c = 0
# Save PNG file
# ---------------------------------------------------------

p_grid <- seq(0.001, 0.10, by = 0.001)
n <- 5
N <- 500

Pa_pois_vec  <- exp(-n * p_grid)
Pa_binom_vec <- (1 - p_grid)^n
Pa_hyper_vec <- sapply(p_grid, function(p) {
  M <- round(N * p)
  phyper(q = 0, m = M, n = N - M, k = n)
})

# output file
png_zero_compare <- file.path(fig_dir, "zero_acceptance_comparison_n5_c0.png")

png(filename = png_zero_compare, width = 1600, height = 900, res = 180)

plot(p_grid, Pa_binom_vec, type = "l", lwd = 3,
     xlab = "Defect rate p",
     ylab = "Pa = P(X = 0)",
     main = "Zero-Acceptance Plan Comparison (n=5, c=0)")

lines(p_grid, Pa_pois_vec,  lwd = 3, lty = 2)
lines(p_grid, Pa_hyper_vec, lwd = 3, lty = 3)

legend("topright",
       legend = c("Binomial", "Poisson", "Hypergeometric"),
       lwd = 3, lty = c(1, 2, 3), bty = "n")

grid()

dev.off()

cat("Saved:", png_zero_compare, "\n")

# ---------------------------------------------------------
# 12. Homework 5--9: ready-to-run answers
# ---------------------------------------------------------

# Problem 5
N <- 200; M <- 12; n <- 15
ans5 <- list(
  support = 0:min(n, M),
  P_X_eq_2 = dhyper(2, m = M, n = N - M, k = n),
  P_X_eq_0 = dhyper(0, m = M, n = N - M, k = n)
)
ans5

# Problem 6
n <- 20; p <- 0.05
ans6 <- list(
  P_X_le_1_binom = pbinom(1, size = n, prob = p)
)
ans6

# Problem 7
n <- 10; c <- 1; p <- 0.02
ans7 <- list(
  Pa = pbinom(c, size = n, prob = p)
)
ans7

# Problem 8
p <- 0.01
ans8 <- list(
  n_star = find_zero_accept_n(p = p, target = 0.95, n_max = 200),
  Pa_n_star = Pa_zero_binom(find_zero_accept_n(p = p, target = 0.95, n_max = 200), p)
)
ans8

# Problem 9
N <- 500; p <- 0.02; M <- N * p; n <- 50; c <- 1
ans9 <- list(
  Pa_hyper = phyper(c, m = M, n = N - M, k = n),
  Pa_binom = pbinom(c, size = n, prob = p),
  abs_diff = abs(phyper(c, m = M, n = N - M, k = n) -
                   pbinom(c, size = n, prob = p))
)
ans9


cat("Saved figure files:\n")
cat(png_oc_binom_10_1, "\n")
cat(png_oc_binom_50_3, "\n")
cat(png_oc_hyper_50_1, "\n")
cat(png_zero_compare, "\n")
png_oc_binom_10_1 <- file.path(fig_dir, "oc_curve_binomial_n10_c1.png")
png_oc_binom_50_3 <- file.path(fig_dir, "oc_curve_binomial_n50_c3.png")
png_oc_hyper_50_1 <- file.path(fig_dir, "oc_curve_hyper_N500_n50_c1.png")
png_zero_compare  <- file.path(fig_dir, "zero_acceptance_comparison_n5_c0.png")