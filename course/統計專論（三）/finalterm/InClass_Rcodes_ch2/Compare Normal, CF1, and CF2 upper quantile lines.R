############################################################
# Compare Normal, CF1, and CF2 upper quantile lines
# at the SAME quantile level q = 0.9973
# on binomial histograms
# Figures -> figs/
# Numeric results -> outputs/
############################################################

rm(list = ls())
set.seed(123)

setwd("E:/統計品質管制/Week5")

############################################################
# 0. Create folders
############################################################
if (!dir.exists("figs"))    dir.create("figs")
if (!dir.exists("outputs")) dir.create("outputs")

############################################################
# 1. User setting
############################################################
qprob <- 0.9973

############################################################
# 2. Quantile functions on p-hat scale
############################################################
q_normal_phat <- function(n, pi, qprob = 0.9973) {
  z  <- qnorm(qprob)
  se <- sqrt(pi * (1 - pi) / n)
  pi + z * se
}

q_cf1_phat <- function(n, pi, qprob = 0.9973) {
  z  <- qnorm(qprob)
  se <- sqrt(pi * (1 - pi) / n)
  pi + z * se +
    ((z^2 - 1) / (6 * n)) * (1 - 2 * pi)
}

q_cf2_phat <- function(n, pi, qprob = 0.9973) {
  z  <- qnorm(qprob)
  se <- sqrt(pi * (1 - pi) / n)
  
  term1 <- ((z^2 - 1) / (6 * n)) * (1 - 2 * pi)
  
  term2 <- ((z^3 - 3 * z) / (24 * n^2)) *
    (1 - 6 * pi * (1 - pi)) / se
  
  term3 <- -((2 * z^3 - 5 * z) / (36 * n^2)) *
    ((1 - 2 * pi)^2) / se
  
  pi + z * se + term1 + term2 + term3
}

############################################################
# 3. Convert p-hat quantile to count scale
############################################################
phat_to_k <- function(q_phat, n) {
  n * q_phat
}

############################################################
# 4. Exact binomial quantile on count scale
############################################################
q_exact_k <- function(n, pi, qprob = 0.9973) {
  qbinom(qprob, size = n, prob = pi)
}

############################################################
# 5. Safe relative error
############################################################
safe_rel_error <- function(approx, exact) {
  ifelse(exact == 0, NA, abs(approx - exact) / abs(exact))
}

############################################################
# 6. Method recommendation rule
############################################################
choose_method <- function(n, pi) {
  npi   <- n * pi
  n1mpi <- n * (1 - pi)
  npv   <- n * pi * (1 - pi)
  
  if (npv >= 5) {
    method         <- "Normal"
    interpretation <- "Large-sample normal approximation"
  } else if (npv >= 0.25) {
    method         <- "CF1"
    interpretation <- "Moderate skewness"
  } else if (npv >= 0.08) {
    method         <- "CF2"
    interpretation <- "Strong skewness / small pi"
  } else {
    method         <- "Other"
    interpretation <- "Poisson / exact / non-normal"
  }
  
  data.frame(
    n = n,
    pi = pi,
    `n*pi`        = npi,
    `n*(1-pi)`    = n1mpi,
    `n*pi*(1-pi)` = npv,
    recommended   = method,
    interpretation = interpretation,
    check.names = FALSE
  )
}

############################################################
# 7. Summary table of quantile lines + errors
############################################################
quantile_table <- function(n, pi, qprob = 0.9973) {
  qN_phat   <- q_normal_phat(n, pi, qprob)
  qCF1_phat <- q_cf1_phat(n,   pi, qprob)
  qCF2_phat <- q_cf2_phat(n,   pi, qprob)
  
  qN_k   <- phat_to_k(qN_phat,   n)
  qCF1_k <- phat_to_k(qCF1_phat, n)
  qCF2_k <- phat_to_k(qCF2_phat, n)
  qE_k   <- q_exact_k(n, pi, qprob)
  
  data.frame(
    n = n, pi = pi, qprob = qprob,
    Normal_phat = qN_phat,
    CF1_phat    = qCF1_phat,
    CF2_phat    = qCF2_phat,
    Normal_k    = qN_k,
    CF1_k       = qCF1_k,
    CF2_k       = qCF2_k,
    Exact_k     = qE_k,
    Normal_abs_error = abs(qN_k   - qE_k),
    CF1_abs_error    = abs(qCF1_k - qE_k),
    CF2_abs_error    = abs(qCF2_k - qE_k),
    Normal_rel_error = safe_rel_error(qN_k,   qE_k),
    CF1_rel_error    = safe_rel_error(qCF1_k, qE_k),
    CF2_rel_error    = safe_rel_error(qCF2_k, qE_k)
  )
}

############################################################
# 8. Define situation colors (used everywhere)
#    Situation 1 -> steel blue
#    Situation 2 -> coral / salmon
#    Situation 3 -> medium sea green
############################################################
col1        <- "#5B9BD5"   # steel blue    — Situation 1 (Normal)
col2        <- "#F4845F"   # coral         — Situation 2 (CF1)
col3        <- "#56AE7C"   # sea green     — Situation 3 (CF2)
col_border  <- "white"     # bar border for all

############################################################
# 9a. Plot function WITH ALL quantile lines
############################################################
plot_compare_quantiles <- function(sim, n, pi, qprob = 0.9973,
                                   max_n      = n,
                                   bar_col    = "gray90",
                                   main_title = NULL,
                                   show_exact = TRUE) {
  
  qN_k   <- phat_to_k(q_normal_phat(n, pi, qprob), n)
  qCF1_k <- phat_to_k(q_cf1_phat(n,   pi, qprob), n)
  qCF2_k <- phat_to_k(q_cf2_phat(n,   pi, qprob), n)
  
  if (is.null(main_title)) {
    main_title <- paste0("n = ", n, ", pi = ", pi)
  }
  
  hist(sim,
       main   = main_title,
       xlab   = "Number of Successes k",
       ylab   = "Relative Frequency",
       breaks = seq(-0.5, max_n + 0.5, 1),
       xlim   = c(-0.5, max_n + 0.5),
       ylim   = c(0, 1),
       freq   = FALSE,
       xaxt   = "n",
       col    = bar_col,        
       border = col_border)
  
  axis(1, at = 0:max_n)
  
  abline(v = qN_k,   lwd = 2, lty = 1, col = "black")
  abline(v = qCF1_k, lwd = 2, lty = 2, col = "blue")
  abline(v = qCF2_k, lwd = 2, lty = 3, col = "red")
  
  if (show_exact) {
    qE_k <- q_exact_k(n, pi, qprob)
    abline(v = qE_k, lwd = 2, lty = 4, col = "darkgreen")
    legend("topright",
           legend = c(
             paste0("Normal = ", round(qN_k,   3)),
             paste0("CF1    = ", round(qCF1_k, 3)),
             paste0("CF2    = ", round(qCF2_k, 3)),
             paste0("Exact  = ", round(qE_k,   3))
           ),
           lty = c(1, 2, 3, 4),
           col = c("black", "blue", "red", "darkgreen"),
           lwd = 2, bty = "n", cex = 0.8)
  } else {
    legend("topright",
           legend = c(
             paste0("Normal = ", round(qN_k,   3)),
             paste0("CF1    = ", round(qCF1_k, 3)),
             paste0("CF2    = ", round(qCF2_k, 3))
           ),
           lty = c(1, 2, 3),
           col = c("black", "blue", "red"),
           lwd = 2, bty = "n", cex = 0.8)
  }
}

############################################################
# 9b. Plot function WITHOUT quantile lines (histogram only)
############################################################
plot_hist_only <- function(sim, n, pi,
                           max_n      = n,
                           bar_col    = "gray90",
                           main_title = NULL) {
  
  if (is.null(main_title)) {
    main_title <- paste0("n = ", n, ", pi = ", pi)
  }
  
  hist(sim,
       main   = main_title,
       xlab   = "Number of Successes k",
       ylab   = "Relative Frequency",
       breaks = seq(-0.5, max_n + 0.5, 1),
       xlim   = c(-0.5, max_n + 0.5),
       ylim   = c(0, 1),
       freq   = FALSE,
       xaxt   = "n",
       col    = bar_col,
       border = col_border)
  
  axis(1, at = 0:max_n)
}

############################################################
# 9c. Plot function WITH SUGGESTED quantile line only
############################################################
plot_suggested_only <- function(sim, n, pi, method, qprob = 0.9973,
                                max_n      = n,
                                bar_col    = "gray90",
                                main_title = NULL) {
  
  if (is.null(main_title)) {
    main_title <- paste0("n = ", n, ", pi = ", pi, "\nSuggested: ", method)
  }
  
  hist(sim,
       main   = main_title,
       xlab   = "Number of Successes k",
       ylab   = "Relative Frequency",
       breaks = seq(-0.5, max_n + 0.5, 1),
       xlim   = c(-0.5, max_n + 0.5),
       ylim   = c(0, 1),
       freq   = FALSE,
       xaxt   = "n",
       col    = bar_col,
       border = col_border)
  
  axis(1, at = 0:max_n)
  
  qE_k <- q_exact_k(n, pi, qprob)
  abline(v = qE_k, lwd = 2, lty = 4, col = "darkgreen")
  
  if (method == "Normal") {
    q_k <- phat_to_k(q_normal_phat(n, pi, qprob), n)
    abline(v = q_k, lwd = 2, lty = 1, col = "black")
    leg_txt <- paste0("Normal = ", round(q_k, 3))
    leg_col <- "black"
    leg_lty <- 1
  } else if (method == "CF1") {
    q_k <- phat_to_k(q_cf1_phat(n, pi, qprob), n)
    abline(v = q_k, lwd = 2, lty = 2, col = "blue")
    leg_txt <- paste0("CF1 = ", round(q_k, 3))
    leg_col <- "blue"
    leg_lty <- 2
  } else if (method == "CF2") {
    q_k <- phat_to_k(q_cf2_phat(n, pi, qprob), n)
    abline(v = q_k, lwd = 2, lty = 3, col = "red")
    leg_txt <- paste0("CF2 = ", round(q_k, 3))
    leg_col <- "red"
    leg_lty <- 3
  } else {
    q_k <- NA
    leg_txt <- "Other"
    leg_col <- "gray"
    leg_lty <- 1
  }
  
  legend("topright",
         legend = c(leg_txt, paste0("Exact = ", round(qE_k, 3))),
         lty = c(leg_lty, 4),
         col = c(leg_col, "darkgreen"),
         lwd = 2, bty = "n", cex = 0.8)
}

############################################################
# 10. Three situations
############################################################
n1 <- 20; p1 <- 0.500   # np(1-p) = 5.000  -> Normal
n2 <- 20; p2 <- 0.015   # np(1-p) = 0.296  -> CF1
n3 <- 20; p3 <- 0.005   # np(1-p) = 0.0995 -> CF2

max_n <- max(n1, n2, n3)   # = 20

############################################################
# 11. Simulate data
############################################################
sim1 <- rbinom(10000, size = n1, prob = p1)
sim2 <- rbinom(10000, size = n2, prob = p2)
sim3 <- rbinom(10000, size = n3, prob = p3)

############################################################
# 12. Recommendation table
############################################################
cat("====================================================\n")
cat("Method recommendation by rule\n")
cat("====================================================\n")
rec_table <- rbind(
  cbind(case = "Situation 1", choose_method(n1, p1)),
  cbind(case = "Situation 2", choose_method(n2, p2)),
  cbind(case = "Situation 3", choose_method(n3, p3))
)
print(rec_table)
cat("\n")

############################################################
# 13. Quantile comparison tables
############################################################
cat("====================================================\n")
cat("Quantile comparison at qprob = 0.9973\n")
cat("====================================================\n")
tab1 <- cbind(case = "Situation 1", quantile_table(n1, p1, qprob))
tab2 <- cbind(case = "Situation 2", quantile_table(n2, p2, qprob))
tab3 <- cbind(case = "Situation 3", quantile_table(n3, p3, qprob))
print(tab1); print(tab2); print(tab3)
cat("\n")

############################################################
# 14. Save numeric outputs
############################################################
write.csv(rec_table, file = "outputs/recommendation_table.csv",     row.names = FALSE)
write.csv(tab1,      file = "outputs/quantile_table_situation1.csv", row.names = FALSE)
write.csv(tab2,      file = "outputs/quantile_table_situation2.csv", row.names = FALSE)
write.csv(tab3,      file = "outputs/quantile_table_situation3.csv", row.names = FALSE)
write.csv(rbind(tab1, tab2, tab3),
          file = "outputs/quantile_table_all_situations.csv",        row.names = FALSE)

############################################################
# 15. Long-format error table
############################################################
error_table_long <- rbind(
  data.frame(case="Situation 1", method="Normal", n=n1, pi=p1,
             abs_error=tab1$Normal_abs_error, rel_error=tab1$Normal_rel_error),
  data.frame(case="Situation 1", method="CF1",    n=n1, pi=p1,
             abs_error=tab1$CF1_abs_error,    rel_error=tab1$CF1_rel_error),
  data.frame(case="Situation 1", method="CF2",    n=n1, pi=p1,
             abs_error=tab1$CF2_abs_error,    rel_error=tab1$CF2_rel_error),
  
  data.frame(case="Situation 2", method="Normal", n=n2, pi=p2,
             abs_error=tab2$Normal_abs_error, rel_error=tab2$Normal_rel_error),
  data.frame(case="Situation 2", method="CF1",    n=n2, pi=p2,
             abs_error=tab2$CF1_abs_error,    rel_error=tab2$CF1_rel_error),
  data.frame(case="Situation 2", method="CF2",    n=n2, pi=p2,
             abs_error=tab2$CF2_abs_error,    rel_error=tab2$CF2_rel_error),
  
  data.frame(case="Situation 3", method="Normal", n=n3, pi=p3,
             abs_error=tab3$Normal_abs_error, rel_error=tab3$Normal_rel_error),
  data.frame(case="Situation 3", method="CF1",    n=n3, pi=p3,
             abs_error=tab3$CF1_abs_error,    rel_error=tab3$CF1_rel_error),
  data.frame(case="Situation 3", method="CF2",    n=n3, pi=p3,
             abs_error=tab3$CF2_abs_error,    rel_error=tab3$CF2_rel_error)
)
write.csv(error_table_long, file = "outputs/error_table_long.csv", row.names = FALSE)

############################################################
# 16. Individual plots — WITH ALL quantile lines
############################################################
png("figs/situation1_n20_pi0500.png", width = 1600, height = 1200, res = 200)
plot_compare_quantiles(sim1, n1, p1, qprob, max_n,
                       bar_col    = col1,
                       main_title = "Situation 1: n=20, pi=0.500",
                       show_exact = TRUE)
dev.off()

png("figs/situation2_n20_pi0015.png", width = 1600, height = 1200, res = 200)
plot_compare_quantiles(sim2, n2, p2, qprob, max_n,
                       bar_col    = col2,
                       main_title = "Situation 2: n=20, pi=0.015",
                       show_exact = TRUE)
dev.off()

png("figs/situation3_n20_pi0005.png", width = 1600, height = 1200, res = 200)
plot_compare_quantiles(sim3, n3, p3, qprob, max_n,
                       bar_col    = col3,
                       main_title = "Situation 3: n=20, pi=0.005",
                       show_exact = TRUE)
dev.off()

############################################################
# 17. Individual plots — WITHOUT quantile lines
############################################################
png("figs/situation1_n20_pi0500_hist_only.png", width = 1600, height = 1200, res = 200)
plot_hist_only(sim1, n1, p1, max_n,
               bar_col    = col1,
               main_title = "Situation 1: n=20, pi=0.500")
dev.off()

png("figs/situation2_n20_pi0015_hist_only.png", width = 1600, height = 1200, res = 200)
plot_hist_only(sim2, n2, p2, max_n,
               bar_col    = col2,
               main_title = "Situation 2: n=20, pi=0.015")
dev.off()

png("figs/situation3_n20_pi0005_hist_only.png", width = 1600, height = 1200, res = 200)
plot_hist_only(sim3, n3, p3, max_n,
               bar_col    = col3,
               main_title = "Situation 3: n=20, pi=0.005")
dev.off()

############################################################
# 18. Combined plot — WITH ALL quantile lines
############################################################
png("figs/combined_three_situations.png", width = 2400, height = 900, res = 200)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

plot_compare_quantiles(sim1, n1, p1, qprob, max_n,
                       bar_col    = col1,
                       main_title = "Situation 1\n(n=20, pi=0.500)",
                       show_exact = TRUE)
plot_compare_quantiles(sim2, n2, p2, qprob, max_n,
                       bar_col    = col2,
                       main_title = "Situation 2\n(n=20, pi=0.015)",
                       show_exact = TRUE)
plot_compare_quantiles(sim3, n3, p3, qprob, max_n,
                       bar_col    = col3,
                       main_title = "Situation 3\n(n=20, pi=0.005)",
                       show_exact = TRUE)

par(mfrow = c(1, 1))
dev.off()

############################################################
# 19. Combined plot — WITHOUT quantile lines
############################################################
png("figs/combined_three_situations_hist_only.png", width = 2400, height = 900, res = 200)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

plot_hist_only(sim1, n1, p1, max_n,
               bar_col    = col1,
               main_title = "Situation 1\n(n=20, pi=0.500)")
plot_hist_only(sim2, n2, p2, max_n,
               bar_col    = col2,
               main_title = "Situation 2\n(n=20, pi=0.015)")
plot_hist_only(sim3, n3, p3, max_n,
               bar_col    = col3,
               main_title = "Situation 3\n(n=20, pi=0.005)")

par(mfrow = c(1, 1))
dev.off()

############################################################
# 20. Combined plot — SUGGESTED quantile line only
############################################################
png("figs/combined_three_situations_suggested_only.png", width = 2400, height = 900, res = 200)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

# Extract the recommended method dynamically
rec1 <- as.character(choose_method(n1, p1)$recommended)
rec2 <- as.character(choose_method(n2, p2)$recommended)
rec3 <- as.character(choose_method(n3, p3)$recommended)

plot_suggested_only(sim1, n1, p1, method = rec1, qprob = qprob, max_n = max_n,
                    bar_col = col1, main_title = paste0("Situation 1 (n=20, pi=0.500)\nSuggested: ", rec1))
plot_suggested_only(sim2, n2, p2, method = rec2, qprob = qprob, max_n = max_n,
                    bar_col = col2, main_title = paste0("Situation 2 (n=20, pi=0.015)\nSuggested: ", rec2))
plot_suggested_only(sim3, n3, p3, method = rec3, qprob = qprob, max_n = max_n,
                    bar_col = col3, main_title = paste0("Situation 3 (n=20, pi=0.005)\nSuggested: ", rec3))

par(mfrow = c(1, 1))
dev.off()

############################################################
# 21. Console summary
############################################################
cat("====================================================\n")
cat("Files saved successfully.\n")
cat("\n")
cat("  Color scheme:\n")
cat("    Situation 1 -> Steel Blue  (#5B9BD5)\n")
cat("    Situation 2 -> Coral       (#F4845F)\n")
cat("    Situation 3 -> Sea Green   (#56AE7C)\n")
cat("\n")
cat("  WITH ALL quantile lines:\n")
cat("    figs/situation1_n20_pi0500.png\n")
cat("    figs/situation2_n20_pi0015.png\n")
cat("    figs/situation3_n20_pi0005.png\n")
cat("    figs/combined_three_situations.png\n")
cat("\n")
cat("  WITHOUT quantile lines (histogram only):\n")
cat("    figs/situation1_n20_pi0500_hist_only.png\n")
cat("    figs/situation2_n20_pi0015_hist_only.png\n")
cat("    figs/situation3_n20_pi0005_hist_only.png\n")
cat("    figs/combined_three_situations_hist_only.png\n")
cat("\n")
cat("  WITH SUGGESTED quantile line only:\n")
cat("    figs/combined_three_situations_suggested_only.png\n")
cat("\n")
cat("Numeric results saved in: outputs/\n")
cat("====================================================\n")



