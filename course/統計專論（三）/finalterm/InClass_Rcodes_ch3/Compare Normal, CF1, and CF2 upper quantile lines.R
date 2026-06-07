############################################################
# Compare Normal, CF1, and CF2 upper quantile lines
# Larger PNG text version
############################################################

rm(list = ls())
set.seed(123)

setwd("E:/統計品質管制/Week11")

############################################################
# 0. Create folders
############################################################
if (!dir.exists("figs_journal"))    dir.create("figs_journal")
if (!dir.exists("outputs")) dir.create("outputs")

############################################################
# 1. User setting
############################################################
alpha <- 0.0027
qprob <- 1 - alpha / 2   # 0.99865, two-sided 3-sigma upper tail

############################################################
# 2. Quantile functions on p-hat scale
############################################################
q_normal_phat <- function(n, pi, qprob) {
  z  <- qnorm(qprob)
  se <- sqrt(pi * (1 - pi) / n)
  pi + z * se
}

q_cf1_phat <- function(n, pi, qprob) {
  z  <- qnorm(qprob)
  se <- sqrt(pi * (1 - pi) / n)
  
  pi + z * se +
    ((z^2 - 1) / (6 * n)) * (1 - 2 * pi)
}

q_cf2_phat <- function(n, pi, qprob) {
  z  <- qnorm(qprob)
  se <- sqrt(pi * (1 - pi) / n)
  se <- max(se, 1e-12)
  
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
q_exact_k <- function(n, pi, qprob) {
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
  
  if (npi >= 10 && n1mpi >= 10) {
    method <- "Normal"
    interpretation <- "Large-sample normal approximation"
  } else if (npv >= 0.25) {
    method <- "CF1"
    interpretation <- "Moderate skewness"
  } else if (npv >= 0.08) {
    method <- "CF2"
    interpretation <- "Strong skewness / small pi"
  } else {
    method <- "Other"
    interpretation <- "Poisson / exact / non-normal"
  }
  
  data.frame(
    n = n,
    pi = pi,
    `n*pi` = npi,
    `n*(1-pi)` = n1mpi,
    `n*pi*(1-pi)` = npv,
    recommended = method,
    interpretation = interpretation,
    check.names = FALSE
  )
}

############################################################
# 7. Summary table
############################################################
quantile_table <- function(n, pi, qprob) {
  qN_phat   <- q_normal_phat(n, pi, qprob)
  qCF1_phat <- q_cf1_phat(n, pi, qprob)
  qCF2_phat <- q_cf2_phat(n, pi, qprob)
  
  qN_k   <- phat_to_k(qN_phat, n)
  qCF1_k <- phat_to_k(qCF1_phat, n)
  qCF2_k <- phat_to_k(qCF2_phat, n)
  qE_k   <- q_exact_k(n, pi, qprob)
  
  data.frame(
    n = n,
    pi = pi,
    qprob = qprob,
    Normal_phat = qN_phat,
    CF1_phat = qCF1_phat,
    CF2_phat = qCF2_phat,
    Normal_k = qN_k,
    CF1_k = qCF1_k,
    CF2_k = qCF2_k,
    Exact_k = qE_k,
    Normal_abs_error = abs(qN_k - qE_k),
    CF1_abs_error = abs(qCF1_k - qE_k),
    CF2_abs_error = abs(qCF2_k - qE_k),
    Normal_rel_error = safe_rel_error(qN_k, qE_k),
    CF1_rel_error = safe_rel_error(qCF1_k, qE_k),
    CF2_rel_error = safe_rel_error(qCF2_k, qE_k)
  )
}

############################################################
# 8. Colors
############################################################
col1 <- "#5B9BD5"
col2 <- "#F4845F"
col3 <- "#56AE7C"
col_border <- "white"

############################################################
# 9a. Plot function WITH ALL quantile lines
############################################################
plot_compare_quantiles <- function(sim, n, pi, qprob,
                                   max_n = n,
                                   bar_col = "gray90",
                                   main_title = NULL,
                                   show_exact = TRUE) {
  
  qN_k   <- phat_to_k(q_normal_phat(n, pi, qprob), n)
  qCF1_k <- phat_to_k(q_cf1_phat(n, pi, qprob), n)
  qCF2_k <- phat_to_k(q_cf2_phat(n, pi, qprob), n)
  
  if (is.null(main_title)) {
    main_title <- paste0("n = ", n, ", pi = ", pi)
  }
  
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  par(
    mar = c(6, 6, 5, 1.5),
    mgp = c(3.4, 1.1, 0),
    cex.main = 1.45,
    cex.lab  = 1.35,
    cex.axis = 1.20
  )
  
  hist(sim,
       main = main_title,
       xlab = "Number of Successes k",
       ylab = "Relative Frequency",
       breaks = seq(-0.5, max_n + 0.5, 1),
       xlim = c(-0.5, max_n + 0.5),
       ylim = c(0, 1),
       freq = FALSE,
       xaxt = "n",
       col = bar_col,
       border = col_border)
  
  axis(1, at = 0:max_n, cex.axis = 1.20)
  
  abline(v = qN_k,   lwd = 3, lty = 1, col = "black")
  abline(v = qCF1_k, lwd = 3, lty = 2, col = "blue")
  abline(v = qCF2_k, lwd = 3, lty = 3, col = "red")
  
  if (show_exact) {
    qE_k <- q_exact_k(n, pi, qprob)
    abline(v = qE_k, lwd = 3.5, lty = 4, col = "darkgreen")
    
    legend("topright",
           legend = c(
             paste0("Normal = ", round(qN_k, 3)),
             paste0("CF1 = ", round(qCF1_k, 3)),
             paste0("CF2 = ", round(qCF2_k, 3)),
             paste0("Exact = ", round(qE_k, 3))
           ),
           lty = c(1, 2, 3, 4),
           col = c("black", "blue", "red", "darkgreen"),
           lwd = c(3, 3, 3, 3.5),
           bty = "n",
           cex = 1.20)
  } else {
    legend("topright",
           legend = c(
             paste0("Normal = ", round(qN_k, 3)),
             paste0("CF1 = ", round(qCF1_k, 3)),
             paste0("CF2 = ", round(qCF2_k, 3))
           ),
           lty = c(1, 2, 3),
           col = c("black", "blue", "red"),
           lwd = 3,
           bty = "n",
           cex = 1.20)
  }
}

############################################################
# 9b. Histogram only
############################################################
plot_hist_only <- function(sim, n, pi,
                           max_n = n,
                           bar_col = "gray90",
                           main_title = NULL) {
  
  if (is.null(main_title)) {
    main_title <- paste0("n = ", n, ", pi = ", pi)
  }
  
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  par(
    mar = c(6, 6, 5, 1.5),
    mgp = c(3.4, 1.1, 0),
    cex.main = 1.45,
    cex.lab  = 1.35,
    cex.axis = 1.20
  )
  
  hist(sim,
       main = main_title,
       xlab = "Number of Successes k",
       ylab = "Relative Frequency",
       breaks = seq(-0.5, max_n + 0.5, 1),
       xlim = c(-0.5, max_n + 0.5),
       ylim = c(0, 1),
       freq = FALSE,
       xaxt = "n",
       col = bar_col,
       border = col_border)
  
  axis(1, at = 0:max_n, cex.axis = 1.20)
}

############################################################
# 9c. Suggested quantile line only
############################################################
plot_suggested_only <- function(sim, n, pi, method, qprob,
                                max_n = n,
                                bar_col = "gray90",
                                main_title = NULL) {
  
  if (is.null(main_title)) {
    main_title <- paste0("n = ", n, ", pi = ", pi, "\nSuggested: ", method)
  }
  
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  par(
    mar = c(6, 6, 5, 1.5),
    mgp = c(3.4, 1.1, 0),
    cex.main = 1.35,
    cex.lab  = 1.30,
    cex.axis = 1.15
  )
  
  hist(sim,
       main = main_title,
       xlab = "Number of Successes k",
       ylab = "Relative Frequency",
       breaks = seq(-0.5, max_n + 0.5, 1),
       xlim = c(-0.5, max_n + 0.5),
       ylim = c(0, 1),
       freq = FALSE,
       xaxt = "n",
       col = bar_col,
       border = col_border)
  
  axis(1, at = 0:max_n, cex.axis = 1.15)
  
  qE_k <- q_exact_k(n, pi, qprob)
  abline(v = qE_k, lwd = 3.5, lty = 4, col = "darkgreen")
  
  if (method == "Normal") {
    q_k <- phat_to_k(q_normal_phat(n, pi, qprob), n)
    abline(v = q_k, lwd = 3, lty = 1, col = "black")
    leg_txt <- paste0("Normal = ", round(q_k, 3))
    leg_col <- "black"
    leg_lty <- 1
  } else if (method == "CF1") {
    q_k <- phat_to_k(q_cf1_phat(n, pi, qprob), n)
    abline(v = q_k, lwd = 3, lty = 2, col = "blue")
    leg_txt <- paste0("CF1 = ", round(q_k, 3))
    leg_col <- "blue"
    leg_lty <- 2
  } else if (method == "CF2") {
    q_k <- phat_to_k(q_cf2_phat(n, pi, qprob), n)
    abline(v = q_k, lwd = 3, lty = 3, col = "red")
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
         lwd = c(3, 3.5),
         bty = "n",
         cex = 1.15)
}

############################################################
# 10. Three situations
############################################################
n1 <- 20; p1 <- 0.500
n2 <- 20; p2 <- 0.015
n3 <- 20; p3 <- 0.005

max_n <- max(n1, n2, n3)

############################################################
# 11. Simulate data
############################################################
sim1 <- rbinom(10000, size = n1, prob = p1)
sim2 <- rbinom(10000, size = n2, prob = p2)
sim3 <- rbinom(10000, size = n3, prob = p3)

############################################################
# 12. Recommendation table
############################################################
rec_table <- rbind(
  cbind(case = "Situation 1", choose_method(n1, p1)),
  cbind(case = "Situation 2", choose_method(n2, p2)),
  cbind(case = "Situation 3", choose_method(n3, p3))
)

print(rec_table)

############################################################
# 13. Quantile comparison tables
############################################################
tab1 <- cbind(case = "Situation 1", quantile_table(n1, p1, qprob))
tab2 <- cbind(case = "Situation 2", quantile_table(n2, p2, qprob))
tab3 <- cbind(case = "Situation 3", quantile_table(n3, p3, qprob))

print(tab1)
print(tab2)
print(tab3)

############################################################
# 14. Save numeric outputs
############################################################
write.csv(rec_table, "outputs/recommendation_table.csv", row.names = FALSE)
write.csv(tab1, "outputs/quantile_table_situation1.csv", row.names = FALSE)
write.csv(tab2, "outputs/quantile_table_situation2.csv", row.names = FALSE)
write.csv(tab3, "outputs/quantile_table_situation3.csv", row.names = FALSE)

write.csv(rbind(tab1, tab2, tab3),
          "outputs/quantile_table_all_situations.csv",
          row.names = FALSE)

############################################################
# 15. Save individual plots
############################################################
png("figs/situation1_n20_pi0500_large_text.png",
    width = 2200, height = 1600, res = 250)
plot_compare_quantiles(sim1, n1, p1, qprob, max_n,
                       bar_col = col1,
                       main_title = "Situation 1: n=20, pi=0.500",
                       show_exact = TRUE)
dev.off()

png("figs/situation2_n20_pi0015_large_text.png",
    width = 2200, height = 1600, res = 250)
plot_compare_quantiles(sim2, n2, p2, qprob, max_n,
                       bar_col = col2,
                       main_title = "Situation 2: n=20, pi=0.015",
                       show_exact = TRUE)
dev.off()

png("figs/situation3_n20_pi0005_large_text.png",
    width = 2200, height = 1600, res = 250)
plot_compare_quantiles(sim3, n3, p3, qprob, max_n,
                       bar_col = col3,
                       main_title = "Situation 3: n=20, pi=0.005",
                       show_exact = TRUE)
dev.off()

############################################################
# 16. Combined plot
############################################################
png("figs/combined_three_situations_large_text.png",
    width = 3000, height = 1200, res = 250)

par(mfrow = c(1, 3),
    mar = c(6, 5.5, 5, 1.2),
    mgp = c(3.2, 1.0, 0),
    cex.main = 1.25,
    cex.lab = 1.20,
    cex.axis = 1.05)

plot_compare_quantiles(sim1, n1, p1, qprob, max_n,
                       bar_col = col1,
                       main_title = "Situation 1\n(n=20, pi=0.500)",
                       show_exact = TRUE)

plot_compare_quantiles(sim2, n2, p2, qprob, max_n,
                       bar_col = col2,
                       main_title = "Situation 2\n(n=20, pi=0.015)",
                       show_exact = TRUE)

plot_compare_quantiles(sim3, n3, p3, qprob, max_n,
                       bar_col = col3,
                       main_title = "Situation 3\n(n=20, pi=0.005)",
                       show_exact = TRUE)

dev.off()

############################################################
# 17. Combined histogram only
############################################################
png("figs/combined_three_situations_hist_only_large_text.png",
    width = 3000, height = 1200, res = 250)

par(mfrow = c(1, 3),
    mar = c(6, 5.5, 5, 1.2),
    mgp = c(3.2, 1.0, 0),
    cex.main = 1.25,
    cex.lab = 1.20,
    cex.axis = 1.05)

plot_hist_only(sim1, n1, p1, max_n,
               bar_col = col1,
               main_title = "Situation 1\n(n=20, pi=0.500)")

plot_hist_only(sim2, n2, p2, max_n,
               bar_col = col2,
               main_title = "Situation 2\n(n=20, pi=0.015)")

plot_hist_only(sim3, n3, p3, max_n,
               bar_col = col3,
               main_title = "Situation 3\n(n=20, pi=0.005)")

dev.off()

############################################################
# 18. Combined suggested only
############################################################
png("figs/combined_three_situations_suggested_only_large_text.png",
    width = 3000, height = 1200, res = 250)

par(mfrow = c(1, 3),
    mar = c(6, 5.5, 5, 1.2),
    mgp = c(3.2, 1.0, 0),
    cex.main = 1.25,
    cex.lab = 1.20,
    cex.axis = 1.05)

rec1 <- as.character(choose_method(n1, p1)$recommended)
rec2 <- as.character(choose_method(n2, p2)$recommended)
rec3 <- as.character(choose_method(n3, p3)$recommended)

plot_suggested_only(sim1, n1, p1, rec1, qprob, max_n,
                    bar_col = col1,
                    main_title = paste0("Situation 1\nSuggested: ", rec1))

plot_suggested_only(sim2, n2, p2, rec2, qprob, max_n,
                    bar_col = col2,
                    main_title = paste0("Situation 2\nSuggested: ", rec2))

plot_suggested_only(sim3, n3, p3, rec3, qprob, max_n,
                    bar_col = col3,
                    main_title = paste0("Situation 3\nSuggested: ", rec3))

dev.off()

############################################################
# 19. Console summary
############################################################
cat("====================================================\n")
cat("Files saved successfully.\n")
cat("PNG files with larger text saved in figs/.\n")
cat("Numeric results saved in outputs/.\n")
cat("====================================================\n")