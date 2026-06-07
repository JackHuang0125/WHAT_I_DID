############################################################
# Rank Tests for Slides
# Outputs:
#   figs/    -> PNG figures
#   outputs/ -> CSV summaries + tabular-only TeX
############################################################

rm(list = ls())

setwd("E:/統計品質管制/Week6")

############################################################
# 0. Folders
############################################################
if (!dir.exists("figs")) dir.create("figs", recursive = TRUE)
if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)

############################################################
# 1. Helper functions
############################################################

# Escape LaTeX special chars in plain text
latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x
}

# Write a data.frame as tabular-only TeX (Beamer-safe)
latex_tabular_only <- function(df, file, align = NULL, digits = NULL) {
  if (!is.null(digits)) {
    for (j in seq_along(df)) {
      if (is.numeric(df[[j]])) df[[j]] <- format(round(df[[j]], digits), nsmall = digits)
    }
  }
  
  if (is.null(align)) {
    align <- paste(rep("c", ncol(df)), collapse = "")
  }
  
  header <- paste(names(df), collapse = " & ")
  rows <- apply(df, 1, function(z) paste(latex_escape(z), collapse = " & "))
  
  tex <- c(
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    paste0(rows, " \\\\"),
    "\\bottomrule",
    "\\end{tabular}"
  )
  
  writeLines(tex, con = file, useBytes = TRUE)
}

# Save a named vector / list as two-column CSV
save_named_list_csv <- function(x, file) {
  out <- data.frame(
    item = names(x),
    value = unlist(x),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  write.csv(out, file, row.names = FALSE)
}

############################################################
# 2. Example data
############################################################

# One-sample examples
x <- c(12, 9, 11, 8, 13, 7, 10, 14, 6, 15)
mu0 <- 10

# Two-sample examples
x1 <- c(5, 7, 9)
x2 <- c(2, 4, 6, 8)

############################################################
# 3. Sign test
############################################################

signs_raw <- ifelse(x > mu0, "+", ifelse(x < mu0, "-", "0"))
x_notie <- x[x != mu0]
n_sign <- length(x_notie)
Y <- sum(x_notie > mu0)

# exact two-sided p-value used in slide
p_upper <- pbinom(Y - 1, size = n_sign, prob = 0.5, lower.tail = FALSE)
p_lower <- pbinom(Y,     size = n_sign, prob = 0.5, lower.tail = TRUE)
p_sign  <- 2 * min(p_upper, p_lower)
p_sign  <- min(p_sign, 1)

sign_detail <- data.frame(
  i = seq_along(x),
  Xi = x,
  sign = signs_raw,
  stringsAsFactors = FALSE
)

sign_summary <- list(
  mu0 = mu0,
  effective_n = n_sign,
  Y = Y,
  p_upper = round(p_upper, 4),
  p_lower = round(p_lower, 4),
  p_value_two_sided = round(p_sign, 4),
  decision_alpha_005 = ifelse(p_sign < 0.05, "Reject H0", "Do not reject H0")
)

write.csv(sign_detail, "outputs/sign_test_detail.csv", row.names = FALSE)
save_named_list_csv(sign_summary, "outputs/sign_test_summary.csv")

latex_tabular_only(
  sign_detail,
  file = "outputs/sign_test_detail.tex",
  align = "ccc"
)

latex_tabular_only(
  data.frame(
    mu0 = mu0,
    n = n_sign,
    Y = Y,
    p_value = round(p_sign, 4),
    decision = ifelse(p_sign < 0.05, "Reject H0", "Do not reject H0")
  ),
  file = "outputs/sign_test_summary.tex",
  align = "ccccc"
)

# Figure: Binomial pmf with observed Y
png("figs/sign_test_binomial_pmf.png", width = 1600, height = 900, res = 150)
par(mar = c(5, 5, 3, 1))
k <- 0:n_sign
pmf <- dbinom(k, size = n_sign, prob = 0.5)
barplot(
  pmf,
  names.arg = k,
  ylab = "Probability",
  xlab = "Y",
  main = "Sign Test: Binomial(9, 0.5)",
  ylim = c(0, max(pmf) * 1.25),
  col = ifelse(k == Y, "tomato", "grey80"),
  border = "grey40"
)
text(
  x = which(k == Y),
  y = pmf[k == Y + 1] + 0.02,
  labels = paste0("Observed Y = ", Y),
  cex = 1.0
)
dev.off()

############################################################
# 4. Wilcoxon signed-rank test
############################################################

d <- x - mu0
keep <- d != 0
x_wsr <- x[keep]
d_wsr <- d[keep]
abs_d <- abs(d_wsr)
ranks_abs <- rank(abs_d, ties.method = "average")
sign_wsr <- ifelse(d_wsr > 0, "+", "-")

S_plus <- sum(ranks_abs[d_wsr > 0])

wsr_detail <- data.frame(
  Xi = x_wsr,
  di = d_wsr,
  abs_di = abs_d,
  rank_abs_di = ranks_abs,
  sign = sign_wsr,
  stringsAsFactors = FALSE
)

# Normal approximation used in slide
n_wsr <- length(d_wsr)
mu_S  <- n_wsr * (n_wsr + 1) / 4
sd_S  <- sqrt(n_wsr * (n_wsr + 1) * (2 * n_wsr + 1) / 24)
z_wsr <- (S_plus - mu_S) / sd_S
p_wsr_norm <- 2 * (1 - pnorm(abs(z_wsr)))

# R's wilcox.test with exact = FALSE because of ties in |d_i|
wt_one <- wilcox.test(x, mu = mu0, exact = FALSE, correct = FALSE)

wsr_summary <- list(
  mu0 = mu0,
  effective_n = n_wsr,
  S_plus = round(S_plus, 4),
  mu_S = round(mu_S, 4),
  sd_S = round(sd_S, 4),
  z_normal_approx = round(z_wsr, 4),
  p_value_normal_approx = round(p_wsr_norm, 4),
  wilcox_test_statistic_from_R = unname(wt_one$statistic),
  p_value_from_R = round(wt_one$p.value, 4),
  decision_alpha_005 = ifelse(p_wsr_norm < 0.05, "Reject H0", "Do not reject H0")
)

write.csv(wsr_detail, "outputs/wilcoxon_signed_rank_detail.csv", row.names = FALSE)
save_named_list_csv(wsr_summary, "outputs/wilcoxon_signed_rank_summary.csv")

latex_tabular_only(
  wsr_detail,
  file = "outputs/wilcoxon_signed_rank_detail.tex",
  align = "ccccc",
  digits = 1
)

latex_tabular_only(
  data.frame(
    n = n_wsr,
    S_plus = round(S_plus, 1),
    mu = round(mu_S, 1),
    sigma = round(sd_S, 2),
    Z = round(z_wsr, 2),
    p_value = round(p_wsr_norm, 4),
    decision = ifelse(p_wsr_norm < 0.05, "Reject H0", "Do not reject H0")
  ),
  file = "outputs/wilcoxon_signed_rank_summary.tex",
  align = "ccccccc"
)

# Figure: signed ranks
png("figs/wilcoxon_signed_rank_plot.png", width = 1600, height = 900, res = 150)
par(mar = c(5, 5, 3, 1))
plot(
  ranks_abs, rep(0, length(ranks_abs)),
  pch = ifelse(d_wsr > 0, 24, 25),
  bg  = ifelse(d_wsr > 0, "tomato", "skyblue3"),
  cex = 2,
  yaxt = "n",
  ylab = "",
  xlab = "Assigned ranks of |d_i|",
  main = "Wilcoxon Signed-Rank: Positive and Negative Ranks",
  xlim = c(0, max(ranks_abs) + 1)
)
abline(h = 0, col = "grey50")
text(ranks_abs, rep(0.15, length(ranks_abs)), labels = d_wsr, cex = 0.9)
legend(
  "topleft",
  legend = c("positive d_i", "negative d_i"),
  pch = c(24, 25),
  pt.bg = c("tomato", "skyblue3"),
  bty = "n"
)
mtext(paste0("S+ = ", S_plus), side = 3, line = 0.2, adj = 1, cex = 1)
dev.off()

############################################################
# 5. Wilcoxon rank-sum test
############################################################

group <- c(rep("S1", length(x1)), rep("S2", length(x2)))
pooled <- c(x1, x2)
ranks_pooled <- rank(pooled, ties.method = "average")
W <- sum(ranks_pooled[group == "S1"])

ranksum_detail <- data.frame(
  value = pooled,
  group = group,
  rank = ranks_pooled
)
ranksum_detail <- ranksum_detail[order(ranksum_detail$value), ]

n1 <- length(x1)
n2 <- length(x2)
mu_W <- n1 * (n1 + n2 + 1) / 2
sd_W <- sqrt(n1 * n2 * (n1 + n2 + 1) / 12)
z_W <- (W - mu_W) / sd_W
p_ranksum_norm <- 2 * (1 - pnorm(abs(z_W)))

wt_two <- wilcox.test(x1, x2, exact = TRUE, correct = FALSE)

ranksum_summary <- list(
  n1 = n1,
  n2 = n2,
  W = round(W, 4),
  mu_W = round(mu_W, 4),
  sd_W = round(sd_W, 4),
  z_normal_approx = round(z_W, 4),
  p_value_normal_approx = round(p_ranksum_norm, 4),
  p_value_exact_from_R = round(wt_two$p.value, 4),
  decision_alpha_005 = ifelse(p_ranksum_norm < 0.05, "Reject H0", "Do not reject H0")
)

write.csv(ranksum_detail, "outputs/wilcoxon_rank_sum_detail.csv", row.names = FALSE)
save_named_list_csv(ranksum_summary, "outputs/wilcoxon_rank_sum_summary.csv")

latex_tabular_only(
  ranksum_detail,
  file = "outputs/wilcoxon_rank_sum_detail.tex",
  align = "ccc"
)

latex_tabular_only(
  data.frame(
    n1 = n1,
    n2 = n2,
    W = round(W, 1),
    mu_W = round(mu_W, 1),
    sigma_W = round(sd_W, 2),
    Z = round(z_W, 2),
    p_exact = round(wt_two$p.value, 4)
  ),
  file = "outputs/wilcoxon_rank_sum_summary.tex",
  align = "ccccccc"
)

# Figure: pooled ranks by group
png("figs/wilcoxon_rank_sum_ranks.png", width = 1600, height = 900, res = 150)
par(mar = c(5, 5, 3, 1))
plot(
  ranksum_detail$rank,
  rep(1, nrow(ranksum_detail)),
  pch = ifelse(ranksum_detail$group == "S1", 19, 1),
  cex = 2,
  yaxt = "n",
  ylab = "",
  xlab = "Rank in pooled sample",
  main = "Wilcoxon Rank-Sum: Pooled Ranks by Group",
  xlim = c(0.5, max(ranksum_detail$rank) + 0.5)
)
text(ranksum_detail$rank, rep(1.08, nrow(ranksum_detail)), labels = ranksum_detail$value, cex = 0.9)
legend("topleft", legend = c("Sample 1", "Sample 2"), pch = c(19, 1), bty = "n")
mtext(paste0("W = ", W, ",   E(W) = ", mu_W), side = 3, line = 0.2, adj = 1, cex = 1)
dev.off()

############################################################
# 6. Mann-Whitney test
############################################################

# Pairwise win matrix: I(x2 < x1)
U_mat <- outer(x1, x2, function(a, b) as.integer(b < a))
rownames(U_mat) <- paste0("S1_", x1)
colnames(U_mat) <- paste0("S2_", x2)
U <- sum(U_mat)

mw_detail <- as.data.frame(U_mat)
mw_detail <- cbind(Sample1 = rownames(U_mat), mw_detail, row.names = NULL)

mw_summary <- list(
  U = U,
  relation_to_W = paste0(U, " = ", W, " - ", n1 * (n1 + 1) / 2),
  W = W,
  n1 = n1,
  n2 = n2,
  decision_alpha_005 = ifelse(wt_two$p.value < 0.05, "Reject H0", "Do not reject H0")
)

write.csv(mw_detail, "outputs/mann_whitney_matrix.csv", row.names = FALSE)
save_named_list_csv(mw_summary, "outputs/mann_whitney_summary.csv")

latex_tabular_only(
  mw_detail,
  file = "outputs/mann_whitney_matrix.tex",
  align = "ccccc"
)

latex_tabular_only(
  data.frame(
    U = U,
    W = W,
    shift_term = n1 * (n1 + 1) / 2,
    check_relation = paste0(U, " = ", W, " - ", n1 * (n1 + 1) / 2),
    p_exact = round(wt_two$p.value, 4)
  ),
  file = "outputs/mann_whitney_summary.tex",
  align = "ccccc"
)

# Figure: pairwise win matrix
png("figs/mann_whitney_matrix.png", width = 1400, height = 1000, res = 150)
par(mar = c(6, 6, 3, 2))
image(
  1:ncol(U_mat), 1:nrow(U_mat),
  t(U_mat[nrow(U_mat):1, ]),
  axes = FALSE,
  xlab = "Sample 2 values",
  ylab = "Sample 1 values",
  main = "Mann-Whitney Pairwise Win Matrix (1 = win)"
)
axis(1, at = 1:ncol(U_mat), labels = x2)
axis(2, at = 1:nrow(U_mat), labels = rev(x1))
for (i in 1:nrow(U_mat)) {
  for (j in 1:ncol(U_mat)) {
    text(j, nrow(U_mat) - i + 1, labels = U_mat[i, j], cex = 1.2)
  }
}
mtext(paste0("Total U = ", U), side = 3, line = 0.2, adj = 1, cex = 1)
dev.off()

############################################################
# 7. Summary table for all rank tests
############################################################

rank_tests_summary <- data.frame(
  Test = c(
    "Sign test",
    "Wilcoxon signed-rank",
    "Wilcoxon rank-sum",
    "Mann-Whitney"
  ),
  Target = c(
    "one-sample median",
    "one-sample location under symmetry",
    "two-sample location shift",
    "two-sample location shift"
  ),
  Main_Idea = c(
    "count plus/minus signs",
    "rank absolute deviations with signs",
    "sum pooled ranks from one sample",
    "count pairwise dominance relations"
  ),
  stringsAsFactors = FALSE
)

write.csv(rank_tests_summary, "outputs/rank_tests_summary.csv", row.names = FALSE)

latex_tabular_only(
  rank_tests_summary,
  file = "outputs/rank_tests_summary.tex",
  align = "p{0.28\\textwidth}p{0.24\\textwidth}p{0.38\\textwidth}"
)

############################################################
# 8. Console summary
############################################################
cat("\nFiles created:\n")
cat("  outputs/sign_test_detail.csv\n")
cat("  outputs/sign_test_summary.csv\n")
cat("  outputs/sign_test_detail.tex\n")
cat("  outputs/sign_test_summary.tex\n")
cat("  figs/sign_test_binomial_pmf.png\n\n")

cat("  outputs/wilcoxon_signed_rank_detail.csv\n")
cat("  outputs/wilcoxon_signed_rank_summary.csv\n")
cat("  outputs/wilcoxon_signed_rank_detail.tex\n")
cat("  outputs/wilcoxon_signed_rank_summary.tex\n")
cat("  figs/wilcoxon_signed_rank_plot.png\n\n")

cat("  outputs/wilcoxon_rank_sum_detail.csv\n")
cat("  outputs/wilcoxon_rank_sum_summary.csv\n")
cat("  outputs/wilcoxon_rank_sum_detail.tex\n")
cat("  outputs/wilcoxon_rank_sum_summary.tex\n")
cat("  figs/wilcoxon_rank_sum_ranks.png\n\n")

cat("  outputs/mann_whitney_matrix.csv\n")
cat("  outputs/mann_whitney_summary.csv\n")
cat("  outputs/mann_whitney_matrix.tex\n")
cat("  outputs/mann_whitney_summary.tex\n")
cat("  figs/mann_whitney_matrix.png\n\n")

cat("  outputs/rank_tests_summary.csv\n")
cat("  outputs/rank_tests_summary.tex\n\n")
cat("Done.\n")