############################################################
# Journal-quality p-chart / np-chart for simulated binomial data
# Output: high-resolution PNG files
############################################################

rm(list = ls())

############################################################
# 0. Working directory
############################################################
setwd("E:/統計品質管制/Week11")

############################################################
# 1. Packages
############################################################
pkgs <- c("ggplot2", "dplyr")

for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

############################################################
# 2. Simulation settings
############################################################
seed_value <- 11

m <- 100          # number of subgroups
n <- 50           # subgroup size
p_true <- 0.005   # true binomial probability

alpha <- 0.0027

add_ooc_points <- FALSE
ooc_index <- c(12, 24)
ooc_counts <- c(5, 6)

fig_dir <- "figs_journal"

if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

############################################################
# 3. Method selection rule
############################################################
choose_method <- function(n, p_hat) {
  npv <- n * p_hat * (1 - p_hat)
  
  if (npv >= 5) {
    method <- "Normal"
    interpretation <- "Large-sample normal approximation"
  } else if (npv >= 0.25) {
    method <- "CF1"
    interpretation <- "Moderate skewness"
  } else if (npv >= 0.08) {
    method <- "CF2"
    interpretation <- "Strong skewness / small p"
  } else {
    method <- "Poisson"
    interpretation <- "Rare-event case; Poisson approximation"
  }
  
  data.frame(
    n = n,
    p_hat = p_hat,
    np_hat = n * p_hat,
    n1mp_hat = n * (1 - p_hat),
    npv = npv,
    Method = method,
    Interpretation = interpretation
  )
}

############################################################
# 4. UCL calculation
############################################################
compute_ucl <- function(n, p_hat, alpha = 0.0027, method) {
  
  z  <- qnorm(1 - alpha)
  se <- sqrt(p_hat * (1 - p_hat) / n)
  se <- max(se, 1e-12)
  
  ucl_p <- switch(
    method,
    
    "Normal" = {
      p_hat + z * se
    },
    
    "CF1" = {
      p_hat + z * se +
        ((z^2 - 1) / (6 * n)) * (1 - 2 * p_hat)
    },
    
    "CF2" = {
      term1 <- ((z^2 - 1) / (6 * n)) * (1 - 2 * p_hat)
      term2 <- ((z^3 - 3 * z) / (24 * n^2)) *
        (1 - 6 * p_hat * (1 - p_hat)) / se
      term3 <- -((2 * z^3 - 5 * z) / (36 * n^2)) *
        ((1 - 2 * p_hat)^2) / se
      
      p_hat + z * se + term1 + term2 + term3
    },
    
    "Poisson" = {
      qpois(1 - alpha, lambda = n * p_hat) / n
    },
    
    stop("Unknown method.")
  )
  
  ucl_p <- min(max(ucl_p, 0), 1)
  
  ucl_k <- if (method == "Poisson") {
    qpois(1 - alpha, lambda = n * p_hat)
  } else {
    n * ucl_p
  }
  
  list(UCL_p = ucl_p, UCL_k = ucl_k)
}

############################################################
# 5. Main SPC calculation
############################################################
binomial_spc_ucl <- function(x, n, alpha = 0.0027) {
  
  if (length(n) != 1) {
    stop("This version assumes a common subgroup size n.")
  }
  
  if (any(x < 0 | x > n)) {
    stop("All counts must satisfy 0 <= x <= n.")
  }
  
  m <- length(x)
  subgroup <- seq_len(m)
  p_i <- x / n
  p_hat <- mean(p_i)
  
  rec <- choose_method(n, p_hat)
  method_rec <- as.character(rec$Method)
  
  final_ucl <- compute_ucl(
    n = n,
    p_hat = p_hat,
    alpha = alpha,
    method = method_rec
  )
  
  OOC_index_p <- which(p_i > final_ucl$UCL_p)
  OOC_index_k <- which(x > final_ucl$UCL_k)
  
  list(
    alpha = alpha,
    m = m,
    n = n,
    subgroup = subgroup,
    x = x,
    p_i = p_i,
    p_hat = p_hat,
    recommendation = rec,
    final_method = method_rec,
    final_UCL_p = final_ucl$UCL_p,
    final_UCL_k = final_ucl$UCL_k,
    OOC_index_p = OOC_index_p,
    OOC_index_k = OOC_index_k
  )
}

############################################################
# 6. Simulate data
############################################################
set.seed(seed_value)

x <- rbinom(n = m, size = n, prob = p_true)

if (add_ooc_points) {
  x[ooc_index] <- ooc_counts
}

res <- binomial_spc_ucl(x = x, n = n, alpha = alpha)

############################################################
# 7. Prepare plotting data
############################################################
df <- data.frame(
  subgroup = res$subgroup,
  x = res$x,
  p_i = res$p_i,
  OOC_p = res$subgroup %in% res$OOC_index_p,
  OOC_k = res$subgroup %in% res$OOC_index_k
)

############################################################
# 8. Journal theme
############################################################
theme_journal <- function(base_size = 18) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 4, hjust = 0.5),
      plot.subtitle = element_text(size = base_size, hjust = 0.5),
      axis.title = element_text(face = "bold", size = base_size + 2),
      axis.text = element_text(size = base_size),
      legend.title = element_blank(),
      legend.text = element_text(size = base_size),
      legend.position = "top",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.3),
      panel.border = element_rect(linewidth = 0.8),
      plot.margin = margin(15, 20, 15, 20)
    )
}

############################################################
# 9. p-chart: journal version
############################################################
p_chart <- ggplot(df, aes(x = subgroup, y = p_i)) +
  geom_line(linewidth = 0.9) +
  geom_point(aes(shape = OOC_p), size = 3.2, stroke = 1.0) +
  geom_hline(aes(yintercept = res$p_hat, linetype = "Center line"),
             linewidth = 1.1) +
  geom_hline(aes(yintercept = res$final_UCL_p, linetype = "UCL"),
             linewidth = 1.1) +
  scale_shape_manual(
    values = c(`FALSE` = 16, `TRUE` = 17),
    labels = c(`FALSE` = "In-control", `TRUE` = "Out-of-control")
  ) +
  scale_linetype_manual(
    values = c("Center line" = "solid", "UCL" = "dashed")
  ) +
  labs(
    title = "p-Chart",
    subtitle = paste0(
      "Model: X_i ~ Binomial(n = ", n,
      ", p = ", p_true,
      "); Selected method: ", res$final_method,
      "; alpha = ", alpha
    ),
    x = "Subgroup",
    y = expression(hat(p)[i])
  ) +
  coord_cartesian(
    ylim = c(0, max(df$p_i, res$final_UCL_p, res$p_hat) * 1.25 + 0.005)
  ) +
  theme_journal(base_size = 20)

############################################################
# 10. np-chart: journal version
############################################################
center_k <- n * res$p_hat

np_chart <- ggplot(df, aes(x = subgroup, y = x)) +
  geom_line(linewidth = 0.9) +
  geom_point(aes(shape = OOC_k), size = 3.2, stroke = 1.0) +
  geom_hline(aes(yintercept = center_k, linetype = "Center line"),
             linewidth = 1.1) +
  geom_hline(aes(yintercept = res$final_UCL_k, linetype = "UCL"),
             linewidth = 1.1) +
  scale_shape_manual(
    values = c(`FALSE` = 16, `TRUE` = 17),
    labels = c(`FALSE` = "In-control", `TRUE` = "Out-of-control")
  ) +
  scale_linetype_manual(
    values = c("Center line" = "solid", "UCL" = "dashed")
  ) +
  labs(
    title = "mp-Chart",
    subtitle = paste0(
      "Model: X_i ~ Binomial(n = ", n,
      ", p = ", p_true,
      "); Selected method: ", res$final_method,
      "; alpha = ", alpha
    ),
    x = "Subgroup",
    y = "Number of nonconforming items"
  ) +
  coord_cartesian(
    ylim = c(0, max(df$x, center_k, res$final_UCL_k) * 1.25 + 1)
  ) +
  theme_journal(base_size = 20)

############################################################
# 11. Save high-resolution figures
############################################################
ggsave(
  filename = file.path(fig_dir, "journal_p_chart.png"),
  plot = p_chart,
  width = 12,
  height = 7,
  dpi = 400
)

ggsave(
  filename = file.path(fig_dir, "journal_np_chart.png"),
  plot = np_chart,
  width = 12,
  height = 7,
  dpi = 400
)

############################################################
# 12. Save numerical summary
############################################################
summary_table <- data.frame(
  m = res$m,
  n = res$n,
  p_true = p_true,
  p_hat = res$p_hat,
  alpha = res$alpha,
  method = res$final_method,
  CL_p = res$p_hat,
  UCL_p = res$final_UCL_p,
  CL_np = center_k,
  UCL_np = res$final_UCL_k,
  OOC_p = ifelse(length(res$OOC_index_p) == 0,
                 "None",
                 paste(res$OOC_index_p, collapse = ", ")),
  OOC_np = ifelse(length(res$OOC_index_k) == 0,
                  "None",
                  paste(res$OOC_index_k, collapse = ", "))
)

write.csv(summary_table,
          file = file.path(fig_dir, "journal_spc_summary.csv"),
          row.names = FALSE)

############################################################
# 13. Print output
############################################################
print(summary_table)

cat("\n====================================================\n")
cat("Journal-quality figures saved:\n")
cat(file.path(fig_dir, "journal_p_chart.png"), "\n")
cat(file.path(fig_dir, "journal_np_chart.png"), "\n")
cat("Summary saved:\n")
cat(file.path(fig_dir, "journal_spc_summary.csv"), "\n")
cat("====================================================\n")