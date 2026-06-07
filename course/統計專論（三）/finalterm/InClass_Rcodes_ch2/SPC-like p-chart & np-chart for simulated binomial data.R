############################################################
# SPC-like p-chart / np-chart for simulated binomial data
# Refined complete version
#
# Features:
#   1. Simulation settings placed at the beginning
#   2. Automatic method selection based on n*p_hat*(1-p_hat)
#   3. Supported UCL methods: Normal / CF1 / CF2 / Poisson
#   4. Consistent OOC logic for p-chart and np-chart
#   5. UCL is not integer-rounded for Normal / CF1 / CF2
#   6. PNG files are saved to folder: figs
############################################################

rm(list = ls())

############################################################
# 0. Working directory
############################################################
# Uncomment and modify if needed
setwd("E:/統計品質管制/Week5")

############################################################
# 1. Simulation settings
############################################################
seed_value <- 11

# Number of subgroups
m <- 100

# Common subgroup size
n <- 50

# True binomial proportion used for simulation
p_true <- 0.005

# Method selection based on n*pi*(1-pi)
# p_true <- 0.50  -> symmetric -> Normal
# p_true <- 0.05  -> moderate skew -> CF1
# p_true <- 0.005 -> strong skew -> CF2
# very small p_true -> rare event -> Poisson / exact

# Upper-tail probability for UCL
alpha <- 0.0027

# Whether to inject some out-of-control points manually
add_ooc_points <- FALSE

# Which subgroups to modify (only used if add_ooc_points = TRUE)
ooc_index <- c(12, 24)

# Replacement counts for those OOC points
ooc_counts <- c(5, 6)

# Output folder for figures
fig_dir <- "figs"

############################################################
# 2. Create output folder if needed
############################################################
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
    `n*p_hat` = n * p_hat,
    `n*(1-p_hat)` = n * (1 - p_hat),
    `n*p_hat*(1-p_hat)` = npv,
    Method = method,
    Interpretation = interpretation,
    check.names = FALSE
  )
}

############################################################
# 4. Unified UCL engine
############################################################
compute_ucl <- function(n, p_hat, alpha = 0.0027, method) {
  z  <- qnorm(1 - alpha)
  se <- sqrt(p_hat * (1 - p_hat) / n)
  
  # Numerical guard for extreme cases
  se <- max(se, 1e-12)
  
  ucl_p <- switch(
    method,
    
    "Normal" = {
      p_hat + z * se
    },
    
    "CF1" = {
      p_hat + z * se + ((z^2 - 1) / (6 * n)) * (1 - 2 * p_hat)
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
  
  # Truncate to [0, 1]
  ucl_p <- min(max(ucl_p, 0), 1)
  
  # Count-scale UCL
  ucl_k <- if (method == "Poisson") {
    qpois(1 - alpha, lambda = n * p_hat)
  } else {
    n * ucl_p
  }
  
  list(UCL_p = ucl_p, UCL_k = ucl_k)
}

############################################################
# 5. Candidate UCL table
############################################################
compute_all_candidates <- function(n, p_hat, alpha = 0.0027) {
  methods <- c("Normal", "CF1", "CF2", "Poisson")
  
  out <- lapply(methods, function(md) {
    tmp <- compute_ucl(n = n, p_hat = p_hat, alpha = alpha, method = md)
    data.frame(Method = md, UCL_p = tmp$UCL_p, UCL_k = tmp$UCL_k)
  })
  
  do.call(rbind, out)
}

############################################################
# 6. Main SPC calculation function
############################################################
binomial_spc_ucl <- function(x, n, alpha = 0.0027) {
  
  if (length(n) != 1) {
    stop("This version assumes a common subgroup size n.")
  }
  if (any(x < 0 | x > n)) {
    stop("All x must satisfy 0 <= x <= n.")
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
  
  candidates <- compute_all_candidates(
    n = n,
    p_hat = p_hat,
    alpha = alpha
  )
  
  # OOC decisions
  # p-chart uses proportion limit
  # np-chart uses count limit
  OOC_index_p <- which(p_i > final_ucl$UCL_p)
  OOC_index_k <- which(x > final_ucl$UCL_k)
  
  list(
    alpha = alpha,
    qprob = 1 - alpha,
    m = m,
    n = n,
    subgroup = subgroup,
    x = x,
    p_i = p_i,
    total_success = sum(x),
    total_trials = m * n,
    p_hat = p_hat,
    recommendation = rec,
    candidates = candidates,
    final_method = method_rec,
    final_UCL_p = final_ucl$UCL_p,
    final_UCL_k = final_ucl$UCL_k,
    OOC_index_p = OOC_index_p,
    OOC_index_k = OOC_index_k
  )
}

############################################################
# 7. Build refined chart titles
############################################################
make_simulation_title <- function(chart_type, m, n, p_true, method) {
  paste0(
    chart_type, "\n",
    "Simulation model: X[i] ~ Binomial(n = ", n,
    ", p = ", formatC(p_true, format = "f", digits = 4), ")",
    ",  i = 1, ..., ", m,
    "   |   Selected UCL method: ", method
  )
}

############################################################
# 8. Plot functions
############################################################
plot_p_chart <- function(res,
                         p_true,
                         main = NULL,
                         show_labels = TRUE) {
  
  if (is.null(main)) {
    main <- make_simulation_title(
      chart_type = "SPC-like p-Chart",
      m = res$m,
      n = res$n,
      p_true = p_true,
      method = res$final_method
    )
  }
  
  x <- res$subgroup
  y <- res$p_i
  
  ymax <- max(c(y, res$final_UCL_p, res$p_hat))
  ymax <- min(1, 1.15 * ymax + 0.01)
  
  par(mar = c(5, 4.5, 5, 1) + 0.1)
  
  plot(x, y,
       type = "b",
       pch = 16,
       lwd = 1.2,
       cex = 1.0,
       ylim = c(0, ymax),
       xlab = "Subgroup",
       ylab = expression(hat(p)[i]),
       main = main,
       cex.main = 0.95)
  
  abline(h = res$p_hat, lty = 1, lwd = 2)
  abline(h = res$final_UCL_p, lty = 2, lwd = 2)
  abline(h = 0, lty = 3, lwd = 1)
  
  if (length(res$OOC_index_p) > 0) {
    points(res$OOC_index_p,
           res$p_i[res$OOC_index_p],
           pch = 19, col = "red", cex = 1.3)
    if (show_labels) {
      text(res$OOC_index_p,
           res$p_i[res$OOC_index_p],
           labels = res$OOC_index_p,
           pos = 3, cex = 0.75, col = "red")
    }
  }
  
  legend("topright",
         legend = c(
           paste0("CL = p-hat = ", round(res$p_hat, 4)),
           paste0("UCL = ", round(res$final_UCL_p, 4)),
           paste0("Method = ", res$final_method),
           paste0("alpha = ", res$alpha)
         ),
         bty = "n", cex = 0.90)
}

plot_np_chart <- function(res,
                          p_true,
                          main = NULL,
                          show_labels = TRUE) {
  
  if (is.null(main)) {
    main <- make_simulation_title(
      chart_type = "SPC-like np-Chart",
      m = res$m,
      n = res$n,
      p_true = p_true,
      method = res$final_method
    )
  }
  
  x <- res$subgroup
  y <- res$x
  center_line <- res$n * res$p_hat
  
  ymax <- max(c(y, center_line, res$final_UCL_k))
  ymax <- 1.15 * ymax + 0.5
  
  par(mar = c(5, 4.5, 5, 1) + 0.1)
  
  plot(x, y,
       type = "b",
       pch = 16,
       lwd = 1.2,
       cex = 1.0,
       ylim = c(0, ymax),
       xlab = "Subgroup",
       ylab = "Number of nonconforming items",
       main = main,
       cex.main = 0.95)
  
  abline(h = center_line, lty = 1, lwd = 2)
  abline(h = res$final_UCL_k, lty = 2, lwd = 2)
  abline(h = 0, lty = 3, lwd = 1)
  
  if (length(res$OOC_index_k) > 0) {
    points(res$OOC_index_k,
           res$x[res$OOC_index_k],
           pch = 19, col = "red", cex = 1.3)
    if (show_labels) {
      text(res$OOC_index_k,
           res$x[res$OOC_index_k],
           labels = res$OOC_index_k,
           pos = 3, cex = 0.75, col = "red")
    }
  }
  
  legend("topright",
         legend = c(
           paste0("CL = n*p-hat = ", round(center_line, 3)),
           paste0("UCL = ", round(res$final_UCL_k, 3)),
           paste0("Method = ", res$final_method),
           paste0("alpha = ", res$alpha)
         ),
         bty = "n", cex = 0.90)
}

############################################################
# 9. Print summary
############################################################
print_spc_summary <- function(res) {
  cat("====================================================\n")
  cat("SPC summary for simulated binomial subgroup data\n")
  cat("====================================================\n")
  cat("Number of subgroups        :", res$m, "\n")
  cat("Subgroup size n            :", res$n, "\n")
  cat("alpha                      :", res$alpha, "\n")
  cat("Estimated p-hat            :", round(res$p_hat, 6), "\n")
  cat("Recommended method         :", res$final_method, "\n")
  cat("Final UCL on p scale       :", round(res$final_UCL_p, 6), "\n")
  cat("Final UCL on count scale   :", round(res$final_UCL_k, 6), "\n")
  cat("----------------------------------------------------\n")
  cat("Condition check:\n")
  print(res$recommendation, row.names = FALSE)
  cat("----------------------------------------------------\n")
  cat("Candidate UCLs:\n")
  print(res$candidates, row.names = FALSE)
  cat("----------------------------------------------------\n")
  cat("Out-of-control points (p-chart) : ",
      ifelse(length(res$OOC_index_p) == 0,
             "None",
             paste(res$OOC_index_p, collapse = ", ")),
      "\n", sep = "")
  cat("Out-of-control points (np-chart): ",
      ifelse(length(res$OOC_index_k) == 0,
             "None",
             paste(res$OOC_index_k, collapse = ", ")),
      "\n", sep = "")
  cat("====================================================\n")
}

############################################################
# 10. Simulate example data
############################################################
set.seed(seed_value)
x <- rbinom(n = m, size = n, prob = p_true)

if (add_ooc_points) {
  if (length(ooc_index) != length(ooc_counts)) {
    stop("ooc_index and ooc_counts must have the same length.")
  }
  if (any(ooc_index < 1 | ooc_index > m)) {
    stop("ooc_index contains invalid subgroup positions.")
  }
  if (any(ooc_counts < 0 | ooc_counts > n)) {
    stop("ooc_counts must be between 0 and n.")
  }
  x[ooc_index] <- ooc_counts
}

############################################################
# 11. Apply SPC procedure
############################################################
res <- binomial_spc_ucl(x = x, n = n, alpha = alpha)

############################################################
# 12. Console output
############################################################
cat("Simulation settings:\n")
cat("seed_value     =", seed_value, "\n")
cat("m              =", m, "\n")
cat("n              =", n, "\n")
cat("p_true         =", p_true, "\n")
cat("alpha          =", alpha, "\n")
cat("add_ooc_points =", add_ooc_points, "\n")
if (add_ooc_points) {
  cat("ooc_index      =", paste(ooc_index, collapse = ", "), "\n")
  cat("ooc_counts     =", paste(ooc_counts, collapse = ", "), "\n")
}
cat("\n")

cat("Simulated counts x:\n")
print(x)
cat("\n")

print_spc_summary(res)

############################################################
# 13. Draw charts on screen
############################################################
plot_p_chart(res, p_true = p_true)
plot_np_chart(res, p_true = p_true)

############################################################
# 14. Save PNG files to folder figs
############################################################
png(filename = file.path(fig_dir, "simulated_p_chart.png"),
    width = 1800, height = 1100, res = 180)
plot_p_chart(res, p_true = p_true)
dev.off()

png(filename = file.path(fig_dir, "simulated_np_chart.png"),
    width = 1800, height = 1100, res = 180)
plot_np_chart(res, p_true = p_true)
dev.off()

cat("\nPNG files saved to:\n")
cat(file.path(fig_dir, "simulated_p_chart.png"), "\n")
cat(file.path(fig_dir, "simulated_np_chart.png"), "\n")