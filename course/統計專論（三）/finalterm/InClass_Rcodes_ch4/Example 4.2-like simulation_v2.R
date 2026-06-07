# ============================================================
# Example 4.2-like simulation
# Xbar chart vs Two-sided DI-CUSUM
# Save figs/ and outputs/
# ============================================================

rm(list = ls())

setwd("E:/統計品質管制/Week12")

library(ggplot2)
library(patchwork)

set.seed(42)

dir.create("figs", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

# ------------------------------------------------
# 1. Settings
# ------------------------------------------------
m <- 5
n_ic <- 10
n_oc <- 10
n_total <- n_ic + n_oc

mu0 <- 0
sigma <- 1

tau <- 11
delta <- 0.5

k <- 0.25
h <- 5.597

# Shewhart Xbar limits
ucl_xbar <- mu0 + 3 * sigma / sqrt(m)
cl_xbar  <- mu0
lcl_xbar <- mu0 - 3 * sigma / sqrt(m)

# ------------------------------------------------
# 2. Generate Example 4.2-like batch data
# ------------------------------------------------
x_ic <- matrix(
  rnorm(n_ic * m, mean = mu0, sd = sigma),
  nrow = n_ic
)

x_oc <- matrix(
  rnorm(n_oc * m, mean = mu0 + delta, sd = sigma),
  nrow = n_oc
)

x_batch <- rbind(x_ic, x_oc)

xbar <- rowMeans(x_batch)

Z_n <- (xbar - mu0) / (sigma / sqrt(m))

# ------------------------------------------------
# 3. Two-sided DI-CUSUM
# ------------------------------------------------
Cplus <- numeric(n_total)
Cminus <- numeric(n_total)

for (i in 1:n_total) {
  
  Cplus_prev <- ifelse(i == 1, 0, Cplus[i - 1])
  Cminus_prev <- ifelse(i == 1, 0, Cminus[i - 1])
  
  Cplus[i] <- max(
    0,
    Cplus_prev + Z_n[i] - k
  )
  
  Cminus[i] <- min(
    0,
    Cminus_prev + Z_n[i] + k
  )
}

# ------------------------------------------------
# 4. Output table
# ------------------------------------------------
result_table <- data.frame(
  n = 1:n_total,
  phase = c(rep("IC", n_ic), rep("Shift", n_oc)),
  xbar = round(xbar, 4),
  Z_n = round(Z_n, 4),
  Z_n_minus_k = round(Z_n - k, 4),
  Z_n_plus_k = round(Z_n + k, 4),
  Cplus = round(Cplus, 4),
  Cminus = round(Cminus, 4),
  Xbar_signal = (xbar > ucl_xbar) | (xbar < lcl_xbar),
  CUSUM_upper_signal = Cplus > h,
  CUSUM_lower_signal = Cminus < -h,
  CUSUM_two_sided_signal = (Cplus > h) | (Cminus < -h)
)

print(result_table)

write.csv(
  result_table,
  "outputs/example42_xbar_vs_two_sided_cusum.csv",
  row.names = FALSE
)

# ------------------------------------------------
# 5. Xbar chart
# ------------------------------------------------
p_xbar <- ggplot(
  result_table,
  aes(x = n, y = xbar)
) +
  geom_line(linewidth = 0.9) +
  geom_point(aes(shape = phase), size = 2.8) +
  
  geom_hline(
    yintercept = ucl_xbar,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_hline(
    yintercept = cl_xbar,
    linetype = "solid",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = lcl_xbar,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = tau,
    linetype = "dotted",
    linewidth = 0.8
  ) +
  
  annotate(
    "text",
    x = tau + 1,
    y = cl_xbar + 0.12,
    label = "tau==11",
    parse = TRUE,
    size = 5
  ) +
  annotate(
    "text",
    x = n_total - 1.8,
    y = ucl_xbar + 0.08,
    label = "UCL==3/sqrt(5)",
    parse = TRUE,
    size = 4.5
  ) +
  annotate(
    "text",
    x = n_total - 1.8,
    y = lcl_xbar - 0.08,
    label = "LCL==-3/sqrt(5)",
    parse = TRUE,
    size = 4.5
  ) +
  
  labs(
    title = expression(bar(X)~"Chart"),
    subtitle = expression(
      paste("Shewhart limits: ", mu[0] %+-% 3 * sigma / sqrt(m))
    ),
    x = "Sample number",
    y = expression(bar(X)[n]),
    shape = NULL
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# ------------------------------------------------
# 6. Two-sided DI-CUSUM chart
# ------------------------------------------------
cusum_df <- rbind(
  data.frame(
    n = 1:n_total,
    value = Cplus,
    type = "Cplus"
  ),
  data.frame(
    n = 1:n_total,
    value = Cminus,
    type = "Cminus"
  )
)

p_cusum <- ggplot(
  cusum_df,
  aes(x = n, y = value, linetype = type)
) +
  geom_line(linewidth = 1) +
  
  geom_hline(
    yintercept = h,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_hline(
    yintercept = -h,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.5
  ) +
  geom_vline(
    xintercept = tau,
    linetype = "dotted",
    linewidth = 0.8
  ) +
  
  annotate(
    "text",
    x = tau + 1,
    y = 0.4,
    label = "tau==11",
    parse = TRUE,
    size = 5
  ) +
  annotate(
    "text",
    x = n_total - 2,
    y = h + 0.35,
    label = "h==5.597",
    parse = TRUE,
    size = 5
  ) +
  annotate(
    "text",
    x = n_total - 2,
    y = -h - 0.35,
    label = "-h==-5.597",
    parse = TRUE,
    size = 5
  ) +
  
  scale_linetype_manual(
    values = c(
      "Cplus" = "solid",
      "Cminus" = "dotted"
    ),
    labels = c(
      "Cplus" = expression(C[n]^"+"),
      "Cminus" = expression(C[n]^"-")
    )
  ) +
  
  labs(
    title = "Two-sided DI-CUSUM",
    subtitle = expression(
      paste(C[n]^"+", " and ", C[n]^"-")
    ),
    x = "Sample number",
    y = "CUSUM statistic",
    linetype = NULL
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# ------------------------------------------------
# 7. Combine and save
# ------------------------------------------------
p_compare <- p_xbar / p_cusum +
  plot_annotation(
    title = "Comparison: Shewhart Xbar Chart vs Two-sided DI-CUSUM"
  )

print(p_compare)

ggsave(
  filename = "figs/ch4_example42_xbar_vs_two_sided_cusum.png",
  plot = p_compare,
  width = 8.2,
  height = 7.2,
  dpi = 300
)

ggsave(
  filename = "figs/ch4_example42_xbar_chart.png",
  plot = p_xbar,
  width = 7.5,
  height = 4.5,
  dpi = 300
)

ggsave(
  filename = "figs/ch4_example42_two_sided_cusum.png",
  plot = p_cusum,
  width = 7.5,
  height = 4.5,
  dpi = 300
)

# ------------------------------------------------
# 8. Summary
# ------------------------------------------------
sink("outputs/example42_xbar_vs_two_sided_cusum_summary.txt")

cat("Example 4.2-like simulation: Xbar chart vs Two-sided DI-CUSUM\n\n")

cat("m =", m, "\n")
cat("mu0 =", mu0, "\n")
cat("sigma =", sigma, "\n")
cat("tau =", tau, "\n")
cat("delta =", delta, "\n")
cat("k =", k, "\n")
cat("h =", h, "\n\n")

cat("Xbar UCL =", round(ucl_xbar, 4), "\n")
cat("Xbar CL  =", round(cl_xbar, 4), "\n")
cat("Xbar LCL =", round(lcl_xbar, 4), "\n\n")

cat("Xbar signal time = ",
    ifelse(any(result_table$Xbar_signal),
           which(result_table$Xbar_signal)[1],
           NA),
    "\n")

cat("CUSUM upper signal time = ",
    ifelse(any(result_table$CUSUM_upper_signal),
           which(result_table$CUSUM_upper_signal)[1],
           NA),
    "\n")

cat("CUSUM lower signal time = ",
    ifelse(any(result_table$CUSUM_lower_signal),
           which(result_table$CUSUM_lower_signal)[1],
           NA),
    "\n\n")

cat("max Cplus =", round(max(Cplus), 4), "\n")
cat("min Cminus =", round(min(Cminus), 4), "\n")

sink()

cat(
  "\nSaved files:\n",
  "figs/ch4_example42_xbar_vs_two_sided_cusum.png\n",
  "figs/ch4_example42_xbar_chart.png\n",
  "figs/ch4_example42_two_sided_cusum.png\n",
  "outputs/example42_xbar_vs_two_sided_cusum.csv\n",
  "outputs/example42_xbar_vs_two_sided_cusum_summary.txt\n"
)