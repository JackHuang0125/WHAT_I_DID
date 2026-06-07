############################################################
# Quantile-Based Bootstrap Control Limits under Overdispersion
# Recap Week 3 -> Correct control limits by tail probabilities
############################################################

rm(list = ls())

setwd("E:/統計品質管制/Week11")

#----------------------------------------------------------
# 0. Packages
#----------------------------------------------------------
pkgs <- c("ggplot2", "dplyr", "tidyr", "patchwork")

for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

#----------------------------------------------------------
# 1. Basic settings
#----------------------------------------------------------
set.seed(20260430)

dir.create("figs", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

alpha <- 0.0027
z <- qnorm(1 - alpha / 2)

N_phase1 <- 300
N_phase2 <- 500
B <- 3000

#----------------------------------------------------------
# 2. Helper functions
#----------------------------------------------------------

signal_rate <- function(x, LCL, UCL) {
  mean(x < LCL | x > UCL)
}

bootstrap_empirical_limits <- function(x, alpha = 0.0027, B = 3000) {
  n <- length(x)
  
  boot_x <- replicate(B, sample(x, size = n, replace = TRUE))
  boot_values <- as.vector(boot_x)
  
  LCL <- as.numeric(quantile(boot_values, probs = alpha / 2, type = 1))
  UCL <- as.numeric(quantile(boot_values, probs = 1 - alpha / 2, type = 1))
  
  c(LCL = max(0, floor(LCL)), UCL = ceiling(UCL))
}

plot_compare_chart <- function(df, title_text) {
  
  ggplot(df, aes(x = Time, y = X)) +
    geom_line(linewidth = 0.45) +
    geom_point(aes(color = Signal), size = 1.7) +
    geom_hline(aes(yintercept = CL),
               linewidth = 0.8) +
    geom_hline(aes(yintercept = Classical_UCL),
               linetype = "dashed",
               linewidth = 0.8) +
    geom_hline(aes(yintercept = Classical_LCL),
               linetype = "dashed",
               linewidth = 0.8) +
    geom_hline(aes(yintercept = Bootstrap_UCL),
               linetype = "dotdash",
               linewidth = 0.9) +
    geom_hline(aes(yintercept = Bootstrap_LCL),
               linetype = "dotdash",
               linewidth = 0.9) +
    scale_color_manual(
      values = c("No signal" = "grey35", "Signal" = "red")
    ) +
    labs(
      title = title_text,
      subtitle = "Dashed: classical limits; dot-dash: bootstrap quantile limits",
      x = "Time / subgroup index",
      y = "Observed count",
      color = ""
    ) +
    theme_bw(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 13),
      legend.position = "bottom"
    )
}

#----------------------------------------------------------
# 3. Case 1: Poisson-Gamma overdispersion
#----------------------------------------------------------
lambda0 <- 5

gamma_shape <- 2
gamma_rate  <- gamma_shape / lambda0

# Phase I data
lambda_phase1 <- rgamma(N_phase1, shape = gamma_shape, rate = gamma_rate)
x_pg_phase1 <- rpois(N_phase1, lambda = lambda_phase1)

# Phase II data
lambda_phase2 <- rgamma(N_phase2, shape = gamma_shape, rate = gamma_rate)
x_pg_phase2 <- rpois(N_phase2, lambda = lambda_phase2)

# Classical Poisson limits using Phase I mean
lambda_hat <- mean(x_pg_phase1)

CL_pg <- lambda_hat
Classical_LCL_pg <- max(0, lambda_hat - z * sqrt(lambda_hat))
Classical_UCL_pg <- lambda_hat + z * sqrt(lambda_hat)

# Bootstrap quantile limits
boot_lim_pg <- bootstrap_empirical_limits(
  x_pg_phase1,
  alpha = alpha,
  B = B
)

Bootstrap_LCL_pg <- boot_lim_pg["LCL"]
Bootstrap_UCL_pg <- boot_lim_pg["UCL"]

df_pg <- data.frame(
  Time = 1:N_phase2,
  X = x_pg_phase2,
  CL = CL_pg,
  Classical_LCL = Classical_LCL_pg,
  Classical_UCL = Classical_UCL_pg,
  Bootstrap_LCL = Bootstrap_LCL_pg,
  Bootstrap_UCL = Bootstrap_UCL_pg
) %>%
  mutate(
    Signal = ifelse(
      X < Bootstrap_LCL | X > Bootstrap_UCL,
      "Signal", "No signal"
    )
  )

#----------------------------------------------------------
# 4. Case 2: Beta-Binomial overdispersion
#----------------------------------------------------------
n_bin <- 100
pi0 <- 0.05
concentration <- 20

alpha_beta <- pi0 * concentration
beta_beta  <- (1 - pi0) * concentration

# Phase I data
pi_phase1 <- rbeta(N_phase1, shape1 = alpha_beta, shape2 = beta_beta)
x_bb_phase1 <- rbinom(N_phase1, size = n_bin, prob = pi_phase1)

# Phase II data
pi_phase2 <- rbeta(N_phase2, shape1 = alpha_beta, shape2 = beta_beta)
x_bb_phase2 <- rbinom(N_phase2, size = n_bin, prob = pi_phase2)

# Classical Binomial limits using Phase I pi-hat
pi_hat <- mean(x_bb_phase1) / n_bin

CL_bb <- n_bin * pi_hat
Classical_LCL_bb <- max(0, n_bin * pi_hat - z * sqrt(n_bin * pi_hat * (1 - pi_hat)))
Classical_UCL_bb <- n_bin * pi_hat + z * sqrt(n_bin * pi_hat * (1 - pi_hat))

# Bootstrap quantile limits
boot_lim_bb <- bootstrap_empirical_limits(
  x_bb_phase1,
  alpha = alpha,
  B = B
)

Bootstrap_LCL_bb <- boot_lim_bb["LCL"]
Bootstrap_UCL_bb <- boot_lim_bb["UCL"]

df_bb <- data.frame(
  Time = 1:N_phase2,
  X = x_bb_phase2,
  CL = CL_bb,
  Classical_LCL = Classical_LCL_bb,
  Classical_UCL = Classical_UCL_bb,
  Bootstrap_LCL = Bootstrap_LCL_bb,
  Bootstrap_UCL = Bootstrap_UCL_bb
) %>%
  mutate(
    Signal = ifelse(
      X < Bootstrap_LCL | X > Bootstrap_UCL,
      "Signal", "No signal"
    )
  )

#----------------------------------------------------------
# 5. False alarm comparison
#----------------------------------------------------------
summary_tab <- data.frame(
  Case = c("Poisson-Gamma random lambda",
           "Beta-Binomial random pi"),
  
  Classical_LCL = c(Classical_LCL_pg, Classical_LCL_bb),
  Classical_UCL = c(Classical_UCL_pg, Classical_UCL_bb),
  Bootstrap_LCL = c(Bootstrap_LCL_pg, Bootstrap_LCL_bb),
  Bootstrap_UCL = c(Bootstrap_UCL_pg, Bootstrap_UCL_bb),
  
  Classical_FAR = c(
    signal_rate(x_pg_phase2, Classical_LCL_pg, Classical_UCL_pg),
    signal_rate(x_bb_phase2, Classical_LCL_bb, Classical_UCL_bb)
  ),
  
  Bootstrap_FAR = c(
    signal_rate(x_pg_phase2, Bootstrap_LCL_pg, Bootstrap_UCL_pg),
    signal_rate(x_bb_phase2, Bootstrap_LCL_bb, Bootstrap_UCL_bb)
  )
)

summary_tab <- summary_tab %>%
  mutate(
    Classical_ARL = 1 / Classical_FAR,
    Bootstrap_ARL = 1 / Bootstrap_FAR
  )

print(summary_tab)

write.csv(
  summary_tab,
  file = "outputs/bootstrap_overdispersion_summary.csv",
  row.names = FALSE
)

#----------------------------------------------------------
# 6. Plots
#----------------------------------------------------------
p_pg <- plot_compare_chart(
  df_pg,
  "Poisson-Gamma Overdispersion: Bootstrap Quantile Limits"
)

p_bb <- plot_compare_chart(
  df_bb,
  "Beta-Binomial Overdispersion: Bootstrap Quantile Limits"
)

ggsave(
  "figs/bootstrap_poisson_gamma_limits.png",
  p_pg,
  width = 10,
  height = 5.6,
  dpi = 300
)

ggsave(
  "figs/bootstrap_beta_binomial_limits.png",
  p_bb,
  width = 10,
  height = 5.6,
  dpi = 300
)

p_combined <- p_pg / p_bb

ggsave(
  "figs/bootstrap_overdispersion_two_cases.png",
  p_combined,
  width = 11,
  height = 10,
  dpi = 300
)

#----------------------------------------------------------
# 7. False alarm rate bar plot
#----------------------------------------------------------
far_long <- summary_tab %>%
  select(Case, Classical_FAR, Bootstrap_FAR) %>%
  pivot_longer(
    cols = c(Classical_FAR, Bootstrap_FAR),
    names_to = "Method",
    values_to = "False_Alarm_Rate"
  ) %>%
  mutate(
    Method = recode(
      Method,
      Classical_FAR = "Classical limits",
      Bootstrap_FAR = "Bootstrap quantile limits"
    )
  )

p_far <- ggplot(
  far_long,
  aes(x = Case, y = False_Alarm_Rate, fill = Method)
) +
  geom_col(position = "dodge", width = 0.7) +
  geom_hline(
    yintercept = alpha,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  geom_text(
    aes(label = sprintf("%.4f", False_Alarm_Rate)),
    position = position_dodge(width = 0.7),
    vjust = -0.35,
    size = 4.5
  ) +
  labs(
    title = "False Alarm Rate Comparison",
    subtitle = "Dashed line: nominal alpha = 0.0027",
    x = "",
    y = "Empirical false alarm rate",
    fill = ""
  ) +
  theme_bw(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 15, hjust = 1),
    legend.position = "bottom"
  )

ggsave(
  "figs/bootstrap_false_alarm_rate_comparison.png",
  p_far,
  width = 11,
  height = 6,
  dpi = 300
)

#----------------------------------------------------------
# 8. Final message
#----------------------------------------------------------
cat("\nBootstrap simulation completed.\n")
cat("Figures saved in: figs/\n")
cat("Summary saved in: outputs/bootstrap_overdispersion_summary.csv\n")
cat("\nTeaching message:\n")
cat("Classical limits use mean-variance assumptions.\n")
cat("Bootstrap quantile limits use tail behavior directly.\n")
cat("Under overdispersion, bootstrap limits are wider and reduce false alarms.\n")