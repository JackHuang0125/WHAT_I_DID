############################################################
# Recap Week 3: Random Parameters and Overdispersion
# Complete R code for teaching discrete control charts
############################################################

rm(list = ls())

setwd("E:/統計品質管制/Week11")

#----------------------------------------------------------
# 0. Packages
#----------------------------------------------------------
pkgs <- c("ggplot2", "dplyr", "tidyr", "patchwork")

for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

#----------------------------------------------------------
# 1. Basic settings
#----------------------------------------------------------
set.seed(20260430)

N <- 500
alpha <- 0.0027
z <- qnorm(1 - alpha / 2)

dir.create("figs", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

#----------------------------------------------------------
# 2. Case A: Poisson with fixed lambda
#----------------------------------------------------------
lambda0 <- 5

x_pois <- rpois(N, lambda = lambda0)

CL_pois  <- lambda0
UCL_pois <- lambda0 + z * sqrt(lambda0)
LCL_pois <- max(0, lambda0 - z * sqrt(lambda0))

df_pois <- data.frame(
  Time = 1:N,
  X = x_pois,
  Case = "Poisson: fixed lambda",
  CL = CL_pois,
  UCL = UCL_pois,
  LCL = LCL_pois
)

#----------------------------------------------------------
# 3. Case B: Poisson-Gamma mixture
#    lambda random -> Negative Binomial marginally
#----------------------------------------------------------
gamma_shape <- 2
gamma_rate  <- gamma_shape / lambda0

lambda_random <- rgamma(N, shape = gamma_shape, rate = gamma_rate)
x_pois_gamma <- rpois(N, lambda = lambda_random)

df_pois_gamma <- data.frame(
  Time = 1:N,
  X = x_pois_gamma,
  Case = "Poisson-Gamma: random lambda",
  CL = CL_pois,
  UCL = UCL_pois,
  LCL = LCL_pois
)

#----------------------------------------------------------
# 4. Case C: Binomial with fixed pi
#----------------------------------------------------------
n_bin <- 100
pi0 <- 0.05

x_bin <- rbinom(N, size = n_bin, prob = pi0)

CL_bin  <- n_bin * pi0
UCL_bin <- n_bin * pi0 + z * sqrt(n_bin * pi0 * (1 - pi0))
LCL_bin <- max(0, n_bin * pi0 - z * sqrt(n_bin * pi0 * (1 - pi0)))

df_bin <- data.frame(
  Time = 1:N,
  X = x_bin,
  Case = "Binomial: fixed pi",
  CL = CL_bin,
  UCL = UCL_bin,
  LCL = LCL_bin
)

#----------------------------------------------------------
# 5. Case D: Binomial-Beta mixture
#    pi random -> Beta-Binomial marginally
#----------------------------------------------------------
concentration <- 20
alpha_beta <- pi0 * concentration
beta_beta  <- (1 - pi0) * concentration

pi_random <- rbeta(N, shape1 = alpha_beta, shape2 = beta_beta)
x_beta_bin <- rbinom(N, size = n_bin, prob = pi_random)

df_beta_bin <- data.frame(
  Time = 1:N,
  X = x_beta_bin,
  Case = "Beta-Binomial: random pi",
  CL = CL_bin,
  UCL = UCL_bin,
  LCL = LCL_bin
)

#----------------------------------------------------------
# 6. Add signal indicator
#----------------------------------------------------------
add_signal <- function(df) {
  df %>%
    mutate(
      Signal = ifelse(X > UCL | X < LCL, "Signal", "No signal")
    )
}

df_pois       <- add_signal(df_pois)
df_pois_gamma <- add_signal(df_pois_gamma)
df_bin        <- add_signal(df_bin)
df_beta_bin   <- add_signal(df_beta_bin)

df_all <- bind_rows(
  df_pois,
  df_pois_gamma,
  df_bin,
  df_beta_bin
)

#----------------------------------------------------------
# 7. Summary table
#----------------------------------------------------------
summary_tab <- df_all %>%
  group_by(Case) %>%
  summarise(
    Mean = mean(X),
    Variance = var(X),
    CL = first(CL),
    LCL = first(LCL),
    UCL = first(UCL),
    Signals = sum(Signal == "Signal"),
    False_Alarm_Rate = mean(Signal == "Signal"),
    ARL = ifelse(False_Alarm_Rate > 0,
                 1 / False_Alarm_Rate,
                 Inf),
    .groups = "drop"
  )

print(summary_tab)

write.csv(
  summary_tab,
  file = "outputs/overdispersion_summary.csv",
  row.names = FALSE
)

#----------------------------------------------------------
# 8. Plot function
#----------------------------------------------------------
plot_chart <- function(df, title_text) {
  
  df <- df %>%
    mutate(
      Signal = factor(Signal, levels = c("No signal", "Signal"))
    )
  
  ggplot(df, aes(x = Time, y = X)) +
    geom_line(linewidth = 0.45) +
    geom_point(aes(color = Signal), size = 1.8) +
    geom_hline(aes(yintercept = CL),
               linewidth = 0.8) +
    geom_hline(aes(yintercept = UCL),
               linetype = "dashed",
               linewidth = 0.8) +
    geom_hline(aes(yintercept = LCL),
               linetype = "dashed",
               linewidth = 0.8) +
    scale_color_manual(
      values = c("No signal" = "grey35",
                 "Signal" = "red"),
      labels = c("No signal", "Signal")
    ) +
    labs(
      title = title_text,
      subtitle = "Dashed lines: classical control limits based on fixed-parameter model",
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
      legend.position = "bottom",
      legend.text = element_text(size = 13)
    )
}

#----------------------------------------------------------
# 9. Individual control-chart figures
#----------------------------------------------------------
p1 <- plot_chart(df_pois,
                 "Case A: Poisson with fixed lambda")

p2 <- plot_chart(df_pois_gamma,
                 "Case B: Poisson-Gamma mixture with random lambda")

p3 <- plot_chart(df_bin,
                 "Case C: Binomial with fixed pi")

p4 <- plot_chart(df_beta_bin,
                 "Case D: Beta-Binomial mixture with random pi")

ggsave(
  filename = "figs/case_A_poisson_fixed_lambda.png",
  plot = p1,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "figs/case_B_poisson_gamma_random_lambda.png",
  plot = p2,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "figs/case_C_binomial_fixed_pi.png",
  plot = p3,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "figs/case_D_beta_binomial_random_pi.png",
  plot = p4,
  width = 9,
  height = 5,
  dpi = 300
)

#----------------------------------------------------------
# 10. Combined figure
#----------------------------------------------------------
p_combined <- (p1 / p2) | (p3 / p4)

ggsave(
  filename = "figs/overdispersion_four_cases.png",
  plot = p_combined,
  width = 15,
  height = 10,
  dpi = 300
)

#----------------------------------------------------------
# 11. Histogram comparison
#----------------------------------------------------------
df_hist <- df_all %>%
  mutate(
    Group = case_when(
      Case %in% c("Poisson: fixed lambda",
                  "Poisson-Gamma: random lambda") ~ "Count data",
      TRUE ~ "Attribute data"
    )
  )

p_hist <- ggplot(df_hist, aes(x = X, fill = Case)) +
  geom_histogram(
    position = "identity",
    alpha = 0.45,
    bins = 30
  ) +
  facet_wrap(~ Group, scales = "free") +
  labs(
    title = "Fixed Parameter vs Random Parameter",
    subtitle = "Random parameters create wider marginal distributions",
    x = "Observed count",
    y = "Frequency",
    fill = ""
  ) +
  theme_bw(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  )

ggsave(
  filename = "figs/overdispersion_histogram_comparison.png",
  plot = p_hist,
  width = 12,
  height = 6,
  dpi = 300
)

#----------------------------------------------------------
# 12. Bar plot: empirical false alarm rates
#----------------------------------------------------------
p_far <- ggplot(summary_tab,
                aes(x = Case, y = False_Alarm_Rate)) +
  geom_col(width = 0.65) +
  geom_hline(
    yintercept = alpha,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  geom_text(
    aes(label = sprintf("%.4f", False_Alarm_Rate)),
    vjust = -0.4,
    size = 4.5
  ) +
  labs(
    title = "Empirical False Alarm Rate",
    subtitle = "Dashed line: nominal alpha = 0.0027",
    x = "",
    y = "False alarm rate"
  ) +
  theme_bw(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 20, hjust = 1),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13)
  )

ggsave(
  filename = "figs/false_alarm_rate_comparison.png",
  plot = p_far,
  width = 11,
  height = 6,
  dpi = 300
)

#----------------------------------------------------------
# 13. Final message
#----------------------------------------------------------
cat("\nSimulation completed successfully.\n")
cat("Figures saved in: figs/\n")
cat("Summary table saved in: outputs/overdispersion_summary.csv\n\n")
cat("Main teaching point:\n")
cat("Fixed parameter -> baseline variance -> classical limits work approximately.\n")
cat("Random parameter -> overdispersion -> classical limits become too narrow.\n")
cat("Therefore, false alarm rate increases under overdispersion.\n")