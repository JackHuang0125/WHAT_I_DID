############################################################
# Recap Week 3: Random Parameters and Overdispersion
# Simulated cases for teaching discrete control charts
############################################################

rm(list = ls())

setwd("E:/統計品質管制/Week11")

#----------------------------------------------------------
# 0. Packages
#----------------------------------------------------------
pkgs <- c("ggplot2", "dplyr", "tidyr", "patchwork")

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
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

N <- 500       # number of subgroups / time points
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
#    lambda is random -> Negative Binomial marginally
#----------------------------------------------------------
# Choose Gamma so that E(lambda)=lambda0
# Var(lambda) controls overdispersion
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
#    pi is random -> Beta-Binomial marginally
#----------------------------------------------------------
# Choose Beta so that E(pi)=pi0
# Smaller concentration -> stronger overdispersion
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
# 6. Combine data
#----------------------------------------------------------
df_all <- bind_rows(
  df_pois,
  df_pois_gamma,
  df_bin,
  df_beta_bin
)

df_all <- df_all %>%
  mutate(
    Signal = ifelse(X > UCL | X < LCL, "Signal", "No signal")
  )

#----------------------------------------------------------
# 7. Summary table: empirical false alarm rate
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
    .groups = "drop"
  )

print(summary_tab)

write.csv(summary_tab,
          file = "outputs/overdispersion_summary.csv",
          row.names = FALSE)

#----------------------------------------------------------
# 8. Teaching plot function
#----------------------------------------------------------
plot_chart <- function(df, title_text) {
  
  ggplot(df, aes(x = Time, y = X)) +
    geom_line(linewidth = 0.5) +
    geom_point(aes(color = Signal), size = 1.8) +
    geom_hline(aes(yintercept = CL),
               linewidth = 0.8) +
    geom_hline(aes(yintercept = UCL),
               linetype = "dashed",
               linewidth = 0.8) +
    geom_hline(aes(yintercept = LCL),
               linetype = "dashed",
               linewidth = 0.8) +
    scale_color_manual(values = c("No signal" = "black",
                                  "Signal" = "red")) +
    labs(
      title = title_text,
      subtitle = "Dashed lines: classical control limits based on fixed-parameter model",
      x = "Time / subgroup index",
      y = "Count",
      color = ""
    ) +
    theme_bw(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", size = 17),
      plot.subtitle = element_text(size = 12),
      legend.position = "bottom"
    )
}

#----------------------------------------------------------
# 9. Generate four individual figures
#----------------------------------------------------------
p1 <- plot_chart(df_pois, "Case A: Poisson with fixed lambda")
p2 <- plot_chart(df_pois_gamma, "Case B: Poisson-Gamma mixture: random lambda")
p3 <- plot_chart(df_bin, "Case C: Binomial with fixed pi")
p4 <- plot_chart(df_beta_bin, "Case D: Beta-Binomial mixture: random pi")

ggsave("figs/case_A_poisson_fixed_lambda.png",
       p1, width = 9, height = 5, dpi = 300)

ggsave("figs/case_B_poisson_gamma_random_lambda.png",
       p2, width = 9, height = 5, dpi = 300)

ggsave("figs/case_C_binomial_fixed_pi.png",
       p3, width = 9, height = 5, dpi = 300)

ggsave("figs/case_D_beta_binomial_random_pi.png",
       p4, width = 9, height = 5, dpi = 300)

#----------------------------------------------------------
# 10. Combined teaching figure
#----------------------------------------------------------
p_combined <- (p1 / p2) | (p3 / p4)

ggsave("figs/overdispersion_four_cases.png",
       p_combined, width = 15, height = 10, dpi = 300)

#----------------------------------------------------------
# 11. Histogram comparison
#----------------------------------------------------------
df_hist <- df_all %>%
  mutate(Group = case_when(
    Case %in% c("Poisson: fixed lambda",
                "Poisson-Gamma: random lambda") ~ "Count data",
    TRUE ~ "Attribute data"
  ))

p_hist <- ggplot(df_hist, aes(x = X, fill = Case)) +
  geom_histogram(position = "identity",
                 alpha = 0.45,
                 bins = 30) +
  facet_wrap(~ Group, scales = "free") +
  labs(
    title = "Fixed Parameter vs Random Parameter",
    subtitle = "Random parameters create wider marginal distributions",
    x = "Observed count",
    y = "Frequency",
    fill = ""
  ) +
  theme_bw(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )

ggsave("figs/overdispersion_histogram_comparison.png",
       p_hist, width = 12, height = 6, dpi = 300)

#----------------------------------------------------------
# 12. Print final message
#----------------------------------------------------------
cat("\nSimulation completed.\n")
cat("Figures saved in: figs/\n")
cat("Summary table saved in: outputs/overdispersion_summary.csv\n")