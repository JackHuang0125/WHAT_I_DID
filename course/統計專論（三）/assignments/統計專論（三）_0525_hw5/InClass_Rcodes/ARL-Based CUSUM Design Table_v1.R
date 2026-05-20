# ============================================================
# ARL-Based CUSUM Design Table
# Compare:
#   Method 1: Mohamed (2016) regression metamodel
#   Method 2: Siegmund (1985) ARL0 approximation
# ============================================================

rm(list = ls())

setwd("E:/統計品質管制/Week12")

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
dir.create("figs", showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# 1. Mohamed (2016) regression model: without head-start
#    y = 1 / sqrt(ARL)
# ------------------------------------------------------------
arl_mohamed_no_hs <- function(delta, k, h) {
  
  y <-
    0.992 -
    0.192 * delta -
    0.479 * k -
    0.417 * h +
    0.193 * delta * k +
    0.111 * delta * h -
    0.022 * k * h +
    0.4199 * delta^2 +
    0.0658 * k^2 +
    0.086 * h^2 +
    0.0094 * delta * k * h +
    0.0785 * delta^2 * k -
    0.016203 * delta^2 * h -
    0.23916 * delta * k^2 -
    0.01383 * delta * h^2 +
    0.024431 * k^2 * h +
    0.0057 * k * h^2 -
    0.20604 * delta^3 -
    0.0066 * h^3
  
  if (y <= 0) return(NA_real_)
  
  return(1 / y^2)
}

# ------------------------------------------------------------
# 2. Siegmund (1985) ARL0 approximation
# ------------------------------------------------------------
arl_siegmund <- function(k, h) {
  
  a <- 2 * k * (h + 1.166)
  
  arl0 <- (exp(a) - a - 1) / (2 * k^2)
  
  return(arl0)
}

# ------------------------------------------------------------
# 3. Generic root search for h
# ------------------------------------------------------------
find_h <- function(target_arl, arl_fun,
                   h_lower = 0.1,
                   h_upper = 15,
                   grid_size = 5000) {
  
  obj <- function(h) arl_fun(h) - target_arl
  
  h_grid <- seq(h_lower, h_upper, length.out = grid_size)
  obj_grid <- sapply(h_grid, obj)
  
  idx <- which(obj_grid[-1] * obj_grid[-length(obj_grid)] < 0)
  
  if (length(idx) == 0) return(NA_real_)
  
  out <- uniroot(
    obj,
    lower = h_grid[idx[1]],
    upper = h_grid[idx[1] + 1]
  )$root
  
  return(out)
}

# ------------------------------------------------------------
# 4. Design settings
# ------------------------------------------------------------
target_arl0_grid <- c(200, 370, 500)

target_shift_grid <- c(0.25, 0.50, 0.75, 1.00, 1.50, 2.00)

actual_shift_grid <- c(0.00, 0.25, 0.50, 0.75, 1.00, 1.50, 2.00)

# ------------------------------------------------------------
# 5. Construct comparison design table
# ------------------------------------------------------------
out_list <- list()
counter <- 1

for (target_arl0 in target_arl0_grid) {
  
  for (delta0 in target_shift_grid) {
    
    k <- delta0 / 2
    
    # -----------------------------
    # Mohamed-designed h
    # -----------------------------
    h_mohamed <- find_h(
      target_arl = target_arl0,
      arl_fun = function(h) {
        arl_mohamed_no_hs(delta = 0, k = k, h = h)
      }
    )
    
    # -----------------------------
    # Siegmund-designed h
    # -----------------------------
    h_siegmund <- find_h(
      target_arl = target_arl0,
      arl_fun = function(h) {
        arl_siegmund(k = k, h = h)
      }
    )
    
    method_info <- data.frame(
      Method = c("Mohamed_2016", "Siegmund_1985"),
      h = c(h_mohamed, h_siegmund)
    )
    
    for (j in seq_len(nrow(method_info))) {
      
      method <- method_info$Method[j]
      h_use <- method_info$h[j]
      
      if (is.na(h_use)) next
      
      for (delta1 in actual_shift_grid) {
        
        arl_mohamed_eval <- arl_mohamed_no_hs(
          delta = delta1,
          k = k,
          h = h_use
        )
        
        arl_siegmund_eval <- ifelse(
          delta1 == 0,
          arl_siegmund(k = k, h = h_use),
          NA_real_
        )
        
        out_list[[counter]] <- data.frame(
          Target_ARL0 = target_arl0,
          Target_shift_delta0 = delta0,
          k = round(k, 4),
          Method = method,
          h = round(h_use, 4),
          Actual_shift_delta1 = delta1,
          Mohamed_ARL = round(arl_mohamed_eval, 3),
          Siegmund_ARL0 = round(arl_siegmund_eval, 3),
          Type = ifelse(delta1 == 0, "ARL0", "ARL1")
        )
        
        counter <- counter + 1
      }
    }
  }
}

design_compare <- do.call(rbind, out_list)

print(design_compare)

write.csv(
  design_compare,
  "outputs/cusum_design_compare_mohamed_siegmund_long.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 6. Summary table: compare h and ARL0 only
# ------------------------------------------------------------
summary_arl0 <- subset(
  design_compare,
  Actual_shift_delta1 == 0
)

summary_arl0 <- summary_arl0[
  order(
    summary_arl0$Target_ARL0,
    summary_arl0$Target_shift_delta0,
    summary_arl0$Method
  ),
]

print(summary_arl0)

write.csv(
  summary_arl0,
  "outputs/cusum_design_compare_arl0_summary.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 7. Wide teaching table using Mohamed ARL evaluations
# ------------------------------------------------------------
wide_table <- reshape(
  design_compare[, c(
    "Target_ARL0",
    "Target_shift_delta0",
    "k",
    "Method",
    "h",
    "Actual_shift_delta1",
    "Mohamed_ARL"
  )],
  idvar = c(
    "Target_ARL0",
    "Target_shift_delta0",
    "k",
    "Method",
    "h"
  ),
  timevar = "Actual_shift_delta1",
  direction = "wide"
)

names(wide_table) <- gsub(
  "Mohamed_ARL\\.",
  "ARL_delta_",
  names(wide_table)
)

print(wide_table)

write.csv(
  wide_table,
  "outputs/cusum_design_compare_mohamed_siegmund_wide.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 8. Optional plot: compare h values
# ------------------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  
  library(ggplot2)
  
  p_h <- ggplot(
    summary_arl0,
    aes(
      x = Target_shift_delta0,
      y = h,
      linetype = Method,
      shape = Method
    )
  ) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.8) +
    facet_wrap(~ Target_ARL0, labeller = label_both) +
    labs(
      title = "CUSUM Design Comparison",
      subtitle = "Designed decision interval h under Mohamed (2016) and Siegmund (1985)",
      x = expression("Target shift " * delta[0]),
      y = expression("Decision interval " * h),
      linetype = NULL,
      shape = NULL
    ) +
    theme_bw(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  print(p_h)
  
  ggsave(
    filename = "figs/cusum_design_compare_h.png",
    plot = p_h,
    width = 8,
    height = 4.8,
    dpi = 300
  )
}

cat(
  "\nSaved files:\n",
  "outputs/cusum_design_compare_mohamed_siegmund_long.csv\n",
  "outputs/cusum_design_compare_arl0_summary.csv\n",
  "outputs/cusum_design_compare_mohamed_siegmund_wide.csv\n",
  "figs/cusum_design_compare_h.png\n"
)