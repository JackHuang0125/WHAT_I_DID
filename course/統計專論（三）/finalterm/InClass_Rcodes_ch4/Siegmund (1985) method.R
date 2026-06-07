# ============================================================
# Classical CUSUM Design Tables
# Siegmund (1985) + Monte Carlo ARL1
# For ARL0 = 200 and 370
# ============================================================

rm(list = ls())

setwd("E:/統計品質管制/Week12")

dir.create("outputs", showWarnings = FALSE)

set.seed(20260510)

# ------------------------------------------------------------
# Siegmund (1985)
# ------------------------------------------------------------
arl0_siegmund <- function(k, h) {
  
  a <- 2 * k * (h + 1.166)
  
  arl0 <- (exp(a) - a - 1) / (2 * k^2)
  
  return(arl0)
}

# ------------------------------------------------------------
# Solve h
# ------------------------------------------------------------
find_h <- function(target_arl0, k) {
  
  objective <- function(h) {
    arl0_siegmund(k, h) - target_arl0
  }
  
  h <- uniroot(
    objective,
    lower = 0.1,
    upper = 20
  )$root
  
  return(h)
}

# ------------------------------------------------------------
# Monte Carlo ARL1
# ------------------------------------------------------------
estimate_arl1 <- function(
    delta,
    k,
    h,
    B = 5000,
    max_n = 10000
) {
  
  rl <- numeric(B)
  
  for (b in 1:B) {
    
    cp <- 0
    cm <- 0
    
    for (n in 1:max_n) {
      
      z <- rnorm(
        n = 1,
        mean = delta,
        sd = 1
      )
      
      cp <- max(
        0,
        cp + z - k
      )
      
      cm <- min(
        0,
        cm + z + k
      )
      
      if (
        cp > h ||
        cm < -h
      ) {
        
        rl[b] <- n
        
        break
      }
      
    }
    
  }
  
  return(mean(rl))
}

# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------
target_arl0_grid <- c(
  200,
  370
)

delta_grid <- c(
  0.25,
  0.50,
  0.75,
  1.00,
  1.50,
  2.00
)

# ------------------------------------------------------------
# Generate tables
# ------------------------------------------------------------
for (target_arl0 in target_arl0_grid) {
  
  cat(
    "\n==============================\n"
  )
  
  cat(
    "Target ARL0 =",
    target_arl0,
    "\n"
  )
  
  out <- data.frame()
  
  for (delta0 in delta_grid) {
    
    k <- delta0 / 2
    
    h <- find_h(
      target_arl0,
      k
    )
    
    arl1 <- estimate_arl1(
      delta = delta0,
      k = k,
      h = h
    )
    
    tmp <- data.frame(
      Target_shift = delta0,
      k = round(k, 3),
      h = round(h, 2),
      ARL0 = round(
        arl0_siegmund(
          k,
          h
        ),
        1
      ),
      ARL1 = round(
        arl1,
        1
      )
    )
    
    out <- rbind(
      out,
      tmp
    )
    
    print(tmp)
    
  }
  
  write.csv(
    out,
    paste0(
      "outputs/cusum_table_ARL0_",
      target_arl0,
      ".csv"
    ),
    row.names = FALSE
  )
  
}

cat(
  "\nSaved:\n",
  "outputs/cusum_table_ARL0_200.csv\n",
  "outputs/cusum_table_ARL0_370.csv\n"
)