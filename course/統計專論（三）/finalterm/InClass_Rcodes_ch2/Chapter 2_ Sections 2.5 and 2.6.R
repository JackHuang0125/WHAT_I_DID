# =========================================================
# Chapter 2, Sections 2.5 and 2.6
# R code for teaching figures and summary outputs
# =========================================================

rm(list = ls())

# -----------------------------
# Packages
# -----------------------------
# Only base R is required.

# -----------------------------
# Project folders
# -----------------------------

setwd("E:/統計品質管制/Week4")

root_dir <- getwd()
fig_dir  <- file.path(root_dir, "figs")

if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

# -----------------------------
# Utility function
# -----------------------------
save_png <- function(filename, width = 1600, height = 1000, res = 180) {
  png(filename = file.path(fig_dir, filename),
      width = width, height = height, res = res)
}

# =========================================================
# 1. Categorical data example:
#    Frequency table, pie chart, bar chart
# =========================================================

party <- c("Democrat", "Republican", "Green", "Other")
freq  <- c(437, 486, 53, 24)
n_party <- sum(freq)
rel_freq <- freq / n_party

party_table <- data.frame(
  Party = party,
  Frequency = freq,
  Relative_Frequency = round(rel_freq, 3)
)

write.csv(party_table,
          file = file.path(fig_dir, "party_status_frequency_table.csv"),
          row.names = FALSE)

# Pie chart
save_png("party_status_pie_chart.png", width = 1400, height = 1000)
pie(freq,
    labels = paste0(party, "\n", round(100 * rel_freq, 1), "%"),
    main = "Pie Chart of Party Status")
dev.off()

# Bar chart
save_png("party_status_bar_chart.png", width = 1400, height = 1000)
barplot(freq,
        names.arg = party,
        las = 1,
        main = "Bar Chart of Party Status",
        ylab = "Frequency")
dev.off()

# =========================================================
# 2. Numerical data example:
#    Ages of 25 recreation-center members
# =========================================================

ages <- c(18, 27, 56, 19, 33, 24, 19, 48, 37, 25,
          20, 22, 31, 29, 65, 41, 22, 39, 37, 22,
          45, 22, 43, 61, 53)

n_ages <- length(ages)
ages_sorted <- sort(ages)

age_summary <- data.frame(
  n = n_ages,
  Mean = mean(ages),
  Median = median(ages),
  Variance = var(ages),
  SD = sd(ages),
  Min = min(ages),
  Q1 = as.numeric(quantile(ages, 0.25, type = 2)),
  Q3 = as.numeric(quantile(ages, 0.75, type = 2)),
  Max = max(ages),
  IQR = IQR(ages, type = 2)
)

write.csv(age_summary,
          file = file.path(fig_dir, "age_summary_statistics.csv"),
          row.names = FALSE)

# -----------------------------
# Dot plot
# -----------------------------
save_png("age_dot_plot.png", width = 1500, height = 900)
stripchart(ages,
           method = "stack",
           pch = 19,
           cex = 1.2,
           main = "Dot Plot of Ages",
           xlab = "Age")
dev.off()

# -----------------------------
# Stem-and-leaf plot
# Save printed output to text file and also produce a png
# -----------------------------
stem_file <- file.path(fig_dir, "age_stem_leaf_plot.txt")
capture.output(stem(ages), file = stem_file)

save_png("age_stem_leaf_plot.png", width = 1400, height = 1000)
plot.new()
title(main = "Stem-and-Leaf Plot of Ages")
stem_text <- capture.output(stem(ages))
text(0.05, 0.95,
     labels = paste(stem_text, collapse = "\n"),
     adj = c(0, 1),
     family = "mono",
     cex = 1.1)
dev.off()

# -----------------------------
# Box plot
# -----------------------------
save_png("age_box_plot.png", width = 1200, height = 900)
boxplot(ages,
        horizontal = TRUE,
        main = "Box Plot of Ages",
        xlab = "Age")
dev.off()

# =========================================================
# 3. Histograms for the age data
# =========================================================

# Equal-width class intervals:
# [10,20), [20,30), [30,40), [40,50), [50,60), [60,70)
age_breaks <- c(10, 20, 30, 40, 50, 60, 70)

age_hist <- hist(ages,
                 breaks = age_breaks,
                 right = FALSE,
                 plot = FALSE)

age_hist_table <- data.frame(
  Interval_Left = head(age_hist$breaks, -1),
  Interval_Right = tail(age_hist$breaks, -1),
  Frequency = age_hist$counts,
  Relative_Frequency = round(age_hist$counts / sum(age_hist$counts), 3),
  Width = diff(age_hist$breaks),
  Density_Height = round((age_hist$counts / sum(age_hist$counts)) / diff(age_hist$breaks), 4)
)

write.csv(age_hist_table,
          file = file.path(fig_dir, "age_histogram_frequency_table.csv"),
          row.names = FALSE)

# Frequency histogram
save_png("age_frequency_histogram.png", width = 1500, height = 1000)
hist(ages,
     breaks = age_breaks,
     right = FALSE,
     main = "Frequency Histogram of Ages",
     xlab = "Age",
     ylab = "Frequency")
dev.off()

# Relative frequency histogram
save_png("age_relative_frequency_histogram.png", width = 1500, height = 1000)
hist(ages,
     breaks = age_breaks,
     right = FALSE,
     freq = FALSE,
     main = "Relative Frequency / Density Histogram of Ages",
     xlab = "Age",
     ylab = "Density")
# add rug for visibility
rug(ages)
dev.off()

# Density histogram + kernel density
save_png("age_density_histogram.png", width = 1500, height = 1000)
hist(ages,
     breaks = age_breaks,
     right = FALSE,
     freq = FALSE,
     main = "Density Histogram of Ages",
     xlab = "Age",
     ylab = "Density")
lines(density(ages), lwd = 2)
rug(ages)
dev.off()

# =========================================================
# 4. Outlier example for mean vs median / trimmed mean
# =========================================================

salary <- c(67, 84, 56, 210, 79, 85, 73, 64, 88, 93)

salary_summary <- data.frame(
  Mean = mean(salary),
  Median = median(salary),
  Trimmed_Mean_10pct = mean(salary, trim = 0.10),
  Variance = var(salary),
  SD = sd(salary),
  Min = min(salary),
  Max = max(salary)
)

write.csv(salary_summary,
          file = file.path(fig_dir, "salary_outlier_summary.csv"),
          row.names = FALSE)

save_png("salary_box_plot.png", width = 1200, height = 900)
boxplot(salary,
        horizontal = TRUE,
        main = "Salary Example: Effect of an Outlier",
        xlab = "Salary (thousand dollars)")
dev.off()

save_png("salary_dot_plot.png", width = 1400, height = 900)
stripchart(salary,
           method = "stack",
           pch = 19,
           cex = 1.3,
           main = "Dot Plot of Salary Example",
           xlab = "Salary (thousand dollars)")
abline(v = mean(salary), lty = 2, lwd = 2)
abline(v = median(salary), lty = 3, lwd = 2)
abline(v = mean(salary, trim = 0.10), lty = 4, lwd = 2)
legend("topleft",
       legend = c("Mean", "Median", "10% Trimmed Mean"),
       lty = c(2, 3, 4),
       lwd = 2,
       bty = "n")
dev.off()

# =========================================================
# 5. Example for unequal-width bins:
#    frequency histogram vs density histogram
# =========================================================
# Since the textbook figure is illustrative, we create a right-skewed
# teaching dataset to demonstrate why unequal bin widths can mislead.

sio2 <- c(0.3, 0.7, 1.1, 1.5, 1.8, 2.0, 2.3, 2.7, 2.9, 3.1,
          3.4, 3.8, 4.2, 4.8, 5.2, 5.9, 6.4, 7.2, 8.5, 9.7,
          11.5, 13.2, 16.8, 21.4, 28.6, 36.7, 49.3)

# Equal-width breaks
sio2_breaks_equal <- seq(0, 50, by = 10)
sio2_hist_equal <- hist(sio2, breaks = sio2_breaks_equal, plot = FALSE)

sio2_equal_table <- data.frame(
  Interval_Left = head(sio2_hist_equal$breaks, -1),
  Interval_Right = tail(sio2_hist_equal$breaks, -1),
  Frequency = sio2_hist_equal$counts,
  Relative_Frequency = round(sio2_hist_equal$counts / length(sio2), 3),
  Width = diff(sio2_hist_equal$breaks)
)

write.csv(sio2_equal_table,
          file = file.path(fig_dir, "sio2_equal_width_table.csv"),
          row.names = FALSE)

save_png("sio2_frequency_histogram_equal_width.png", width = 1500, height = 1000)
hist(sio2,
     breaks = sio2_breaks_equal,
     right = FALSE,
     main = "SiO2 Example: Equal-Width Frequency Histogram",
     xlab = "Measurement",
     ylab = "Frequency")
rug(sio2)
dev.off()

# Unequal-width breaks
sio2_breaks_unequal <- c(0, 5, 10, 20, 50)
sio2_hist_unequal <- hist(sio2, breaks = sio2_breaks_unequal, plot = FALSE)

sio2_unequal_table <- data.frame(
  Interval_Left = head(sio2_hist_unequal$breaks, -1),
  Interval_Right = tail(sio2_hist_unequal$breaks, -1),
  Frequency = sio2_hist_unequal$counts,
  Relative_Frequency = round(sio2_hist_unequal$counts / length(sio2), 3),
  Width = diff(sio2_hist_unequal$breaks),
  Density_Height = round((sio2_hist_unequal$counts / length(sio2)) / diff(sio2_hist_unequal$breaks), 4)
)

write.csv(sio2_unequal_table,
          file = file.path(fig_dir, "sio2_unequal_width_table.csv"),
          row.names = FALSE)

# Unequal-width frequency histogram
save_png("sio2_frequency_histogram_unequal_width.png", width = 1500, height = 1000)
hist(sio2,
     breaks = sio2_breaks_unequal,
     right = FALSE,
     main = "SiO2 Example: Unequal-Width Frequency Histogram",
     xlab = "Measurement",
     ylab = "Frequency")
rug(sio2)
dev.off()

# Density histogram for unequal-width bins
save_png("sio2_density_histogram_unequal_width.png", width = 1500, height = 1000)
plot(sio2_hist_unequal,
     freq = FALSE,
     right = FALSE,
     main = "SiO2 Example: Density Histogram with Unequal Bins",
     xlab = "Measurement",
     ylab = "Density")
rug(sio2)
dev.off()

# =========================================================
# 6. Smoothed histogram shapes
# =========================================================
set.seed(123)

shape_data <- c(rnorm(180, mean = 0, sd = 1),
                rnorm(80, mean = 3, sd = 0.6))

save_png("smoothed_histogram_shapes.png", width = 1500, height = 1000)
hist(shape_data,
     breaks = 20,
     freq = FALSE,
     main = "Histogram with Smoothed Density Curve",
     xlab = "x",
     ylab = "Density")
lines(density(shape_data), lwd = 2)
rug(shape_data)
dev.off()

# =========================================================
# 7. Console summary
# =========================================================
cat("All figures and tables have been written to:\n")
cat(fig_dir, "\n\n")

cat("Generated PNG files:\n")
print(list.files(fig_dir, pattern = "\\.png$", full.names = FALSE))

cat("\nGenerated CSV/TXT files:\n")
print(list.files(fig_dir, pattern = "\\.(csv|txt)$", full.names = FALSE))

