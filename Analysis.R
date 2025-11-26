# Analysis.R  -- World Happiness (2015-2022)
# Author: <Sivajeevan / GroupB 172>
# Date: <12-12-2025>
# Purpose: Combine yearly CSVs, compute Pearson correlation (Happiness Score vs GDP per capita),
#          save combined CSV and two plots for report and reproducibility.

# Load libraries
library(readr)
library(dplyr)
library(ggplot2)

# 1. Files (assumes 2015.csv ... 2022.csv are in working directory)
years <- 2015:2022
files <- paste0(years, ".csv")
files <- files[file.exists(files)]
if(length(files) == 0) stop("No CSV files found in working directory")

# 2. Read all files as character to avoid type conflicts then bind
dfs <- lapply(files, function(f) {
  read_csv(f, col_types = cols(.default = col_character()), show_col_types = FALSE)
})
df_all <- bind_rows(dfs)

# 3. Choose columns 
ladder_col <- "Happiness Score"                
gdp_col    <- "Economy (GDP per Capita)"      

# 4. Convert chosen columns to numeric and clean
df_all[[ladder_col]] <- as.numeric(gsub(",", "", df_all[[ladder_col]]))
df_all[[gdp_col]]    <- as.numeric(gsub(",", "", df_all[[gdp_col]]))
df_clean <- df_all %>% select(all_of(c(ladder_col, gdp_col))) %>% na.omit()

# 5. Save combined CSV 
readr::write_csv(df_all, "world_happiness_combined_2015_2022.csv")

# 6. Pearson correlation test
cor_test <- cor.test(df_clean[[ladder_col]], df_clean[[gdp_col]], method = "pearson")

# 7. Save a text summary of results
sink("analysis_results.txt")
cat("Pearson correlation test results\n")
print(cor_test)
cat("\nColumns used:\n")
cat("Ladder:", ladder_col, "\nGDP:", gdp_col, "\n")
sink()

# 8. Create and save plots
p <- ggplot(df_clean, aes(x = .data[[gdp_col]], y = .data[[ladder_col]])) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Happiness Score vs GDP per Capita", x = gdp_col, y = ladder_col) +
  theme_minimal(base_size = 12)
ggsave("plot_scatter.png", p, width = 7, height = 5, dpi = 200)

h <- ggplot(df_clean, aes(x = .data[[ladder_col]])) +
  geom_histogram(bins = 20, color = "black", fill = "grey80") +
  labs(title = "Distribution of Happiness Score", x = ladder_col) +
  theme_minimal(base_size = 12)
ggsave("plot_histogram.png", h, width = 7, height = 4, dpi = 200)

# Finish
message("Done. Files created: world_happiness_combined_2015_2022.csv, plot_scatter.png, plot_histogram.png, analysis_results.txt")

