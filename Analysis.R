###############################################################################
# Analysis_final_corrected.R
# World Happiness Report (2015-2022) - corrected single-file lecture-style analysis
# Save this file in the folder containing 2015.csv ... 2022.csv and run:
#    source("Analysis_final_corrected.R")
#
# Author: <Sivajeevan / Groupb 172>
# Date: <DD-MM-2025>
#
# Produces:
# - world_happiness_combined_raw_2015_2022.csv
# - world_happiness_cleaned_2015_2022.csv
# - scatter_base.png, hist_ladder.png, hist_gdp.png, box_by_region.png (if region exists)
# - pearson_results.txt, spearman_results.txt (asymptotic), wilcox_median_gdp.txt
# - chisq_region_happycat.txt OR chisq_region_happycat_combined.txt or fisher_region_happycat.txt
# - analysis_summary_report.txt, one_line_summary.txt, plot_scatter_ggplot.png
###############################################################################

# --- 0. Setup ----------------------------------------------------------------
needed_pkgs <- c("readr", "dplyr", "tidyr", "ggplot2")
have <- needed_pkgs %in% installed.packages()[, "Package"]
if(any(!have)) install.packages(needed_pkgs[!have])
library(readr); library(dplyr); library(tidyr); library(ggplot2)

# --- 1. Detect yearly CSVs and read them -------------------------------------
years <- 2015:2022
files_expected <- paste0(years, ".csv")
files <- files_expected[file.exists(files_expected)]
if(length(files) == 0) stop("No CSV files found. Put 2015.csv ... 2022.csv in working folder and re-run.")

dfs <- list()
for(f in files){
  message("Reading ", f)
  df <- read_csv(f, col_types = cols(.default = col_character()), show_col_types = FALSE)
  df$source_file <- f
  # If file name includes year, extract; else leave NA
  yr <- as.integer(gsub("\\D", "", f))
  df$Year <- ifelse(is.na(yr), NA_integer_, yr)
  dfs[[f]] <- df
}
df_all <- bind_rows(dfs)
message("Combined rows: ", nrow(df_all))

# Save combined raw for appendix
write_csv(df_all, "world_happiness_combined_raw_2015_2022.csv")
message("Saved: world_happiness_combined_raw_2015_2022.csv")

# --- 2. Inspect column names & auto-detect likely columns ---------------------
cat("Column names (first 60):\n")
print(names(df_all)[1:min(60, length(names(df_all)))])

# Candidate lists -- adjust if your files use other names
ladder_candidates <- c("Happiness Score", "Ladder score", "Ladder.Score", "Happiness.Score", "Ladder score in Dystopia", "Ladder score")
gdp_candidates    <- c("Economy (GDP per Capita)", "Economy..GDP.per.Capita.", "GDP per capita", "GDP.per.capita", "Economy..GDP.per.Capita", "Logged GDP per capita", "Explained by: Log GDP per capita")
region_candidates <- c("Region", "region", "Regional indicator", "Region name", "Country or region")
country_candidates <- c("Country", "country", "Country name", "Country or region")

find_col <- function(poss, names_vec){ for(n in poss) if(n %in% names_vec) return(n); NA_character_ }

df_names <- names(df_all)
ladder_col <- find_col(ladder_candidates, df_names)
gdp_col <- find_col(gdp_candidates, df_names)
region_col <- find_col(region_candidates, df_names)
country_col <- find_col(country_candidates, df_names)

if(is.na(ladder_col) || is.na(gdp_col)){
  cat("\n== ERROR: Could not auto-detect Ladder/GDP columns ==\n")
  cat("Please inspect the column names above and edit the script to set ladder_col and gdp_col to the exact header strings.\n")
  stop("Column detection failed. Edit script and re-run.")
}

cat("\nDetected:\n")
cat("  Ladder (DV):", ladder_col, "\n")
cat("  GDP (IV):   ", gdp_col, "\n")
if(!is.na(region_col)) cat("  Region:     ", region_col, "\n") else cat("  Region:      NOT detected (optional analyses skipped)\n")
if(!is.na(country_col)) cat("  Country:    ", country_col, "\n") else cat("  Country:     NOT detected\n")

# --- 3. Convert to numeric & clean -------------------------------------------
# Remove commas and coerce to numeric
df_all[[ladder_col]] <- as.numeric(gsub(",", "", df_all[[ladder_col]]))
df_all[[gdp_col]] <- as.numeric(gsub(",", "", df_all[[gdp_col]]))

# If Year is NA, but source_file had year values earlier, keep; otherwise leave as is
# Keep only complete pairs for main analysis
df_clean <- df_all %>% filter(!is.na(.data[[ladder_col]]) & !is.na(.data[[gdp_col]]))
cat("\nObservations used (complete pairs):", nrow(df_clean), "\n")
if(nrow(df_clean) < 10) warning("Very few observations (<10) - tests may be unreliable.")

# --- 4. Base visualisations ---------------------------------
# 4.1 Base scatter + linear fit
png("scatter_base.png", width=900, height=650)
plot(df_clean[[gdp_col]], df_clean[[ladder_col]],
     main = "Happiness Score vs GDP per Capita",
     xlab = gdp_col, ylab = ladder_col, pch = 19, cex = 0.6)
abline(lm(df_clean[[ladder_col]] ~ df_clean[[gdp_col]]), col="blue", lwd=2)
mtext(paste0("N = ", nrow(df_clean)), side=3, line=0.2, adj=0.01)
dev.off()
message("Saved: scatter_base.png")

# 4.2 Histograms
png("hist_ladder.png", width=800, height=500)
hist(df_clean[[ladder_col]], breaks = 20, main = "Histogram: Happiness Score", xlab = ladder_col)
dev.off()
message("Saved: hist_ladder.png")

png("hist_gdp.png", width=800, height=500)
hist(df_clean[[gdp_col]], breaks = 30, main = "Histogram: GDP per Capita", xlab = gdp_col)
dev.off()
message("Saved: hist_gdp.png")

# 4.3 Boxplot by region if exists
if(!is.na(region_col)){
  png("box_by_region.png", width=1000, height=520)
  boxplot(df_clean[[ladder_col]] ~ df_clean[[region_col]], varwidth = TRUE,
          main = "Happiness Score by Region", xlab = "Region", ylab = ladder_col, las = 2)
  dev.off()
  message("Saved: box_by_region.png")
} else message("Region not present - skipping boxplot by region.")

# --- 5. Skewness estimate & visual prompt ------------------------------------
skewness_est <- function(x){
  x <- x[!is.na(x)]
  m3 <- mean((x - mean(x))^3)
  s3 <- sd(x)^3
  m3 / s3
}
s1 <- skewness_est(df_clean[[ladder_col]])
s2 <- skewness_est(df_clean[[gdp_col]])
cat(sprintf("\nEstimated skewness (ladder, gdp): %.3f , %.3f\n", s1, s2))
cat("Visual checks: inspect hist_ladder.png and hist_gdp.png. If strongly skewed or outliers present, prefer Spearman.\n")

# --- 6. Correlation tests ----------------------------------------------------
# Pearson (parametric)
pearson_res <- cor.test(df_clean[[ladder_col]], df_clean[[gdp_col]], method = "pearson")
cat("\n--- Pearson correlation test ---\n"); print(pearson_res)
writeLines(c("Pearson correlation test output:", capture.output(pearson_res)), "pearson_results.txt")
message("Saved: pearson_results.txt")

# Spearman (robust; use asymptotic exact = FALSE to avoid ties warning)
spearman_res <- cor.test(df_clean[[ladder_col]], df_clean[[gdp_col]], method = "spearman", exact = FALSE)
cat("\n--- Spearman correlation test (asymptotic) ---\n"); print(spearman_res)
writeLines(c("Spearman correlation test output (asymptotic):", capture.output(spearman_res)), "spearman_results.txt")
message("Saved: spearman_results.txt")

# --- 7. Wilcoxon median-split check (lecture example) ------------------------
median_gdp <- median(df_clean[[gdp_col]], na.rm = TRUE)
df_clean$gdp_group <- ifelse(df_clean[[gdp_col]] >= median_gdp, "HighGDP", "LowGDP")
wilcox_res <- wilcox.test(df_clean[[ladder_col]] ~ df_clean$gdp_group)
cat("\n--- Wilcoxon rank-sum (median-split GDP) ---\n"); print(wilcox_res)
writeLines(c("Wilcoxon median-split GDP output:", capture.output(wilcox_res)), "wilcox_median_gdp.txt")
message("Saved: wilcox_median_gdp.txt")

# --- SAFE Chi-square block (replace your existing chi-square code with this) ---
if(!is.na(region_col)){
  # create tertile categories (Low/Mid/High)
  df_clean$happy_cat <- cut(df_clean[[ladder_col]],
                            breaks = quantile(df_clean[[ladder_col]], probs = c(0,0.33,0.66,1), na.rm = TRUE),
                            include.lowest = TRUE, labels = c("Low","Mid","High"))
  
  ct <- table(df_clean[[region_col]], df_clean$happy_cat)
  cat("\nContingency table (region x happy_cat):\n"); print(ct)
  
  # try original chi-square and inspect expected counts
  chi_ok <- FALSE
  chisq_result <- NULL
  
  # compute expected counts safely (wrap in tryCatch to avoid abort)
  chi_try <- tryCatch({
    chisq_temp <- suppressWarnings(chisq.test(ct))
    expected <- chisq_temp$expected
    min_expected <- min(expected, na.rm = TRUE)
    cat("Minimum expected cell (original):", min_expected, "\n")
    if(min_expected >= 5){
      chi_ok <<- TRUE
      chisq_result <<- chisq_temp
      writeLines(c("Chi-square test (region x happy_cat):", capture.output(chisq_temp)), "chisq_region_happycat.txt")
      message("Saved: chisq_region_happycat.txt (original categories, approximation OK)")
    } else {
      cat("Some expected cells < 5. Will attempt to combine categories (Low+Mid -> NotHigh).\n")
    }
  }, error = function(e){
    cat("chisq.test original failed: ", conditionMessage(e), "\n")
  })
  
  if(!chi_ok){
    # combine Low+Mid into NotHigh and re-test
    df_clean$happy_cat2 <- ifelse(df_clean$happy_cat == "High", "High", "NotHigh")
    ct2 <- table(df_clean[[region_col]], df_clean$happy_cat2)
    cat("\nContingency table (region x happy_cat2):\n"); print(ct2)
    
    chi_try2 <- tryCatch({
      chisq_temp2 <- suppressWarnings(chisq.test(ct2))
      expected2 <- chisq_temp2$expected
      min_expected2 <- min(expected2, na.rm = TRUE)
      cat("Minimum expected cell (combined):", min_expected2, "\n")
      if(min_expected2 >= 5){
        chi_ok <<- TRUE
        chisq_result <<- chisq_temp2
        writeLines(c("Chi-square test (region x happy_cat2):", capture.output(chisq_temp2)), "chisq_region_happycat_combined.txt")
        message("Saved: chisq_region_happycat_combined.txt (combined categories, approximation OK)")
      } else {
        cat("Combined table still has small expected counts.\n")
      }
    }, error = function(e){
      cat("chisq.test on combined table failed: ", conditionMessage(e), "\n")
    })
    
    if(!chi_ok){
      # If combined still bad, try Monte Carlo simulation for p-value (good for larger tables)
      cat("Attempting Monte Carlo chi-square (simulate.p.value = TRUE, B = 20000)...\n")
      mc_try <- tryCatch({
        chisq_mc <- suppressWarnings(chisq.test(ct2, simulate.p.value = TRUE, B = 20000))
        writeLines(c("Chi-square (Monte Carlo) for region x happy_cat2:", capture.output(chisq_mc)), "chisq_region_happycat_mc.txt")
        message("Saved: chisq_region_happycat_mc.txt (Monte Carlo p-value)")
        chisq_result <<- chisq_mc
        chi_ok <<- TRUE
      }, error = function(e){
        cat("Monte Carlo chi-square failed: ", conditionMessage(e), "\n")
      })
      
      if(!chi_ok){
        # last resort: try Fisher's exact test on combined (may be slow but exact)
        cat("Monte Carlo failed or not appropriate — trying Fisher's exact on combined table...\n")
        fisher_try <- tryCatch({
          fisher_res <- fisher.test(ct2)
          writeLines(c("Fisher exact test (region x happy_cat2):", capture.output(fisher_res)), "fisher_region_happycat.txt")
          message("Saved: fisher_region_happycat.txt (Fisher exact on combined table)")
          chisq_result <<- fisher_res
          chi_ok <<- TRUE
        }, error = function(e){
          cat("Fisher exact also failed: ", conditionMessage(e), "\n")
        })
      }
    }
  }
  
  # final message
  if(chi_ok){
    cat("Chi-square / alternative test completed and saved.\n")
  } else {
    cat("All attempted chi-square/fallback options failed. See console for errors.\n")
  }
} else {
  message("Region not present. Chi-square example skipped.")
}
# --- end safe chi-square block ---


# --- 9. Save cleaned CSV and summary ------------------------------------------------
write_csv(df_clean, "world_happiness_cleaned_2015_2022.csv")
message("Saved: world_happiness_cleaned_2015_2022.csv")

summary_lines <- c(
  paste("Observations used (complete pairs):", nrow(df_clean)),
  "",
  "Pearson correlation test:",
  capture.output(pearson_res),
  "",
  "Spearman correlation test (asymptotic):",
  capture.output(spearman_res),
  "",
  "Wilcoxon median-split result:",
  capture.output(wilcox_res)
)
# also write chi-square outputs if present (files already saved earlier)
writeLines(summary_lines, "analysis_summary_report.txt")
message("Saved: analysis_summary_report.txt")

# One-line summary for quick copy-paste to report
line_summary <- sprintf("N = %d; Pearson r = %.4f; p-value = %s",
                        nrow(df_clean),
                        as.numeric(pearson_res$estimate),
                        ifelse(pearson_res$p.value < .Machine$double.eps, "<2e-16", format.pval(pearson_res$p.value, digits = 3)))
writeLines(line_summary, "one_line_summary.txt")
message("Saved: one_line_summary.txt")
cat("\nOne-line summary:\n"); cat(line_summary, "\n")

# --- 10. ggplot scatter (safe modern usage) -----------------------------------
if(nrow(df_clean) > 2){
  p <- ggplot(df_clean, aes(x = .data[[gdp_col]], y = .data[[ladder_col]])) +
    geom_point(alpha = 0.6, size = 1.6) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "darkred") +
    labs(title = "Happiness Score vs GDP per Capita (2015-2022)",
         x = gdp_col, y = ladder_col) +
    theme_minimal(base_size = 13)
  ggsave("plot_scatter_ggplot.png", p, width = 8, height = 5, dpi = 200)
  message("Saved: plot_scatter_ggplot.png")
}

# --- FIN -----------------------------------------------------------------------
message("\nAnalysis finished. Files created (check working directory):")
print(list.files(pattern = "world_happiness|scatter|hist|pearson|spearman|wilcox|chisq|fisher|summary|one_line|plot_scatter"))
message("Commit these files")
###############################################################################
