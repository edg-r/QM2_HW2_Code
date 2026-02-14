# GPCO 454 - Quantitative Methods II - Winter 2025
# Homework 2 - Solutions


# ---------------------------
# Section 3.1 - Preliminaries
# ---------------------------

# 1. Set your working directory and load the necessary packages in your R script.
# setwd("/Users/edgar/Documents/01 Projects/GPCO 454 - QM2 - Ravanilla/HomeWork/HW2")
getwd()

# Load necessary packages
# Read Excel files.
library(readxl)
# PSEUDOCODE: load package for reading Excel sheets.
# Use data wrangling and plotting tools.
library(tidyverse)
# PSEUDOCODE: load tidyverse for data manipulation/visualization.
# Export regression tables.
library(stargazer)
# PSEUDOCODE: load package for formatted regression output.
# Combine multiple ggplots into a panel layout.
library(patchwork)
# PSEUDOCODE: load package to combine multiple plots.

# 2. Load the datasets into R.
cagdp_data <- read_excel("HW2_CAGDP2_ALL_AREAS_2001_2023.xlsx", sheet = "County Level Data")
# PSEUDOCODE: read county GDP-by-industry data from Excel sheet.
eras_data <- read_excel("HW2_The_Eras_Tour_US_Schedule.xlsx", sheet = "Host Counties")
# PSEUDOCODE: read county Eras Tour host indicator data.
controls_data <- read_excel("HW2_County_Demographics.xlsx", sheet = "Latest Data")
# PSEUDOCODE: read county demographic controls data.

# 3. Merge cagdp_data and controls_data
# Keep only rows that match in both datasets using GeoFIPS as the county key.
merged_data <- merge(cagdp_data, controls_data, by = "GeoFIPS", all.x = FALSE, all.y = FALSE)
# PSEUDOCODE: inner join GDP and controls using county id (GeoFIPS).

# 4. Drop GeoName.y and rename GeoName.x to GeoName
# Check the structure of the merged dataset to ensure the merge was successful
# Drop duplicate county-name column from the right table and keep a clean GeoName field.
merged_data <- merged_data %>%
  select(-GeoName.y) %>%
  rename(GeoName = GeoName.x)
# PSEUDOCODE: drop duplicate county-name column and keep one standardized GeoName.

# Quick structure check to verify expected variables/types after merge cleanup.
str(merged_data)
# PSEUDOCODE: display variable names/types to confirm merge output.

# 5. Filter the dataset to include only cases where Rural_Urban_Continuum_Code_2023 == 1
# Restrict analysis sample to metro counties (RUCC code 1).
filtered_data <- merged_data %>%
  filter(Rural_Urban_Continuum_Code_2023 == 1)
# PSEUDOCODE: keep only counties with RUCC code equal to 1.

# 6. Drop Rural_Urban_Continuum_Code_2023 from filtered_data
# Remove the filter variable once it is no longer needed downstream.
filtered_data <- filtered_data %>%
  select(-Rural_Urban_Continuum_Code_2023)
# PSEUDOCODE: remove RUCC variable after filtering is complete.

# 7. Merge eras_data into filtered_data. Name the resulting dataset working_data
# Left-join host-county info so non-host counties remain in the analysis data.
working_data <- merge(filtered_data, eras_data, by = "GeoFIPS", all.x = TRUE)
# PSEUDOCODE: left join host info onto filtered counties by GeoFIPS.

# 8. Check the dimension of working_data
# Check final row/column count after all merges and filters.
dim(working_data)
# PSEUDOCODE: print number of rows and columns in working_data.

# Expand dimension check with a readable column reference table.
working_data_column_reference <- tibble(
  column_index = seq_along(working_data),
  column_name = names(working_data),
  column_class = map_chr(working_data, ~ paste(class(.x), collapse = ", "))
)
# PSEUDOCODE: build a table listing each column's position, name, and data type.
print(working_data_column_reference, n = nrow(working_data_column_reference))
# PSEUDOCODE: print all columns so the dimension output is easy to interpret.

# 9. Count the unique number of observations in the GeoName variable
# Check how many unique counties remain in the working sample.
length(unique(working_data$GeoName))
# PSEUDOCODE: count distinct county names in analysis sample.

# Save a snapshot of the Section 3.1 dataset before outcome transformations.
working_data_prelim <- working_data
# PSEUDOCODE: preserve the pre-cleaning/pre-transform data state for answering prelim questions.


# ------------------------------------------------
# Section 3.2 - Data Cleaning: Dependent Variables
# ------------------------------------------------

# 1. Identify the columns corresponding to years 2001 to 2023
# Create a character vector so these year columns can be selected programmatically.
year_columns <- as.character(2001:2023)
# PSEUDOCODE: create list of year column names from 2001 to 2023.

# 2. Replace "(D)" with NA and convert the columns to numeric
# Replace BEA suppression marker "(D)" with NA before numeric conversion.
working_data <- working_data %>%
  mutate(across(all_of(year_columns), ~ ifelse(.x == "(D)", NA, .x)))
# PSEUDOCODE: in each year column, replace "(D)" suppression string with missing value.

# 3. Convert these variables to numeric data type after replacing (D) with NA.
# Convert year columns from character to numeric for arithmetic and regressions.
working_data <- working_data %>%
  mutate(across(all_of(year_columns), as.numeric))
# PSEUDOCODE: convert all year columns to numeric values.

# --------------------------------------------------------------------------------
# Section 3.3 - Variable Creation: Hosting an Eras Tour Concert (Binary Indicator)
# --------------------------------------------------------------------------------

# 1. Add a binary indicator for hosting an Eras Tour concert in 2023
# Create treatment indicator: 1 if county hosted the Eras Tour, 0 otherwise.
working_data <- working_data %>%
  mutate(eras_tour_host = ifelse(!is.na(Hosted) & Hosted == "Yes", 1, 0))
# PSEUDOCODE: create binary host indicator (1 if Hosted is "Yes", else 0).


# --------------------------------------------------
# Section 3.4.4 - Transforming the Outcome Variables
# --------------------------------------------------

# UCSD GPS brand palette.
gps_navy <- "#182B49"
gps_gold <- "#C69214"
gps_gold_50 <- "#E3C98A"
gps_blue_50 <- "#80B1CD"
gps_sand <- "#F5F0E6"
# PSEUDOCODE: define the approved color constants once and reuse across all figures.

# Shared GPS styling for consistent chart formatting.
gps_base_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = gps_sand, color = NA),
    panel.background = element_rect(fill = gps_sand, color = NA),
    panel.grid.major = element_line(color = scales::alpha(gps_navy, 0.18), linewidth = 0.35),
    panel.grid.minor = element_line(color = scales::alpha(gps_navy, 0.08), linewidth = 0.2),
    axis.title = element_text(color = gps_navy, face = "bold"),
    axis.text = element_text(color = gps_navy),
    plot.title = element_text(color = gps_navy, face = "bold"),
    legend.title = element_text(color = gps_navy, face = "bold"),
    legend.text = element_text(color = gps_navy),
    legend.background = element_rect(fill = gps_sand, color = NA),
    legend.key = element_rect(fill = gps_sand, color = NA)
  )
# PSEUDOCODE: create one reusable plot theme with GPS backgrounds, text colors, and grid styling.

# Figure footnotes (embedded in exported images via plot captions).
figure_footnote_1 <- str_wrap(
  "Figure 1 shows the distribution of 2023 NAICS 71 GDP per capita across metropolitan counties. The histogram indicates most counties are concentrated at lower-to-moderate values, with a smaller right tail at higher GDP per-capita levels.",
  width = 120
)
figure_footnote_2 <- str_wrap(
  "Figure 2 shows the distribution of log(1 + 2023 NAICS 71 GDP per capita). The log transform compresses the right tail, yielding a less skewed distribution that is more suitable for linear regression analysis.",
  width = 120
)
figure_footnote_3 <- str_wrap(
  "Figure 3 shows average log GDP per capita trends from 2001-2023 for Eras Tour host and non-host counties in NAICS 71. Host counties remain above non-host counties over time, and both groups show upward long-run trends.",
  width = 140
)
figure_footnote_4 <- str_wrap(
  "Figure 4 compares demographic and socioeconomic averages between Eras Tour host and non-host counties, with treated-group confidence intervals shown for each metric.",
  width = 140
)

# Shared caption styling so all footnotes are italic and visible in saved PNGs.
footnote_caption_theme <- theme(
  plot.caption = element_text(face = "italic", color = gps_navy, hjust = 0, size = 9, margin = margin(t = 10)),
  plot.margin = margin(t = 5.5, r = 5.5, b = 16, l = 5.5)
)
# PSEUDOCODE: define one reusable italic caption style for all exported figures.

# 1. Transform GDP variables to per capita values
# GDP values are in thousands of USD; divide by population to get per capita

# Convert each industry's GDP values to per-capita terms for comparability across counties.
working_data <- working_data %>%
  mutate(across(all_of(year_columns), ~ round(.x / POP_ESTIMATE_2023, 2)))
# PSEUDOCODE: divide each GDP year value by 2023 population to get per-capita GDP.

# 2. Filter the dataset to include only rows for the Arts, Entertainment, and Recreation industry (NAICS 71)

# Keep only NAICS 71 rows for industry-specific descriptive plots.
naics_71_data <- working_data %>%
  filter(IndustryClassification == "71")
# PSEUDOCODE: subset data to NAICS 71 rows only.

# 3. Generate a histogram of the transformed variable, 2023 county-level GDP per capita in this industry, and save it as an image file

# Build histogram to inspect the 2023 per-capita GDP distribution for NAICS 71.
histogram_pc <- ggplot(naics_71_data, aes(x = `2023`)) +
  geom_histogram(binwidth = 0.5, fill = gps_blue_50, color = gps_navy, linewidth = 0.35) +
  labs(title = "Distribution of 2023 GDP Per Capita (NAICS 71)",
       x = "GDP Per Capita (Thousands USD)",
       y = "Number of Counties",
       caption = figure_footnote_1) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 12)) +
  gps_base_theme +
  footnote_caption_theme
# PSEUDOCODE: create histogram object of 2023 NAICS 71 per-capita GDP.

# Display the plot in the current R session for visual QA.
print(histogram_pc)
# PSEUDOCODE: display per-capita GDP histogram.

# Export the figure for homework submission artifacts.
ggsave("naics_71_gdp_per_capita_histogram.png", plot = histogram_pc, width = 8, height = 5, dpi = 300, bg = gps_sand)
# PSEUDOCODE: save histogram image file to disk.

# 4. Transform GDP variables to log of (1 + thousand per capita) values

# Apply log(1 + x) transform to compress right tail and handle zeros.
working_data <- working_data %>%
  mutate(across(all_of(year_columns), ~ log(1 + .x)))
# PSEUDOCODE: transform each year value to log(1 + value).

# 5. Generate a histogram of the transformed variable, 2023 county-level GDP per capita in this industry, and save it as an image file

# Rebuild NAICS 71 subset after transformation so plotted values are logged.
naics_71_log_data <- working_data %>%
  filter(IndustryClassification == "71")
# PSEUDOCODE: subset transformed data to NAICS 71 rows.

# Build histogram to inspect shape after log transformation.
histogram_log <- ggplot(naics_71_log_data, aes(x = `2023`)) +
  geom_histogram(binwidth = 0.5, fill = gps_gold_50, color = gps_navy, linewidth = 0.35) +
  labs(title = "Distribution of Log(1 + GDP Per Capita) in 2023 (NAICS 71)",
       x = "Log(1 + GDP Per Capita in Thousands USD)",
       y = "Number of Counties",
       caption = figure_footnote_2) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 12)) +
  gps_base_theme +
  footnote_caption_theme
# PSEUDOCODE: create histogram object of logged 2023 NAICS 71 per-capita GDP.

# Display transformed histogram for quick visual verification.
print(histogram_log)
# PSEUDOCODE: display logged GDP histogram.

# Save the transformed histogram as a submission figure.
ggsave("naics_71_log_gdp_per_capita_histogram.png", plot = histogram_log, width = 8, height = 5, dpi = 300, bg = gps_sand)
# PSEUDOCODE: save logged histogram image file. 

# ------------------------------------------------------------------------------------------------------------------------
# Section 3.5 - Analyzing Time Trends in Arts, Entertainment, and Recreation Industry (NAICS 71) Annual Log GDP per Capita
# ------------------------------------------------------------------------------------------------------------------------

# 1. Filter the data to focus on the Arts, Entertainment, and Recreation industry (NAICS 71)

# Keep NAICS 71 only for time-trend comparisons.
naics_71_trends <- working_data %>%
  filter(IndustryClassification == "71")
# PSEUDOCODE: keep NAICS 71 data for trend analysis.

# 2. Transform the data to long format and calculate average log GDP per capita grouped by eras_tour_host and year

# Reshape wide year columns to long format, then compute group-year means by treatment status.
trends_long <- naics_71_trends %>%
  pivot_longer(cols = all_of(year_columns), names_to = "Year", values_to = "log_gdp_pc") %>%
  group_by(eras_tour_host, Year) %>%
  summarize(avg_log_gdp_pc = mean(log_gdp_pc, na.rm = TRUE), .groups = "drop")
# PSEUDOCODE: reshape years to long format and compute mean log GDP by host status and year.

# 3. Convert Year to a numeric variable for proper x-axis ordering

# Convert year labels to numeric so ggplot can order and scale the x-axis correctly.
trends_long <- trends_long %>%
  mutate(Year = as.numeric(Year))
# PSEUDOCODE: convert year labels from text to numeric.

# 4. Create the plot

# Draw average trend lines for host vs non-host counties.
trends_plot <- ggplot(trends_long, aes(x = Year, y = avg_log_gdp_pc,
                                        color = as.factor(eras_tour_host),
                                        group = as.factor(eras_tour_host))) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  labs(title = "Average Log GDP Per Capita (NAICS 71): Eras Tour Host vs Non-Host Counties",
       x = "Year",
       y = "Average Log GDP Per Capita",
       color = "Eras Tour Host",
       caption = figure_footnote_3) +
  scale_color_manual(
    values = c("0" = gps_navy, "1" = gps_gold),
    labels = c("0" = "Non-Host", "1" = "Host")
  ) +
  scale_x_continuous(breaks = 2001:2023, labels = 2001:2023) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 12)) +
  gps_base_theme +
  footnote_caption_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# PSEUDOCODE: build line plot comparing average log GDP trends for host vs non-host counties.

# Render the trend figure in the active plotting device.
print(trends_plot)
# PSEUDOCODE: display trend comparison plot.

# 5. Save the plot as an image file

# Save the trend figure for reporting.
ggsave("naics_71_log_gdp_plot.png", plot = trends_plot, width = 12, height = 6, dpi = 300, bg = gps_sand)
# PSEUDOCODE: save trend comparison plot image.


# ----------------------------------------------------------------------------------------------------------------------------
# Section 3.6 - Visualizing the Data: Demographic Differences Across Counties that Hosted the Eras Tour and Those That Did Not
# ----------------------------------------------------------------------------------------------------------------------------

# 1. Install and Load Necessary Packages (Can be done at top of script)
# Already loaded: tidyverse, patchwork

# 2. Prepare population characteristics for visualization

# Convert raw counts into comparable percentage shares by county population.
regression_data <- working_data %>%
  mutate(
    Less_than_HS_Share = (`Less than a high school diploma, 2018-22` / POP_ESTIMATE_2023) * 100,
    HS_Only_Share = (`High school diploma only, 2018-22` / POP_ESTIMATE_2023) * 100,
    Some_College_Share = (`Some college or associate's degree, 2018-22` / POP_ESTIMATE_2023) * 100,
    Bachelors_or_Higher_Share = (`Bachelor's degree or higher, 2018-22` / POP_ESTIMATE_2023) * 100
  )
# PSEUDOCODE: compute county education shares as percentages of population.

# 3. Summarize Data by Treatment Group

# Compute treatment/control means and treated-group 95% CI values for each metric.
summary_with_ci <- regression_data %>%
  group_by(eras_tour_host) %>%
  summarize(
    Less_than_HS = mean(Less_than_HS_Share, na.rm = TRUE),
    Less_than_HS_CI = if (first(eras_tour_host) == 1) 1.96 * sd(Less_than_HS_Share, na.rm = TRUE) / sqrt(n()) else NA_real_,

    HS_Only = mean(HS_Only_Share, na.rm = TRUE),
    HS_Only_CI = if (first(eras_tour_host) == 1) 1.96 * sd(HS_Only_Share, na.rm = TRUE) / sqrt(n()) else NA_real_,

    Some_College = mean(Some_College_Share, na.rm = TRUE),
    Some_College_CI = if (first(eras_tour_host) == 1) 1.96 * sd(Some_College_Share, na.rm = TRUE) / sqrt(n()) else NA_real_,

    Bachelors_Higher = mean(Bachelors_or_Higher_Share, na.rm = TRUE),
    Bachelors_Higher_CI = if (first(eras_tour_host) == 1) 1.96 * sd(Bachelors_or_Higher_Share, na.rm = TRUE) / sqrt(n()) else NA_real_,

    Poverty = mean(PCTPOVALL_2021, na.rm = TRUE),
    Poverty_CI = if (first(eras_tour_host) == 1) 1.96 * sd(PCTPOVALL_2021, na.rm = TRUE) / sqrt(n()) else NA_real_,

    Net_Migration = mean(NET_MIG_2023, na.rm = TRUE),
    Net_Migration_CI = if (first(eras_tour_host) == 1) 1.96 * sd(NET_MIG_2023, na.rm = TRUE) / sqrt(n()) else NA_real_,
    .groups = "drop"
  )
# PSEUDOCODE: summarize means by host group and compute treated-group 95% confidence intervals.

# 4. Generate the save the bar plots in a panel
# Helper to standardize bar-chart creation across demographic variables.
plot_bar_with_ci <- function(variable, ci_variable, title, y_label) {
  # Keep rows where the plotted summary statistic exists.
  plot_data <- summary_with_ci %>%
    filter(!is.na(.data[[variable]]))
  # PSEUDOCODE: keep rows where selected summary variable is present.

  # Draw bars for means and CI whiskers for the treated group.
  ggplot(plot_data, aes(x = as.factor(eras_tour_host), y = .data[[variable]], fill = as.factor(eras_tour_host))) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7, color = gps_navy, linewidth = 0.35) +
    geom_errorbar(
      aes(ymin = .data[[variable]] - .data[[ci_variable]], ymax = .data[[variable]] + .data[[ci_variable]]),
      width = 0.2,
      data = filter(plot_data, eras_tour_host == 1),
      color = gps_navy,
      linewidth = 0.7
    ) +
    labs(title = title, x = "Eras Tour Host", y = y_label, fill = "Eras Tour Host") +
    scale_fill_manual(
      values = c("0" = gps_blue_50, "1" = gps_gold_50),
      labels = c("0" = "Non-Host", "1" = "Host")
    ) +
    scale_x_discrete(labels = c("0" = "Non-Host", "1" = "Host")) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 12)) +
    gps_base_theme +
    theme(plot.title = element_text(size = 10), axis.text.x = element_text(angle = 20, hjust = 1))
  # PSEUDOCODE: return bar chart with treated-group CI error bar and labels/theme.
}

# Build each demographic comparison plot using the same helper function.
plot1 <- plot_bar_with_ci("Less_than_HS", "Less_than_HS_CI", "Less than High School Diploma", "Percentage (%)")
# PSEUDOCODE: create plot for less-than-high-school share.
plot2 <- plot_bar_with_ci("HS_Only", "HS_Only_CI", "High School Diploma Only", "Percentage (%)")
# PSEUDOCODE: create plot for high-school-only share.
plot3 <- plot_bar_with_ci("Some_College", "Some_College_CI", "Some College or Associate Degree", "Percentage (%)")
# PSEUDOCODE: create plot for some-college share.
plot4 <- plot_bar_with_ci("Bachelors_Higher", "Bachelors_Higher_CI", "Bachelor's Degree or Higher", "Percentage (%)")
# PSEUDOCODE: create plot for bachelor's-or-higher share.
plot5 <- plot_bar_with_ci("Poverty", "Poverty_CI", "Poverty Rate (2021)", "Percentage (%)")
# PSEUDOCODE: create plot for poverty rate.
plot6 <- plot_bar_with_ci("Net_Migration", "Net_Migration_CI", "Net Migration (2023)", "Net Migration Rate")
# PSEUDOCODE: create plot for net migration.

# Arrange all plots in a 3x2 panel and display
# Combine six charts into a 3x2 layout for side-by-side interpretation.
final_panel <- ((plot1 | plot2) / (plot3 | plot4) / (plot5 | plot6)) +
  plot_annotation(
    caption = figure_footnote_4,
    theme = theme(
      plot.caption = element_text(face = "italic", color = gps_navy, hjust = 0, size = 9, margin = margin(t = 10)),
      plot.background = element_rect(fill = gps_sand, color = NA)
    )
  )
# PSEUDOCODE: arrange six plots into a 3-rows-by-2-columns panel.

# Show the combined panel in the plotting device.
print(final_panel)
# PSEUDOCODE: display combined demographic panel.

# Export the demographic panel figure.
ggsave("demographics_comparison_panel.png", plot = final_panel, width = 12, height = 10, dpi = 300, bg = gps_sand)
# PSEUDOCODE: save demographic panel image.

# ------------------------------------------
# Section 3.7 - Multiple Regression Analyses
# ------------------------------------------

# Define per-industry metadata for looped regression execution.
industry_specs <- tribble(
  ~industry_code, ~table_title, ~table_out,
  "71", "Regression Results: NAICS 71 - Arts, Entertainment, and Recreation", "regression_table_naics_71.html",
  "72", "Regression Results: NAICS 72 - Accommodation and Food Services", "regression_table_naics_72.html",
  "54", "Regression Results: NAICS 54 - Professional, Scientific, and Technical Services", "regression_table_naics_54.html",
  "11", "Regression Results: NAICS 11 - Agriculture, Forestry, Fishing, and Hunting", "regression_table_naics_11.html"
)
# PSEUDOCODE: store industry code and output metadata in a lookup table for looping.

# Helper: build one industry's regression dataset with standardized controls.
prepare_industry_regression_data <- function(data, industry_code) {
  data %>%
    filter(IndustryClassification == industry_code) %>%
    mutate(
      Less_than_HS_Share = (`Less than a high school diploma, 2018-22` / POP_ESTIMATE_2023) * 100,
      HS_Only_Share = (`High school diploma only, 2018-22` / POP_ESTIMATE_2023) * 100,
      Some_College_Share = (`Some college or associate's degree, 2018-22` / POP_ESTIMATE_2023) * 100,
      Bachelors_or_Higher_Share = (`Bachelor's degree or higher, 2018-22` / POP_ESTIMATE_2023) * 100
    )
}
# PSEUDOCODE: filter to one NAICS industry and compute education-share controls.

# Helper: fit the five assignment model specifications (unchanged formulas).
fit_industry_models <- function(reg_data) {
  list(
    model1 = lm(`2023` ~ eras_tour_host, data = reg_data),
    model2 = lm(`2023` ~ eras_tour_host + Less_than_HS_Share + HS_Only_Share + Some_College_Share + Bachelors_or_Higher_Share, data = reg_data),
    model3 = lm(`2023` ~ eras_tour_host + Less_than_HS_Share + HS_Only_Share + Some_College_Share + Bachelors_or_Higher_Share + PCTPOVALL_2021, data = reg_data),
    model4 = lm(`2023` ~ eras_tour_host + Less_than_HS_Share + HS_Only_Share + Some_College_Share + Bachelors_or_Higher_Share + PCTPOVALL_2021 + NET_MIG_2023, data = reg_data),
    model5 = lm(`2023` ~ eras_tour_host + Less_than_HS_Share + HS_Only_Share + Some_College_Share + Bachelors_or_Higher_Share + PCTPOVALL_2021 + NET_MIG_2023 + `2022`, data = reg_data)
  )
}
# PSEUDOCODE: fit the same five nested OLS models used previously.

# Execute regressions by looping through industries and export one stargazer table per industry.
industry_regression_results <- list()
# PSEUDOCODE: create a container to store each industry's data and fitted models.

for (i in seq_len(nrow(industry_specs))) {
  spec <- industry_specs[i, ]
  industry_code <- spec$industry_code[[1]]
  table_title <- spec$table_title[[1]]
  table_out <- spec$table_out[[1]]

  reg_data <- prepare_industry_regression_data(working_data, industry_code)
  models <- fit_industry_models(reg_data)
  industry_regression_results[[industry_code]] <- list(data = reg_data, models = models)

  m1 <- models$model1
  m2 <- models$model2
  m3 <- models$model3
  m4 <- models$model4
  m5 <- models$model5
  # PSEUDOCODE: assign models to plain object names so stargazer parses them reliably.

  stargazer(
    m1, m2, m3, m4, m5,
    type = "text",
    title = table_title,
    dep.var.labels = "Log GDP Per Capita (2023)",
    covariate.labels = c("Eras Tour Host", "Less than HS Share", "HS Only Share",
                         "Some College Share", "Bachelors or Higher Share",
                         "Poverty Rate (2021)", "Net Migration (2023)", "Log GDP Per Capita (2022)"),
    digits = 4,
    out = table_out
  )
}
# PSEUDOCODE: loop across NAICS codes, fit models, store results, and export formatted regression tables.

# Helpers to access loop-generated models for support tables.
get_industry_model <- function(industry_code, model_number) {
  model_key <- paste0("model", model_number)
  industry_regression_results[[industry_code]]$models[[model_key]]
}
# PSEUDOCODE: return one fitted model from loop results using industry code and model number.


# ------------------------------------------------------
# Section 3.8 - Submission Template Answer Support Code
# ------------------------------------------------------

# Helper to print support outputs with explicit table labels.
print_support_table <- function(table_label, table_data) {
  cat("\n", table_label, "\n", strrep("-", nchar(table_label)), "\n", sep = "")
  print(table_data)
}
# PSEUDOCODE: define a small printer so each output has a readable table title.

# Q1 and Q2 support: observations, variables, and unique counties represented.
q1_q2_support <- tibble(
  n_observations = nrow(working_data_prelim),
  n_variables = ncol(working_data_prelim),
  n_unique_counties = n_distinct(working_data_prelim$GeoFIPS)
)
# PSEUDOCODE: create a one-row summary with dataset size and number of distinct counties.
print_support_table("Table Q1-Q2. Dataset Dimensions and County Coverage", q1_q2_support)
# PSEUDOCODE: display Q1/Q2 support values for direct use in the template.

# Q3 support: show why total rows exceed number of counties.
q3_support <- working_data_prelim %>%
  summarize(
    total_rows = n(),
    unique_counties = n_distinct(GeoFIPS),
    unique_industries = n_distinct(IndustryClassification),
    avg_rows_per_county = n() / n_distinct(GeoFIPS)
  )
# PSEUDOCODE: summarize rows, counties, industries, and average rows per county.
print_support_table("Table Q3. Rows per County and Industry Structure", q3_support)
# PSEUDOCODE: display evidence that multiple industry rows exist per county.

# Q4 support: show example rows where 2001-2023 columns are industry GDP values.
q4_support <- working_data_prelim %>%
  select(GeoFIPS, GeoName, IndustryClassification, all_of(year_columns)) %>%
  slice_head(n = 3)
# PSEUDOCODE: select identifying columns plus year GDP columns and show a small sample.
print_support_table("Table Q4. Example Rows with Year GDP Columns", q4_support)
# PSEUDOCODE: display sample structure to support interpretation of year variables.

# Q5 support: compare distribution shape before and after log transformation.
q5_support <- tibble(
  distribution = c("PerCapita_2023", "LogPerCapita_2023"),
  mean_value = c(mean(naics_71_data$`2023`, na.rm = TRUE), mean(naics_71_log_data$`2023`, na.rm = TRUE)),
  median_value = c(median(naics_71_data$`2023`, na.rm = TRUE), median(naics_71_log_data$`2023`, na.rm = TRUE)),
  sd_value = c(sd(naics_71_data$`2023`, na.rm = TRUE), sd(naics_71_log_data$`2023`, na.rm = TRUE)),
  p90_value = c(as.numeric(quantile(naics_71_data$`2023`, 0.90, na.rm = TRUE)), as.numeric(quantile(naics_71_log_data$`2023`, 0.90, na.rm = TRUE))),
  p99_value = c(as.numeric(quantile(naics_71_data$`2023`, 0.99, na.rm = TRUE)), as.numeric(quantile(naics_71_log_data$`2023`, 0.99, na.rm = TRUE)))
) %>%
  mutate(p99_to_median_ratio = p99_value / median_value)
# PSEUDOCODE: summarize pre/post-log distribution moments and tail heaviness for Q5 discussion.
print_support_table("Table Q5. Distribution Diagnostics Before and After Log Transform", q5_support)
# PSEUDOCODE: print distribution diagnostics to justify the log transformation.

# Q6 support: estimate and compare linear slopes of average log GDP trends by host status.
q6_slope_support <- trends_long %>%
  group_by(eras_tour_host) %>%
  summarize(
    slope_per_year = coef(lm(avg_log_gdp_pc ~ Year))[["Year"]],
    intercept = coef(lm(avg_log_gdp_pc ~ Year))[["(Intercept)"]],
    .groups = "drop"
  )
# PSEUDOCODE: fit simple trend lines by group and extract slope/intercept.
print_support_table("Table Q6. Average Log GDP Trend Slopes by Host Status", q6_slope_support)
# PSEUDOCODE: print slope comparison for host vs non-host groups.

# Q8 support: compare group means and host-minus-nonhost differences.
q8_support <- regression_data %>%
  group_by(eras_tour_host) %>%
  summarize(
    Less_than_HS_Share = mean(Less_than_HS_Share, na.rm = TRUE),
    HS_Only_Share = mean(HS_Only_Share, na.rm = TRUE),
    Some_College_Share = mean(Some_College_Share, na.rm = TRUE),
    Bachelors_or_Higher_Share = mean(Bachelors_or_Higher_Share, na.rm = TRUE),
    Poverty = mean(PCTPOVALL_2021, na.rm = TRUE),
    Net_Migration = mean(NET_MIG_2023, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -eras_tour_host, names_to = "metric", values_to = "mean_value") %>%
  pivot_wider(names_from = eras_tour_host, values_from = mean_value, names_prefix = "host_") %>%
  mutate(host_minus_nonhost = host_1 - host_0)
# PSEUDOCODE: compute means by group, reshape, and calculate host-minus-nonhost gaps.
print_support_table("Table Q8. Host vs Non-Host Covariate Means", q8_support)
# PSEUDOCODE: print demographic/socioeconomic differences used in Q8/Q9 discussion.

# Helper to extract Eras Tour Host coefficient details from each model.
extract_host_effect <- function(model, industry, model_name) {
  coef_table <- summary(model)$coefficients
  # PSEUDOCODE: pull the coefficient matrix from the fitted model summary.

  beta <- unname(coef_table["eras_tour_host", "Estimate"])
  # PSEUDOCODE: read the point estimate for the host indicator.
  se <- unname(coef_table["eras_tour_host", "Std. Error"])
  # PSEUDOCODE: read the standard error for the host indicator.
  p_value <- unname(coef_table["eras_tour_host", "Pr(>|t|)"])
  # PSEUDOCODE: read the p-value for statistical significance.

  tibble(
    industry = industry,
    model = model_name,
    estimate = beta,
    std_error = se,
    p_value = p_value,
    ci_low_95 = beta - 1.96 * se,
    ci_high_95 = beta + 1.96 * se,
    implied_pct_change = (exp(beta) - 1) * 100
  )
  # PSEUDOCODE: return a tidy one-row summary including CI and implied percent effect.
}

# Q10 support: Model 5 host effect in NAICS 71.
q10_support <- extract_host_effect(get_industry_model("71", 5), "NAICS 71", "Model 5")
# PSEUDOCODE: extract Model 5 host effect details for NAICS 71.
print_support_table("Table Q10. NAICS 71 Model 5 Host Effect", q10_support)
# PSEUDOCODE: print estimate and significance needed for Q10.

# Q12 support: host coefficient movement across NAICS 71 models.
q12_support <- bind_rows(
  lapply(1:5, function(m) {
    extract_host_effect(get_industry_model("71", m), "NAICS 71", paste("Model", m))
  })
)
# PSEUDOCODE: stack NAICS 71 model summaries to track coefficient changes with added controls.
print_support_table("Table Q12. NAICS 71 Host Coefficient Across Models 1-5", q12_support)
# PSEUDOCODE: display coefficient path used to discuss omitted variable bias in Q12.

# Q13 support: compare Model 5 host effects across industries.
q13_specs <- tribble(
  ~industry_code, ~industry_label,
  "71", "NAICS 71",
  "72", "NAICS 72",
  "54", "NAICS 54",
  "11", "NAICS 11"
)
# PSEUDOCODE: define industries requested in Q13 and their display labels.

q13_support <- q13_specs %>%
  mutate(
    model_summary = map2(
      industry_code, industry_label,
      ~ extract_host_effect(get_industry_model(.x, 5), .y, "Model 5")
    )
  ) %>%
  pull(model_summary) %>%
  bind_rows()
# PSEUDOCODE: combine Model 5 host effects across all requested industries.
print_support_table("Table Q13. Model 5 Host Effects Across Industries", q13_support)
# PSEUDOCODE: print cross-industry evidence for Q13.

# Q14 support: implied GDP impact calculation using provided hint values.
beta_71_model5 <- q10_support$estimate[1]
# PSEUDOCODE:
# 1) Read the Eras Tour host coefficient from NAICS 71 Model 5.
# 2) Treat this as a log-point treatment effect on GDP per capita (because the dependent variable is log GDP per capita).

q14_support <- tibble(
  beta_eras_tour_host = beta_71_model5,
  avg_naics71_gdp_host_thousand_usd = 4100000,
  avg_county_population = 2300000
) %>%
  mutate(
    # 3) Convert log points to proportional change: proportional_increase = exp(beta) - 1.
    proportional_increase = exp(beta_eras_tour_host) - 1,
    # 4) Convert proportional change to percentage points for reporting.
    percentage_increase = proportional_increase * 100,
    # 5) Apply the proportional change to average NAICS 71 GDP (in thousand USD) to get total GDP increase.
    implied_total_gdp_increase_thousand_usd = proportional_increase * avg_naics71_gdp_host_thousand_usd,
    # 6) Convert from thousand USD to USD.
    implied_total_gdp_increase_usd = implied_total_gdp_increase_thousand_usd * 1000,
    # 7) Divide implied total increase by average county population to get per-capita increase in USD.
    implied_per_capita_increase_usd = implied_total_gdp_increase_usd / avg_county_population
  )
# PSEUDOCODE: return one-row table with model effect, assumed baseline levels, and implied total/per-capita dollar impacts.
print_support_table("Table Q14. Implied GDP Impact for Eras Tour Host Counties", q14_support)
# PSEUDOCODE: print final calculation table for Q14/Q15 write-up.

q14_interpretation_support <- q14_support %>%
  transmute(
    model = "Model 5 (NAICS 71)",
    beta_eras_tour_host = round(beta_eras_tour_host, 3),
    implied_percent_higher_gdp_per_capita = round(percentage_increase, 2),
    baseline_gdp_host_counties_thousand_usd = avg_naics71_gdp_host_thousand_usd,
    implied_total_increase_thousand_usd = round(implied_total_gdp_increase_thousand_usd, 0),
    implied_total_increase_million_usd = round(implied_total_gdp_increase_usd / 1e6, 1),
    average_population = avg_county_population,
    implied_per_capita_increase_usd = round(implied_per_capita_increase_usd, 2)
  )
# PSEUDOCODE:
# 1) Keep only interpretation-ready fields.
# 2) Round outputs to match write-up language (about 1.61%, 66,128 thousand USD / 66.1 million USD, and 28.75 USD per capita).
print_support_table("Table Q14 Interpretation. Code Form of the Written Findings", q14_interpretation_support)
# PSEUDOCODE: print concise Q14 interpretation table ready to paste into A14.

q14_per_capita_support <- q14_support %>%
  transmute(
    metric = "Implied per-capita GDP increase (USD)",
    value_usd = round(implied_per_capita_increase_usd, 2)
  )
# PSEUDOCODE: create a one-metric table so the per-capita estimate is explicit and easy to find.
print_support_table("Table Q14 Per-Capita Impact. Highlighted Result", q14_per_capita_support)
# PSEUDOCODE: print the standalone per-capita estimate table (about 28.75 USD).

q14_simple_summary <- q14_support %>%
  transmute(
    percent_increase = round(percentage_increase, 2),
    total_gdp_increase_usd = round(implied_total_gdp_increase_usd, 0),
    total_gdp_increase_million_usd = round(implied_total_gdp_increase_usd / 1e6, 1),
    per_capita_gdp_increase_usd = round(implied_per_capita_increase_usd, 2)
  )
# PSEUDOCODE: build one compact table with percent, total GDP increase, and per-capita increase.
print_support_table("Table Q14 Simple Summary. Percent, Total GDP, and Per-Capita Impact", q14_simple_summary)
# PSEUDOCODE: print a single table containing the key Q14 values together.

cat(
  "\nQ14 key values (cat output)\n",
  "Percent increase: ", sprintf("%.2f%%", q14_simple_summary$percent_increase[1]), "\n",
  "Total GDP increase (USD): $", format(q14_simple_summary$total_gdp_increase_usd[1], big.mark = ",", scientific = FALSE), "\n",
  "Total GDP increase (million USD): $", sprintf("%.1fM", q14_simple_summary$total_gdp_increase_million_usd[1]), "\n",
  "Per-capita GDP increase (USD): $", sprintf("%.2f", q14_simple_summary$per_capita_gdp_increase_usd[1]), "\n",
  sep = ""
)
# PSEUDOCODE: cat the same Q14 key values in plain text for quick copy/paste into the write-up.
