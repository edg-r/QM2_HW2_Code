# HW2 Workflow README

This README documents how `/Users/edgar/Documents/01 Projects/GPCO 454 - QM2 - Ravanilla/HomeWork/HW2/HW2_ScriptTemplate.R` runs, in execution order, with pseudocode and notes on the specific merge, cleaning, and regression patterns used.

## 1. What the Script Produces

- Cleaned and merged analysis dataset in memory (`working_data`)
- Four branded figure files (`.png`) with embedded italic footnotes
- Four regression output tables (`regression_table_naics_71.html`, `72`, `54`, `11`)
- Question-support tables (Q1-Q14) printed to console

## 2. Input Files

- `HW2_CAGDP2_ALL_AREAS_2001_2023.xlsx` (county GDP by industry/year)
- `HW2_County_Demographics.xlsx` (demographic and socioeconomic controls)
- `HW2_The_Eras_Tour_US_Schedule.xlsx` (host-county indicator)

## 3. High-Level Execution Flow (Pseudocode)

```text
START

CHECK working directory and required files exist
LOAD libraries: readxl, tidyverse, stargazer, patchwork

READ Excel sheets into:
  cagdp_data
  controls_data
  eras_data

MERGE (left join behavior) GDP + controls on GeoFIPS
  keep all GDP rows and append controls where available
  remove duplicate GeoName column

FILTER to metro counties (Rural_Urban_Continuum_Code_2023 == 1)
DROP RUCC column after filtering

MERGE (left join behavior) host schedule into filtered county data
  keep all county rows
  add host info if available

CREATE year column vector: 2001:2023
CLEAN year columns:
  replace "(D)" with NA
  convert to numeric

CREATE treatment variable:
  eras_tour_host = 1 if Hosted == "Yes" else 0

DEFINE GPS palette and reusable theme objects
DEFINE figure footnote text and italic caption style

TRANSFORM outcome to per-capita:
  year_value = year_value / POP_ESTIMATE_2023

PLOT Figure 1 histogram (2023 NAICS 71 per-capita GDP), save PNG

TRANSFORM outcome to log scale:
  year_value = log(1 + year_value)

PLOT Figure 2 histogram (logged 2023 NAICS 71), save PNG

RESHAPE to long format and aggregate mean by host x year
PLOT Figure 3 trend lines, save PNG

BUILD demographic summary-by-host with treated-group CIs
PLOT 6 bar charts in one panel (Figure 4), save PNG

FOR each industry in {71, 72, 54, 11}:
  filter industry
  construct education-share controls
  fit Model 1..5 with progressively richer controls
  export stargazer table

BUILD support tables for Q1-Q14
  extract host effects
  compare models/industries
  compute Q14 implied GDP impact and per-capita effect

END
```

## 4. Merge Strategy Used

### Merge A: GDP + Demographics

```r
merged_data <- merge(cagdp_data, controls_data, by = "GeoFIPS", all.x = TRUE)
```

- This is a base R `merge()` with left-join behavior from the GDP table.
- All GDP rows are retained; controls are attached when available.

### Merge B: Add Eras Tour Host Info

```r
working_data <- merge(filtered_data, eras_data, by = "GeoFIPS", all.x = TRUE)
```

- This is a base R `merge()` with left-join behavior.
- All analysis counties are kept, even if they did not host.

## 5. Data Cleaning Pattern Used

### Suppression handling + numeric conversion

```r
working_data <- working_data %>%
  mutate(across(all_of(year_columns), ~ ifelse(.x == "(D)", NA, .x))) %>%
  mutate(across(all_of(year_columns), as.numeric))
```

- Uses `across()` to clean all 2001-2023 columns in one pattern.
- Converts BEA suppression marker `"(D)"` to missing values before numeric conversion.

### Outcome transformations

```r
# Per-capita scale (thousand USD per person)
mutate(across(all_of(year_columns), ~ round(.x / POP_ESTIMATE_2023, 2)))

# Log transform for modeling
mutate(across(all_of(year_columns), ~ log(1 + .x)))
```

- First normalizes by county population.
- Then applies `log(1 + x)` for right-skew compression and zero safety.

## 6. Regression Structure

## 6.1 Current pattern in the script

- The script uses explicit `lm()` calls per model and per industry.
- Each industry gets five nested models:
  - Model 1: host only
  - Model 2: + education shares
  - Model 3: + poverty
  - Model 4: + net migration
  - Model 5: + lagged GDP (`2022`)

This is repeated for NAICS `71`, `72`, `54`, and `11`.

## 6.2 “Loop-like” reuse pattern actually used

- A helper function `extract_host_effect(model, industry, model_name)` is used to avoid repeated post-estimation extraction logic.
- `bind_rows()` is used to stack model summaries across models/industries.

So the script currently uses function-based reuse for output assembly, even though model estimation itself is written explicitly.

## 6.3 For-loop style pseudocode equivalent (conceptual)

```text
industries = [71, 72, 54, 11]
for industry in industries:
  data_i = filter working_data to this industry
  create education-share controls

  fit model_1 to model_5 with nested controls
  export one stargazer table for this industry
```

## 7. Unique R Features Used

- Base R `merge()` with left-join behavior for both merge steps
- `dplyr::across()` for bulk year-column cleaning/transforms
- `ifelse()` for BEA suppression replacement and treatment creation
- `pivot_longer()` and `pivot_wider()` for reshaping
- `patchwork` for multi-plot panel assembly with panel-level footnote caption
- Custom helper functions for table labeling and coefficient extraction
- Log-effect interpretation conversion via `exp(beta) - 1`

## 8. Q14 Calculation Logic (Pseudocode)

```text
beta = host coefficient from NAICS 71 Model 5
proportional_increase = exp(beta) - 1
percentage_increase = proportional_increase * 100

total_increase_thousand_usd = proportional_increase * 4,100,000
total_increase_usd = total_increase_thousand_usd * 1000
per_capita_increase_usd = total_increase_usd / 2,300,000
```

## 9. Output Files to Check After Running

- `naics_71_gdp_per_capita_histogram.png`
- `naics_71_log_gdp_per_capita_histogram.png`
- `naics_71_log_gdp_plot.png`
- `demographics_comparison_panel.png`
- `regression_table_naics_71.html`
- `regression_table_naics_72.html`
- `regression_table_naics_54.html`
- `regression_table_naics_11.html`

## 10. Run Command

```bash
Rscript HW2_ScriptTemplate.R
```
