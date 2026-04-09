
# Statistical Analysis of Tanzanian Hadza, Rural, IE MHQ Data


## Project Background
This project investigates the lifestyle drivers of Mental Health Quotient (MHQ) in rural Tanzania and why mental health scores in Rural are so different from Urban TZ.

## Main Script

- **TZdata_mhq_stats.R**
  - MHQ vs Age : One-way ANOVA and Tukey's HSD for differences between groups
  - MHQ vs Age : Trend analysis of mean MHQ with Age Band
  - MHQ Dimensions : One-way ANOVA and Tukey's HSD for differences between groups
  - Capacities and Problems : One-way ANOVA and Tukey's HSD for differences between groups
  - Computes and exports:
    - Summary statistics including differences between groups, p-values and slope in the case of trend analysis

## Data
- Input: `Study_DemoDataset_N10000.csv` Demo data with subset of 10000 participants randomly selected
- Mapping: `mhq_field_labels2.csv` Mapping the dimensions, capacities and problem field names
- For Figure 2 - `GM_World_mhq_by_age.csv` Have IE-Global data mean and sd values

## Usage

1. Run the main script from the folder:
   ```bash
   R CMD BATCH Predict_MHQ_from_Life.R
   Rscript -e "rmarkdown::render('TZdata_mhq_figures.Rmd')
   ```
2. Results will be saved in the same folder. Figures in TZdata_mhq_figures.html

## Requirements
- R version 4.0.3+
- Packages : mblm, plyr, kableExtra, knitR, reshape2


## Output Files
- `MHQ_vs_Age_Anova_Trend.csv`: One-way ANOVA and Tukey's HSD, Trend for differences between groups
- `MHQDims_AnovaTukeys.csv`: One-way ANOVA and Tukey's HSD MHQ dimensions
- `MHQCapacities_Tukeys.csv`: One-way ANOVA and Tukey's HSD MHQ dimensions
- `MHQProblems_Tukeys.csv`: One-way ANOVA and Tukey's HSD MHQ dimensions
- `TZdata_mhq_figures.html` : Figures generated for the study (when the complete data is uploaded)

## Contributors
- Dhanya Parameshwaran (dhanya@sapienlabs.org)
- Tara Thiagarajan

For questions or contributions, contact the project maintainer.
