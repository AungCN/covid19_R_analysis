# COVID-19 Statistical & Time-Series Analysis in R

Exploratory data analysis, spatial mapping, correlation analysis, and time-series statistical modeling of global and US COVID-19 cases (confirmed, deaths, and recovered) from the Johns Hopkins University CSSE dataset using **R**.

## Repository Contents

- `COVID19_R_Statisstics.R` — Main R script for data cleaning, wrangling, time-series decomposition, correlation matrices, geographical visualization, and forecasting.
- **Datasets**:
  - `confirmed_global.csv` / `time_series_covid19_confirmed_global.csv`
  - `deaths_global.csv` / `time_series_covid19_deaths_global.csv`
  - `recovered.csv` / `time_series_covid19_recovered_global.csv`
  - `time_series_covid19_confirmed_US.csv`
  - `time_series_covid19_deaths_US.csv`
  - `covid19_multi_mg_final.csv` & `diseases_multi_mg_final.csv`
- `csse_covid_19_time_series.html` — Dataset documentation reference.
- `requirements.txt` — List of required R packages.

## Getting Started

1. Install **R** (and optionally **RStudio**).
2. Install the required R packages listed in `requirements.txt`:
   ```r
   pkgs <- grep("^#", readLines("requirements.txt"), invert = TRUE, value = TRUE)
   install.packages(pkgs[nzchar(pkgs)])
   ```
3. Update the dataset directory path in `COVID19_R_Statisstics.R` to your local working directory and run the script.

## License

This project is licensed under the [MIT License](LICENSE).
