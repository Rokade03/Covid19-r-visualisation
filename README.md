# COVID-19 Data Visualisation (R)

This repo contains an R project that cleans a COVID-19 dataset and generates visualisations saved into `plots/` (so they render nicely on GitHub).

## Project structure

- `BDA_Project.Rproj` — RStudio project
- `scripts/Project_github.R` — main script (creates plots + saves them)
- `scripts/packages_install.R` — optional helper to install required packages
- `data/` — put your dataset here as `data/raw_data.csv`
- `plots/` — generated figures (PNG)

## How to run

1. Put your dataset at: `data/raw_data.csv`
2. Open `BDA_Project.Rproj` in RStudio
3. Run:

```r
source("scripts/Project_github.R")
```

All images will be written to the `plots/` folder.

## Visualisations (auto-filled once you run the script)

### India vs USA total cases (line chart)
![India vs USA Cases](plots/india_vs_usa_total_cases_line.png)

### India total cases (bar)
![India Total Cases](plots/india_total_cases_bar.png)

### USA total cases (bar)
![USA Total Cases](plots/usa_total_cases_bar.png)

### India cases vs deaths (bars)
![India Cases vs Deaths](plots/india_cases_vs_deaths_bar.png)

### USA cases vs deaths (bars)
![USA Cases vs Deaths](plots/usa_cases_vs_deaths_bar.png)

### India average cases vs deaths (pie)
![India Pie](plots/india_avg_cases_vs_deaths_pie.png)

### USA average cases vs deaths (pie)
![USA Pie](plots/usa_avg_cases_vs_deaths_pie.png)

### World average cases vs deaths (pie)
![World Pie](plots/world_avg_cases_vs_deaths_pie.png)

### Average GDP per capita by country (bar)
![GDP](plots/avg_gdp_per_capita_by_country.png)
