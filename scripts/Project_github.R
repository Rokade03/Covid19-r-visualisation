# ------------------------------------------------------------------------------
# Project_github.R
# - Loads dataset from data/raw_data.csv (recommended)
# - Cleans NA values (median imputation, numeric columns)
# - Generates visualisations and SAVES them as PNG files into plots/
# ------------------------------------------------------------------------------

# ---- Helpers ----
dir.create("plots", showWarnings = FALSE)

find_data_file <- function() {
  candidates <- c(
    "data/raw_data.csv",
    "raw_data.csv",
    "raw_data.csv/raw_data.csv" # kept for backward compatibility with your original script
  )
  for (p in candidates) {
    if (file.exists(p)) return(p)
  }
  stop("Dataset not found. Put it at 'data/raw_data.csv' (recommended).")
}

save_base_plot <- function(filename, expr, width = 1400, height = 700, res = 150) {
  png(file.path("plots", filename), width = width, height = height, res = res)
  on.exit(dev.off(), add = TRUE)
  force(expr)
}

save_gg <- function(filename, plot_obj, width = 10, height = 6, dpi = 300) {
  ggplot2::ggsave(
    filename = file.path("plots", filename),
    plot = plot_obj,
    width = width, height = height, dpi = dpi
  )
}

# ---- Packages ----
# (Keep installs OUT of the main script. If needed, run packages.R once.)
library(dplyr)
library(tidyr)
library(ggplot2)
library(naniar)

options(scipen = 999) # avoid scientific notation

# ---- Load data ----
data_path <- find_data_file()
covid19_data <- read.csv(data_path)

# ---- Light cleaning to match your original script ----
# Drop the last 5 columns (your original script removed cols 10:14).
# This is safe-guarded so it won't crash if the dataset has fewer columns.
if (ncol(covid19_data) >= 14) {
  covid19_data <- covid19_data[, -c(10, 11, 12, 13, 14)]
}

# Missing data visual
# (This will show in RStudio; it doesn't save automatically.)
vis_miss(covid19_data)

# Median imputation for numeric fields used later (guarded if columns exist)
median_impute <- function(df, col) {
  if (col %in% names(df)) {
    df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
  }
  df
}
covid19_data <- median_impute(covid19_data, "total_cases")
covid19_data <- median_impute(covid19_data, "total_deaths")
covid19_data <- median_impute(covid19_data, "stringency_index")
covid19_data <- median_impute(covid19_data, "gdp_per_capita")
covid19_data <- median_impute(covid19_data, "human_development_index")

# Ensure date is Date
if ("date" %in% names(covid19_data)) {
  covid19_data$date <- as.Date(covid19_data$date)
}

# ---- Filter India & USA ----
india_data <- covid19_data %>% filter(location == "India")
usa_data   <- covid19_data %>% filter(location == "United States")

# ---- Bar plots: total cases over date ----
save_base_plot("india_total_cases_bar.png", {
  barplot(
    india_data$total_cases,
    names.arg = as.character(india_data$date),
    las = 2,
    col = "skyblue",
    main = "Total COVID-19 Cases in India",
    xlab = "Date",
    ylab = "Total Cases",
    cex.names = 0.7
  )
})

save_base_plot("usa_total_cases_bar.png", {
  barplot(
    usa_data$total_cases,
    names.arg = as.character(usa_data$date),
    las = 2,
    col = "red",
    main = "Total COVID-19 Cases in USA",
    xlab = "Date",
    ylab = "Total Cases",
    cex.names = 0.7
  )
})

# ---- Line plot: India vs USA total cases ----
combined_data <- data.frame(
  date = india_data$date,
  total_cases_IND = india_data$total_cases,
  total_cases_USA = usa_data$total_cases
)

combined_long <- pivot_longer(
  combined_data,
  cols = c(total_cases_IND, total_cases_USA),
  names_to = "Country",
  values_to = "Total_Cases"
)

p_line <- ggplot(combined_long, aes(x = date, y = Total_Cases, color = Country)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Total COVID-19 Cases: India vs. USA",
    x = "Date", y = "Total Cases", color = "Country"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("total_cases_IND" = "blue", "total_cases_USA" = "red"))

save_gg("india_vs_usa_total_cases_line.png", p_line, width = 11, height = 6)

# ---- Cases vs deaths over time (side-by-side bars) ----
india_dc_agg <- aggregate(total_cases ~ total_deaths + date, data = india_data, FUN = sum)
india_dc_agg$date <- as.Date(india_dc_agg$date)

india_long <- pivot_longer(
  india_dc_agg,
  cols = c(total_cases, total_deaths),
  names_to = "Metric",
  values_to = "Count"
)

p_india_bars <- ggplot(india_long, aes(x = date, y = Count, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "India: Total Cases vs Total Deaths Over Time",
    x = "Date", y = "Count", fill = "Metric"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_gg("india_cases_vs_deaths_bar.png", p_india_bars, width = 12, height = 6)

usa_dc_agg <- aggregate(total_cases ~ total_deaths + date, data = usa_data, FUN = sum)
usa_dc_agg$date <- as.Date(usa_dc_agg$date)

usa_long <- pivot_longer(
  usa_dc_agg,
  cols = c(total_cases, total_deaths),
  names_to = "Metric",
  values_to = "Count"
)

p_usa_bars <- ggplot(usa_long, aes(x = date, y = Count, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "USA: Total Cases vs Total Deaths Over Time",
    x = "Date", y = "Count", fill = "Metric"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_gg("usa_cases_vs_deaths_bar.png", p_usa_bars, width = 12, height = 6)

# ---- Pie charts: average cases vs deaths ----
make_pie <- function(avg_cases, avg_deaths, title, file, colors) {
  pie_data <- data.frame(
    Category = c("Average Total Cases", "Average Total Deaths"),
    Count = c(avg_cases, avg_deaths)
  )
  pie_data$Percentage <- (pie_data$Count / sum(pie_data$Count)) * 100

  p <- ggplot(pie_data, aes(x = "", y = Count, fill = Category)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y") +
    labs(title = title) +
    theme_void() +
    scale_fill_manual(values = colors) +
    geom_text(
      aes(label = paste0(round(Percentage, 1), "%")),
      position = position_stack(vjust = 0.5),
      color = "white"
    )

  save_gg(file, p, width = 7, height = 5)
}

avg_total_cases_india  <- mean(india_dc_agg$total_cases, na.rm = TRUE)
avg_total_deaths_india <- mean(india_dc_agg$total_deaths, na.rm = TRUE)
make_pie(
  avg_total_cases_india, avg_total_deaths_india,
  "India: Average Total Cases vs Total Deaths",
  "india_avg_cases_vs_deaths_pie.png",
  c("Average Total Cases" = "orange", "Average Total Deaths" = "blue")
)

avg_total_cases_usa  <- mean(usa_dc_agg$total_cases, na.rm = TRUE)
avg_total_deaths_usa <- mean(usa_dc_agg$total_deaths, na.rm = TRUE)
make_pie(
  avg_total_cases_usa, avg_total_deaths_usa,
  "USA: Average Total Cases vs Total Deaths",
  "usa_avg_cases_vs_deaths_pie.png",
  c("Average Total Cases" = "red", "Average Total Deaths" = "blue")
)

# World averages (mean across country-level means, matching your original logic)
average_cases <- covid19_data %>%
  group_by(location) %>%
  summarize(average_cases = mean(total_cases, na.rm = TRUE), .groups = "drop")

average_deaths <- covid19_data %>%
  group_by(location) %>%
  summarize(average_deaths = mean(total_deaths, na.rm = TRUE), .groups = "drop")

avg_total_cases_world  <- mean(average_cases$average_cases, na.rm = TRUE)
avg_total_deaths_world <- mean(average_deaths$average_deaths, na.rm = TRUE)

make_pie(
  avg_total_cases_world, avg_total_deaths_world,
  "World: Average Total Cases vs Total Deaths",
  "world_avg_cases_vs_deaths_pie.png",
  c("Average Total Cases" = "green", "Average Total Deaths" = "lightblue")
)

# ---- Average GDP per capita by country ----
average_gdp_per_country <- covid19_data %>%
  group_by(location) %>%
  summarize(average_gdp = mean(gdp_per_capita, na.rm = TRUE), .groups = "drop")

p_gdp <- ggplot(average_gdp_per_country, aes(x = average_gdp, y = reorder(location, average_gdp))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Average GDP Per Capita by Country",
    x = "Average GDP Per Capita",
    y = "Country"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlim(c(0, max(average_gdp_per_country$average_gdp, na.rm = TRUE) * 1.0))

save_gg("avg_gdp_per_capita_by_country.png", p_gdp, width = 10, height = 12)

message("Done ✅ Plots saved into: ", normalizePath("plots"))
