covid19_data <- read.csv("raw_data.csv/raw_data.csv") #load the dataset

head(covid19_data)

covid19_data <- covid19_data[, -c(10, 11, 12, 13, 14)] #remove the columns X, X.1, X.2, X.3 and X4

head(covid19_data)

summary(covid19_data)

sum(is.na(covid19_data)) #Total No. of missing Values

colSums(is.na(covid19_data)) #Total No. of missing values per column

library(naniar)
vis_miss(covid19_data) #Visualizing the Data which is missing

colSums(is.na(covid19_data))
library(dplyr)
glimpse(covid19_data) #getting type of data in the dataset with respect to columns

#Replacing NA values with means due to Numeric Values
covid19_data$total_cases[is.na(covid19_data$total_cases)] <- median(covid19_data$total_cases, na.rm = TRUE)
covid19_data$total_deaths[is.na(covid19_data$total_deaths)] <- median(covid19_data$total_deaths, na.rm = TRUE)
covid19_data$stringency_index[is.na(covid19_data$stringency_index)] <- median(covid19_data$stringency_index, na.rm = TRUE)
covid19_data$gdp_per_capita[is.na(covid19_data$gdp_per_capita)] <- median(covid19_data$gdp_per_capita, na.rm = TRUE)
covid19_data$human_development_index[is.na(covid19_data$human_development_index)] <- median(covid19_data$human_development_index, na.rm = TRUE)

sum(is.na(covid19_data))

summary(covid19_data)

unique_counts <- unique(covid19_data$location) #Listing what are the unique countries are there in the given dataset 
print(unique_counts)
print(length(unique_counts)) #Total number of unique countries

india_data = covid19_data %>% #Filtering Location only for India
  filter(location == "India")

india_data$date <- as.Date(india_data$date) #Converting Date into Date Format
print(india_data$date)

options(scipen = 999) #Converting Scientific Numeric into Whole Numeric

barplot(india_data$total_cases, # Creating a Bargraph for the Dates and Total Cases
        names.arg = as.character(india_data$date), 
        las = 2,                      # Rotate axis labels
        col = "skyblue",             # Color of the bars
        main = "Total COVID-19 Cases in India", 
        xlab = "Date", 
        ylab = "Total Cases",
        cex.names = 0.7)

#Doing the same for USA
usa_data <- covid19_data %>%
  filter(location == "United States")

usa_data$date <- as.Date(usa_data$date)
print(usa_data$date)

barplot(usa_data$total_cases, # Creating a Bargraph for the Dates and Total Cases
        names.arg = as.character(usa_data$date), 
        las = 2,                      # Rotate axis labels
        col = "red",             # Color of the bars
        main = "Total COVID-19 Cases in China", 
        xlab = "Date", 
        ylab = "Total Cases",
        cex.names = 0.7)

# Now we aggregate the data for india and usa with respective to date and total cases
india_agg <- aggregate(total_cases ~ date, data = india_data, FUN = sum)
usa_agg <- aggregate(total_cases ~ date, data = usa_data, FUN = sum)
combined_data <- merge(india_agg, usa_agg, by = "date", suffixes = c("_IND","_USA")) #combining the data with common column which is date
print(combined_data)

#using pivot_longer to join the total_cases and data for one date
library(tidyr)
combined_long <- pivot_longer(combined_data, 
                              cols = c(total_cases_IND, total_cases_USA),
                              names_to = "Country",
                              values_to = "Total_Cases")

#using Line plot to visualize the trend
library(ggplot2)
ggplot(combined_long, aes(x = date, y = Total_Cases, color = Country)) +
  geom_line(size = 1) +
  labs(title = "Total COVID-19 Cases: India vs. USA",
       x = "Date",
       y = "Total Cases",
       color = "Country") +
  theme_minimal() +
  scale_color_manual(values = c("total_cases_IND" = "blue", "total_cases_USA" = "red"))

#Now lets Visualize the trend for total deaths for both countries
india_dc_agg<- aggregate(total_cases ~ total_deaths + date , data = india_data, FUN = sum)
india_dc_agg$date <- as.Date(india_dc_agg$date)

india_long <- pivot_longer(india_dc_agg, 
                           cols = c(total_cases, total_deaths),
                           names_to = "Metric", 
                           values_to = "Count")

ggplot(india_long, aes(x = date, y = Count, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +  # 'dodge' makes bars side-by-side
  labs(title = "Comparison of Total Cases and Total Deaths Over Time",
       x = "Date",
       y = "Count",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


usa_dc_agg<- aggregate(total_cases ~ total_deaths + date , data = usa_data, FUN = sum)
usa_dc_agg$date <- as.Date(usa_dc_agg$date)

usa_long <- pivot_longer(usa_dc_agg, 
                           cols = c(total_cases, total_deaths),
                           names_to = "Metric", 
                           values_to = "Count")

ggplot(usa_long, aes(x = date, y = Count, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +  # 'dodge' makes bars side-by-side
  labs(title = "Comparison of Total Cases and Total Deaths Over Time",
       x = "Date",
       y = "Count",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Lets visualize what was the total average population was infected and Died
avg_total_cases_india <- mean(india_dc_agg$total_cases, na.rm = TRUE)
print(avg_total_cases_india)
avg_total_deaths_india <- mean(india_dc_agg$total_deaths, na.rm = TRUE)
print(avg_total_deaths_india)

pie_data <- data.frame(
  Category = c("Average Total Cases", "Average Total Deaths"),
  Count = c(avg_total_cases_india, avg_total_deaths_india)
)

pie_data$Percentage <- (pie_data$Count / sum(pie_data$Count)) * 100
print(pie_data$Percentage)

ggplot(pie_data, aes(x = "", y = Count, fill = Category)) +
  geom_bar(width = 1, stat = "identity") +  # Create a bar chart with one bar per category
  coord_polar("y") +  # Convert the bar chart into a pie chart
  labs(title = "Pie Chart: Average Total Cases, Total Deaths, and Remaining Population in India") +
  theme_void() +  # Remove background and axis details for a clean pie chart
  scale_fill_manual(values = c("Average Total Cases" = "orange", 
                               "Average Total Deaths" = "blue"))+
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white")

# For USA
avg_total_cases_usa <- mean(usa_dc_agg$total_cases, na.rm = TRUE)
print(avg_total_cases_usa)
avg_total_deaths_usa <- mean(usa_dc_agg$total_deaths, na.rm = TRUE)
print(avg_total_deaths_usa)

pie_data <- data.frame(
  Category = c("Average Total Cases", "Average Total Deaths"),
  Count = c(avg_total_cases_usa, avg_total_deaths_usa)
)

pie_data$Percentage <- (pie_data$Count / sum(pie_data$Count)) * 100
print(pie_data$Percentage)

ggplot(pie_data, aes(x = "", y = Count, fill = Category)) +
  geom_bar(width = 1, stat = "identity") +  # Create a bar chart with one bar per category
  coord_polar("y") +  # Convert the bar chart into a pie chart
  labs(title = "Pie Chart: Average Total Cases, Total Deaths, and Remaining Population in USA") +
  theme_void() +  # Remove background and axis details for a clean pie chart
  scale_fill_manual(values = c("Average Total Cases" = "red", 
                               "Average Total Deaths" = "blue"))+
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white")


#World
average_cases <- covid19_data %>%
  group_by(location) %>%
  summarize(average_cases = mean(total_cases, na.rm = TRUE))

average_deaths <- covid19_data %>%
  group_by(location) %>%
  summarize(average_deaths = mean(total_deaths, na.rm = TRUE))

avg_total_cases <- mean(average_cases$average_cases, na.rm = TRUE)
print(avg_total_cases)
avg_total_deaths <- mean(average_deaths$average_deaths, na.rm = TRUE)
print(avg_total_deaths)

pie_data <- data.frame(
  Category = c("Average Total Cases", "Average Total Deaths"),
  Count = c(avg_total_cases, avg_total_deaths)
)

pie_data$Percentage <- (pie_data$Count / sum(pie_data$Count)) * 100
print(pie_data$Percentage)

ggplot(pie_data, aes(x = "", y = Count, fill = Category)) +
  geom_bar(width = 1, stat = "identity") +  # Create a bar chart with one bar per category
  coord_polar("y") +  # Convert the bar chart into a pie chart
  labs(title = "Pie Chart: Average Total Cases, Total Deaths, and Remaining Population in World") +
  theme_void() +  # Remove background and axis details for a clean pie chart
  scale_fill_manual(values = c("Average Total Cases" = "green", 
                               "Average Total Deaths" = "lightblue"))+
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white")
#Now that we found out the trend of Infection and Deaths now we will see how economy was affected.
average_gdp_per_country <- covid19_data %>%
  group_by(location) %>%
  summarize(average_gdp = mean(gdp_per_capita, na.rm = TRUE))

ggplot(average_gdp_per_country, aes(x = average_gdp , y = reorder(location, average_gdp))) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Bar graph
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Average GDP Per Capita by Country",
       x = "Country",
       y = "Average GDP Per Capita") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlim(c(0, max(average_gdp_per_country$average_gdp) * 1.0))

#Thus We Find Qatar is having the Highest GDP and Central Africa is having the lowest.




