#importing libraries
library(fpp)
library(fpp2)
library(seasonal)
library(utils)
library(readr)
library(astsa)
library(ggplot2)
library(quantmod)
library(dplyr)
library(tidyr)
install.packages("corrplot")
library(corrplot)
install.packages("maps")
library(maps)
install.packages("lubridate")
library(lubridate)
library(tsibble)

#import datasets
covid_confirm_gbl <- read_csv('/Users/acnlittlemac/ACN_WorkStation/1.MSDS_BatStateU/511MSDS_TimeSerise/511_MSDS_TimeSeries_Resourses/R_Practice_DrAvashti/COVID19_DataSets/confirmed_global.csv', show_col_types = FALSE)
str(covid_confirm_gbl)
covid_death_gbl <- read_csv('/Users/acnlittlemac/ACN_WorkStation/1.MSDS_BatStateU/511MSDS_TimeSerise/511_MSDS_TimeSeries_Resourses/R_Practice_DrAvashti/COVID19_DataSets/deaths_global.csv', show_col_types = FALSE)
str(covid_death_gbl)
covid_rec_gbl <- read_csv('/Users/acnlittlemac/ACN_WorkStation/1.MSDS_BatStateU/511MSDS_TimeSerise/511_MSDS_TimeSeries_Resourses/R_Practice_DrAvashti/COVID19_DataSets/recovered.csv', show_col_types = FALSE)
str(covid_rec_gbl)
View(covid_confirm_gbl)
View(covid_death_gbl)
View(covid_rec_gbl)


#check for missing values
sum(is.na(covid_confirm_gbl))
sum(is.na(covid_death_gbl))
sum(is.na(covid_rec_gbl))
 
#Handling missing values
#confirmed_gbl<- na.omit(covid_confirm_gbl)
#death_gbl<- na.omit(covid_death_gbl)
#recovered_gbl <- na.omit(covid_rec_gbl)

#check column names
colnames(confirmed_gbl)
colnames(death_gbl)
colnames(recovered_gbl)

#Aggregate each dataset
confirmed <- confirmed_gbl %>%
  gather(key="date", value ="confirmed", -
  c(`Country/Region`, `Province/State`, Lat, Long)) %>%
  group_by(`Country/Region`,date) %>% summarize(confirmed=sum(confirmed)) %>%
  ungroup()

print(confirmed)
View(confirmed)

death <- death_gbl   %>%
  gather(key="date", value ="death", -
           c(`Country/Region`, `Province/State`, Lat, Long)) %>%
  group_by(`Country/Region`,date) %>% summarize(death=sum(death)) %>%
  ungroup()

print(death)
View(death)

recovered <- recovered_gbl %>%
  gather(key="date", value ="recovered", -
           c(`Country/Region`, `Province/State`, Lat, Long)) %>%
  group_by(`Country/Region`,date) %>% summarize(recovered=sum(recovered)) %>%
  ungroup()

print(recovered)
View(recovered)

#Merge Data
merg_data <- full_join(confirmed,death) %>% full_join(recovered)
str(merg_data)
View(merg_data)

#fix the date
merg_data <- merg_data %>% 
  filter(!is.na(date)) %>% 
  mutate(date = as.Date(date, format="%m/%d/%y"))
str(merg_data)

#new variable, number of days
merg_data <- merg_data %>% 
  group_by('Country/Region') %>% mutate(acumulativecfm = cumsum(confirmed), 
  days = date - first(date) + 1)
View(merg_data)

#Aggregating all data

aggre_data <- merg_data %>% 
  filter(!is.na(date)) %>%  # Ensure no NAs in date
  group_by(date) %>% 
  summarize(
    confirmed = sum(confirmed, na.rm = TRUE),
    acumulativecfm = sum(acumulativecfm, na.rm = TRUE),
    death = sum(death, na.rm = TRUE),
    recovered = sum(recovered, na.rm = TRUE)
  ) %>% 
  mutate(days = as.integer(date - first(date) + 1))

print(aggre_data)
View(aggre_data)
#extract one country: Australia
Australia <- merg_data %>% filter(`Country/Region` == "Australia")
View(Australia)

#extract one country: France
France <- merg_data %>% filter(`Country/Region` == "France")
View(France)

#extract one country: United Kingdom
UK <- merg_data %>% filter(`Country/Region` == "United Kingdom")
View(UK)

#Summary
summary(merg_data)
by(merg_data$confirmed, merg_data$`Country/Region`, summary)
   by(merg_data$acumulativecfm, merg_data$`Country/Region`, summary)
   by(merg_data$death, merg_data$`Country/Region`, summary)
   by(merg_data$recovered, merg_data$`Country/Region`, summary)
summary(aggre_data)


#DataVisualization

#Barchart of the confirmed cases overtime (yearly)
ggplot(aggre_data, aes(x=date, y=confirmed)) +
  geom_bar(stat="identity", width=0.1) +
  theme_linedraw() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Date", y= "Daily confirmed cases") +
theme(plot.title = element_text(hjust = 0.5))


#Barchart of the confirmed cases overtime - UK
ggplot(UK, aes(x=date, y=confirmed)) +
  geom_bar(stat="identity", width=0.1) +
  theme_get() +
  labs(title = "Covid-19 Confirmed Cases in United Kingdom", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

China<- merg_data %>% filter(`Country/Region` == "China") 
View(China)
ggplot(China, aes(x=date, y=confirmed)) +
  geom_bar(stat="identity", width=0.1) +
  theme_get() +
  labs(title = "Covid-19 Confirmed Cases in China", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))


#Timeseries plot
ggplot(aggre_data, aes(x=date)) +
  geom_line(aes(y=confirmed, color = "confirmed")) +
  geom_line(aes(y=death, color = "death")) +
  geom_line(aes(y=recovered, color = "recovered")) +
  labs(title = "Global COVID19 Trends", x ="date", y="count") +
  scale_color_manual(values = c("confirmed" = "yellow", "death" = "red", "recovered" = "green"))

# Line graph of confirmed cases, deaths and recovered - World
str(aggre_data)
aggre_data %>%
  select(-acumulativecfm) %>%
  gather("Type", "Cases", -c(date, days)) %>%
  ggplot(aes(x=date, y=Cases, colour=Type)) +
  geom_bar(stat="identity", width=0.2, fill="white") +
  theme_classic() +
  labs(title = "Covid-19 Global Cases", x= "Date", y= "Daily cases") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("confirmed" = "yellow", "death" = "red", "recovered" = "green"))


# Line graph of confirmed cases on log10 scale for select countries

countries <- merg_data %>% filter(`Country/Region`==c("China", "France", "United Kingdom"))
ggplot(countries, aes(x=days, y=confirmed, colour=`Country/Region`)) + 
  geom_line(size=1) + 
  theme_classic() + 
  labs(title = "Covid-19 Confirmed Cases by Country", 
       x = "Days", 
       y = "Daily confirmed cases (log scale)") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(trans="log10")
class(merg_data)
str(merg_data)

#Bar plot for confirmed cases by country
countries_summ <- confirmed %>%
  group_by(`Country/Region`) %>%
  summarise(confirmed = sum(confirmed))
print(countries_summ)

ggplot(countries_summ, aes(x = reorder(`Country/Region`, confirmed), y = confirmed)) +
  geom_bar(stat = "identity", fill = "yellow") +
  coord_flip() +
  labs(title = "COVID-19 Confirmed Cases by Country", x = "Country", y = "Confirmed Cases") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

#Daily new cases, deaths, and recoveries
daily_Covid <- merg_data %>%
  group_by(`Country/Region`) %>%
  arrange(date) %>%
  mutate(daily_confirmed = confirmed - lag(confirmed),
         daily_death = death - lag(death),
         daily_recovered = recovered - lag(recovered)) %>%
  filter(!is.na(daily_confirmed) & !is.na(daily_death) & !is.na(daily_recovered))

ggplot(daily_Covid, aes(x=date)) +
  geom_line(aes(y=daily_confirmed, color ="New Cases")) +
  geom_line(aes(y=daily_death, color = "New Deaths")) +
  geom_line(aes(y=daily_recovered, color = "New Recoveries")) +
  labs(title = "Daily COVID19 Cases, Deaths, and Revoveries", x="Date", y="Count") +
  scale_color_manual(values=c("New Cases" = "yellow", "New Deaths"="red", "New Recoveries"="green"))

#Pie Chart for confirmed cases by countries
countries_summ <- merg_data %>%
  group_by(`Country/Region`) %>%
  summarise(confirmed = sum(confirmed, na.rm = TRUE))

# Create the pie chart for confirmed cases
ggplot(countries_summ, aes(x = "", y = confirmed, fill = `Country/Region`)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() + 
  labs(title = "COVID-19 Confirmed Cases by Country") +
  theme(plot.title = element_text(hjust = 0.5))

#Pie Chart for deaths by countries
countries_summ <- merg_data %>%
  group_by(`Country/Region`) %>%
  summarise(death = sum(death, na.rm = TRUE))

# Create the pie chart for death
ggplot(countries_summ, aes(x = "", y = death, fill = `Country/Region`)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_linedraw() + 
  labs(title = "COVID-19 deaths by Country") +
  theme(plot.title = element_text(hjust = 0.5))

#Pie Chart for recoveries by countries
countries_summ <- merg_data %>%
  group_by(`Country/Region`) %>%
  summarise(recovered = sum(recovered, na.rm = TRUE))

# Create the pie chart for recoveries
ggplot(countries_summ, aes(x = "", y = recovered, fill = `Country/Region`)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_classic() + 
  labs(title = "COVID-19 recoveries by Country") +
  theme(plot.title = element_text(hjust = 0.5))



#Time series Analysis
#Growth rates (cases, deaths, and recoveries)
growth_covid <- merg_data %>%
  group_by(`Country/Region`) %>%
  mutate(growth_confirmed = (confirmed - lag(confirmed))/ lag(confirmed) * 100,
         growth_deaths = (death - lag(death))/ lag(death) * 100,
         growth_recovered = (recovered - lag(recovered))/ lag(recovered))

#finding the significant increases/decreases
significant_geo <- growth_covid %>%
  filter(growth_confirmed > 10 | growth_deaths > 10 | growth_recovered > 10) %>%
  select(`Country/Region`,date,growth_confirmed,growth_deaths,growth_recovered)

#correlation among confirmed, deaths, and recoveries
correlation_analysis <- growth_covid %>%
  filter(complete.cases(confirmed, death, recovered)) %>%
  summarise(
    correlation_cfm_deaths = cor(confirmed, death, use = "complete.obs"),
    correlation_cfm_recovered = cor(confirmed, recovered, use = "complete.obs")
  )
print(correlation_analysis)


#plotting correlations between confirmed, deaths, and recoveries
#scatter plot: Confirmed Vs. Deaths
ggplot(merg_data, aes(x=confirmed, y = death)) +
  geom_point(alpha = 0.4, color = "red") +
  geom_smooth(method ="lm", se = FALSE, color ="black") +
  labs(title = "Scatter plot: Confirmed Vs. Deaths", x ="Confirmed Cases", y="Deaths") +
  theme_minimal()

#Scatter plot: Confirmed Vs. Recovered
ggplot(merg_data, aes(x=confirmed, y = recovered)) +
  geom_point(alpha = 0.4, color = "green") +
  geom_smooth(method ="lm", se = FALSE, color ="black") +
  labs(title = "Scatter plot: Confirmed Vs. Recovered", x ="Confirmed Cases", y="Recovered") +
  theme_minimal()

#Scatter plot: Deaths Vs. Recovered
ggplot(merg_data, aes(x=death, y = recovered)) +
  geom_point(alpha = 0.4, color = "brown") +
  geom_smooth(method ="lm", se = FALSE, color ="black") +
  labs(title = "Scatter plot: Deaths Vs. Recovered", x ="Deaths", y="Recovered") +
  theme_classic()

#Visualization with correlation Heatmap
# num columns selection
num_data <- aggre_data %>%
  select(confirmed, death, recovered) %>%
  filter(!is.na(confirmed) & !is.na(death) & !is.na(recovered))

# Calculate the correlation matrix
cor_matrix <- cor(num_data, use="complete.obs")

print(cor_matrix)

cor_matrix <- as.matrix(cor_matrix)
corrplot(cor_matrix, method = "circle", type = "full", tl.col = "black", title = "Correlation Heatmap")

#combining world map with COVID19 Data

world_map <- map_data("world")
world_covid <- merg_data %>%
  group_by(`Country/Region`) %>%
  summarise(confirmed= sum(confirmed, na.rm = TRUE))

#check the country names
#world_covid <- world_covid %>%
 # mutate(`Country/Region` = recode(`Country/Region`,
  #                                 "United States" = "USA",
   #                                "UK"="United Kingdom",
    #                               "South Korea"="Korea, South"))

covid_wd<- world_map %>%
  left_join(world_covid, by = c("region"="Country/Region"))
str(covid_wd)

#plotting in world map
ggplot() +
  geom_polygon(data = covid_wd, aes(x = long, y = lat, group = group, fill = confirmed), color = "white") +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey50") +
  theme_void() +
  labs(title = "COVID-19 Confirmed Cases by Country",
       fill = "Confirmed Cases")

#import world population density

world_pop_density1 <- read_csv(url("https://raw.githubusercontent.com/autistic96/project-2/main/world_population.csv"))

View(world_pop_density1)

#merge covid data set and world population density
merg_data_pop <- merg_data %>%
  left_join(world_pop_density1, by = c("Country/Region" = "Country/Territory"))
View(merg_data_pop)
str(merg_data_pop)
head(merg_data_pop)

# Add the appropriate population year data
merg_data_pop <- merg_data_pop %>%
  mutate(
    population = case_when(
      year(date) == 2022 ~ `2022 Population`,
      year(date) == 2021 ~ `2020 Population`,  # Adjust based on available data
      year(date) == 2020 ~ `2020 Population`,
      year(date) == 2015 ~ `2015 Population`,
      year(date) == 2010 ~ `2010 Population`,
      TRUE ~ NA_real_
    )
  )

# View the updated data
View(merg_data_pop)


# Calculate correlations between COVID-19 data and population
covid_pop_correlation <- merg_data_pop %>%
  summarise(
    correlation_cfm_deaths = cor(confirmed, death, use = "complete.obs"),
    correlation_cfm_recovered = cor(confirmed, recovered, use = "complete.obs"),
    correlation_cfm_population = cor(confirmed, population, use = "complete.obs")
  )

# View the results
print(covid_pop_correlation)
# Scatter plot: Population Vs. Confirmed Cases
ggplot(merg_data_pop, aes(x = population, y = confirmed)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Population vs Confirmed COVID-19 Cases",
       x = "Population", y = "Confirmed Cases") +
  theme_bw()

# Scatter plot: Population Vs. Deaths
ggplot(merg_data_pop, aes(x = population, y = death)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Population vs COVID-19 Deaths",
       x = "Population", y = "Deaths") +
  theme_light()

# Scatter plot: Population Vs. recovered
ggplot(merg_data_pop, aes(x = population, y = recovered)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Population vs COVID-19 Recovered",
       x = "Population", y = "Recovered") +
  theme_light()

# Time series plot for confirmed cases
ggplot(merg_data_pop, aes(x = date, y = confirmed, color = `Country/Region`)) +
  geom_line() +
  labs(title = "Time Series of Confirmed COVID-19 Cases",
       x = "Date", y = "Confirmed Cases") +
  theme_void()

# Time series plot for deaths
ggplot(merg_data_pop, aes(x = date, y = death, color = `Country/Region`)) +
  geom_line() +
  labs(title = "Time Series of COVID-19 Deaths",
       x = "Date", y = "Deaths") +
  theme_minimal()

# Time series plot for Recovered
ggplot(merg_data_pop, aes(x = date, y = recovered, color = `Country/Region`)) +
  geom_line() +
  labs(title = "Time Series of COVID-19 Recovered",
       x = "Date", y = "Recovered") +
  theme_minimal()

