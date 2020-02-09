# Homework 1
# Hongzhao Ma
# Section 03

# Problem 1-----------------------------------
# Read the two data files
library(data.table)
library(dplyr)
roster <- read.csv("roster.csv", header = T)
attendance <- read.csv("attendance.csv", header = T)

# Split the first name and last name in roster.csv
roster_split <- tstrsplit(roster$names, split = ",")
roster_split <- as.data.frame(roster_split)
colnames(roster_split)[1:2] <- c("lastname", "firstname")

# Count the lectures attended
# Count the numbers of same full names in attendance
attendance$count <- rowid(attendance$firstname, attendance$lastname)

# Remain the maximum count of each full name
attendance <- attendance %>%
  group_by(firstname, lastname) %>%
  summarize(count = max(count))

# Join the two data
result_1 <- right_join(attendance, roster_split)

# Convert NA to 0
result_1[is.na(result_1)] <- 0
result_1


# Problem 2-----------------------------------
# Read the data file
library(data.table)
library(dplyr)
library(tidyr)
crime <- read.csv("Crime_Incident_Reports.csv", header = T, na.strings = "")

# Select OFENSE_CODE_GROUP and DISTRICT
crime <- crime[c("OFFENSE_CODE_GROUP", "DISTRICT")]

# Delete the NA values
crime <- na.omit(crime)

# Convert the dataset into a matrix, which is populated
crime_matrix <- crime %>%
  group_by(OFFENSE_CODE_GROUP, DISTRICT) %>%
  summarise(n = n()) %>%
  spread(key = DISTRICT, value = n, fill = 0)
crime_matrix


# Problem 3-----------------------------------
# Read the data file
library(data.table)
library(dplyr)
library(stringr)
wine <- read.csv('wine_data.csv', header = T, na.strings = '')

# Count the frequence of 'variety'
wine_categories <- wine %>%
  group_by(variety) %>%
  summarise(count = n())

# Sort the variety by count
wine_categories <- 
  wine_categories[order(wine_categories$count, decreasing = T),]

# Display the top 10 variety
head(wine_categories, 10)

# Calculate the average points by country
country_point <- wine %>%
  group_by(country) %>%
  summarise(average_point = mean(points, na.rm = T))
country_point

# Calculate the average prices by province
province_price <- wine %>%
  group_by(province, country) %>%
  summarise(average_price = mean(price, na.rm = T))

# Determine the province with highest average price
province_price[which.max(province_price$average_price),]

# Calculate the average prices by province in US
US_province_price <- wine[wine$country =='US',] %>%
  group_by(province) %>%
  summarise(average_price = mean(price, na.rm = T))

# Determine the province with highest average price in US
US_province_price[which.max(US_province_price$average_price),]

# Determine the index of 20 year old wine
index <- str_detect(wine$designation, '20.[year]')

# Get the numbers of 20 year old wine
wine_20_year <- wine[which(index==TRUE),]
length(wine_20_year[,1])
