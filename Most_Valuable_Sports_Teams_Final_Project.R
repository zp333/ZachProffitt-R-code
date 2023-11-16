#Load Require Libraries
library(tidyverse)
library(readr)
library(stringr)

#Load the csv 
SportsTeams <- read_csv("/Users/zachproffitt/Desktop/inst314-project/sports_teams.csv")

#Convert Value to numeric 
SportsTeams$`Value` <- gsub("\\$", "", SportsTeams$`Value`)
SportsTeams$`Value` <- gsub(",", "", SportsTeams$`Value`)
SportsTeams$`Value` <- ifelse(str_detect(SportsTeams$`Value`, " billion"),
                            as.numeric(str_extract(SportsTeams$`Value`, "\\d+\\.?\\d*")) * 10^9,
                            as.numeric(str_extract(SportsTeams$`Value`, "\\d+\\.?\\d*")) * 10^6)


#Convert Five-Year Change In Value to numeric 
SportsTeams$`Five-Year Change In Value` <- as.numeric(sub("%", "", SportsTeams$`Five-Year Change In Value`))

#Convert 'Year Purchased' to numeric
SportsTeams$`Year Purchased` <- as.numeric(SportsTeams$`Year Purchased`)

#Convert Price Paid to numeric 
SportsTeams$`Price Paid` <- gsub("\\$", "", SportsTeams$`Price Paid`)
SportsTeams$`Price Paid` <- gsub(",", "", SportsTeams$`Price Paid`)
SportsTeams$`Price Paid` <- ifelse(str_detect(SportsTeams$`Price Paid`, " billion"),
                            as.numeric(str_extract(SportsTeams$`Price Paid`, "\\d+\\.?\\d*")) * 10^9,
                            as.numeric(str_extract(SportsTeams$`Price Paid`, "\\d+\\.?\\d*")) * 10^6)

#Drop NA values
SportsTeams <- na.omit(SportsTeams)

#Print descriptive Statistics
summary(SportsTeams)

#Plot Histograms for each numeric variable
numeric_vars <- sapply(SportsTeams, is.numeric)
par(mfrow = c(2, 2))
for (var in names(SportsTeams)[numeric_vars]) {
  hist(SportsTeams[[var]], main = var, xlab = var, vol = "lightblue")
}

#Histograms
ggplot(SportsTeams, aes(x = `Five-Year Change In Value`)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Histogram of Five-Year Change In Value",
       x = "Five-Year Change In Value (%)",
       y = "Count")

SportsTeams$`Log Price Paid` <- log(SportsTeams$`Price Paid`)

ggplot(SportsTeams, aes(x = `Log Price Paid`)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of log-trasnformed Price Paid",
       x = "log-transformed Price paid (in millions)",
       y = "Count")

ggplot(SportsTeams, aes(x = `Value` / 10^9)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Value",
       x = "Value (in billions)",
       y = "Count")


#preform Linear Regression
model <- lm(`Five-Year Change In Value` ~ `Year Purchased`, data = SportsTeams)

#summary of model
summary(model)

#Scatter Plot to help visualize the results with a regression line
ggplot(SportsTeams, aes(x = `Year Purchased`, y = `Five-Year Change In Value`)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Relationship between Year Purchased and Five-Year Change In Value",
       x = "Year Purchased",
       y = "Five-Year Change in Value")



