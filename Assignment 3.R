library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)

#Question 1

data(iris)
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) + geom_point() + labs(x = "Sepal Length", y = "Petal Length") 

#Question 2

data(txhousing)
str(txhousing)
summary(txhousing)
txhousing_2 <- txhousing[complete.cases(txhousing), ]
ggplot(txhousing_2, aes(x = sales, y = volume)) +
  geom_point()
ggplot(txhousing_2, aes(x = year, y = sales)) +
  geom_boxplot()
ggplot(txhousing_2, aes(x = volume)) +
  geom_histogram(binwidth = 100000)
txhousing_avg_sales <- txhousing_clean %>%
  group_by(year) %>%
  summarise(avg_sales = mean(sales))
ggplot(txhousing_avg_sales, aes(x = year, y = avg_sales)) +
  geom_line() +
  labs(x = "Year", y = "Average Sales") +
  theme_minimal()
txhousing_median_sales <- txhousing_clean %>%
  group_by(city) %>%
  summarise(median_sales = median(sales))
ggplot(txhousing_median_sales, aes(x = city, y = median_sales)) +
  geom_bar(stat = "identity") +
  labs(x = "City", y = "Median Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Question 3

titanic <- read.csv("titanic.csv")
survived_died <- c('0' = "Died", '1' = "Survived")
survived_labeller <- function(variable,value){
  return(survived_died[value])
}
final_Plot <- ggplot(titanic, aes(x = Fare, color = factor(Sex))) +
  geom_boxplot() +
  facet_wrap(~Survived, dir="v", labeller  = as_labeller(survived_died), strip.position = "left") +
  labs(x = "Fare", color = "Sex") +
  scale_color_manual(values = c("orange", "cyan")) 
final_Plot
