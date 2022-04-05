library(tidyverse)

filename <- "data/passenger_satisfaction.csv"
df <- read_csv(filename)

summary(df)
str(df)