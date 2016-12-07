rm(list=ls())
cat("\014")

options(scipen=999)

## Loading Libraries
library(ggplot2)
library(scales)
library(reshape2)
library(lubridate)
library(dplyr)
library(doBy)
library(data.table)
library(jsonlite)

## Functions
factor_to_numeric <- function(x) {
  return (as.numeric(as.character(x)))
}

length_unique <- function(x) {
  return (length(unique(x)))
}

give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

my_min <- function(x) {
  return (min(x, na.rm = TRUE))
}


setwd('/Users/annabelle/Documents/Social_Data/')

social_data <- jsonlite::fromJSON("result.json",simplifyDataFrame = T)
head(social_data)
summary(social_data)
str(social_data)
#write.csv(social_data,"social_data.csv")

# Company Name
ggplot(data=social_data,
       aes(x=as.factor(name))) +
  geom_bar(fill = "#06436d",colour = "grey", stat="count") +
  xlab("") + ylab("Users") + ggtitle("Current Company") +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")

# Job title
ggplot(data=social_data,
       aes(x=as.factor(title))) +
  geom_bar(fill = "#06436d",colour = "grey", stat="count") +
  xlab("") + ylab("Users") + ggtitle("Job Title") +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 60, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")


# Gender
ggplot(data=social_data,
       aes(x=as.factor(gender))) +
  geom_bar(fill = "#06436d",colour = "grey", stat="count") +
  xlab("") + ylab("Users") + ggtitle("Gender") +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 60, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")

# Location
ggplot(data=social_data,
       aes(x=as.factor(locationGeneral))) +
  geom_bar(fill = "#06436d",colour = "grey", stat="count") +
  xlab("") + ylab("Users") + ggtitle("Location") +
  theme(legend.position="right",
        panel.background = element_rect(colour = "black", fill="white"),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        text = element_text(size=16),
        axis.text.x = element_text(angle = 60, hjust = 1, size=10),
        legend.text = element_text(size=12),
        legend.justification = "left")

