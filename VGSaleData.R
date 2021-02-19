#Exploratory Data Analysis of Video Game Sales (https://www.kaggle.com/gregorut/videogamesales)
#Script by Colin Beran

library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(tidyr)

#load vgsales.csv
data = read.csv("Data Source Here", na = "N/A", stringsAsFactors=T, head=T)
head(data)

#Remove rows with N/A values
matrix.na = is.na(data)
data1 = na.omit(data)
data <- data1

summary(data)

#View number of games with over 100K sales by year, note incomplete data for 2017 and 2020.
data %>%
  group_by(Year) %>%
  count() %>%
  arrange(desc(n)) %>%
  print(n=39)

#Subset of data with a focus on 21st century video games. Ignoring 2017 and 2020 for incomplete data.
data1 <- data %>%
  mutate(Year = as.character(Year)) %>%
  filter(Year %in% c(2000:2016)) %>%
  mutate(Year = as.factor(Year)) %>%
  group_by(Year)

#Plot subset
data1 %>%
  mutate(color = ifelse(Year == 2009, "Highlight", "normal")) %>%
  ggplot(aes(Year)) + geom_bar(aes(fill = color)) + geom_label(stat = 'Count',
  aes(label = ..count..)) + labs(title= 'Titles with over 100K total sales (2000-2016)') + 
  scale_fill_manual(name = 'color', values = c('blue', 'grey')) + 
  theme(legend.position = 'none')

#Second subset strictly using 2009.
data2 <- data1 %>%
  mutate(Year = as.character(Year)) %>%
  filter(Year %in% c(2009)) %>%
  mutate(Year = as.factor(Year)) %>%
  group_by(Year)

#View number of games with over 100K in sales for 2000-2016 by platform.
data1 %>%
  group_by(Platform) %>%
  count() %>%
  arrange(desc(n)) %>%
  ggplot(aes(n, reorder(Platform, n))) + geom_bar(stat = 'identity') + geom_label(aes(label = n)) + 
  labs(title = 'Top Platforms by number of games released (2000-2016)', y = '', x = 'Count') +
  theme(legend.position = 'none')

#View number of games with over 100K in sales in 2009 by platform.
data2 %>%
  group_by(Platform) %>%
  count() %>%
  arrange(desc(n)) %>%
  ggplot(aes(n, reorder(Platform, n))) + geom_bar(stat = 'identity') + geom_label(aes(label = n)) + 
  labs(title = 'Top Platforms by number of games released (2009)', y = '', x = 'Count') +
  theme(legend.position = 'none')

#View top 20 publishers for 2000-2016 by count of games with over 100K total sales.
data1 %>%
  group_by(Publisher) %>%
  count() %>%
  arrange(desc(n)) %>%
  print(n = 20)

#Plot of top 20 publishers for 2009 by count of games with over 100K total sales.
data2 %>%
  group_by(Publisher) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(20) %>%
  ggplot(aes(n, reorder(Publisher, n))) + geom_bar(stat = 'identity') + geom_label(aes(label = n)) + 
  labs(title = 'Top 20 Publishers by number of games released (2009)', y = '', x = 'Count') +
  theme(legend.position = 'none')

#Plot of top 20 publishers for 2000-2016 by count of games with ofver 100K total sales.
data1 %>%
  group_by(Publisher) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(20) %>%
  ggplot(aes(n, reorder(Publisher, n))) + geom_bar(stat = 'identity') + geom_label(aes(label = n)) + 
  labs(title = 'Top 20 Publishers by number of games released (2000 - 2016)', y = '', x = 'Count') +
  theme(legend.position = 'none')
