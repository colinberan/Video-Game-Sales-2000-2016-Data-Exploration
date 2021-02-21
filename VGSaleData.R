#Exploratory Data Analysis of Video Game Sales (https://www.kaggle.com/gregorut/videogamesales)
#Script by Colin Beran

library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(tidyr)

#load vgsales.csv
data = read.csv("E:/Users/Colin/Documents/Data Science/Data Sets/vgsales.csv", na = "N/A", stringsAsFactors=T, head=T)
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
  group_by(Year) %>%
  ggplot(aes(Year)) + geom_bar(aes(fill = Year)) + geom_label(stat = 'Count',
  aes(label = ..count..)) + labs(title = 'Titles with over 100K total sales (2000-2016)', y = 'Count') + 
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
  ggplot(aes(n, reorder(Platform, n))) + theme_minimal() + geom_bar(stat = 'identity', aes(fill=Platform)) + geom_label(aes(label = n)) + 
  labs(title = 'Top Platforms by number of games released (2000-2016)', y = '', x = 'Count') +
  theme(legend.position = 'none')

#View number of games with over 100K in sales in 2009 by platform.
data2 %>%
  group_by(Platform) %>%
  count() %>%
  arrange(desc(n)) %>%
  ggplot(aes(n, reorder(Platform, n))) + theme_minimal() + geom_bar(stat = 'identity', aes(fill=Platform)) + geom_label(aes(label = n)) + 
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
  ggplot(aes(n, reorder(Publisher, n))) + geom_bar(stat = 'identity', aes(fill=Publisher)) + geom_label(aes(label = n)) + 
  labs(title = 'Top 20 Publishers by number of games released (2009)', y = '', x = 'Count') +
  theme(legend.position = 'none')

#Plot of top 20 publishers for 2000-2016 by count of games with over 100K total sales.
data1 %>%
  group_by(Publisher) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(20) %>%
  ggplot(aes(n, reorder(Publisher, n))) + theme_minimal() + geom_bar(stat = 'identity', aes(fill=Publisher)) + geom_label(aes(label = n)) + 
  labs(title = 'Top 20 Publishers by number of games released (2000 - 2016)', y = '', x = 'Count') +
  theme(legend.position = 'none')

#Plot of top genres for 2000-2016 by count of games with over 100K total sales.
data1 %>%
  group_by(Genre) %>%
  count() %>%
  arrange(desc(n)) %>%
  ggplot(aes(reorder(Genre,-n), n)) + theme_minimal() + geom_bar(stat = 'identity', aes(fill=Genre)) + geom_label(aes(label = n)) + 
  labs(title = 'Top Genre by number of games released (2000 - 2016)', y = 'Count', x = 'Genre') +
  theme(legend.position = 'none')

#Aggregate Sales to combine games with the same name on different platforms.
TopGamesNA <- aggregate(list(NA_Sales = data1$NA_Sales), list(Name = data1$Name), sum)
TopGamesNA <- TopGamesNA[order(TopGamesNA$NA_Sales, decreasing = TRUE), ]

TopGamesJP <- aggregate(list(JP_Sales = data1$JP_Sales), list(Name = data1$Name), sum)
TopGamesJP <- TopGamesJP[order(TopGamesJP$JP_Sales, decreasing = TRUE), ]

TopGamesEU <- aggregate(list(EU_Sales = data1$EU_Sales), list(Name = data1$Name), sum)
TopGamesEU <- TopGamesEU[order(TopGamesEU$EU_Sales, decreasing = TRUE), ]

TopGamesGlobal <- aggregate(list(Global_Sales = data1$Global_Sales), list(Name = data1$Name), sum)
TopGamesGlobal <- TopGamesGlobal[order(TopGamesGlobal$Global_Sales, decreasing = TRUE), ]

#Plot of top 10 games in North America for 2000-2016.
ggplot((head(TopGamesNA, 10)), aes(reorder(Name,-NA_Sales), NA_Sales)) + theme_minimal() +
geom_bar(stat = 'identity', aes(fill=Name)) + geom_label(aes(label = NA_Sales)) +
labs(title = 'Top 10 Games in North America (2000 - 2016)', y = 'Sales (Millions)', x = 'Title') +
theme(legend.position = 'none', axis.text.x = element_text(face = "bold", angle = 25))

#Plot of top 10 games in Japan for 2000-2016.
ggplot((head(TopGamesJP, 10)), aes(reorder(Name,-JP_Sales), JP_Sales)) + theme_minimal() +
  geom_bar(stat = 'identity', aes(fill=Name)) + geom_label(aes(label = JP_Sales)) +
  labs(title = 'Top 10 Games in Japan (2000 - 2016)', y = 'Sales (Millions)', x = 'Title') +
  theme(legend.position = 'none', axis.text.x = element_text(face = "bold", angle = 25))

#Plot of top 10 games in Europe for 2000-2016.
ggplot((head(TopGamesEU, 10)), aes(reorder(Name,-EU_Sales), EU_Sales)) + theme_minimal() +
  geom_bar(stat = 'identity', aes(fill=Name)) + geom_label(aes(label = EU_Sales)) +
  labs(title = 'Top 10 Games in Europe (2000 - 2016)', y = 'Sales (Millions)', x = 'Title') +
  theme(legend.position = 'none', axis.text.x = element_text(face = "bold", angle = 25))

#Plot of top 10 games Globally for 2000-2016.
ggplot((head(TopGamesGlobal, 10)), aes(reorder(Name,-Global_Sales), Global_Sales)) +
  geom_bar(stat = 'identity', aes(fill=Name)) + geom_label(aes(label = Global_Sales)) +
  labs(title = 'Top 10 Games Globally (2000 - 2016)', y = 'Sales (Millions)', x = 'Title') +
  theme(legend.position = 'none', axis.text.x = element_text(face = "bold", angle = 25))

