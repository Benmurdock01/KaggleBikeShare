# install.packages('GGally')
# install.packages('patchwork')
library(tidyverse)
library(ggplot2)
library(tidymodels)
library(vroom)
library(dplyr)
library(GGally)
library(patchwork)

Bike <- vroom("Git/KaggleBikeShare/train.csv")

Bike <- Bike %>% 
  mutate(
    weather = as.factor(weather),
    holiday = as.factor(holiday),
    workingday = as.factor(workingday),
    season = as.factor(season)
  )

Bike <- Bike %>%
  mutate(season = factor(season,
                         levels = c(1, 2, 3, 4),
                         labels = c("Spring", "Summer", "Fall", "Winter")))

plot1 <- ggplot(data = Bike, mapping = aes(x = season, y = count))+
  geom_bar(stat = "summary", fun = "mean")

plot2 <- ggplot(data = Bike, aes(x=weather,y = count))+
  geom_bar(stat = "summary", fun = "mean")

plot3 <- ggplot(data = Bike, aes(x=workingday, y = count))+
  geom_bar(stat = 'summary', fun = 'mean')

plot4 <- ggplot(data = Bike, aes(x=windspeed, y = count))+
  geom_point()

(plot1 + plot2)/(plot3 + plot4)
