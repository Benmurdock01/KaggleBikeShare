# install.packages('GGally')
# install.packages('patchwork')
library(tidyverse)
library(ggplot2)
library(tidymodels)
library(vroom)
library(dplyr)
library(GGally)
library(patchwork)
library(tidymodels)

BikeTrain <- vroom("Git/KaggleBikeShare/train.csv")

BikeTrain <- BikeTrain %>% 
  mutate(
    weather = as.factor(weather),
    holiday = as.factor(holiday),
    workingday = as.factor(workingday),
    season = as.factor(season)
  )

BikeTrain <- BikeTrain %>%
  mutate(season = factor(season,
                         levels = c(1, 2, 3, 4),
                         labels = c("Spring", "Summer", "Fall", "Winter")))
Bike <- BikeTrain
plot1 <- ggplot(data = Bike, mapping = aes(x = season, y = count))+
  geom_bar(stat = "summary", fun = "mean")

plot2 <- ggplot(data = Bike, aes(x=weather,y = count))+
  geom_bar(stat = "summary", fun = "mean")

plot3 <- ggplot(data = Bike, aes(x=workingday, y = count))+
  geom_bar(stat = 'summary', fun = 'mean')

plot4 <- ggplot(data = Bike, aes(x=windspeed, y = count))+
  geom_point()

(plot1 + plot2)/(plot3 + plot4)

#gernerating LM
linear_m <- linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression') %>% 
  fit(count~.-datetime, data=BikeTrain)

#Generating prediction using lm
bike_prediction <- predict(linear_m,
                           new_data=Bike)
bike_prediction

#adjusting for kaggle
kaggle_submission <- bike_prediction %>% 
  bind_cols(.,Bike) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0,count)) %>% 
  mutate(datetime=as.character(format(datetime)))

vroom_write(x=kaggle_submission, file = './LinearPreds.csv', delim = ',')
