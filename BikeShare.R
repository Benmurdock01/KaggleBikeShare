library(tidyverse)
library(ggplot2)
library(tidymodels)
library(vroom)
library(dplyr)
library(GGally)
library(patchwork)
# install.packages("workflows")

BikeTrain <- vroom("Git/KaggleBikeShare/train.csv")

BikeTrain <- BikeTrain %>% 
  mutate(
    weather = as.factor(weather),
    holiday = as.factor(holiday),
    workingday = as.factor(workingday),
    season = as.factor(season)
  ) %>% 
  select(.,-casual) %>% 
  select(.,-registered)

BikeTest <- vroom("Git/KaggleBikeShare/test.csv") %>% 
  mutate(
    weather = as.factor(weather),
    holiday = as.factor(holiday),
    workingday = as.factor(workingday),
    season = as.factor(season))


#gernerating LM
linear_m <- linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression') %>% 
  fit(count~.-datetime, data=BikeTrain)

#Generating prediction using lm
bike_prediction <- predict(linear_m,
                           new_data=BikeTest)
bike_prediction

#adjusting for kaggle
kaggle_submission <- bike_prediction %>% 
  bind_cols(.,BikeTest) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0,count)) %>% 
  mutate(datetime=as.character(format(datetime)))

vroom_write(x=kaggle_submission, file = './LinearPreds.csv', delim = ',')


#EDA plots
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


#Homework recipe

# test data
testData <- vroom("Git/KaggleBikeShare/test.csv")

# training data
trainData <- vroom("Git/KaggleBikeShare/train.csv") %>%
  select(-casual, -registered) %>%
  mutate(count = log1p(count))   # log transform

# recipe
bike_recipe <- recipe(count ~ ., data = trainData) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>% 
  step_mutate(weather = factor(weather, levels = c(1,2,3))) %>% 
  step_time(datetime, features = c("hour")) %>% 
  step_mutate(season = factor(season, 
                              levels = c(1,2,3,4),
                              labels = c("spring","summer","fall","winter"))) %>% 
prepped_recipe <- prep(bike_recipe)

baked_train <- bake(prepped_recipe, new_data = trainData)
baked_test <- bake(prepped_recipe, new_data = testData)

# model
bike.lm <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# workflow
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(bike.lm) %>%
  fit(data = trainData)

# Keep a raw copy for submission
raw_test <- vroom("Git/KaggleBikeShare/test.csv")

# Predictions + backtransform
lin_preds <- predict(bike_workflow, new_data = testData) %>%
  mutate(count = pmax(0, round(expm1(.pred)))) %>%
  bind_cols(raw_test %>% select(datetime)) %>%
  select(datetime, count) %>%
  mutate(datetime = format(as.POSIXct(datetime, tz = "UTC"), "%Y-%m-%d %H:%M:%S"))

write_csv(lin_preds, "submission.csv")



