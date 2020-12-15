

library(tidyverse)
library(rvest)
library(modelr)
library(caret)
library(httr)
library(jsonlite)

VGSales <- read_csv("Video_Games_Sales.csv")

baseurl <- "https://api.rawg.io/api/games?key=8fdd449e387c4197ad765b1222eafa73&page_size=39&dates=1980-01-01,2017-01-01"
pages <- list()
for(i in 1:100){
  mydata <- fromJSON(paste0(baseurl, "&page=", i))
  pages[[i+1]] <- mydata$results
}
RAWGData <- rbind_pages(pages)

head(RAWGData)

RAWGDataShowcase <- RAWGData %>%
  select(-background_image, -ratings, -updated, -tags, -short_screenshots, -parent_platforms, -platforms, -genres)
head(RAWGDataShowcase)
count(RAWGData)

RAWGDataPlaytime <- RAWGData %>%
  select(name, playtime)

finalData <- inner_join(VGSales,RAWGDataPlaytime,by=c("Name" = "name"))

ggplot(data = finalData, aes(x = playtime, y = Global_Sales)) + geom_point(alpha = 0.2) + geom_smooth(se=FALSE) +
  labs(x = "Average Playtime (hours)", y = "Global Sales (million copies sold)")


summary(RAWGData)

rest_rows <- as.vector(createDataPartition(finalData$Global_Sales, p = 0.8, list = FALSE))
test <- finalData[-rest_rows, ]
rest <- finalData[rest_rows, ]

train_rows <- as.vector(createDataPartition(rest$Global_Sales, p = 0.75, list = FALSE))

train <- rest[train_rows, ]
validate <- rest[-train_rows, ]
VGmodel <- lm(Global_Sales ~ Critic_Score + User_Score + playtime, data = train)

predictions <- add_predictions(validate, VGmodel)

ggplot(data = predictions, mapping = aes(x = Global_Sales, y = pred)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

resids <- add_residuals(validate, VGmodel)


ggplot(data = resids, mapping = aes(x = Critic_Score, y = resid)) + 
  geom_ref_line(h = 0) +
  geom_point()

clean <- predictions %>%
  filter(!is.na(pred))

summary(VGmodel)
R2(clean$pred, clean$Global_Sales)
MAE(clean$pred, clean$Global_Sales)
RMSE(clean$pred, clean$Global_Sales)

predictions <- add_predictions(test, VGmodel)
predictions

ggplot(data = predictions, mapping = aes(x = Global_Sales, y = pred)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

clean <- predictions %>%
  filter(!is.na(pred))

R2(clean$pred, clean$Global_Sales)
MAE(clean$pred, clean$Global_Sales)
RMSE(clean$pred, clean$Global_Sales)

resids <- add_residuals(test, VGmodel)

ggplot(data = resids, mapping = aes(x = Critic_Score, y = resid)) + 
  geom_ref_line(h = 0) +
  geom_point()








lowSales <-  finalData %>%
  filter(Global_Sales <= 5) %>%
  filter(Global_Sales >= 0.2) 

train_rows <- as.vector(createDataPartition(lowSales$Global_Sales, p = 0.8, list = FALSE))
train_rows

train <- lowSales[train_rows, ]
test <- lowSales[-train_rows, ]
VGmodel <- lm(Global_Sales ~ Critic_Score + User_Score + Genre + playtime, data = train)
summary(VGmodel)
predictions <- predict(VGmodel, test)
predictions

predictions <- add_predictions(test, VGmodel)
predictions

ggplot(data = predictions, mapping = aes(x = Global_Sales, y = pred)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

resids <- add_residuals(test, VGmodel)
resids

ggplot(data = resids, mapping = aes(x = Critic_Score, y = resid)) + 
  geom_ref_line(h = 0) +
  geom_point()

clean <- predictions %>%
  filter(!is.na(pred))

R2(clean$pred, clean$Global_Sales)
MAE(clean$pred, clean$Global_Sales)
RMSE(clean$pred, clean$Global_Sales)




rest_rows <- as.vector(createDataPartition(lowSales$Global_Sales, p = 0.8, list = FALSE))
test <- finalData[-rest_rows, ]
rest <- finalData[rest_rows, ]

train_rows <- as.vector(createDataPartition(rest$Global_Sales, p = 0.75, list = FALSE))

train <- rest[train_rows, ]
validate <- rest[-train_rows, ]
VGmodel <- lm(Global_Sales ~ Critic_Score + User_Score + Genre + playtime, data = train)

predictions <- add_predictions(validate, VGmodel)

ggplot(data = predictions, mapping = aes(x = Global_Sales, y = pred)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

resids <- add_residuals(validate, VGmodel)


ggplot(data = resids, mapping = aes(x = Critic_Score, y = resid)) + 
  geom_ref_line(h = 0) +
  geom_point()

clean <- predictions %>%
  filter(!is.na(pred))

summary(VGmodel)
R2(clean$pred, clean$Global_Sales)
MAE(clean$pred, clean$Global_Sales)
RMSE(clean$pred, clean$Global_Sales)

predictions <- add_predictions(test, VGmodel)
predictions

ggplot(data = predictions, mapping = aes(x = Global_Sales, y = pred)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

clean <- predictions %>%
  filter(!is.na(pred))

R2(clean$pred, clean$Global_Sales)
MAE(clean$pred, clean$Global_Sales)
RMSE(clean$pred, clean$Global_Sales)

resids <- add_residuals(test, VGmodel)

ggplot(data = resids, mapping = aes(x = Critic_Score, y = resid)) + 
  geom_ref_line(h = 0) +
  geom_point()










highSales <-  finalData %>%
  filter(Global_Sales > 5)
  

train_rows <- as.vector(createDataPartition(highSales$Global_Sales, p = 0.8, list = FALSE))
train_rows

train <- highSales[train_rows, ]
test <- highSales[-train_rows, ]
VGmodel <- lm(Global_Sales ~ Critic_Score + User_Score + Genre + playtime, data = train)
summary(VGmodel)
predictions <- predict(VGmodel, test)
predictions

predictions <- add_predictions(test, VGmodel)
predictions

ggplot(data = predictions, mapping = aes(x = Global_Sales, y = pred)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

resids <- add_residuals(test, VGmodel)
resids

ggplot(data = resids, mapping = aes(x = Critic_Score, y = resid)) + 
  geom_ref_line(h = 0) +
  geom_point()

clean <- predictions %>%
  filter(!is.na(pred))

R2(clean$pred, clean$Global_Sales)
MAE(clean$pred, clean$Global_Sales)
RMSE(clean$pred, clean$Global_Sales)





rest_rows <- as.vector(createDataPartition(highSales$Global_Sales, p = 0.8, list = FALSE))
test <- finalData[-rest_rows, ]
rest <- finalData[rest_rows, ]

train_rows <- as.vector(createDataPartition(rest$Global_Sales, p = 0.75, list = FALSE))

train <- rest[train_rows, ]
validate <- rest[-train_rows, ]
VGmodel <- lm(Global_Sales ~ Critic_Score + User_Score + Genre + playtime, data = train)

predictions <- add_predictions(validate, VGmodel)

ggplot(data = predictions, mapping = aes(x = Global_Sales, y = pred)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

resids <- add_residuals(validate, VGmodel)


ggplot(data = resids, mapping = aes(x = Critic_Score, y = resid)) + 
  geom_ref_line(h = 0) +
  geom_point()

clean <- predictions %>%
  filter(!is.na(pred))

summary(VGmodel)
R2(clean$pred, clean$Global_Sales)
MAE(clean$pred, clean$Global_Sales)
RMSE(clean$pred, clean$Global_Sales)

predictions <- add_predictions(test, VGmodel)
predictions

ggplot(data = predictions, mapping = aes(x = Global_Sales, y = pred)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

clean <- predictions %>%
  filter(!is.na(pred))

R2(clean$pred, clean$Global_Sales)
MAE(clean$pred, clean$Global_Sales)
RMSE(clean$pred, clean$Global_Sales)

resids <- add_residuals(test, VGmodel)

ggplot(data = resids, mapping = aes(x = Critic_Score, y = resid)) + 
  geom_ref_line(h = 0) +
  geom_point()







rest_rows <- as.vector(createDataPartition(VGSales$Global_Sales, p = 0.8, list = FALSE))
test <- finalData[-rest_rows, ]
rest <- finalData[rest_rows, ]

train_rows <- as.vector(createDataPartition(rest$Global_Sales, p = 0.75, list = FALSE))

train <- rest[train_rows, ]
validate <- rest[-train_rows, ]
VGmodel <- lm(Global_Sales ~ Critic_Score + User_Score, data = train)

predictions <- add_predictions(validate, VGmodel)

ggplot(data = predictions, mapping = aes(x = Global_Sales, y = pred)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

resids <- add_residuals(validate, VGmodel)


ggplot(data = resids, mapping = aes(x = Critic_Score, y = resid)) + 
  geom_ref_line(h = 0) +
  geom_point()

clean <- predictions %>%
  filter(!is.na(pred))

summary(VGmodel)
R2(clean$pred, clean$Global_Sales)
MAE(clean$pred, clean$Global_Sales)
RMSE(clean$pred, clean$Global_Sales)

predictions <- add_predictions(test, VGmodel)
predictions

ggplot(data = predictions, mapping = aes(x = Global_Sales, y = pred)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

clean <- predictions %>%
  filter(!is.na(pred))

R2(clean$pred, clean$Global_Sales)
MAE(clean$pred, clean$Global_Sales)
RMSE(clean$pred, clean$Global_Sales)

resids <- add_residuals(test, VGmodel)

ggplot(data = resids, mapping = aes(x = Critic_Score, y = resid)) + 
  geom_ref_line(h = 0) +
  geom_point()
