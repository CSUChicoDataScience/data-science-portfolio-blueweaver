library(tidyverse)
library(rvest)

vgChartsG <- tibble(Name = character(), Global_Sales = double())
categories <- c("Global")
for (category in categories){
  vgChartsG.data <- read_html(paste("https://www.vgchartz.com/yearly/2017/", category, sep = "")) %>%
    html_nodes("body")
  
  vgChartsG.Name <- vgChartsG.data %>%
    xml_find_all("//*[(@id = 'new_entry')]//td+//td//a") %>% 
    html_text() 
  
  vgChartsG.gs <- vgChartsG.data %>%
    xml_find_all("//*[(@id = 'new_entry')]//td[(((count(preceding-sibling::*) + 1) = 5) and parent::*)]") %>% 
    html_text() %>%
    as.double()
  
  vgChartsG <- add_row(vgChartsG, Name = vgChartsG.Name, Global_Sales = vgChartsG.gs)
}

metaC <- tibble(Name = character(), Critic_Score = integer(), User_Score = double())
categories <- c("2017", "2018")

for (category in categories){
  metaC.data <- read_html(paste("https://www.metacritic.com/browse/games/score/metascore/year/all/filtered?view=condensed&sort=desc&year_selected="),category, sep = "") %>%
    html_nodes("body")
  
  metaC.name <- metaC.data %>%
    xml_find_all("//a//h3") %>%
    html_text() 
  
  metaC.cs <- metaC.data %>%
    xml_find_all("//*[contains(concat( '', @class, '' ), concat( '', 'clamp-metascore', '' ))]//*[contains(concat( '', @class, '' ), concat( '', 'metascore_anchor', '' ))]") %>%
    html_text() %>%
    as.integer()
  
  metaC.us <- metaC.data %>%
    xml_find_all("//*[contains(concat( '', @class, '' ), concat( '', 'user', '' ))]") %>%
    html_text() %>%
    as.double()
  
  metaC <- add_row(metaC, Name = metaC.name, Critic_Score = metaC.cs, User_Score = metaC.us)
  
}

VGSales <- read_csv("Video_Games_Sales.csv")
set.seed(12345)



salesByGenre <- VGSales %>%
  select(Genre, NA_Sales, JP_Sales, EU_Sales, Other_Sales) %>%
  group_by(Genre) %>%
  summarise(NorthAmerica = sum(NA_Sales), Japan = sum(JP_Sales), Europe = sum(EU_Sales), Other = sum(Other_Sales)) %>%
  gather("Region", "Games_Sold", -Genre)

VGmodel <- lm(Global_Sales ~ Critic_Score + User_Score + Genre + Platform, data = VGSales)
summary(VGmodel)


newGames <- VGSales %>%
  filter(Year_of_Release >= 2003)
train_rows <- as.vector(createDataPartition(newGames$Global_Sales, p = 0.8, list = FALSE))
train_rows

train <- newGames[train_rows, ]
test <- newGames[-train_rows, ]
VGmodel <- lm(Global_Sales ~ Critic_Score + User_Score + Genre + Platform , data = train)

predictions <- predict(VGmodel, test)
predictions

predictions <- add_predictions(test, VGmodel)
predictions

ggplot(data = predictions, mapping = aes(x = Global_Sales, y = pred)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

resids <- add_residuals(test, VGmodel)
resids

ggplot(data = resids, mapping = aes(x = Global_Sales, y = resid)) + 
  geom_ref_line(h = 0) +
  geom_point()

train_rows <- as.vector(createDataPartition(VGSales$NA_Sales, p = 0.8, list = FALSE))
train_rows

train <- VGSales[train_rows, ]
test <- VGSales[-train_rows, ]
VGmodel <- lm(NA_Sales ~ Critic_Score + User_Score + Genre + Rating , data = train)

predictions <- predict(VGmodel, test)
predictions

predictions <- add_predictions(test, VGmodel)
predictions

ggplot(data = predictions, mapping = aes(x = NA_Sales, y = pred)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

resids <- add_residuals(test, VGmodel)
resids

ggplot(data = resids, mapping = aes(x = NA_Sales, y = resid)) + 
  geom_ref_line(h = 0) +
  geom_point()


lowSales <-  VGSales %>%
  filter(Global_Sales <= 5) %>%
  filter(Global_Sales >= 0.2) 

train_rows <- as.vector(createDataPartition(lowSales$Global_Sales, p = 0.8, list = FALSE))
train_rows

train <- lowSales[train_rows, ]
test <- lowSales[-train_rows, ]
VGmodel <- lm(Global_Sales ~ Critic_Score + User_Score + Genre, data = train)
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



train_rows <- as.vector(createDataPartition(VGSales$Global_Sales, p = 0.8, list = FALSE))
train_rows

train <- VGSales[train_rows, ]
test <- VGSales[-train_rows, ]
VGmodel <- lm(Global_Sales ~ Critic_Score + User_Score + Genre, data = train)

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



newGames <- VGSales %>%
  filter(Year_of_Release >= 2003)
train_rows <- as.vector(createDataPartition(newGames$Global_Sales, p = 0.8, list = FALSE))
train_rows

train <- newGames[train_rows, ]
test <- newGames[-train_rows, ]
VGmodel <- lm(Global_Sales ~ Critic_Score + User_Score  + Platform , data = train)

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


R2(predictions$pred, predictions$Global_Sales)
MAE(predictions$pred, predictions$Global_Sales)
RMSE(predictions$pred, predictions$Global_Sales)

clean <- predictions %>%
  filter(!is.na(pred))
R2(clean$pred, clean$Global_Sales)


library(httr)
library(jsonlite)

url <-"https://api.rawg.io/api/games?key=5e133e1135f541f395101593d7cad2b8&dates=2017-01-01,2020-09-30"



baseurl <- "https://api.rawg.io/api/games?key=8fdd449e387c4197ad765b1222eafa73&page_size=40&dates=1980-01-01,2017-01-01&ordering=metacritic"
pages <- list()
for(i in 1:10){
  mydata <- fromJSON(paste0(baseurl, "&page=", i))
  pages[[i+1]] <- mydata$results
}
RAWGData <- rbind_pages(pages)
