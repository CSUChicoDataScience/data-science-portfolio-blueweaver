library(tidyverse)

VGSales <- read_csv("Video_Games_Sales.csv")
View(VGSales)

VGSales$User_Score <- VGSales$User_Score * 10
ggplot(data = VGSales, aes(x = Global_Sales)) + geom_histogram() + facet_wrap(Platform~Genre)

salesByGenre <- VGSales %>%
  select(Platform, Genre, Global_Sales) %>%
  group_by(Platform, Genre) %>%
  summarise(totalSales = sum(Global_Sales))

salesByGenreNaAndJp <- VGSales %>%
  select(Platform, Genre, NA_Sales, JP_Sales) %>%
  group_by(Genre) %>%
  summarise(totalSalesNa = sum(NA_Sales), totalSalesJp = sum(JP_Sales))

salesByGenre <- VGSales %>%
  select(Genre, NA_Sales, JP_Sales, EU_Sales, Other_Sales) %>%
  group_by(Genre) %>%
  summarise(totalSalesNa = sum(NA_Sales), totalSalesJp = sum(JP_Sales), totalSalesEu = sum(EU_Sales), totalSalesOther = sum(Other_Sales)) %>%
  gather("Region", "Games_Sold", -Genre)



dat_long <- salesByGenreNaAndJp %>%
  gather("Stat", "Value", -Genre)

ggplot(data = salesByGenre) + geom_bar(aes(x=Genre, y=Games_Sold, fill = Region),stat = "identity", position = "dodge") +
  labs(y = "Number of games sold (millions)") + scale_fill_discrete(labels=c("Europe", "Japan", "North America", "Other"))






ggplot(data = salesByGenreNaAndJp) + geom_boxplot(aes(x=Genre, y=totalSalesJp), position = position_dodge(1)) 

ggplot(data = salesByGenreNaAndJp) + geom_bar(aes(x=Genre, y=totalSalesNa),stat = "identity") 
ggplot(data = salesByGenreNaAndJp) + geom_bar(aes(x=Genre, y=totalSalesJp),stat = "identity")

ggplot(data = salesByGenre) + geom_boxplot(aes(x = Genre, y = totalSales))
View(salesByGenre)
print(VGSales)

head(VGSales)
View(salesByGenreNA)
summary(VGSales)

ggplot(data = VGSales) + geom_histogram(mapping = aes(x = Global_Sales), binwidth = 1) + xlim(0,40) + ylim(0, 3000) + geom_vline(aes(xintercept=mean(Global_Sales, na.rm=T)), color="red", linetype="dashed", size=1)

#ggplot(data = diamonds, aes(x = price)) + geom_histogram(binwidth = 2000) + geom_vline(aes(xintercept=mean(price, na.rm=T)), color="red", linetype="dashed", size=1)

ggplot(data = VGSales, aes(x = Critic_Score, y = Global_Sales)) + geom_point(alpha = 0.2) + geom_smooth(se=FALSE) + ylim(0, 40)
ggplot(data = VGSales, aes(x = User_Score, y = Global_Sales)) + geom_point(alpha = 0.2) + geom_smooth(se=FALSE) + ylim(0, 40)


ggplot(data = VGSales, aes(x = Rating, y = NA_Sales)) + geom_boxplot() + ylim(0,20)

