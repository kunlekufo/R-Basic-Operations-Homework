
library(dplyr)
library(ggplot2)

data <- read.csv("C:\Users\Kunle Kuforiji\Downloads\train.csv")
head(data)
str(data)
summary(data)

data$Age[is.na(data$Age)] <- median(data$Age, na.rm = TRUE)
data$RoomService[is.na(data$RoomService)] <- median(data$RoomService, na.rm = TRUE)


for (column in c("HomePlanet", "CryoSleep", "Cabin", "Destination", "VIP")) {
  mode <- as.character(sort(table(data[[column]]), decreasing = TRUE)[1])
  data[[column]][is.na(data[[column]])] <- mode
}


ggplot(data, aes(x=HomePlanet)) + geom_bar() + labs(title="Distribution of HomePlanet")
ggplot(data, aes(x=CryoSleep)) + geom_bar() + labs(title="Distribution of CryoSleep")



ggplot(data, aes(x=Age)) + geom_histogram(bins=30, fill="blue", color="black") +
  labs(title="Age Distribution")


library(corrplot)
expenditure_data <- data[c("RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck")]
cor_matrix <- cor(expenditure_data, use="complete.obs")
corrplot(cor_matrix, method="circle")


planet_transported <- table(data$HomePlanet, data$Transported)
vip_transported <- table(data$VIP, data$Transported)

barplot(planet_transported, beside=TRUE, legend=TRUE, 
        col=c("red", "green"), 
        main="HomePlanet vs Transported Status")
barplot(vip_transported, beside=TRUE, legend=TRUE, 
        col=c("red", "green"), 
        main="VIP Status vs Transported Status")


ggsave("Age_Distribution.png")


