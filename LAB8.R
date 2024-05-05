library(caret)
library(ggplot2)
data <- read.csv("D:\\DESKTOP\\Cogntive Analytics\\Experiments\\Datasets\\GenderClassification.csv", stringsAsFactors = TRUE)
# Using set.seed()
# Generating random number
set.seed(10)
# Cleaning the data set
data$Favorite.Color <- as.numeric(data$Favorite.Color)
data$Favorite.Music.Genre <- as.numeric(data$Favorite.Music.Genre)
data$Favorite.Beverage <- as.numeric(data$Favorite.Beverage)
data$Favorite.Soft.Drink <- as.numeric(data$Favorite.Soft.Drink)
# Split into train and test data set
TrainingSize <- createDataPartition(data$Gender, p = 0.8, list = FALSE)
TrainingData <- data[TrainingSize, ]
TestingData <- data[-TrainingSize, ]
# Using CARET package
# Importing the library
library(caret)
# Using the train() available in
# Caret package
model <- train(Gender ~ .,
    data = TrainingData,
    method = "svmPoly",
    na.action = na.omit,
    preProcess = c("scale", "center"),
    trControl = trainControl(method = "none"),
    tuneGrid = data.frame(degree = 1, scale = 1, C = 1)
)
model.cv <- train(Gender ~ .,
    data = TrainingData,
    method = "svmPoly",
    na.action = na.omit,
    preProcess = c("scale", "center"),
    trControl = trainControl(method = "cv", number = 6),
    tuneGrid = data.frame(degree = 1, scale = 1, C = 1)
)
print(model)
print(model.cv)


ggplot2(data, aes(Favorite.Color)) + geom_bar(fill = "#0073C2FF")
