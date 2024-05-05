dataset <- read.csv("D:/DESKTOP/Cogntive Analytics/Experiments/Datasets/Salary.csv")
library(caTools)
split <- sample.split(dataset$Salary, SplitRatio = 0.7)
trainingset <- subset(dataset, split == TRUE)
testset <- subset(dataset, split == FALSE)
lm.r <- lm(formula = Salary ~ YearsExperience, data = trainingset)
coef(lm.r)
ypred <- predict(lm.r, newdata = testset)
library(ggplot2)
ggplot() + geom_point(aes(x = trainingset$YearsExperience,
                          y = trainingset$Salary), colour = 'red') 
+ geom_line(aes(x = trainingset$YearsExperience, y = predict(lm.r, newdata = trainingset)),
           colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

ggplot() + geom_point(aes(x = testset$YearsExperience,
                          y = testset$Salary),
                      colour = 'red') +
  geom_line(aes(x = trainingset$YearsExperience,
                y = predict(lm.r, newdata = trainingset)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')



library(ClusterR)
library(cluster)

data(iris)
str(iris)

iris_1 <- iris[, -5]
set.seed(240)

kmeans.re <- kmeans(iris_1, centers = 3, nstart = 20)
kmeans.re
kmeans.re$cluster

cm <- table(iris$Species, kmeans.re$cluster)
cm

plot(iris_1[c("Sepal.Length", "Sepal.Width")])
plot(iris_1[c("Sepal.Length", "Sepal.Width")], col = kmeans.re$cluster)

plot(iris_1[c("Sepal.Length", "Sepal.Width")], col = kmeans.re$cluster, main = "K-means with 3 clusters")
kmeans.re$centers
kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")]
points(kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex = 3)

y_kmeans <- kmeans.re$cluster
clusplot(iris_1[, c("Sepal.Length", "Sepal.Width")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster iris"),
         xlab = 'Sepal.Length',
         ylab = 'Sepal.Width')

