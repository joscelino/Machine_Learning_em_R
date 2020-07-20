library(ggplot2)

data("iris")
str(iris)

qplot(Petal.Length, Petal.Width, data=iris, color=Species)

# Support Vector Machine
library(e1071) 
mymodel <-svm(Species~., data = iris)
summary(mymodel)

plot(mymodel, data = iris, Petal.Width~Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))

# Tuning
set.seed(123)
tmodel <- tune(svm, Species~., data = iris, 
              ranges = list(epsilon = seq(0, 1, 0.1), cost = 2^(2:9)))
plot(tmodel)
summary(tmodel)

# best model
mymodel <- tmodel$best.model
summary(mymodel)

# Confusion Matrix
pred <- predict(mymodel, iris)
tab <- table(Predicted = pred, Actual = iris$Species)
tab

sum(diag(tab))/sum(tab)
