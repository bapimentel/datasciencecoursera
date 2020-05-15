setwd("~/GitHub/datasciencecoursera")

library(randomForest)

data_training <- read.csv(file = 'Data/pml-training.csv')
data_training <- data_training[,-append(append(1:7, 12:36), 50:159)]
#data_training <- data_training[,-(1:7)]
data_training[is.na(data_training)] <- 0

data_testing <- read.csv(file = 'Data/pml-testing.csv')
data_testing <- data_testing[,-append(append(1:7, 12:36), 50:159)]
data_testing[is.na(data_testing)] <- 0

# Fit the models
require(splines)

set.seed(1000)
n_train <- nrow(data_training)
n_folds <- 10
n_df <- 30
df <- 1:n_df
folds_i <- sample(rep(1:n_folds, length.out = n_train))
cv_tmp <- double(n_folds)
for (k in 1:n_folds) {
  print(k)
  test_i <- which(folds_i == k)
  train_xy <- data_training[-test_i, ]
  x_train <- subset(train_xy, select = -classe)
  y_train <- train_xy$classe
  fitted_model <- randomForest(x_train, y_train, prox=TRUE, ntree=5)
  
  test_xy <- data_training[test_i, ]
  x_test <- subset(test_xy, select = -classe)
  y_test <- test_xy$classe
  
  pred<-predict(fitted_model,x_test) #Predictions on Test Set
  print(sum(y_test == pred)/length(y_test))
  cv_tmp[k]= sum(y_test == pred)/length(y_test) #Test Error
}
print(mean(cv_tmp))
print(sd(cv_tmp))