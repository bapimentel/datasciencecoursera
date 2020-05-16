setwd("~/GitHub/datasciencecoursera")

library(randomForest)
library(rpart)
require(splines)

pre_process <- function(name_file){
  data <- read.csv(file = paste0('Data/',name_file,'.csv'))
  data <- data[,-append(append(1:7, 12:36), 50:159)]
  data[is.na(data)] <- 0
  return(data)
}

compute_error <- function(y_pred, y_test){
  error <- 1.0 - sum(y_test == pred_RF)/length(y_test)
  return(error)
}

data_training <- pre_process('pml-training')
data_testing <- pre_process('pml-testing')

#set.seed(1000)
n_train <- nrow(data_training)
n_folds <- 10
folds_i <- sample(rep(1:n_folds, length.out = n_train))
cv_tmp <- matrix(NA, n_folds, 2)
for (k in 1:n_folds) {
  print(k)
  test_i <- which(folds_i == k)
  
  #Training set
  train_xy <- data_training[-test_i, ]
  x_train <- subset(train_xy, select = -classe)
  y_train <- train_xy$classe
  
  #Testing set
  test_xy <- data_training[test_i, ]
  x_test <- subset(test_xy, select = -classe)
  y_test <- test_xy$classe
  
  #Random Forest (RF)
  fitted_RF <- randomForest(x_train, y_train, prox=TRUE, ntree=10)
  #Decision Tree (DT)
  fitted_DT <- rpart(formula = classe ~ ., data = train_xy,control = rpart.control(minsplit = 10))
  
  #Predictions on testing set
  pred_RF <- predict(fitted_RF,x_test) 
  error_RF <- compute_error(pred_RF, y_test)
  print(error_RF)
  
  pred_DT <- predict(fitted_DT,x_test) 
  error_DT <- compute_error(pred_DT, y_test)
  print(error_DT)
  
  
  cv_tmp[k,1] <- error_RF
  cv_tmp[k,2] <- error_DT
}
cat('\n')
cat("Results:")
cat(mean(cv_tmp[,1]))
cat(sd(cv_tmp[,1]))
cat(mean(cv_tmp[,2]))
cat(sd(cv_tmp[,2]))