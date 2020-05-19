setwd("~/GitHub/datasciencecoursera")

library(randomForest)
library(rpart)
library(rpart.plot)
require(splines)


#-------- Pre-processing

#Load data and remove variables
pre_process <- function(name_file){
  data <- read.csv(file = paste0('Data/',name_file,'.csv'))
  data <- data[,-append(append(1:7, 12:36), 50:159)]
  data[is.na(data)] <- 0
  return(data)
}

data_training <- pre_process('pml-training')
data_testing <- pre_process('pml-testing')


#-------- Cross validation

#Compute error
compute_error <- function(y_pred, y_test){
  error <- 1.0 - sum(y_test == y_pred)/length(y_test)
  return(error)
}

set.seed(1000)
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
  fitted_RF <- randomForest(x_train, y_train, prox=TRUE, ntree=200)
  #Decision Tree (DT)
  fitted_DT <- rpart(formula = classe ~ ., data = train_xy,control = rpart.control(minsplit = 10, maxdepth = 15))
  
  #Predictions on testing set
  pred_RF <- predict(fitted_RF,x_test) 
  error_RF <- compute_error(pred_RF, y_test)
  print(error_RF)
  
  pred_DT <- predict(fitted_DT, newdata = x_test, type = "class") 
  error_DT <- compute_error(pred_DT, y_test)
  print(error_DT)
  
  cv_tmp[k,1] <- error_RF
  cv_tmp[k,2] <- error_DT
}

#Print mean and standard deviation
print_result <- function(name, result){
  cat('\n', name)
  cat(': Mean = ', mean(result))
  cat(' Std = ', sd(result))
}

#-------- Results
cat('\n')
cat("Results:")
print_result('RF', cv_tmp[,1])
print_result('DT', cv_tmp[,2])

#Prediction of test data
pred_RF <- predict(fitted_RF,data_testing) 
cat('\n Test prediction (RF): ', as.vector(pred_RF))

pred_DT <- predict(fitted_DT, newdata = data_testing, type = "class") 
cat('\n Test prediction (DT): ', as.vector(pred_DT))

#-------- Figures
#Importances
importances <- importance(fitted_RF)
variables_name = row.names(importances)
barplot(as.vector(importances), main="Importance of variables",xlab=NULL, ylab = "Importance", horiz=FALSE, names.arg=variables_name, las=2, cex.names = 0.7)

#Tree
rpart.plot(fitted_DT)

#Number of trees versus error
layout(matrix(c(1,2),nrow=1), width=c(4,1))
plot(fitted_RF, main="Number of trees versus error", col=10:15)
legend("top", inset=c(0,0), colnames(fitted_RF$err.rate),col=10:15,cex=0.5,fill=10:15)

#Most important variables
plot(data_training[,1:3], col=data_training$classe)

#Pair of variables
a = 10
b = 5
plot(x=data_training[,a], y=data_training[,b], xlab=variables_name[a], ylab=variables_name[b], col=data_training$classe)

