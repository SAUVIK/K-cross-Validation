
#Installing and importing the libraries need for this modelling
library(caret)
install.packages("gmodels")
install.packages("Hmisc")
install.packages("pROC")
install.packages("ResourceSelection")
install.packages("car")
install.packages("caret")
install.packages("dplyr")
install.packages("Rcpp")
library(Rcpp)
library(gmodels)
library(Hmisc)
library(pROC)
library(ResourceSelection)
library(car)
library(caret)
library(dplyr)
install.packages("InformationValue")
library(InformationValue)

#Function starts ------------------------------------------>>>>>>>>>>>>>>>>>>>>>>>>>>>

#Defining the user defined function for K-cross validation
k_cross <- function(dataset,folds,equation,model,dependent_variable) {
  
#Initialising the Accuracy and mean_square_error for benifit of future compuatation.  
accuracy <- 0
mean_square_error <- 0
#Randomising data to exclude out of sample of error.
dataset_frame <- dataset[sample(nrow(dataset)),]
dataset_interval <- cut( seq(1,nrow(dataset_frame)) , breaks=folds, labels=FALSE)

for(i in 1:folds){
  testIndexes <- which(dataset_interval==i,arr.ind=TRUE)
  
  #Dividing data into test and train based on fold index 
  test_data <- dataset[testIndexes,]
  train_data <- dataset[-testIndexes,]
  
  #Executing the respective model based on user input
  if(model=="glm"){
  generalised_linear_model <- glm(formula = equation,data=train_data,family=binomial)
  prediction <- predict(generalised_linear_model,newdata=test_data,type=c("response"))
  predicted_value<- ifelse(prediction>0.5,1,0)
  xtab <- table(predicted_value,test_data$Survived)
  
  #calculating accuracy of each fold and storing it to further average it out at later stage.
  accuracy[i] <- sum(diag(xtab))/sum(xtab)
  }
  
  else if(model=="lm"){
    linearMod <- lm(formula = equation, data=train_data)
    summary <- summary(linearMod)
    mean_square_error[i] <- mean(summary$residuals^2)
  }
}
if(model=="glm") {
cat("Average accuracy",mean(accuracy))
}
if(model=="lm"){
cat("Average mean square error",mean(mean_square_error))
}
}
#<<<<<<<<<<<<<<<<<<<<---------------------------------------Function ENDS

##################################---Calculating the accuracy by dividing it into test & train set---#####################################
cat("\014") # Clearing the screen
#Reading the data set provided
titanic_data <- read.csv("/Users/stacy/downloads/train-5.csv")
View(titanic_data)
ncol(titanic_data)
str(titanic_data)

#Reducing data points which don't seem to contribute significantly on the dependent variable(survival) 
reduced <- subset(titanic_data,select=-c(PassengerId,Embarked,Ticket,Cabin,Name,Fare))
View(reduced)

#Checking the number of NA's in the rest of the dataset for further imputation.
table(is.na(reduced$Sex))
table(is.na(reduced$Age))
table(is.na(reduced$Pclass))
table(is.na(reduced$SibSp))
table(is.na(reduced$Parch))
summary(reduced)
#Impute the missing value of age with the mean age of the rest of the values
removeAge_dataset <- reduced[!is.na(reduced$Age),]
View(removeAge_dataset)
meanAge <- mean(removeAge_dataset$Age)
reduced$Age[is.na(reduced$Age)]= meanAge
#Checking if there is still null values in age
table(is.na(reduced$Age))
Imputed_dataset <- reduced
#Setting the seed for reproducibility.
set.seed(1234)
random <- runif(nrow(Imputed_dataset))
#Dividing the dataset to training and test
Train_Imputed_dataset <- Imputed_dataset[random<0.7,]
Test_Imputed_dataset <- Imputed_dataset[random>=0.7,]
CrossTable((Train_Imputed_dataset$Survived))
CrossTable(Test_Imputed_dataset$Survived)
nrow(Train_Imputed_dataset)
ncol(Train_Imputed_dataset)
#Check the model if the variables included are all significant . If not then remove the insignificant variables based on Pvalues
titanic_prediction_imputed <- glm(formula = Survived ~ Pclass+Sex+Age+SibSp+Parch,data=Train_Imputed_dataset)
prediction <- predict(titanic_prediction_imputed,newdata=Test_Imputed_dataset,type=c("response"))
predicted_value<- ifelse(prediction>0.5,1,0)
xtab <- table(predicted_value,Test_Imputed_dataset$Survived)
#Checking the accuracy of the model on the test data.
cat("Accuracy of model achieved on dividing it into trainig and test data set is :",sum(diag(xtab))/sum(xtab))



##################K-fold-cross validation-Check with the dataset.########################
equation <- Survived ~ Pclass+Sex+Age+SibSp+Parch #formula to be used in the model for K-fold validation
folds <-5 #number of folds in K-fold
model <- "glm" #model
dependent <- Imputed_dataset$Survived #Depedent varaible to calculate the accuracy of each model
View(Imputed_dataset)
k_cross(Imputed_dataset,folds,equation,model,dependent)
#######################################################################











