data("iris")
#install.packages("caret")
#install.packages("C50")
library(caret)
library(C50)

inTraininglocal <- createDataPartition(iris$Species,p=.70,list=F)
training <- iris[inTraininglocal,]
testing <- iris[-inTraininglocal,]

#Model Building
model <- C5.0(training$Species~.,data=training) 

#Generate the model summary
summary(model)
#Predict the test data set
pred <- predict.C5.0(model,testing[,-5])
a <- table(testing$Species,pred)
sum(diag(a))/sum(a)

plot(model)



#### Boosting

inTraininglocal <- createDataPartition(iris$Species,p=.70,list=F)
training <- iris[inTraininglocal,]
testing <- iris[-inTraininglocal,]
#Model Building
model <- C5.0(training$Species~.,data=training,trials=20) #Trials - Boosting parameters
#Generate the model summary
summary(model)
#Predict the test data set
pred <- predict.C5.0(model,testing[,-5])
a <- table(testing$Species,pred)
sum(diag(a))/sum(a)
plot(model)





##### Bagging 
acc <- c()
for (i in 1:100) 
  {
  print(i)
  ##Data Partition
  inTraininglocal <- createDataPartition(iris$Species,p=.70,list=F)
  training1 <- iris[inTraininglocal,]
  testing <- iris[-inTraininglocal,]
  ## Model Building
  fittree <- C5.0(training1$Species~.,data=training1)
  #Predicting
  pred <- predict.C5.0(fittree,testing[,-5])
  a<-table(testing$Species,pred)
  #Accuracy
  acc<-c(acc,sum(diag(a))/sum(a))
  }
summary(acc)
boxplot(acc)






##### Bagging and Boosting
acc <- c()
for (i in 1:100) 
{
  print(i)
  ##Data Partition
  inTraininglocal <- createDataPartition(iris$Species,p=.70,list=F)
  training1 <- iris[inTraininglocal,]
  testing <- iris[-inTraininglocal,]
  ## Model Building
  fittree <- C5.0(training1$Species~.,data=training1,trials=20)
  #Predicting
  pred <- predict.C5.0(fittree,testing[,-5])
  a<-table(testing$Species,pred)
  #Accuracy
  acc<-c(acc,sum(diag(a))/sum(a))
}
summary(acc)
boxplot(acc)





