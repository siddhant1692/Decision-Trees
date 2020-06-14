



fraud <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\Decision Trees\\Fraud_check_r.csv')

#install.packages("caret")
#install.packages("C50")
library(caret)
library(C50)

inTraininglocal <- createDataPartition(fraud$Status,p=.70,list=F)
training <- fraud[inTraininglocal,]
testing <- fraud[-inTraininglocal,]

#Model Building
model <- C5.0(training$Status~.,data=training) 

#Generate the model summary
summary(model)
#Predict the test data set
pred <- predict.C5.0(model,testing[,-7])
a <- table(testing$Status,pred)
sum(diag(a))/sum(a)

plot(model)



#### Boosting

inTraininglocal <- createDataPartition(fraud$Status,p=.70,list=F)
training <- fraud[inTraininglocal,]
testing <- fraud[-inTraininglocal,]
#Model Building
model <- C5.0(training$Status~.,data=training,trials=25) #Trials - Boosting parameters
#Generate the model summary
summary(model)
#Predict the test data set
pred <- predict.C5.0(model,testing[,-7])
a <- table(testing$Status,pred)
sum(diag(a))/sum(a)

plot(model)




##### Bagging 
acc <- c()
for (i in 1:100) 
  {
  print(i)
  ##Data Partition
  inTraininglocal <- createDataPartition(fraud$Status,p=.70,list=F)
  training1 <- fraud[inTraininglocal,]
  testing <- fraud[-inTraininglocal,]
  ## Model Building
  fittree <- C5.0(training1$Status~.,data=training1)
  #Predicting
  pred <- predict.C5.0(fittree,testing[,-7])
  a<-table(testing$Status,pred)
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
  inTraininglocal <- createDataPartition(fraud$Status,p=.70,list=F)
  training1 <- fraud[inTraininglocal,]
  testing <- fraud[-inTraininglocal,]
  ## Model Building
  fittree <- C5.0(training1$Status~.,data=training1,trials=20)
  #Predicting
  pred <- predict.C5.0(fittree,testing[,-7])
  a<-table(testing$Status,pred)
  #Accuracy
  acc<-c(acc,sum(diag(a))/sum(a))
}
summary(acc)
boxplot(acc)





