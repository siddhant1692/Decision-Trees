



company <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\Decision Trees\\Company_Data_r.csv')

#install.packages("caret")
#install.packages("C50")
library(caret)
library(C50)

inTraininglocal <- createDataPartition(company$Target.Sales,p=.70,list=F)
training <- company[inTraininglocal,]
testing <- company[-inTraininglocal,]

#Model Building
model <- C5.0(training$Target.Sales~.,data=training) 

#Generate the model summary
summary(model)
#Predict the test data set
pred <- predict.C5.0(model,testing[,-12])
a <- table(testing$Target.Sales,pred)
sum(diag(a))/sum(a)

plot(model)



#### Boosting

inTraininglocal <- createDataPartition(company$Target.Sales,p=.70,list=F)
training <- company[inTraininglocal,]
testing <- company[-inTraininglocal,]
#Model Building
model <- C5.0(training$Target.Sales~.,data=training,trials=25) #Trials - Boosting parameters
#Generate the model summary
summary(model)
#Predict the test data set
pred <- predict.C5.0(model,testing[,-12])
a <- table(testing$Target.Sales,pred)
sum(diag(a))/sum(a)

plot(model)




##### Bagging 
acc <- c()
for (i in 1:100) 
  {
  print(i)
  ##Data Partition
  inTraininglocal <- createDataPartition(company$Target.Sales,p=.70,list=F)
  training1 <- company[inTraininglocal,]
  testing <- company[-inTraininglocal,]
  ## Model Building
  fittree <- C5.0(training1$Target.Sales~.,data=training1)
  #Predicting
  pred <- predict.C5.0(fittree,testing[,-12])
  a<-table(testing$Target.Sales,pred)
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
  inTraininglocal <- createDataPartition(company$Target.Sales,p=.70,list=F)
  training1 <- company[inTraininglocal,]
  testing <- company[-inTraininglocal,]
  ## Model Building
  fittree <- C5.0(training1$Target.Sales~.,data=training1,trials=20)
  #Predicting
  pred <- predict.C5.0(fittree,testing[,-12])
  a<-table(testing$Target.Sales,pred)
  #Accuracy
  acc<-c(acc,sum(diag(a))/sum(a))
}
summary(acc)
boxplot(acc)





