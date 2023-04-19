dataframe<- read.csv('D:\\thapar\\topsis_suddhasattwa_khan_102003687\\diabetes_prediction_dataset.csv')
df<- subset(dataframe,smoking_history!='No Info')
df
library(mltools)
library(data.table)
df$smoking_history <- as.factor(df$smoking_history)
newdata <- one_hot(as.data.table(df))
newdata
newdata$gender <- as.factor(newdata$gender)
newdata <- one_hot(as.data.table(newdata))
newdata
nrow(newdata)
newdata<- newdata[1:30000]
newdata
arr1<- c()
arr2<- c()
arr3<- c()
arr4<- c()
arr5<- c()
arr6<- c()
arr7<- c()
arr8<- c()
arr9<- c()
arr10<- c()

# for sample 1
df1 <- newdata[sample(nrow(newdata), size=3000), ]
df1$bmi<- scale(df1$bmi)
df1$HbA1c_level<- scale(df1$HbA1c_level)
df1$blood_glucose_level<- scale(df1$blood_glucose_level)
df1$age<- scale(df1$age)
df1

library(caTools)
split=sample.split(df1$diabetes,SplitRatio = 0.8)
training_set<- subset(df1,split==TRUE)
test_set<- subset(df1,split==FALSE)
y_test<- test_set$diabetes

#install.packages("kernlab")                    
library(kernlab)

#test$neighbourhood
bestAccuracy1=0
bestKernel=0
bestNu=0
bestEpsilon=0
iteration=0
kernelList=c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','anovadot')
fitnessfunction<- function(k,n,e){
  model<- ksvm(as.factor(diabetes) ~ .,data=training_set,kernel=k,nu=n,epsilon=e,kpar=list())
  y_pred<- predict(model,newdata=test_set[,-15])
  conf_matrix <- table(y_test, y_pred)
  accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
  return(accuracy)
}
#acc<- fitnessfunction('rbfdot',0.2309025,0.8109799)
for(i in 1:100){
  k=sample(kernelList,1)
  n=runif(1)
  e=runif(1)
  Accuracy= fitnessfunction(k,n,e)
  if(Accuracy>bestAccuracy1){
    bestKernel=k
    bestNu=n
    bestEpsilon=e
    bestAccuracy1=Accuracy
  }
  arr1 <- c(arr1, bestAccuracy1)
  print(k)
  print(n)
  print(e)
  print(Accuracy)
}


#forsample 2
df2 <- newdata[sample(nrow(newdata), size=3000), ]
df2$bmi<- scale(df2$bmi)
df2$HbA1c_level<- scale(df2$HbA1c_level)
df2$blood_glucose_level<- scale(df2$blood_glucose_level)
df2$age<- scale(df2$age)
df2

library(caTools)
split=sample.split(df2$diabetes,SplitRatio = 0.8)
training_set<- subset(df2,split==TRUE)
test_set<- subset(df2,split==FALSE)
y_test<- test_set$diabetes

#install.packages("kernlab")                    
library(kernlab)

#test$neighbourhood
bestAccuracy2=0
arr<-c()
kernelList=c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','anovadot')
fitnessfunction<- function(k,n,e){
  model<- ksvm(as.factor(diabetes) ~ .,data=training_set,kernel=k,nu=n,epsilon=e,kpar=list())
  y_pred<- predict(model,newdata=test_set[,-15])
  conf_matrix <- table(y_test, y_pred)
  accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
  return(accuracy)
}
#acc<- fitnessfunction('rbfdot',0.2309025,0.8109799)
#acc
for(i in 1:100){
  k=sample(kernelList,1)
  n=runif(1)
  e=runif(1)
  Accuracy= fitnessfunction(k,n,e)
  
  if(Accuracy>bestAccuracy2){
    bestKernel=k
    bestNu=n
    bestEpsilon=e
    bestAccuracy2=Accuracy
  }
  arr2 <- c(arr2, bestAccuracy2)
  print(k)
  print(n)
  print(e)
  print(Accuracy)
}


#forsample 3

df3 <- newdata[sample(nrow(newdata), size=3000), ]
df3$bmi<- scale(df3$bmi)
df3$HbA1c_level<- scale(df3$HbA1c_level)
df3$blood_glucose_level<- scale(df3$blood_glucose_level)
df3$age<- scale(df3$age)
df3

library(caTools)
split=sample.split(df3$diabetes,SplitRatio = 0.8)
training_set<- subset(df3,split==TRUE)
test_set<- subset(df3,split==FALSE)
y_test<- test_set$diabetes

#install.packages("kernlab")                    
library(kernlab)

#test$neighbourhood
bestAccuracy3=0
arr<-c()
kernelList=c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','anovadot')
fitnessfunction<- function(k,n,e){
  model<- ksvm(as.factor(diabetes) ~ .,data=training_set,kernel=k,nu=n,epsilon=e,kpar=list())
  y_pred<- predict(model,newdata=test_set[,-15])
  conf_matrix <- table(y_test, y_pred)
  accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
  return(accuracy)
}
#acc<- fitnessfunction('rbfdot',0.2309025,0.8109799)
#acc
for(i in 1:100){
  k=sample(kernelList,1)
  n=runif(1)
  e=runif(1)
  Accuracy= fitnessfunction(k,n,e)
  
  if(Accuracy>bestAccuracy3){
    bestKernel=k
    bestNu=n
    bestEpsilon=e
    bestAccuracy3=Accuracy
  }
  arr3<- append(arr3,bestAccuracy3)
  print(k)
  print(n)
  print(e)
  print(Accuracy)
}

#sample 4
df4 <- newdata[sample(nrow(newdata), size=3000), ]
df4$bmi<- scale(df4$bmi)
df4$HbA1c_level<- scale(df4$HbA1c_level)
df4$blood_glucose_level<- scale(df4$blood_glucose_level)
df4$age<- scale(df4$age)
df4

library(caTools)
split=sample.split(df4$diabetes,SplitRatio = 0.8)
training_set<- subset(df4,split==TRUE)
test_set<- subset(df4,split==FALSE)
y_test<- test_set$diabetes

#install.packages("kernlab")                    
library(kernlab)

#test$neighbourhood
bestAccuracy4=0
kernelList=c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','anovadot')
fitnessfunction<- function(k,n,e){
  model<- ksvm(as.factor(diabetes) ~ .,data=training_set,kernel=k,nu=n,epsilon=e,kpar=list())
  y_pred<- predict(model,newdata=test_set[,-15])
  conf_matrix <- table(y_test, y_pred)
  accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
  return(accuracy)
}
#acc<- fitnessfunction('rbfdot',0.2309025,0.8109799)
#acc
for(i in 1:100){
  k=sample(kernelList,1)
  n=runif(1)
  e=runif(1)
  Accuracy= fitnessfunction(k,n,e)
  
  if(Accuracy>bestAccuracy4){
    bestKernel=k
    bestNu=n
    bestEpsilon=e
    bestAccuracy4=Accuracy
  }
  arr4 <- c(arr4, bestAccuracy4)
  print(k)
  print(n)
  print(e)
  print(Accuracy)
}

#sample 5
df5 <- newdata[sample(nrow(newdata), size=3000), ]
df5$bmi<- scale(df5$bmi)
df5$HbA1c_level<- scale(df5$HbA1c_level)
df5$blood_glucose_level<- scale(df5$blood_glucose_level)
df5$age<- scale(df5$age)
df5

library(caTools)
split=sample.split(df5$diabetes,SplitRatio = 0.8)
training_set<- subset(df5,split==TRUE)
test_set<- subset(df5,split==FALSE)
y_test<- test_set$diabetes
#install.packages("kernlab")                    
library(kernlab)

#test$neighbourhood
bestAccuracy5=0
arr<-c()
kernelList=c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','anovadot')
fitnessfunction<- function(k,n,e){
  model<- ksvm(as.factor(diabetes) ~ .,data=training_set,kernel=k,nu=n,epsilon=e,kpar=list())
  y_pred<- predict(model,newdata=test_set[,-15])
  conf_matrix <- table(y_test, y_pred)
  accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
  return(accuracy)
}
#acc<- fitnessfunction('rbfdot',0.2309025,0.8109799)
#acc
for(i in 1:100){
  k=sample(kernelList,1)
  n=runif(1)
  e=runif(1)
  Accuracy= fitnessfunction(k,n,e)
  
  if(Accuracy>bestAccuracy5){
    bestKernel=k
    bestNu=n
    bestEpsilon=e
    bestAccuracy5=Accuracy
  }
  arr5 <- c(arr5, bestAccuracy5)
  print(k)
  print(n)
  print(e)
  print(Accuracy)
}
print(bestAccuracy5)

#sample 6
df6 <- newdata[sample(nrow(newdata), size=3000), ]
df6$bmi<- scale(df6$bmi)
df6$HbA1c_level<- scale(df6$HbA1c_level)
df6$blood_glucose_level<- scale(df6$blood_glucose_level)
df6$age<- scale(df6$age)
df6

library(caTools)
split=sample.split(df6$diabetes,SplitRatio = 0.8)
training_set<- subset(df6,split==TRUE)
test_set<- subset(df6,split==FALSE)
y_test<- test_set$diabetes
  #install.packages("kernlab")                    
  library(kernlab)

#test$neighbourhood
bestAccuracy6=0
arr<-c()
kernelList=c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','anovadot')
fitnessfunction<- function(k,n,e){
  model<- ksvm(as.factor(diabetes) ~ .,data=training_set,kernel=k,nu=n,epsilon=e,kpar=list())
  y_pred<- predict(model,newdata=test_set[,-15])
  conf_matrix <- table(y_test, y_pred)
  accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
  return(accuracy)
}
#acc<- fitnessfunction('rbfdot',0.2309025,0.8109799)
#acc
for(i in 1:100){
  k=sample(kernelList,1)
  n=runif(1)
  e=runif(1)
  Accuracy= fitnessfunction(k,n,e)
  
  if(Accuracy>bestAccuracy6){
    bestKernel=k
    bestNu=n
    bestEpsilon=e
    bestAccuracy6=Accuracy
  }
  arr6 <- c(arr6, bestAccuracy6)
  print(k)
  print(n)
  print(e)
  print(Accuracy)
}
print(bestAccuracy6)

#sample 7
df7 <- newdata[sample(nrow(newdata), size=3000), ]
df7$bmi<- scale(df7$bmi)
df7$HbA1c_level<- scale(df7$HbA1c_level)
df7$blood_glucose_level<- scale(df7$blood_glucose_level)
df7$age<- scale(df7$age)
df7

library(caTools)
split=sample.split(df7$diabetes,SplitRatio = 0.8)
training_set<- subset(df7,split==TRUE)
test_set<- subset(df7,split==FALSE)
y_test<- test_set$diabetes
bestAccuracy7=0
arr<-c()
kernelList=c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','anovadot')
fitnessfunction<- function(k,n,e){
  model<- ksvm(as.factor(diabetes) ~ .,data=training_set,kernel=k,nu=n,epsilon=e,kpar=list())
  y_pred<- predict(model,newdata=test_set[,-15])
  conf_matrix <- table(y_test, y_pred)
  accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
  return(accuracy)
}
#acc<- fitnessfunction('rbfdot',0.2309025,0.8109799)
#acc
for(i in 1:100){
  k=sample(kernelList,1)
  n=runif(1)
  e=runif(1)
  Accuracy= fitnessfunction(k,n,e)
  
  if(Accuracy>bestAccuracy7){
    bestKernel=k
    bestNu=n
    bestEpsilon=e
    bestAccuracy7=Accuracy
  }
  arr7 <- c(arr7, bestAccuracy7)
  print(k)
  print(n)
  print(e)
  print(Accuracy)
}
print(bestAccuracy7)


#sample 8
df8 <- newdata[sample(nrow(newdata), size=3000), ]
df8$bmi<- scale(df8$bmi)
df8$HbA1c_level<- scale(df8$HbA1c_level)
df8$blood_glucose_level<- scale(df8$blood_glucose_level)
df8$age<- scale(df8$age)
df8

library(caTools)
split=sample.split(df8$diabetes,SplitRatio = 0.8)
training_set<- subset(df8,split==TRUE)
test_set<- subset(df8,split==FALSE)
y_test<- test_set$diabetes
bestAccuracy8=0
arr<-c()
kernelList=c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','anovadot')
fitnessfunction<- function(k,n,e){
  model<- ksvm(as.factor(diabetes) ~ .,data=training_set,kernel=k,nu=n,epsilon=e,kpar=list())
  y_pred<- predict(model,newdata=test_set[,-15])
  conf_matrix <- table(y_test, y_pred)
  accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
  return(accuracy)
}
#acc<- fitnessfunction('rbfdot',0.2309025,0.8109799)
#acc
for(i in 1:100){
  k=sample(kernelList,1)
  n=runif(1)
  e=runif(1)
  Accuracy= fitnessfunction(k,n,e)
  
  if(Accuracy>bestAccuracy8){
    bestKernel=k
    bestNu=n
    bestEpsilon=e
    bestAccuracy8=Accuracy
  }
  arr8 <- c(arr8, bestAccuracy8)
  print(k)
  print(n)
  print(e)
  print(Accuracy)
}
print(bestAccuracy8)

#sample9
df9 <- newdata[sample(nrow(newdata), size=3000), ]
df9$bmi<- scale(df9$bmi)
df9$HbA1c_level<- scale(df9$HbA1c_level)
df9$blood_glucose_level<- scale(df9$blood_glucose_level)
df9$age<- scale(df9$age)
df9

library(caTools)
split=sample.split(df9$diabetes,SplitRatio = 0.8)
training_set<- subset(df9,split==TRUE)
test_set<- subset(df9,split==FALSE)
y_test<- test_set$diabetes
bestAccuracy9=0
arr<-c()
kernelList=c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','anovadot')
fitnessfunction<- function(k,n,e){
  model<- ksvm(as.factor(diabetes) ~ .,data=training_set,kernel=k,nu=n,epsilon=e,kpar=list())
  y_pred<- predict(model,newdata=test_set[,-15])
  conf_matrix <- table(y_test, y_pred)
  accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
  return(accuracy)
}
#acc<- fitnessfunction('rbfdot',0.2309025,0.8109799)
#acc
for(i in 1:100){
  k=sample(kernelList,1)
  n=runif(1)
  e=runif(1)
  Accuracy= fitnessfunction(k,n,e)
  
  if(Accuracy>bestAccuracy9){
    bestKernel=k
    bestNu=n
    bestEpsilon=e
    bestAccuracy9=Accuracy
  }
  arr9 <- c(arr9, bestAccuracy9)
  print(k)
  print(n)
  print(e)
  print(Accuracy)
}
print(bestAccuracy9)

#sample 10
df10 <- newdata[sample(nrow(newdata), size=3000), ]
df10$bmi<- scale(df10$bmi)
df10$HbA1c_level<- scale(df10$HbA1c_level)
df10$blood_glucose_level<- scale(df10$blood_glucose_level)
df10$age<- scale(df10$age)
df10

library(caTools)
split=sample.split(df10$diabetes,SplitRatio = 0.8)
training_set<- subset(df10,split==TRUE)
test_set<- subset(df10,split==FALSE)
y_test<- test_set$diabetes
bestAccuracy10=0
arr<-c()
kernelList=c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','anovadot')
fitnessfunction<- function(k,n,e){
  model<- ksvm(as.factor(diabetes) ~ .,data=training_set,kernel=k,nu=n,epsilon=e,kpar=list())
  y_pred<- predict(model,newdata=test_set[,-15])
  conf_matrix <- table(y_test, y_pred)
  accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
  return(accuracy)
}
#acc<- fitnessfunction('rbfdot',0.2309025,0.8109799)
#acc
for(i in 1:100){
  k=sample(kernelList,1)
  n=runif(1)
  e=runif(1)
  Accuracy= fitnessfunction(k,n,e)
  
  if(Accuracy>bestAccuracy10){
    bestKernel=k
    bestNu=n
    bestEpsilon=e
    bestAccuracy10=Accuracy
  }
  arr10 <- c(arr10, bestAccuracy10)
  print(k)
  print(n)
  print(e)
  print(Accuracy)
}

print(bestAccuracy1)
print(bestAccuracy2)
print(bestAccuracy3)
print(bestAccuracy4)
print(bestAccuracy5)
print(bestAccuracy6)
print(bestAccuracy7)
print(bestAccuracy8)
print(bestAccuracy9)
print(bestAccuracy10)

arr<- c(bestAccuracy1,bestAccuracy2,bestAccuracy3,bestAccuracy4,bestAccuracy5,bestAccuracy6,bestAccuracy7,bestAccuracy8,bestAccuracy9,bestAccuracy10)
maxi=max(arr)
maxi

if(maxi==bestAccuracy1)
{
  plot(arr1,type='o',xlab='Iteration',ylab='Accuracy')
}
if(maxi==bestAccuracy2)
{
  plot(arr2,type='o',xlab='Iteration',ylab='Accuracy')
}
if(maxi==bestAccuracy3)
{
  plot(arr3,type='o',xlab='Iteration',ylab='Accuracy')
}
if(maxi==bestAccuracy4)
{
  plot(arr4,type='o',xlab='Iteration',ylab='Accuracy')
}
if(maxi==bestAccuracy5)
{
  plot(arr5,type='o',xlab='Iteration',ylab='Accuracy')
}
if(maxi==bestAccuracy6)
{
  plot(arr6,type='o',xlab='Iteration',ylab='Accuracy')
}
if(maxi==bestAccuracy7)
{
  plot(arr7,type='o',xlab='Iteration',ylab='Accuracy')
}
if(maxi==bestAccuracy8)
{
  plot(arr8,type='o',xlab='Iteration',ylab='Accuracy')
}
if(maxi==bestAccuracy9)
{
  plot(arr9,type='o',xlab='Iteration',ylab='Accuracy')
}
if(maxi==bestAccuracy10)
{
  plot(arr10,type='o',xlab='Iteration',ylab='Accuracy')
}

