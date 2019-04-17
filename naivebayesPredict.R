data(readr)
library(readxl)
dataset<-read_excel(file.choose())
install.packages('DataExplorer')
library(DataExplorer)
str(dataset)

#Exploratory data Ananlysis
datasett<-dataset[-1]
str(datasett)
datasett$`Oil Leakage`<-as.factor(dataset$`Oil Leakage`)
datasett$`Fuel supply`<-as.factor(dataset$`Fuel supply`)
datasett$`Vector Group`<-as.factor(dataset$`Vector Group`)
datasett$Insulation<-as.factor(datasett$Insulation)
dataset$`Energy Losses`<-as.factor(datasett$`Energy Losses`)
datasett$`Pressure Relay`<-as.factor(datasett$`Pressure Relay`)
datasett$`Cooling Operation`<-as.factor(datasett$`Cooling Operation`)
datasett$Bushing<-as.factor(datasett$Bushing)
datasett$`Over Current Protection (OC)`<-as.factor(datasett$`Over Current Protection (OC)`)
datasett$`Fire Fighting Systems (FFS)`<-as.factor(datasett$`Fire Fighting Systems (FFS)`)
datasett$`Silica Gel Color`<-as.factor(datasett$`Silica Gel Color`)
datasett$Outage<-as.factor(datasett$Outage)
#structure of dataset
str(datasett)
#plot variables
plot_str(datasett)
#plot_missing
plot_missing(datasett)
sum(is.na(datasett))
#pakcages 
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")
library(funModeling) 
library(tidyverse) 
library(Hmisc)
basic_eda <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}
basic_eda(datasett)
freq(datasett)
plot_num(datasett)
plot_histogram(datasett)
plot_density(datasett)
plot_correlation(datasett, type = 'continuous','Review.Date')
plot_bar(datasett)
boxplot(datasett)
summary(datasett)
dim(datasett)


#radomdise
set.seed(1234)
#PARTITITOON DATASET into traning and testing
sampling<-sample(2,nrow(datasett),replace = T,prob=c(0.8,0.2))
train<-datasett[sampling==1,]
test<-datasett[sampling==2,]
dim(train)
dim(test)

#modelbuilding
library(e1071)
Naive_Bayes_Model=naiveBayes(Outage ~., data=train)
Naive_Bayes_Model

#Prediction on the dataset
Predictions=predict(Naive_Bayes_Model,test)
#Confusion matrix to check accuracy
table(Predictions,test$Outage)

printALL=function(model){
  trainPred=predict(Naive_Bayes_Model, newdata = train, type = "class")
  trainTable=table(train$Outage, trainPred)
  print(trainTable)
  print(1-sum(diag(trainTable))/sum(trainTable))
  testPred=predict(Naive_Bayes_Model, newdata=test, type="class")
  testTable=table(test$Outage, testPred)
  print(testTable)
  print(1-sum(diag(testTable))/sum(testTable))
}
printALL(Naive_Bayes_Model)
