#objective- predicting whether the patient is suffering from diabetes or not
#data importing
data<-read.csv(file.choose())
#data conversion as per the model requirement
data$Outcome<-as.factor(data$Outcome)

#data partition
library(caret)
Train<-createDataPartition(data$Outcome, p=0.7, list = FALSE)
training<-data[Train,]
testing <- data[-Train,]

#model
library(e1071)
svm_model1 <- svm(Outcome~., data=training) # this . means all variables
summary(svm_model1) ##output- if y is num, svm type is R-regresson, if Y is categorical, svm tyoe is c-classification
#o/p- 2 levels- no of support vectors for 0 =151, 1=158
training$fiited<-svm_model1$fitted
#table(ypred, training$outcome)
confusionMatrix(svm_model1$fitted,training$Outcome) 

#Accuracy on testing data set 7783
ypred1 = predict(svm_model1,testing)
#table (ypred1, testing$Outcome)
confusionMatrix(ypred1, testing$Outcome)

training$fiited<-NULL # deleting this column befor testing another model

#second model
svm_model2<-svm(Outcome ~., data=training, kernel ="linear")  #by ceafult kernel takes gaussian
#make a habbit of using linear kernel
summary(svm_model2)
confusionMatrix(svm_model2$fitted, training$Outcome)
#Accuracy on testing data set 7783
ypred1=predict(svm_model2, testing)
#table 9ypred1, testing$Outcome)
confusionMatrix(ypred1, testing$Outcome)



