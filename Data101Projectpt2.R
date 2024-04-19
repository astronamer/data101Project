readtrain<-function(){#function which returns a dataframe with the data from the training set CSV
  train<-cleanData(read.csv('archive/train.csv'))
  train
}
readtest<-function(){#function which returns a dataframe with the data from the testing set CSV
  test<-cleanData(read.csv('archive/test.csv'))
  test
}
cleanData<-function(data){#function to clean dataframe
  data$Emphasize<-((data$CoapplicantIncome)^2)
  data$Gender<-factor(data$Gender)
  data$Dependents<-factor(data$Dependents)
  data$Education<-factor(data$Education)
  data$Self_Employed<-factor(data$Self_Employed)
  data$CoapplicantIncome<-as.integer(data$CoapplicantIncome)
  data$Credit_History<-factor(data$Credit_History)
  data$Property_Area<-factor(data$Property_Area)
  data$totincome<-data$ApplicantIncome+data$CoapplicantIncome
  data$loanOverIncome<-(data$LoanAmount/data$totincome)
  data#CreditScoreOv

  
}
model<-function(frame){#creates the tree for the training data
  library(rpart)
  tree<-rpart(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area, data=frame, control=rpart.control(cp=.007), method="class")
  tree
}
graphTree<-function(tree){#graphs the tree
  library(rpart.plot)
  rpart.plot(tree,roundint=FALSE, box.palette ="RdYlGn")
}
NaiveBayes<-function(frame){#creates a naive bayes model out of the dataframe
  library(e1071)
  naive_bayes_model <- naiveBayes(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area, data=frame)
  naive_bayes_model

}
NaivePredict<-function(model, frame){#makes a prediction based off of the naive bayes model model
  frame$predictions <- predict(model, newdata = frame)
  frame
}
#load library
library(rpart)
library(carat)
#make the dataframes
train<-readtrain()
test<-readtest()

#make a decision tree model
tree<-model(train)
graphTree(tree)

#use the decision tree to predict loan status
pred_Loan_Status<-rpart.predict(tree, train, type='class')
test$pred_Loan_Status<-rpart.predict(tree, test, type='class')

#print a confusion matrix from predictions
Loan_Tree<-table(pred_Loan_Status, train$Loan_Status)
caret::confusionMatrix(Loan_Tree)

#generate a naive bayes model
model<-NaiveBayes(train)

#predict the loan status using a naive bayes model
train<-NaivePredict(model, train)
test<-NaivePredict(model, test)

#print a confusion matrix based on the predictions of the naive bayes model
Loan_Bayes<-table(train$predictions, train$Loan_Status)
caret::confusionMatrix(Loan_Bayes)

pdf("plot.pdf")

boxplot(train$ApplicantIncome~train$Loan_Status, xlab = "Loan Status" ,ylab = "Applicant Income ", main = " applicant income  vs loan status ", col = "red")

boxplot(train$CoapplicantIncome~train$Loan_Status,xlab = "Loan Status" ,ylab = "Coapplicant Income" , main = " CoapplicantIncome vs loan status  ", col = "red")


mosaicplot(train$Dependents~train$Loan_Status,ylab = "Loan Status" ,xlab = "Dependents" , main = "num dependents vs loan status", col = "red")

boxplot(train$LoanAmount~train$Loan_Status,xlab = "Loan Status" ,ylab = "loan amount" , main = " loan amount  vs loan status  ", col = "red")

boxplot(train$Loan_Amount_Term~train$Loan_Status,xlab = "Loan Status" ,ylab = "loan term" , main = " loan  term  vs loan status  ", col = "red")

mosaicplot(train$Property_Area~train$Loan_Status,ylab = "Loan Status" ,xlab="Property Area", main = "Property Area vs loan status", col = "red")

mosaicplot(train$Credit_History~train$Loan_Status,ylab = "Loan Status" ,xlab="Credit History", main = "Credit History vs loan status", col = "red")

mosaicplot(train$Gender~train$Loan_Status,ylab = "Loan Status" ,xlab="Gender", main = "Gender vs loan status", col = "red")

mosaicplot(train$Married~train$Loan_Status,ylab = "Loan Status" ,xlab="Married", main = "Married vs loan status", col = "red")

mosaicplot(train$Education~train$Loan_Status,ylab = "Loan Status" ,xlab="Education", main = "Education vs loan status", col = "red")

mosaicplot(train$Self_Employed~train$Loan_Status,ylab = "Loan Status" ,xlab="Self Employed", main = "Self Employed vs loan status", col = "red")

dev.off()