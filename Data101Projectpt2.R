readtrain<-function(){#function which returns a dataframe with the data from the training set CSV
  train<-cleanData(read.csv('archive/train.csv'))
  train
}
readtest<-function(){#function which returns a dataframe with the data from the testing set CSV
  test<-cleanData(read.csv('archive/test.csv'))
  test
}
cleanData<-function(data){#function to clean dataframe
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
  tree<-rpart(Loan_Status ~ totincome + Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area, data=frame, control=rpart.control(cp=.005), method="class")
  tree
}
graphTree<-function(tree){#graphs the tree
  library(rpart.plot)
  rpart.plot(tree,roundint=FALSE, box.palette ="RdYlGn")
}
NaiveBayes<-function(frame){#creates a naive bayes model out of the dataframe
  library(e1071)
  naive_bayes_model <- naiveBayes(Loan_Status ~ loanOverIncome + totincome + Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area, data=frame)
  naive_bayes_model

}
NaivePredict<-function(model, frame){#makes a prediction based off of the naive bayes model model
  frame$predictions <- predict(model, newdata = frame)
  frame
}
#load library
library(rpart)

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
table(pred_Loan_Status, train$Loan_Status)

#generate a naive bayes model
model<-NaiveBayes(train)

#predict the loan status using a naive bayes model
train<-NaivePredict(model, train)
test<-NaivePredict(model, test)

#print a confusion matrix based on the predictions of the naive bayes model
Loan_Bayes<-table(train$predictions, train$Loan_Status)
caret::confusionMatrix(Loan_Bayes)
