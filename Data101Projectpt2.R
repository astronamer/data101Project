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
  data
}
model<-function(frame){#creates the tree for the training data
  library(rpart)
  tree<-rpart(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area, data=frame, control=rpart.control(cp=.005), method="class")
  tree
}
graphTree<-function(tree){#graphs the tree
  library(rpart.plot)
  rpart.plot(tree,roundint=FALSE, box.palette ="RdYlGn")
}
library(rpart)
train<-readtrain()
tree<-model(train)
graphTree(tree)
test<-readtest()
pred_Loan_Status<-rpart.predict(tree, train, type='class')
test$pred_Loan_Status<-rpart.predict(tree, test, type='class')
table(pred_Loan_Status, train$Loan_Status)