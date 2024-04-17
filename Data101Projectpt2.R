readtrain<-function(){#function which returns a dataframe with the data from the training set CSV
  train<-read.csv('archive/train.csv')
  train
}
readtest<-function(){#function which returns a dataframe with the data from the testing set CSV
  test<-read.csv('archive/test.csv')
  test
}
cleanData<-function(data){#function to clean dataframe, to be completed
  data$Gender<-factor(data$Gender)
  data$Dependents<-factor(data$Dependents)
  data$Education<-factor(data$Education)
  data$Self_Employed<-factor(data$Self_Employed)
  
  
}
train<-readtrain()
train
test<-readtest()
#col<-train$Self_Employed
#table(col)
#class(col)