#Uses packages rpart for building decision trees and packages rattle and rpart.plot and RColorBrewer for visualization


BuildTree= function() 
{
  train1=read.csv(file.choose(),header=TRUE)
  train1$titles=ifelse(grepl("Mr[.]",train$Name),"Mr",ifelse(grepl("Mrs[.]",train$Name),"Mrs",ifelse(grepl("Miss",train$Name),"Miss",ifelse(grepl("Master",train$Name),"Master",ifelse(grepl("Rev",train$Name),"Rev","NA")))))  
  library(rpart)
  library(rattle)
  tree1 = rpart(Survived ~ Pclass + Sex + Age + Fare + titles,data=train1,method="class")
  fancyRpartPlot(tree1)
  save(tree1,file="dtmodel.rda")
  
}

PredictTest = function()
{
  tdata=read.csv(file.choose(),header=TRUE)
  tdata$titles=ifelse(grepl("Mr[.]",tdata$Name),"Mr",ifelse(grepl("Mrs[.]",tdata$Name),"Mrs",ifelse(grepl("Miss",tdata$Name),"Miss",ifelse(grepl("Master",tdata$Name),"Master",ifelse(grepl("Rev",tdata$Name),"Rev","NA")))))  
  str(tdata)
  library(rpart)
  library(rattle)
  load(file="dtmodel.rda")
  fancyRpartPlot(tree1)
  prediction = predict(tree1,tdata,type="class")
  soln = data.frame(PassengerId = tdata$PassengerId, Survived = prediction)
  nrow(soln)
  write.csv(soln,file="testresults.csv",row.names=FALSE)
}
