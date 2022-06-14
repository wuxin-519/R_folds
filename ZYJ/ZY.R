library(randomForest)
library(caret)
library(pROC)
guangpu=read.csv('D:\\Anaconda\\practise\\QXZH\\dc\\预处理光谱.csv',header=TRUE)
preValue<-c()
trueValue<-c()
dataInUse <- guangpu
for(i in 1:nrow (dataInUse))
{
  DIU_traing<- dataInUse[-i,2:ncol(guangpu)]
  DIU_test<-dataInUse[i ,2:ncol(guangpu)]
  dataInuse.1<- dataInUse [-i,]
  tru<- as.vector(dataInUse[i ,1])
  trueValue<- c(trueValue,tru)
  # print(tt)
  set.seed(6666)
  rm<- randomForest(x=DIU_traing,y=t(t(as.factor(dataInuse.1$质地))),ntree=48)
  pre<-predict(rm,DIU_test)
  pre<-as.vector(pre)
  preValue<-c(preValue,pre)
}
confusionMatrix(factor (preValue),factor (trueValue))