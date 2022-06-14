setwd("E:/寒假小论文/R代码")
library(kernlab)
guangpu<- read.csv("E:/寒假小论文/粉粒.csv",header = TRUE)
guangpu
preValue<-c()
trueValue<-c()
dataInUse <- guangpu
for(i in 1:nrow(dataInUse))
{
  DIU_traing<-dataInUse[-i,2:ncol(guangpu)]
  DIU_test<-dataInUse[i,2:ncol(guangpu)]
  dataInUse.1<-dataInUse[-i,]
  tru<-as.vector(dataInUse[i,1])
  trueValue<-c(trueValue,tru)
  set.seed(10)
  moxing<-ksvm(as.factor(dataInUse$粉粒)~.,dataInUse,type = "eps-bsvr")
  pre<-predict(moxing,DIU_test)
  pre<-as.vector(pre) 
  preValue<-c(preValue,pre)
} 
prep <- predict(moxing,guangpu)
prep
write.table(preValue,"粉粒svm.csv",sep = ",")
