library(caret)
library(randomForest)
library(pROC)
library(aqp)
library(kernlab)
#library(caret)
data_all <- read.csv("D:\\Anaconda\\practise\\QXZH\\dc\\hanjia\\LY_01.csv",header=TRUE)
  #read_excel("D:\\Anaconda\\practise\\QXZH\\dc\\hanjia\\LY.xlsx")
preValue<-c()
trueValue<-c()
dataInUse <- data_all
for(i in 1:nrow (dataInUse))
{
  DIU_traing<- data.matrix(dataInUse[-i,5:10])
  DIU_test<-dataInUse[i ,5:10]
  dataInuse.1<- dataInUse [-i,]
  tru<- as.vector(dataInUse[i ,2])
  trueValue<- c(trueValue,tru)
  # print(tt)
  set.seed(200)
  #rm<- randomForest(x=DIU_traing,y=t(t(as.factor(dataInuse.1$PM))),ntree=48)
  Svm=ksvm(DIU_traing,t(t(as.factor(dataInuse.1$PM))),type="C-svc",kernel='rbf',kpar=list(sigma=1),C=1)
  pre<-predict(Svm,DIU_test)
  pre<-as.vector(pre)
  preValue<-c(preValue,pre)
}
#confusionMatrix(factor (preValue),factor (trueValue))#我的版本过低，用不了这个包，所以用了以下这个table函数来求混淆矩阵
hun_matrix=table(preValue,trueValue)
#接下来求Tau系数和Kappa,以及用户精度和系统精度
Tau_u=tauW(hun_matrix)