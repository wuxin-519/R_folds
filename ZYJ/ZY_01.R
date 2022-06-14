library(kernlab)
guangpu<- read.csv("D:/Anaconda/practise/QXZH/dc/·ÛÁ£.csv",header = TRUE)
guangpu
preValue<-c()
trueValue<-c()
dataInUse <- guangpu

DIU_traing<-dataInUse[-1,2:ncol(guangpu)]
DIU_test<-dataInUse[1,2:ncol(guangpu)]
dataInUse.1<-dataInUse[-1,]
tru<-as.vector(dataInUse[1,1])
trueValue<-c(trueValue,tru)
set.seed(10)
model=ksvm(DIU_traing,t(t(as.factor(dataInUse.1$·ÛÁ£))),type="C-svc",kernel='rbf',kpar=list(sigma=1),C=1)
# moxing<-ksvm(as.factor(dataInUse$·ÛÁ£)~.,dataInUse,type = "eps-bsvr")
# pre<-predict(moxing,DIU_test)
# pre<-as.vector(pre) 
# preValue<-c(preValue,pre)
# 
# prep <- predict(moxing,guangpu)
# prep
# write.table(preValue,"·ÛÁ£svm.csv",sep = ",")
