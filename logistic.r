library(ISLR)
library(MASS)
print(Default)
rownumber=sample(1:nrow(Default),0.6*nrow(Default))
rownumber
train=Default[rownumber,]
test=Default[-rownumber,]
GLM=glm(default~student+balance+income,data=train,family=binomial)
pred=predict(GLM,newdata=test)
pred=ifelse(pred>0.5,'Yes','No')
pred
con<-table(pred,test$default)
con
print((con[1,1]+con[2,2])/4000)

