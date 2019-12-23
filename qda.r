library(ISLR)
library(MASS)
Default
row_num=sample(1:nrow(Default),0.6*nrow(Default))

train=Default[row_num,]
test=Default[-row_num,]
QDA=qda(default~student+balance+income,data=train)
pred=predict(QDA,newdata=test)
pred
con<-table(pred$class,test$default)
print((con[1,1]+con[2,2])/4000)
plot(pred$posterior[,2], pred$class, col=test$default)
pred$posterior[,2]
pred$class
