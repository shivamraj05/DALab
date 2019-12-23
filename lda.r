library(ISLR)
library(MASS)
Default
row_num=sample(1:nrow(Default),0.6*nrow(Default))

train=Default[row_num,]
test=Default[-row_num,]
LDA=lda(default~student+balance+income,data=train)
pred=predict(LDA,newdata=test)
pred
con<-table(pred$class,test$default)
con
print((con[1,1]+con[2,2])/4000)
plot(pred$class, pred$x, col='red')
pred$class
ldahist(pred$x[,1], g= pred$class)
