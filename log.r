data<-read.csv('logistic.csv')
data
X<-as.matrix(data$def,data$bank_balance,data$qualification))
X
Y<-as.matrix(data$default)
bal=readline("enter balance")
qual=readline("enter qual")
balance<-as.integer(bal)
qualification<-as.integer(qual)
Beta_all<-solve(t(X)%*%X)%*%t(X)%*%Y
Beta_all
#y=bo+b1x1+b2x2+b3x3
data$pred<-exp(-X%*%Beta_all)
Beta_all
y<-0.034523+balance*Beta_all[1]+qualification*Beta_all[2]
#p=1/1+e(-y)
p=1/1+exp(-y)
res<-log(p/1-p)
if(res>0)
{
  print("Yes")
}else
{
  print("No")
}

