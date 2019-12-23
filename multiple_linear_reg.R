df<-read.csv('2.csv')
#data

multiple_linear_reg(X,Y,data)
x=as.matrix(cbind(1,df$experience,df$training))  
y=as.matrix(df$publications)
beta_hat=solve(t(x)%*%x)%*%t(x)%*%y
beta_hat
df$pred=x%*%beta_hat
sum1=sum((df$publications-df$pred)**2)
sum2=sum((df$publications-mean(df$publications))**2)
rse=1-sum1/sum2
rse
model=lm(publications~experience+training,data=df)
pred1=predict(model,data.frame(experience=df$experience,training=df$training))
df$pred1=pred1
summary(model)
