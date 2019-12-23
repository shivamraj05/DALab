df<-read.csv('loan.csv')
data=as.data.frame(df[,c(2,3,4)])
x<-readline("enter age")
y<-readline("enter amount")
z<-readline("enter k value")
age<-as.integer(x)
loan<-as.integer(y)
k<-as.integer(z)
plot(data$Age,data$Loan)
for(i in 1:nrow(data))
{
  data$distance[i]<-sqrt((data$Age[i]-age)**2+(data$Loan[i]-loan)**2)
}
df <-data[order(data$distance),]
count=0
for(i in 1:k)
{
 count=count+ifelse(df$Paid[i]=="yes",1,-1)
}
if(count>0)
{
  print("Loan Approved")
}else if(count==0){
  print("Not Sure")
}else
{
  print("Not Approved")
}

