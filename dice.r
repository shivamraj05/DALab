data<-read.csv("prog2.csv")
r=mean(data$Remember)
a=mean(data$Analyse)
ap=mean(data$Apply)
e=mean(data$Evaluate)
u=mean(data$Understand)
x<-c(r,a,ap,e,u)
names <- c("Remember","Analyse","Apply","Evaluate","Understand")
min<-0
for(i in 1:length(x))
{
  if(x[i]>min)
  {
    min<-x[i]
    blooms<-names[i]
  }
}
print(blooms)
print(min)
pie(x,names)