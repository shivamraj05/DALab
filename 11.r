df<-read.csv("11.csv",header=TRUE,sep=",")
summary(df)

a<-45
b<-35

dis<-transform(df,distance=sqrt((a-df$m1)^2+(b-df$m2)^2))
dis
odis<-dis[order(dis$distance),]
odis
k<-3
nn<-head(odis,k)
knn<-table(nn$distance)
nn
knn
table(nn$grade)
max(table(nn$grade))
which(table(nn$grade)==max(table(nn$grade)))
t<-names(which(table(nn$grade)==max(table(nn$grade))))
t
cat("grade:",t)

