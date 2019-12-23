x<-c("Hello","world","world","world","shivam","ayush")
x<-as.numeric(as.factor(x))
print(x)
h<-function(var){
  return ((6*var+1)%%5)
}
H<-c()
for(i in 1:length(x)){
  H[i]<-h(x[i])
}
H
b<-function(var){
  l1=as.numeric(rev(intToBits(var)))
  l1=rev(tail(l1,n=4))
  print(l1)
  index<-match(1,l1)-1
  return (index)
}
B<-c()
for(i in 1:length(H)){
  B[i]<-b(H[i])
}
B
B[is.na(B)]<-0
B
print(2^max(B))