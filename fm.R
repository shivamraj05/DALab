library(digest)
n<-16
bits<-rep(0,n)
count_distinct<-function(stream)
{
  
  for(s in stream)
  {
    x<-strtoi(substr(digest(s,algo="md5"),1,8),16L)
    if(!is.na(x)){
    bits[index(tail((intToBits(x)),16))]<-1
  }
  }
  return(2^index(rev(bits)))
}
index<-function(z)
{
  return(min(which(z!=1)))
}
stream =c("Hello","world","Ralph","Internet")
ans<-count_distinct(stream)
ans

