library(digest)
p=0.10
logged_in=c("shubham","parth","shivam","hasrh","rohit","shivdutt","rahul","ramesh","monti","ankita")
not_logged_in=c("harshan","varsha","gaurav","rohitawa","shurbhi","ayush","sonu","modiji","amitshah","vicky")
m=-(length(logged_in) * log(p))/(log(2)**2) 
m=as.integer(m)
#m=50
hash_count<-as.integer((m/length(logged_in)) *log(2)) 
#hash_count=4
filter=rep(0,m)
hash_function<-function(value,seed)
{
  x<-digest(object=value,algo='murmur32',serialize = TRUE,seed=seed)
 #print(x)
  y=paste('0x',x,sep="")
  #print(y)
  index<-as.numeric(y)%%m
  return(index)
}
for(i in 1:length(logged_in))
{
 for(j in 1:hash_count)
   {
   filter[hash_function(logged_in[i],j)+1]<-1
 }  
}

check<-function(value){
  for(i in 1:hash_count){
    index<-hash_function(value,i)
    index<-index+1
    if(filter[index]==0)
    {
      return(FALSE)
    } 
    
  }
  return(TRUE)
}

test_data=c(logged_in[1:3],not_logged_in)
test_data

for(i in 1:length(test_data))
{
  if(check(test_data[i]))
  {
    if(test_data[i] %in% not_logged_in)
    {
      
      
      density0<-exp(-(hash_count*length(logged_in))/m)
      density1<-1-density0
      probability<-density1**hash_count#where 2 is no of hash function
      cat(test_data[i],"is a false positive with prbability-",probability,"\n")
    }
    else
    {
      cat(test_data[i],"is probably present\n")
    
    }
    
  }
  else
  {
    cat(test_data[i],"is definitely not present\n")
   }
}