library(digest)
m=50
filter = rep(0,m)
URL_clicked=c('www.google.com','www.facebook.com','www.yahoo.com','www.gamespot.com','www.ign.com',
              'www.stayfit.com','www.gamer.com','www.gmail.com','www.outlook.com','www.mailer.com')


URL_notclicked=c('www.insider.com','www.outsider.com','www.paytm.com','www.gpay.com','www.gameology.com',
                 'www.youpay.com','www.msrit.com','www.theforum.com','www.bms.com','www.pvr.com')
hf1<-function(value)
{
  x<-digest(object=value,algo='crc32',serialize = TRUE)
  y=paste('0x',x,sep="")
  index<-as.numeric(y)%%m
  return(index)
}
hf2<-function(value)
{
  x<-digest(object=value,algo='murmur32',serialize = TRUE)
  y=paste('0x',x,sep="")
  index<-as.numeric(y)%%m
  return(index)
}

for(i in 1:length(URL_clicked))
{

  l[hf1(URL_clicked[i])]<-1
  l[hf2(URL_clicked[i])]<-1
}
check<-function(value)
{
  
  index1<-hf1(value)
  index2<-hf2(value)
  if(l[index1]=='1' && l[index2]=='1')
  {
    return(TRUE)
  }
  return(FALSE)
}
test_data=c(URL_clicked[1:5],URL_notclicked)
test_data
for(i in 1:length(test_data))
{
  if(check(test_data[i]))
  {
    if(test_data[i] %in% URL_notclicked)
    {
      print("Its a false positive with");
      density0<-exp(-(2*10)/m)
      density1<-1-density0
      probability<-density1**2#where 2 is no of hash function
      print(probability*100)
    }
    else
    {
      print("URL is clicked previoulsy")
    }
    
  }
  else
  {
    print("this url is not clicked--")
  }
}

