library(digest)
word<-readline("enter")
arr<-c(strsplit(word))
arr
for(string in arr)
  {
  hash_val<-digest(string, "md5", serialize = FALSE)
  hash_val
}

