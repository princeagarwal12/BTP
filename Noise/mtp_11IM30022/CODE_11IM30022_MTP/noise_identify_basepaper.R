# change working directory accordingly
setwd("C:/")  
data <- read.csv("training.csv")
x <- data.framet(data)
y <- data.frame(t(x))
nid <- data.frame(x)
# Assigning user to different classes
user_type =  matrix(0, nrow = nrow(x), ncol=1 )
user_type= data.frame(user_type)
k <- 2
v <- 4
for (i in 1:nrow(x)){
  w = 0
  a = 0
  s = 0
  
  for (j in 1:ncol(x)){
    if (x[i,j] > 0 && x[i,j] < k){
      w = w + 1 ;
    }
    else if(x[i,j] >= k && x[i,j] < v){
      a = a + 1;
    }
    else if(x[i,j] >= v) {
      s = s + 1;
    }
  }
  if(w >= a + s){
    user_type[i,1] = 0
  }
  else if (a >= w + s){
    user_type[i,1] = 1
  }
  else if ( s >= w + a) {
    user_type[i,1] = 2
  }
  else{
    user_type[i,1] = 3
  }
  
}
write.csv(user_type, "user_type.csv")

# Assigning user to different classes

item_type =  matrix(0, nrow = nrow(y), ncol=1 )
item_type= data.frame(item_type)
for (i in 1:nrow(y)){
    w = 0
    a = 0
    s = 0
    
    for (j in 1:ncol(y)){
      if (y[i,j] > 0 && y[i,j] < k){
        w = w + 1 ;
      }
      else if(y[i,j] >= k && y[i,j] < v){
        a = a + 1;
      }
      else if(y[i,j] >= v) {
        s = s + 1;
      }
    }
    if(w >= a + s){
      item_type[i,1] = 0
    }
    else if (a >= w + s){
      item_type[i,1] = 1
    }
    else if ( s >= w + a) {
      item_type[i,1] = 2
    }
    else{
      item_type[i,1] = 3
    }
  } 
write.csv(item_type, "item_type.csv")

for(i in 1:nrow(x))
{
  for(j in 1:ncol(x)){
    if(user_type[i,1]== 0 & item_type[j,1]==0 & x[i,j] >=k){
      nid[i,j] = NA}
    else if(user_type[i,1]==1 & item_type[j,1]==1 & x[i,j] >= v || (x[i,j] < k & x[i,j] > 0 )){
      nid[i,j] = NA}
    if(user_type[i,1]== 2 & item_type[j,1]== 2 & x[i,j] < v & x[i,j] !=0){
      nid[i,j] = NA}
  }
}
write.csv(nid, "noise.csv")