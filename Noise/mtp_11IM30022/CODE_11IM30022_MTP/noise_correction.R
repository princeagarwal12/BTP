# code for noise correction using our method
setwd("C:/")  
x <- read.csv("training.csv")
user_type = read.csv("user_type.csv")
item_type = read.csv("item_type.csv")

for(i in 1:nrow(x))
{
  for(j in 1:ncol(x)){
    if(user_type[i,1]== 0 & item_type[j,1]==0 & x[i,j] >=k){
      x[i,j] = k}
    else if(user_type[i,1]==1 & item_type[j,1]==1 & x[i,j] >= v || (x[i,j] < k & x[i,j] > 0 )){
      x[i,j] = (k+v)/2}
    else if(user_type[i,1]== 2 & item_type[j,1]== 2 & x[i,j] < v & x[i,j] !=0){
      x[i,j] = v}
  }
}
write.csv(x, "noise_removed.csv")