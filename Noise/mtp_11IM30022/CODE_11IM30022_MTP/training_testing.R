setwd("E:/New Folder/")
data = read.csv("Movielense.csv",header = FALSE)
x <- data.frame(data)
rating = 0
for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    if(x[i,j]>0){
      rating = rating + 1
    }
  }
}
count = 0
while(count < (0.9*rating)){
  x1 = round(runif(1,1,nrow(x)))
  y1 = round(runif(1,1,ncol(x)))
  if(x[x1,y1] > 0){
    x[x1,y1] = 0
    count = count + 1
  }
}
write.csv(x,"sparse.csv") 
# creating testing data
#x = read.csv("sparse.csv",header = FALSE)   
testing = matrix(0, nrow =nrow(x), ncol =ncol(x))
actual= matrix(0, nrow =nrow(x), ncol =ncol(x))
testing = data.frame(testing)
actual = data.frame(actual)

rating = 0
for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    if(x[i,j]>0){
      rating = rating + 1
    }
  }
}
count = 0
while(count < (0.3*rating)){
  x1 = round(runif(1,1,nrow(x)))
  y1 = round(runif(1,1,ncol(x)))
  if(x[x1,y1] > 0){
    if(testing[x1,y1] == 0){
      testing[x1,y1] = 100
      actual[x1,y1] = x[x1,y1]
      count = count + 1
    }
  }
}

write.csv(actual,"actual.csv")
write.csv(testing, "testing.csv")
for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    if(testing[i,j]==100){
      x[i,j]=0
    }
  }
}
write.csv(x,"training.csv")