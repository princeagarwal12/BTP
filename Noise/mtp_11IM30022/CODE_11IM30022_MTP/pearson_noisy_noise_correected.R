setwd("E:/New folder/")
# change data accordingly, for re-predict use "training.csv"
# for our method to predcit with noise corrected data "noise_removed.csv"
data = read.csv("training.csv",header = FALSE)
x <- data.frame(data)


binary = matrix(0, nrow = nrow(x), ncol = ncol(x))
binary = data.frame(binary)
for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    if(x[i,j]>0){
      binary[i,j] = 1
    }
    else{
      binary[i,j] = 0
    }
  }
}

z = matrix(0, nrow = nrow(x), ncol = ncol(x))
z = data.frame(z)

for(i in 1:nrow(x)){
  for(j in 1:5){
    z[i,j]= length(which(x[i,] == j))
  }
}

denominator = matrix(0, nrow= nrow(x), ncol =1)
denominator = data.frame(denominator)
for(i in 1:nrow(x)){
  denominator[i,1]= rowSums(z[i,1:5])
}

mean = matrix(0 , nrow = nrow(x), ncol= 1)
mean = data.frame(mean)
for(i in 1:nrow(x)){
  mean[i,1] = rowSums(x[i,])/denominator[i,]
}


for(i in 1:nrow(x)){
  if(is.na(mean[i,1])){
    mean[i,1] = 0
  }
}

dif = matrix(0, nrow = nrow(x), ncol = ncol(x))
dif = data.frame(dif)

for(i in 1:nrow(x)){
  dif[i,] = (x[i,]- mean[i,1])*binary[i,]
  
}

dif = as.matrix(dif)

pearson_sim = matrix(0, nrow = nrow(x), ncol = nrow(x))
pearson_sim = data.frame(pearson_sim)
for(i in 1:nrow(x)){
  for(j in i:nrow(x)){
    pearson_sim[i,j] = crossprod(dif[i,],dif[j,])/(sqrt(sum(dif[i,]*dif[i,]*binary[j,]))*(sqrt(sum(dif[j,]*dif[j,]*binary[i,]))))
  }
}


for(i in 2:nrow(x)){
  for(j in 1: (i-1)){
    pearson_sim[i,j] = pearson_sim[j,i]
  }
}

for(i in 1:nrow(x)){
  for(j in 1:nrow(x)){
    if(is.na( pearson_sim[i,j])){
      pearson_sim[i,j] = 0
    }
  }
}

for(i in 1:nrow(x)){
  for(j in 1:nrow(x)){
    if(pearson_sim[i,j] == 0){
      pearson_sim[i,j] = -100
    }
  }
}


pearson_sim = signif(pearson_sim, digits = 8)


sorted = matrix(0, nrow = nrow(pearson_sim), ncol = ncol(pearson_sim)) 
sorted = data.frame(sorted)

for(i in 1:nrow(pearson_sim)){
  sorted[i,] = sort(pearson_sim[i,], decreasing = TRUE)
}

for(i in 1:nrow(x)){
  for(j in 1:nrow(x)){
    if(sorted[i,j] == -100){
      sorted[i,j] = 0
    }
  }
}

for(i in 1:nrow(x)){
  for(j in 1:nrow(x)){
    if(pearson_sim[i,j] == -100){
      pearson_sim[i,j] = 0
    }
  }
}

pearson_sim = signif(pearson_sim, digits = 8)




find <- matrix(0, nrow = nrow(x), ncol= 100)
find = data.frame(find)

for(i in 1:nrow(x)){
  for(k in 1:50){
    if(as.matrix(dim(as.matrix( which(pearson_sim[i,] == sorted[i,k]))))[1,1] == 1){
      find[i,k] =  which(pearson_sim[i,] == sorted[i,k])
    }
    if(as.matrix(dim(as.matrix( which(pearson_sim[i,] == sorted[i,k]))))[1,1] > 1) {
      l =  as.matrix(dim(as.matrix( which(pearson_sim[i,] == sorted[i,k]))))[1,1]
      many = as.matrix(which(pearson_sim[i,] == sorted[i,k]))
      find[i,k] = many[1,1]
      pearson_sim[i,many[1,1]] = 0
      
    }
    
  }
}
write.csv(find,"find.csv")

mean = matrix(0 , nrow = nrow(x), ncol= 1)
mean = data.frame(mean)
for(i in 1:nrow(x)){
  mean[i,1] = rowSums(x[i,])/denominator[i,]
}
for(i in 1:nrow(x)){
  if(is.na(mean[i,1])){
    mean[i,1] = 0
  }
}
write.csv(mean,"mean.csv")

knn = 50
actual = read.csv("Movielense.csv",header = FALSE)
testing = read.csv("testing.csv",header = FALSE)
predict = matrix(0 , nrow = nrow(x) ,ncol =ncol(x))
predict = data.frame(predict)
for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    if(testing[i,j]==100){
      for(k in 1:knn){
        if(x[find[i,k],j] > 0){
          predict[i,j] = predict[i,j] + sorted[i,k]*(x[find[i,k],j]- mean[find[i,k],1])
        }
      }
      
    }
  }
}

predict_deno = matrix(0 , nrow = nrow(x) ,ncol = ncol(x))
predict_deno = data.frame(predict_deno)

for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    if(testing[i,j]==100){
      for(k in 1:knn){
        if(x[find[i,k],j] > 0){
          predict_deno[i,j] = predict_deno[i,j] + abs(sorted[i,k])
        }
      }
      
    }
  }
}

prediction = matrix(0 , nrow = nrow(x), ncol= ncol(x))
prediction = data.frame(prediction)
for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    if(testing[i,j]==100){
      prediction[i,j] = mean[i,1]+(predict[i,j]/predict_deno[i,j])
    }
  }
  
}

write.csv(prediction,"prediction.csv")

#remove the na value from prediction 
for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    if(is.na(prediction[i,j])){
      prediction[i,j] = 0
  }
}
}



MAE = 0
count = 0 
sum_test = sum(testing)/100

for(i in 1:nrow(prediction)){
  for(j in 1:ncol(prediction)){
    if(testing[i,j]=100){
      count = count + 1
      MAE = MAE + abs(actual[i,j]- prediction[i,j])
    }
    
  }
}
MAE = MAE/sum_test

# -------------------- RMSE
RMSE = 0
for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    if(testing[i,j]=100){
      RMSE = RMSE + (actual[i,j]- prediction[i,j])*(actual[i,j]- prediction[i,j])
    }
    
  }
}

RMSE = sqrt(RMSE/sum_test)

##########################################################################################
# true positive
tp = 0
for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    if(prediction[i,j] >=4 & actual[i,j]>=4){
      tp = tp + 1
    }
  }
}

fp = 0
for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    if(prediction[i,j] >=4 & actual[i,j] < 4){
      fp = fp+1
    }
  }
}

precision = tp/(fp+tp)

fn = 0
for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    if(testing[i,j]==100){
      if(actual[i,j] >=4 & prediction[i,j] <4 & prediction[i,j] > 0){
        fn = fn+1
      }
      
    }
  }
}

recall = tp/(tp+fn)
F1 = (2*precision*recall)/(precision+recall)

knn
MAE
RMSE
precision
recall
F1