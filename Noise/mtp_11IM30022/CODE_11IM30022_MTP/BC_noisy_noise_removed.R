# code for prediction with BC using our noise correction method
setwd("E:/New Folder/")
x= read.csv("noise_removed.csv",header = FALSE)
x = data.frame(t(x))
testing = read.csv("testing.csv",header = FALSE)
testing = data.frame(t(testing))
actual = read.csv("actual.csv",header = FALSE)
actual = data.frame(t(actual))
max_rate = 5
z = matrix(0, nrow =nrow(x), ncol =5)
z =data.frame(z)
for(i in 1:nrow(x)){
  for(j in 1:max_rate){
    z[i,j]= length(which(x[i,] == j))
  }
}
write.csv(z,"z.csv")
denominator = matrix(0, nrow= nrow(x), ncol =1)
denominator = data.frame(denominator)
for(i in 1:nrow(x)){
  denominator[i,1]= rowSums(z[i,])
}
similarity = matrix(0, nrow= nrow(x), ncol = 5)
similarity = data.frame(similarity)
for(i in 1:nrow(x)){
  for(j in 1:5){
    similarity[i,j] = sqrt(z[i,j]/denominator[i,1])
  }
}
write.csv(similarity, "similarity.csv")
sim_bc = matrix(0, nrow = nrow(x), ncol =nrow(x))
sim_bc = data.frame(sim_bc)
for (i in 1:nrow(x)){
  for(j in i:nrow(x)){
    sim_bc[i,j] = rowSums(similarity[i,]*similarity[j,])
  }
}
for(i in 2:nrow(x)){
  for(j in 1: (i-1)){
    sim_bc[i,j] = sim_bc[j,i]
  }
}

for(i in 1:nrow(x)){
  for(j in 1:nrow(x)){
    if(is.na(sim_bc[i,j])){
      sim_bc[i,j] = 0
    }
  }
}
write.csv(sim_bc, "sim_bc.csv")
write.csv(denominator, "denominator.csv")
sorted = matrix(0, nrow = nrow(sim_bc), ncol = nrow(sim_bc)) 
sorted = data.frame(sorted)
for(i in 1:nrow(sim_bc)){
  sorted[i,] = sort(sim_bc[i,], decreasing = TRUE)
}
write.csv(sorted,"sorted.csv")
find <- matrix(0, nrow = nrow(sim_bc), ncol= 100)
find = data.frame(find)

for(i in 1:nrow(sim_bc)){
  for(k in 1:100 ){
    if(as.matrix(dim(as.matrix( which(sim_bc[i,] == sorted[i,k]))))[1,1] == 1){
      find[i,k] =  which(sim_bc[i,] == sorted[i,k])
    }
    if(as.matrix(dim(as.matrix( which(sim_bc[i,] == sorted[i,k]))))[1,1] > 1) {
      l =  as.matrix(dim(as.matrix( which(sim_bc[i,] == sorted[i,k]))))[1,1]
      many = as.matrix(which(sim_bc[i,] == sorted[i,k]))
      find[i,k] = many[1,1]
      sim_bc[i,many[1,1]] = 0
      
    }
    
  }
}
write.csv(find,"find.csv")
mean = matrix(0 , nrow = nrow(x), ncol= 1)
mean = data.frame(mean)
for(i in 1:nrow(x)){
  mean[i,1] = rowSums(x[i,])/denominator[i,1]
}
for(i in 1:nrow(x)){
  if(is.na(mean[i,1])){
    mean[i,1] = 0
  }
}
write.csv(mean,"mean.csv")
knn = 100
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

for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    if(is.na(prediction[i,j])){
      prediction[i,j] = 0
    }
  }
}

sum_text = sum(testing)/100

MAE = 0
count = 0 

for(i in 1:nrow(prediction)){
  for(j in 1:ncol(prediction)){
    if(testing[i,j]==100){
      count = count + 1
      MAE = MAE + abs(actual[i,j]- prediction[i,j])
    }
    
  }
}
MAE = MAE/sum_text

# -------------------- RMSE
RMSE = 0
for(i in 1:nrow(x)){
  for(j in 1:ncol(x)){
    if(testing[i,j]==100){
      RMSE = RMSE + (actual[i,j]- prediction[i,j])*(actual[i,j]- prediction[i,j])
    }
    
  }
}

RMSE = sqrt(RMSE/sum_test)


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