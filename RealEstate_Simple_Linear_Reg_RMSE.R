realestate_temp <- read.csv("real_estate.csv")

realestate1 <- realestate_temp[,c(3,4,5,8)]

year = ifelse(realestate_temp$X1.transaction.date < 2012.999,0,1)

realestate = cbind(year,realestate1)

names(realestate)[1] <- "Year"
names(realestate)[2] <- "House.Age"
names(realestate)[3] <- "Distance.MRT"
names(realestate)[4] <- "Convenience.Stores"
names(realestate)[5] <- "House.Price"

nor = function(x) { 
  (x -min(x))/(max(x)-min(x))   
}

  realestate_norm = as.data.frame(lapply(realestate[,1:4], nor))
  
  numdata = nrow(realestate_norm)
  
  realestate_pred = numeric(numdata)
  
  for (i in 1:numdata) {
    
    realestate_trainX = realestate_norm[-i,]
    realestate_trainY = as.data.frame(realestate[-i,5])
    names(realestate_trainY)[1]="House.Price"
    
    realestate_testX = realestate_norm[i,]
    realestate_testY = realestate[i,5]
    
    realestate_traindata = data.frame(realestate_trainX,realestate_trainY)
    
    fit = lm(House.Price~Year+House.Age+Distance.MRT+Convenience.Stores,
         data = realestate_traindata)
    
    realestate_pred[i] = predict(fit, realestate_testX)
    
  }
  
  library(ModelMetrics)
  realestate_rmse = rmse(realestate[,5],realestate_pred)
  
  print(sprintf("RMSE for multiple regression model: %.4f",realestate_rmse))