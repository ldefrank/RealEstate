realestate_temp <- read.csv("real_estate.csv")

#extract columns 3, 4, 5, and 8 
realestate1 <- realestate_temp[,c(3,4,5,8)]

#convert dates in 2012 to 0 and 2013 to 1 in order to use as categorical variable
year = ifelse(realestate_temp$X1.transaction.date < 2012.999,0,1)

#combine new variable year to 4 extracted columns
realestate = cbind(year,realestate1)

#rename columns 
names(realestate)[1] <- "Year"
names(realestate)[2] <- "House.Age"
names(realestate)[3] <- "Distance.MRT"
names(realestate)[4] <- "Convenience.Stores"
names(realestate)[5] <- "House.Price"

#create first order model
reg = lm(House.Price~Year+House.Age+Distance.MRT+Convenience.Stores, data=realestate)
summary(reg)

#check for multicollinearity
library(car)
vif(reg)

#residual analysis
resid = reg$resid
fitted = reg$fitted
par(mfrow=c(2,2))
plot(fitted,resid,pch=19,col="blue",main="Residuals vs Fitted")
abline(0,0)
plot(resid~realestate$House.Price,pch=19,col="magenta",main="Residuals vs Y")
abline(0,0)
hist(resid,main="Histogram of Residuals")
qqnorm(resid,main="Q-Q Plot")
qqline(resid)
par(mfrow=c(2,2))
plot(resid~realestate$House.Age,pch=19,col="green",main="Residuals vs X2")
abline(0,0)
plot(resid~realestate$Distance.MRT,pch=19,col="green",main="Residuals vs X3")
abline(0,0)
plot(resid~realestate$Convenience.Stores,pch=19,col="green",main="Residuals vs X4")
abline(0,0)

#create second order model
reg2 = lm(log(House.Price)~(Year+House.Age+Distance.MRT+Convenience.Stores)^2+
        I(House.Age^2)+I(Distance.MRT^2)+I(Convenience.Stores^2),data=realestate)
      summary(reg2)

# extract some of the quantitative variables:
# X2.house.age, X3.distance.to.the.nearest.MRT.station, X4.number.of.convenience.stores
sub = realestate[,c(3,4,5,8)]
head(sub)

# normalize columns of the dataset
set.seed(1)
subtemp_norm <- as.data.frame(lapply(sub, nor))
head(subtemp_norm)

# create dummy variable 
year=ifelse(realestate1$X1.transaction.date<2012.999,0,1)
sub_norm <- cbind(year, subtemp_norm)


# Randomly select 90% of the indices (rows) of the 
# dataset. This will be the indices of the training set
inTrain <- sample(1:nrow(realestate1), 0.9 * nrow(realestate1)) 

# extract training set
realestate_trainX <- sub_norm[inTrain,]
realestate_trainY <- realestate1[inTrain,8]

# extract testing set
realestate_testX <- sub_norm[-inTrain,]
realestate_testY <- realestate1[-inTrain,8]

library(caret)
library(ModelMetrics)

numk <- 30
RMSEvec <- numeric(numk)
for (k in c(1:numk)) {
  # run knnreg function
  fit <- knnreg(realestate_trainX, realestate_trainY, k)
  
  # predict on test set
  realestate_pred <- predict(fit, realestate_testX)
  
  # calculate rmse
  RMSEvec[k] <- rmse(realestate_testY,realestate_pred)
}
plot(c(1:numk),RMSEvec,type="l",col="blue",
     xlab="k",ylab="RMSE on test set")

#set normalization function 
nor = function(x) { 
  (x -min(x))/(max(x)-min(x))   
}

#normalize predictor columns
  realestate_norm = as.data.frame(lapply(realestate[,1:4], nor))
  summary(realestate_norm)

#create variable = number of rows
  numdata = nrow(realestate_norm)
  
#create prediction variable column
  realestate_pred = numeric(numdata)
 
#Leave One Out Cross Validation - LOOCV
  for (i in 1:numdata) {
    
    #extract training set 
    realestate_trainX = realestate_norm[-i,]
    realestate_trainY = as.data.frame(realestate[-i,5])
    names(realestate_trainY)[1]="House.Price"
    
    #extract testing set 
    realestate_testX = realestate_norm[i,]
    realestate_testY = realestate[i,5]
    
    realestate_traindata = data.frame(realestate_trainX,realestate_trainY)
    
    #run kNN function
    fit = lm(House.Price~Year+House.Age+Distance.MRT+Convenience.Stores,
         data = realestate_traindata)
    
    realestate_pred[i] = predict(fit, realestate_testX)
    
  }
  
  library(ModelMetrics)
  realestate_rmse = rmse(realestate[,5],realestate_pred)
  
  print(sprintf("RMSE for multiple regression model: %.4f",realestate_rmse))

realestate1 <- realestate[, c(2, 3, 4, 5, 8)]
head(realestate1)

# create normalization function
nor <-function(x) {
  (x -min(x))/(max(x)-min(x)) }
  
# extract some of the quantitative variables:
# X2.house.age, X3.distance.to.the.nearest.MRT.station, X4.number.of.convenience.stores

sub = realestate1[,c(2:5)]
head(sub)
# normalize columns of the dataset
subtemp_norm <- as.data.frame(lapply(sub, nor))
head(subtemp_norm)

# create dummy variable 
year = ifelse(realestate1$X1.transaction.date < 2012.999, 0, 1)

sub_norm <- cbind(year, subtemp_norm)
head(sub_norm)
set.seed(1)
inTrain <- sample(1:nrow(sub_norm), 0.9*nrow(sub_norm))

sub_traindata <- sub_norm[inTrain,]
head(sub_traindata)

sub_testdata <- sub_norm[-inTrain,]
head(sub_testdata)

library(neuralnet)

numHiddenNodes = 7
NN <- neuralnet(Y.house.price.of.unit.area ~ X2.house.age + 
                  X3.distance.to.the.nearest.MRT.station + 
                  X4.number.of.convenience.stores + year, 
                data = sub_traindata, hidden = numHiddenNodes, linear.output = T)

plot(NN)

sub_pred <- predict(NN, sub_testdata[,1:4])
head(sub_pred)
head(sub_testdata)

plot(sub_testdata[,5], sub_pred)

origscale <-function(x, xmin, xmax) {
  xmin + x*(xmax - xmin)
}

MinHousePrice<-min(realestate1[,5])
MaxHousePrice<-max(realestate1[,5])
pred_origscale <- origscale(sub_pred, MinHousePrice, MaxHousePrice)
head(pred_origscale)

library(ModelMetrics)
realestate_rmse <- rmse(realestate1[-inTrain,5], pred_origscale)

print(paste('RMSE of Neural Network with ', numHiddenNodes,
            ' hidden nodes on test set: ', realestate_rmse))

