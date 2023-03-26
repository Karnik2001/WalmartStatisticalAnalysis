##################  XGBoost 

## Walmart
# reproduce the dataset 
set.seed(100)

# split the dataset into a training and test dataset
wd <- createDataPartition(walmart$Weekly_Sales, p= .75, list = F)
wtrain <- walmart[wd,]
wtest <- walmart[-wd,]

# predictor and response variables in training set
w_xtrain = data.matrix(wtrain[,-4])
w_ytrain = (wtrain[,4])

# predictor and response variables in test set
w_xtest = data.matrix(wtest[,-4])
w_ytest = (wtest[,4])

# final training and testing sets
w_xgb = xgb.DMatrix(data = w_xtrain, label = w_ytrain)
w_xgb1 = xgb.DMatrix(data = w_xtest, label = w_ytest)

# create the watchlist
wlist = list(wtrain = w_xgb, wtest = w_xgb1)

# Fit the XGBoost model and show the training and test datasets of each round
wmodel = xgb.train(data = w_xgb, max.depth = 7, watchlist = wlist, nrounds =  300)

# create the final model
wfinal = xgboost(data = w_xgb, max.depth = 7, nrounds = 100, verbose = 0)

# predict the final model from the XGBoost model
w_ypred <- predict(wfinal, w_xgb1)

# MSE and R2
mean((w_ytest - w_ypred)^2)
caret::R2(w_ytest,w_ypred)

## Walmart 1

# reproduce the dataset 
set.seed(100)

# split the dataset into a training and test dataset
wd1 <- createDataPartition(walmart1$Weekly_Sales, p= .75, list = F)
wtrain1 <- walmart1[wd1,]
wtest1 <- walmart1[-wd1,]

# predictor and response variables in training set
w_xtrain1 = data.matrix(wtrain1[,-4])
w_ytrain1 = (wtrain1[,4])

# predictor and response variables in test set
w_xtest1 = data.matrix(wtest1[,-4])
w_ytest1 = (wtest1[,4])

# final training and testing sets
w_xgb2 = xgb.DMatrix(data = w_xtrain1, label = w_ytrain1)
w_xgb3 = xgb.DMatrix(data = w_xtest1, label = w_ytest1)

wlist1 = list(wtrain1 = w_xgb3, wtest1 = w_xgb3)

# Fit the XGBoost model and show the training and test datasets of each round
wmodel2 = xgb.train(data = w_xgb2, max.depth = 7, watchlist = wlist1, nrounds =  300)

# create the final model
wfinal1 = xgboost(data = w_xgb2, max.depth = 7, nrounds = 100,, verbose = 0)

# predict the final model from the XGBoost model
w_ypred1 <- predict(wfinal1, w_xgb3)

mean((w_ytest1 - w_ypred1)^2)
caret::R2(w_ytest1,w_ypred1)

## Walmart 2

# reproduce the dataset 
set.seed(100)

# split the dataset into a training and test dataset
wd2 <- createDataPartition(walmart2$Weekly_Sales, p= .75, list = F)
wtrain2 <- walmart2[wd2,]
wtest2 <- walmart2[-wd2,]

# predictor and response variables in training set
w_xtrain2 = data.matrix(wtrain2[,-4])
w_ytrain2 = (wtrain2[,4])

# predictor and response variables in test set
w_xtest2 = data.matrix(wtest2[,-4])
w_ytest2 = (wtest2[,4])

# final training and testing sets
w_xgb4 = xgb.DMatrix(data = w_xtrain2, label = w_ytrain2)
w_xgb5 = xgb.DMatrix(data = w_xtest2, label = w_ytest2)

# create the watchlist
wlist = list(wtrain = w_xgb4, wtest = w_xgb5)

# Fit the XGBoost model and show the training and test datasets of each round
wmodel2 = xgb.train(data = w_xgb4, max.depth = 7, watchlist = wlist, nrounds =  300)

# create the final model
wfinal2 = xgboost(data = w_xgb4, max.depth = 7, nrounds = 100, verbose = 0)

# predict the final model from the XGBoost model
w_ypred2 <- predict(wfinal2, w_xgb5)

# MSE and R2
mean((w_ytest2 - w_ypred2)^2)
caret::R2(w_ytest2,w_ypred2)

## Walmart 3

# reproduce the dataset 
set.seed(100)

# split the dataset into a training and test dataset
wd3 <- createDataPartition(walmart3$Weekly_Sales, p= .75, list = F)
wtrain3 <- walmart3[wd3,]
wtest3 <- walmart3[-wd3,]

# predictor and response variables in training set
w_xtrain3 = data.matrix(wtrain3[,-4])
w_ytrain3 = (wtrain3[,4])

# predictor and response variables in test set
w_xtest3 = data.matrix(wtest3[,-4])
w_ytest3 = (wtest3[,4])

# final training and testing sets
w_xgb6 = xgb.DMatrix(data = w_xtrain3, label = w_ytrain3)
w_xgb7 = xgb.DMatrix(data = w_xtest3, label = w_ytest3)

# create the watchlist
wlist3 = list(wtrain3 = w_xgb6, wtest3 = w_xgb6)

# Fit the XGBoost model and show the training and test datasets of each round
wmodel3 = xgb.train(data = w_xgb6, max.depth = 7, watchlist = wlist3, nrounds =  300)

# create the final model
wfinal3 = xgboost(data = w_xgb6, max.depth = 7, nrounds = 100, verbose = 0)

# predict the final model from the XGBoost model
w_ypred3 <- predict(wfinal3, w_xgb7)

# MSE and R2
mean((w_ytest3 - w_ypred3)^2)
caret::R2(w_ytest3,w_ypred3)

## Walmart 4

# reproduce the dataset 
set.seed(100)

# split the dataset into a training and test dataset
wd4 <- createDataPartition(walmart4$Weekly_Sales, p= .75, list = F)
wtrain4 <- walmart4[wd4,]
wtest4 <- walmart4[-wd4,]

# predictor and response variables in training set
w_xtrain4 = data.matrix(wtrain4[,-4])
w_ytrain4 = (wtrain4[,4])

# predictor and response variables in test set
w_xtest4 = data.matrix(wtest4[,-4])
w_ytest4 = (wtest4[,4])

# final training and testing sets
w_xgb8 = xgb.DMatrix(data = w_xtrain4, label = w_ytrain4)
w_xgb9 = xgb.DMatrix(data = w_xtest4, label = w_ytest4)

# create the watchlist
wlist4 = list(wtrain4 = w_xgb8, wtest4 = w_xgb8)

# Fit the XGBoost model and show the training and test datasets of each round
wmodel4 = xgb.train(data = w_xgb8, max.depth = 7, watchlist = wlist4, nrounds =  300)

# create the final model
wfinal4 = xgboost(data = w_xgb8, max.depth = 7, nrounds = 100, verbose = 0)

# predict the final model from the XGBoost model
w_ypred4 <- predict(wfinal4, w_xgb9)

# MSE and R2
mean((w_ytest4 - w_ypred4)^2) 
caret::R2(w_ytest4,w_ypred4)

## Walmart 5

# reproduce the dataset 
set.seed(100)

# split the dataset into a training and test dataset
wd5 <- createDataPartition(walmart5$Weekly_Sales, p= .75, list = F)
wtrain5 <- walmart5[wd4,]
wtest5 <- walmart5[-wd4,]

# predictor and response variables in training set
w_xtrain5 = data.matrix(wtrain5[,-4])
w_ytrain5 = (wtrain5[,4])

# predictor and response variables in test set
w_xtest5 = data.matrix(wtest5[,-4])
w_ytest5 = (wtest5[,4])

# final training and testing sets
w_xgb10 = xgb.DMatrix(data = w_xtrain5, label = w_ytrain5)
w_xgb11 = xgb.DMatrix(data = w_xtest5, label = w_ytest5)

# create the watchlist
wlist5 = list(wtrain5 = w_xgb10, wtest5 = w_xgb10)

# Fit the XGBoost model and show the training and test datasets of each round
wmodel5 = xgb.train(data = w_xgb10, max.depth = 7, watchlist = wlist5, nrounds =  300)

# create the final model
wfinal5 = xgboost(data = w_xgb10, max.depth = 7, nrounds = 100, verbose = 0)

# predict the final model from the XGBoost model
w_ypred5 <- predict(wfinal5, w_xgb11)

# MSE and R2
mean((w_ytest5 - w_ypred5)^2) 
caret::R2(w_ytest5,w_ypred5)


################ Random Forest

## Walmart 1

# reproduce the dataset 
set.seed(100)

# create the random forest model
wf <- randomForest(formula = Weekly_Sales ~ Temperature + Fuel_Price + CPI
                   + Unemployment + Volume, data = walmart1)

# summary of the model
print(wf)

# trees that have the lowest test MSE
which.min(wf$mse)

# RMSE of the best model
wf$rsq[which.min(wf$rsq)^2]

# plot the model
plot(wf)

# adjust the parameters
wtune <- tuneRF(x = walmart1[,5:10], y = walmart1$Weekly_Sales, ntreeTry = 550,
                mtryStart = 5, stepFactor = 0.5, improve = 0.05, trace = FALSE)

# create new data frame
tune <- data.frame(Temperature = 100, CPI = 220, Fuel_Price = 5.00, 
                   Unemployment = 11.00, Volume = 160000000)

# predict the sales price
predict(wf, newdata = tune)

## Walmart 2

# reproduce the sample
set.seed(100)

# create the random forest model
wf1 <- randomForest(formula = Weekly_Sales ~ Temperature + Fuel_Price + CPI
                    + Unemployment  + Volume, data = walmart2)

# summary of the model
print(wf1)

# trees that have the lowest test MSE
which.min(wf1$mse)

# RMSE of the best model
wf1$rsq[which.min(wf1$rsq)^2]

# plot the model
plot(wf1)

# adjust the parameters
wtune1 <- tuneRF(x = walmart2[,5:10], y = walmart2$Weekly_Sales, ntreeTry = 550,
                 mtryStart = 5, stepFactor = 0.5, improve = 0.05, trace = FALSE)

# create new data frame
tune1 <- data.frame(Temperature = 100, CPI = 220, Fuel_Price = 5.00, 
                   Unemployment = 11.00, Volume = 160000000)

# predict the sales price
predict(wf1, newdata = tune1)

## Walmart 3

# reproduce the sample
set.seed(100)

# create the random forest model
wf2 <- randomForest(formula = Weekly_Sales ~ Temperature + Fuel_Price + CPI
                    + Unemployment  + Volume, data = walmart3)

# summary of the model
print(wf2)

# trees that have the lowest test MSE
which.min(wf2$mse)

# RMSE of the best model
wf2$rsq[which.min(wf2$rsq)^2]

# plot the model
plot(wf2)

# adjust the parameters
wtune2 <- tuneRF(x = walmart3[,5:10], y = walmart3$Weekly_Sales, ntreeTry = 550,
                 mtryStart = 5, stepFactor = 0.5, improve = 0.05, trace = FALSE)

# create new data frame
tune2 <- data.frame(Temperature = 100, CPI = 220, Fuel_Price = 5.00, 
                    Unemployment = 11.00, Volume = 160000000)

# predict the sales price
predict(wf2, newdata = tune2)

## Walmart 4

# reproduce the sample
set.seed(100)

# create the random forest model
wf3 <- randomForest(formula = Weekly_Sales ~ Temperature + Fuel_Price + CPI
                    + Unemployment  + Volume, data = walmart4)

# summary of the model
print(wf3)

# trees that have the lowest test MSE
which.min(wf3$mse)

# RMSE of the best model
wf3$rsq[which.min(wf3$rsq)^2]

# plot the model
plot(wf3)

# adjust the parameters
wtune2 <- tuneRF(x = walmart4[,5:10], y = walmart4$Weekly_Sales, ntreeTry = 550,
                 mtryStart = 5, stepFactor = 0.5, improve = 0.05, trace = FALSE)

# create new data frame
tune3 <- data.frame(Temperature = 100, CPI = 220, Fuel_Price = 5.00, 
                    Unemployment = 11.00, Volume = 160000000)

# predict the sales price
predict(wf3, newdata = tune3)


## Walmart 5

# create the random forest model

set.seed(100)
wf4 <- randomForest(formula = Weekly_Sales ~ Temperature + Fuel_Price + CPI
                    + Unemployment  + Volume, data = walmart5)

# summary of the model
print(wf4)

# trees that have the lowest test MSE
which.min(wf4$mse)

# RMSE of the best model
wf4$rsq[which.min(wf4$rsq)^2]

# plot the model
plot(wf4)

# adjust the parameters
wtune2 <- tuneRF(x = walmart5[,5:10], y = walmart5$Weekly_Sales, ntreeTry = 550,
                 mtryStart = 5, stepFactor = 0.5, improve = 0.05, trace = FALSE)

# create new data frame
tune4 <- data.frame(Temperature = 100, CPI = 220, Fuel_Price = 5.00, 
                    Unemployment = 11.00, Volume = 160000000)

# predict the sales price
predict(wf4, newdata = tune4)


################## Regression Tree

## Walmart 1

# create decision tree model
wrtreec <- rpart(Weekly_Sales ~ Temperature + CPI + Unemployment + Volume,
                 data = walmart1, control = rpart.control(cp = 0.00001))

# summary of the complex parameters
printcp(wrtreec)

# find the best cp value
wrbest <- wrtreec$cptable[which.min(wrtreec$cptable[,"xerror"]),"CP"]

# create a prune tree
wptreec1 <- prune(wrtreec , cp = wrbest)

# plot the prune tree
prp(wptreec1, faclen = 1, extra = 1, roundint = F, digits = 3)

# create new dataframe
wframe <- data.frame(Temperature = 74, CPI = 250, Unemployment = 11.50, Volume = 16000000)

# predict the sale price
predict(wptreec1, newdata = wframe)

# calculate the mse
wlp <- predict(wptreec1, newdata = wframe)
wo <- walmart1$Weekly_Sales
wlpm <- mean((wo - wlp)^2)
wlpm

# calculate the rsquare
wo1 <- walmart1$Weekly_Sales
wtrpredict <- predict(wptreec1, newdata = walmart1)
wrsse <- sum((wo1 - wtrpredict)^2)
wrsst <- sum((wo1 - mean(wo1))^2)
wrr <- 1 - wrsse/wrsst
wrr

## Walmart 2

# create decision tree model
wrtree1 <- rpart(Weekly_Sales ~ Temperature + CPI + Unemployment + Volume,
                 data = walmart2, control = rpart.control(cp = 0.00001))

# summary of the complex parameters
printcp(wrtree1)

# find the best cp value
wrbest <- wrtree1$cptable[which.min(wrtree1$cptable[,"xerror"]),"CP"]

# create a prune tree
wptree1 <- prune(wrtree1 , cp = wrbest)

# plot the prune tree
prp(wptree1, faclen = 1, extra = 1, roundint = F, digits = 3)

# create new dataframe
wframe1 <- data.frame(Temperature = 81, CPI = 250, Unemployment = 11.50, Volume = 16000000)

# predict the sale price
predict(wptree1, newdata = wframe)

# calculate the mse
wlp1 <- predict(wptree1, newdata = wframe1)
wo1 <- walmart2$Weekly_Sales
wlpm1 <- mean((wo1 - wlp1)^2)
wlpm1

# calculate the rsquare
wo1 <- walmart2$Weekly_Sales
wtrpredict1 <- predict(wrtree1, newdata = walmart2)
wrsse1 <- sum((wo1 - wtrpredict1)^2)
wrsst1 <- sum((wo1 - mean(wo1))^2)
wrr1 <- 1 - wrsse1/wrsst1
wrr1

## Walmart 3 

# create decision tree model
wrtree2 <- rpart(Weekly_Sales ~ Temperature + CPI + Unemployment + Volume,
                 data = walmart3, control = rpart.control(cp = 0.00001))

# summary of the complex parameters
printcp(wrtree2)

# find the best cp value
wrbest2 <- wrtree2$cptable[which.min(wrtree2$cptable[,"xerror"]),"CP"]

# create a prune tree
wptree2 <- prune(wrtree2 , cp = wrbest2)

# plot the prune tree
prp(wptree2, faclen = 1, extra = 1, roundint = F, digits = 3)

# create new dataframe
wframe2 <- data.frame(Temperature = 74, CPI = 250, Unemployment = 11.50, Volume = 16000000)

# predict the sale price
predict(wptree2, newdata = wframe2)

# calculate the mse
wlp2 <- predict(wptree2, newdata = wframe2)
wo2 <- walmart3$Weekly_Sales
wlpm2 <- mean((wo2 - wlp2)^2)
wlpm2

# calculate the rsquare
wtrpredict2 <- predict(wrtree2, newdata = walmart3)
wrsse2 <- sum((wo2 - wtrpredict2)^2)
wrsst2 <- sum((wo2 - mean(wo2))^2)
wrr2 <- 1 - wrsse2/wrsst2
wrr2

# Walmart 4 

# create decision tree model
wrtree3 <- rpart(Weekly_Sales ~ Temperature + CPI + Unemployment + Volume,
                 data = walmart4, control = rpart.control(cp = 0.00001))

# summary of the complex parameters
printcp(wrtree3)

# find the best cp value
wrbest3 <- wrtree3$cptable[which.min(wrtree3$cptable[,"xerror"]),"CP"]

# create a prune tree
wptree3 <- prune(wrtree3 , cp = wrbest3)

# plot the prune tree
prp(wptree3, faclen = 1, extra = 1, roundint = F, digits = 3)

# create new dataframe
wframe3 <- data.frame(Temperature = 74, CPI = 130, Unemployment = 11.50, Volume = 16000000)

# predict the sale price
predict(wptree3, newdata = wframe3)

# calculate the mse
wlp3 <- predict(wptree3, newdata = wframe3)
wo3 <- walmart4$Weekly_Sales
wlpm3 <- mean((wo3 - wlp3)^2)
wlpm3

# calculate the rsquare
wtrpredict3 <- predict(wrtree3, newdata = walmart4)
wrsse3 <- sum((wo3 - wtrpredict3)^2)
wrsst3 <- sum((wo3 - mean(wo3))^2)
wrr3 <- 1 - wrsse3/wrsst3
wrr3

## Walmart 5

# create decision tree model
wrtree4 <- rpart(Weekly_Sales ~ Temperature + CPI + Unemployment + Volume,
                 data = walmart5, control = rpart.control(cp = 0.00001))

# summary of the complex parameters
printcp(wrtree4)

# find the best cp value
wrbest4 <- wrtree4$cptable[which.min(wrtree4$cptable[,"xerror"]),"CP"]

# create a prune tree
wptree4 <- prune(wrtree4 , cp = wrbest4)

# plot the prune tree
prp(wptree4, faclen = 1, extra = 1, roundint = F, digits = 3)

# create new dataframe
wframe4 <- data.frame(Temperature = 74, CPI = 250, Unemployment = 11.50, Volume = 16000000)

# predict the sale price
predict(wptree4, newdata = wframe4)

# calculate the mse
wlp4 <- predict(wptree4, newdata = wframe4)
wo4 <- walmart5$Weekly_Sales
wlpm4 <- mean((wo4 - wlp4)^2)
wlpm4

# calculate the rsquare
wtrpredict4 <- predict(wrtree4, newdata = walmart5)
wrsse4 <- sum((wo4 - wtrpredict4)^2)
wrsst4 <- sum((wo4 - mean(wo4))^2)
wrr4 <- 1 - wrsse4/wrsst4
wrr4









