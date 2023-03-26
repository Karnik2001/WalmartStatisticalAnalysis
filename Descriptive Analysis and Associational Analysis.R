library(readr) # reading csv files
library(Hmisc) # useful functions for advance data analysis
library(psych) # multivariate analysis and scale construction
library(pastecs) # analysis of space-time series
library(doBy) # groupwise statistics
library(dplyr) # data manipulation
library(tidyr) # sorting the data
library(GGally) # visuals with additional geometric objects
library(ggplot2) # visualization
library(stats) # regression analysis
library(car) # regression analysis
library(caret) # model training
library(randomForest)  # classification and regression
library(party) # decision tree
library(xgboost) # extreme gradient boosting
library(forecast) # univariate time series
library(lm.beta) # standardized regression cofficients
library(Metrics) # Evaluate metrics for machine learning models
library(moments) # test for skewness and kurtosis
library(psych) # multivaraite and scale analysis
library(GPArotation) # gradient projection algorithms for factor analysis
library(tidyverse) # transform and better present data
library(devtools) # package development tools
library(gmodels) # model fitting
library(aod) # analysis of overdispersed data
library(rpart) # classification and regression trees
library(rpart.plot) # plot rpart models
library(caret) # model training for classification and regression problems
library(pROC) # plot ROC curves
library(tree) # create decision trees
library(glmnet) # fit a linear model 
library(VGAM) # fit vector linear and additive models
library(ipred) # improving predictive models
library(MLmetrics) # metrics for machine learning models
library(tseries) # time series analysis
library(lmtest) # Testing linear regression model
library(lubridate) # data manipulation of date-times and other functions 
library(zoo) # Irregular time series
library(gridExtra) # combine the plots together
library(goftest) # Statistical testing

# Load the datasets
walmart <- read_csv("C:\\Users\\visha\\Downloads\\walmart_cleaned.csv")
wmt <- read_csv("C:\\Users\\visha\\Downloads\\WMT.csv")

# View the dataset
View(walmart)
View(wmt)

# Drop unnecessary columns
walmart <- walmart[c(-1,-9,-10,-11,-12,-13,-16,-17)]
wmt <- wmt[c(-2,-3,-4,-5,-6)]

# Join the two datasets together
walmart <- merge(walmart,wmt, by="Date")

# View the dataset again
View(walmart)

# Sort the date 
walmart <- walmart %>%
  arrange(mdy(walmart$Date))
View(walmart)

# Check for null values
walmart %>%
  is.na()%>%
  colSums()

# Descriptive statistics of the dataset
describe(walmart$Weekly_Sales)
describe(walmart$Temperature)
describe(walmart$Fuel_Price)
describe(walmart$CPI)
describe(walmart$Unemployment)
describe(walmart$Volume)

# Calculate distribution of each variable in the dataset
quantile(walmart$Weekly_Sales)
quantile(walmart$Temperature)
quantile(walmart$Fuel_Price)
quantile(walmart$CPI)
quantile(walmart$Unemployment)
quantile(walmart$Volume)

# Calculate variance of each variable in the dataset
var(walmart$Weekly_Sales)
var(walmart$Temperature)
var(walmart$Fuel_Price)
var(walmart$CPI)
var(walmart$Unemployment)
var(walmart$Volume)

# Calculate standard deviation of each variable in the dataset
sd(walmart$Weekly_Sales)
sd(walmart$Temperature)
sd(walmart$Fuel_Price)
sd(walmart$CPI)
sd(walmart$Unemployment)
sd(walmart$Volume)

# Coefficient of Variation
sd(walmart$Weekly_Sales)/mean(walmart$Weekly_Sales)
sd(walmart$Temperature)/mean(walmart$Temperature)
sd(walmart$Fuel_Price)/mean(walmart$Fuel_Price)
sd(walmart$CPI)/mean(walmart$CPI)
sd(walmart$Unemployment)/mean(walmart$Unemployment)
sd(walmart$Volume)/mean(walmart$Volume)

# Calculate the measure of symmetry in each variable in the dataset
skewness(walmart$Weekly_Sales)
skewness(walmart$Temperature)
skewness(walmart$Fuel_Price)
skewness(walmart$CPI)
skewness(walmart$Unemployment)
skewness(walmart$Volume)

# Calculate the measure of the tails in each variable in the dataset
kurtosis(walmart$Weekly_Sales)
kurtosis(walmart$Temperature)
kurtosis(walmart$Fuel_Price)
kurtosis(walmart$CPI)
kurtosis(walmart$Unemployment)
kurtosis(walmart$Volume)

# Create regression models
regmodel <- lm(Weekly_Sales ~ Temperature, data = walmart)
summary(regmodel)$r.squared
summary(regmodel)$coefficients

regmodel1 <- lm(Weekly_Sales ~ Fuel_Price, data = walmart)
summary(regmodel1)$r.squared
summary(regmodel1)$coefficients

regmodel2 <- lm(Weekly_Sales ~ CPI, data = walmart)
summary(regmodel2)$r.squared
summary(regmodel2)$coefficients

regmodel3 <- lm(Weekly_Sales ~ Unemployment, data = walmart)
summary(regmodel3)$r.squared
summary(regmodel3)$coefficients

regmodel4 <- lm(Weekly_Sales ~ Volume, data = walmart)
summary(regmodel4)$r.squared
summary(regmodel4)$coefficients

# Plot the Weekly_Sales variable against the other independent variable
wg <- ggplot(walmart) + aes(x = as.numeric(Weekly_Sales)) +
  geom_histogram(aes(y = stat(density)), fill = "coral") + 
  geom_density(col = "brown4") + 
  theme(panel.background = element_rect(fill = "darkslategray1"))

wg2 <- ggplot(walmart) + aes(x = as.numeric(Temperature)) + 
  geom_histogram(aes(y = stat(density)), fill = "blue4") + 
  geom_density(col = "lightpink") + 
  theme(panel.background = element_rect(fill = "lawngreen"))

wg3 <- ggplot(walmart) + aes(x = as.numeric(Fuel_Price)) + 
  geom_histogram(aes(y = stat(density)), fill = "pink") + 
  geom_density(col = "darkblue") + theme(panel.background = element_rect(fill = "deepskyblue"))

wg4 <- ggplot(walmart) + aes(x = as.numeric(CPI)) + 
  geom_histogram(aes(y = stat(density)), fill = "goldenrod1") + 
  geom_density(col = "darkred") + theme(panel.background = element_rect(fill = "deepskyblue"))

wg5 <- ggplot(walmart) + aes(x = as.numeric(Unemployment)) + 
  geom_histogram(aes(y = stat(density)), fill = "aquamarine2") +
  geom_density(col = "chocolate") +  theme(panel.background = element_rect(fill = "dodgerblue4"))

wg6 <- ggplot(walmart) + aes(x = as.numeric(Volume)) + 
  geom_histogram(aes(y = stat(density)), fill = "darkolivegreen1") + 
  geom_density(col = "blueviolet") +  theme(panel.background = element_rect(fill = "aliceblue"))
grid.arrange(wg,wg2,wg3,wg4,wg5)

# Determine any outliers in each variable column
out <- boxplot.stats(walmart$Weekly_Sales)
out_ind <- which(walmart$Weekly_Sales %in% c(out))
out_ind

out1 <- boxplot.stats(walmart$Temperature)
out_ind1 <- which(walmart$Temperature %in% c(out))
out_ind1

out2 <- boxplot.stats(walmart$Fuel_Price)
out_ind2 <- which(walmart$Fuel_Price %in% c(out))
out_ind2

out3 <- boxplot.stats(walmart$CPI)
out_ind3 <- which(walmart$CPI %in% c(out))
out_ind3

out4 <- boxplot.stats(walmart$Unemployment)
out_ind4 <- which(walmart$Unemployment %in% c(out))
out_ind4

out5 <- boxplot.stats(walmart$Volume)
out_ind5 <- which(walmart$Volume %in% c(out))
out_ind5

# Determine outliers by creating boxplots

par(mfrow = c(2, 3))
b <- boxplot(walmart$Weekly_Sales, ylab = "Weekly Sales", col = "green", main = "Boxplot of Weekly Sales")
b2 <- boxplot(walmart$Temperature, ylab = "Temperature", col = "red", main = "Boxplot of Temperature")
b3 <- boxplot(walmart$Fuel_Price, ylab = "Fuel Price", col = "blue", main = "Boxplot of Fuel Prices")
b4 <- boxplot(walmart$CPI, ylab = "CPI", col = "orange", main = "Boxplot of CPI")
b5 <- boxplot(walmart$Unemployment, ylab = "Unemployment Rate", col = "pink", main = "Boxplot of Unemployment")
b6 <- boxplot(walmart$Volume, ylab = "Volume", col = "paleturquoise3", main = "Boxplot of Volume")
