# Filter out store 1- 5 with the number of the Department

walmart1 <- walmart%>%
  filter(Store == 1 & Dept == 1) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  filter(Date >= "2012-01-01", Date <= "2012-12-31")

walmart2 <- walmart%>%
  filter(Store == 2 & Dept == 2) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  filter(Date >= "2012-01-01", Date <= "2012-12-31")

walmart3 <- walmart%>%
  filter(Store == 3 & Dept == 3) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  filter(Date >= "2012-01-01", Date <= "2012-12-31")

walmart4 <- walmart%>%
  filter(Store == 4 & Dept == 4) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  filter(Date >= "2012-01-01", Date <= "2012-12-31")

walmart5 <- walmart%>%
  filter(Store == 5 & Dept == 5) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  filter(Date >= "2012-01-01", Date <= "2012-12-31")

# Graph all 5 stores with each variable

# Weekly Sales
g <-ggplot(data = walmart1, mapping = aes(x = Date, y = Weekly_Sales)) + ggtitle("2012 of Walmart 1 Weekly Sales") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "lightpink"))

g2 <- ggplot(data = walmart2, mapping = aes(x = Date, y = Weekly_Sales)) +  ggtitle("2012 of Walmart 2 Weekly Sales") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "darkslategray3"))
  
g3 <- ggplot(data = walmart3, mapping = aes(x = Date, y = Weekly_Sales)) +  ggtitle("2012 of Walmart 3 Weekly Sales") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "lightblue"))

g4 <- ggplot(data = walmart4, mapping = aes(x = Date, y = Weekly_Sales)) + ggtitle("2012 of Walmart 4 Weekly Sales") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "chocolate3"))

g5 <- ggplot(data = walmart5, mapping = aes(x = Date, y = Weekly_Sales)) + ggtitle("2012 of Walmart 5 Weekly Sales") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "darkcyan")) 

# Temperature
g6 <- ggplot(data = walmart1, mapping = aes(x = Date, y = Temperature)) + ggtitle("2012 of Walmart 1 Temperature") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "khaki2")) 

g7 <- ggplot(data = walmart2, mapping = aes(x = Date, y = Temperature)) + ggtitle("2012 of Walmart 2 Temperature") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "darksalmon")) 

g8 <- ggplot(data = walmart3, mapping = aes(x = Date, y = Temperature)) + ggtitle("2012 of Walmart 3 Temperature") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "honeydew2")) 

g9 <- ggplot(data = walmart4, mapping = aes(x = Date, y = Temperature)) + ggtitle("2012 of Walmart 4 Temperature") +
  geom_line() +
  geom_point() 

g10 <- ggplot(data = walmart5, mapping = aes(x = Date, y = Temperature)) + ggtitle("2012 of Walmart 5 Temperature") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "blueviolet"))

# Fuel Price
g11 <- ggplot(data = walmart1, mapping = aes(x = Date, y = Fuel_Price)) + ggtitle("2012 of Walmart 1 Fuel Price") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "mediumspringgreen")) 

g12 <- ggplot(data = walmart2, mapping = aes(x = Date, y = Fuel_Price)) + ggtitle("2012 of Walmart 2 Fuel Price") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "mediumpurple")) 

g13 <- ggplot(data = walmart3, mapping = aes(x = Date, y = Fuel_Price)) + ggtitle("2012 of Walmart 3 Fuel Price") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "firebrick4")) 

g14 <- ggplot(data = walmart4, mapping = aes(x = Date, y = Fuel_Price)) + ggtitle("2012 of Walmart 4 Fuel Price") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "goldenrod")) 

g15 <- ggplot(data = walmart5, mapping = aes(x = Date, y = Fuel_Price)) + ggtitle("2012 of Walmart 5 Fuel Price") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "darkseagreen1")) 

# Unemployment
g16 <- ggplot(data = walmart1, mapping = aes(x = Date, y = Unemployment)) + ggtitle("2012 of Walmart 1 Unemployment") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "darksalmon")) 

g17 <- ggplot(data = walmart2, mapping = aes(x = Date, y = Unemployment)) + ggtitle("2012 of Walmart 2 Unemployment") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "cyan4")) 

g18 <- ggplot(data = walmart3, mapping = aes(x = Date, y = Unemployment)) + ggtitle("2012 of Walmart 3 Unemployment") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "dodgerblue2")) 

g19 <- ggplot(data = walmart4, mapping = aes(x = Date, y = Unemployment)) + ggtitle("2012 of Walmart 4 Unemployment") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "lightpink2")) 

g20 <- ggplot(data = walmart5, mapping = aes(x = Date, y = Unemployment)) + ggtitle("2012 of Walmart 5 Unemployment") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "magenta1")) 

# CPI
g21 <- ggplot(data = walmart1, mapping = aes(x = Date, y = CPI)) + ggtitle("2012 of Walmart 1 CPI") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "orangered1")) 

g22 <- ggplot(data = walmart2, mapping = aes(x = Date, y = CPI)) + ggtitle("2012 of Walmart 2 CPI") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "palevioletred")) 

g23<- ggplot(data = walmart3, mapping = aes(x = Date, y = CPI)) + ggtitle("2012 of Walmart 3 CPI") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "dodgerblue4")) 

g24 <- ggplot(data = walmart4, mapping = aes(x = Date, y = CPI)) + ggtitle("2012 of Walmart 4 CPI") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "cornsilk1")) 

g25 <- ggplot(data = walmart5, mapping = aes(x = Date, y = CPI)) + ggtitle("2012 of Walmart 5 CPI") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "orchid2")) 

# Volume
g26 <- ggplot(data = walmart1, mapping = aes(x = Date, y = Volume)) + ggtitle("2012 of Walmart 1 Volume") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "darkolivegreen2")) 

g27 <- ggplot(data = walmart2, mapping = aes(x = Date, y = Volume)) + ggtitle("2012 of Walmart 2 Volume") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "coral")) 

g28 <- ggplot(data = walmart3, mapping = aes(x = Date, y = Volume)) + ggtitle("2012 of Walmart 3 Volume") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "lightsteelblue3")) 

g29 <- ggplot(data = walmart4, mapping = aes(x = Date, y = Volume)) + ggtitle("2012 of Walmart 4 Volume") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "chartreuse3")) 

g30 <- ggplot(data = walmart5, mapping = aes(x = Date, y = Volume)) + ggtitle("2012 of Walmart 5 Volume") +
  geom_line() +
  geom_point() +
  theme(plot.background = element_rect(fill = "cadetblue2")) 

grid.arrange(g, g2, g3, g4, g5, ncol=2)
grid.arrange(g6, g7, g8, g9, g10, ncol=2)
grid.arrange(g11, g12, g13, g14, g15, ncol=2)
grid.arrange(g16, g17, g18, g19, g20, ncol=2)
grid.arrange(g21, g22, g23, g25, g25, ncol=2)
grid.arrange(g26, g27, g28, g29, g30, ncol=2)

################## ARIMA Model

## Walmart 1

# TS plot
wts <- ts(walmart1$Weekly_Sales, start = c(2010,2,5), frequency = 12)
autoplot(wts)

# Decompose the data
wts_dec <- decompose(wts, type = "additive")
autoplot(wts_dec)

# Split the dataset into a training and test datasets
w_tesales <- tail(wts, 15)
w_trsales <- head(wts, 139)

# Determine if the data is stationary or not
w_trsales %>%
  adf.test()

# Reduce the p-value less than 0.05
w_trsales %>%
  diff()%>%
  adf.test()

# Plot the data
w_trsales %>%
  diff()%>%
  tsdisplay()

# Plot the Arima model
w_arima <- auto.arima(y = w_trsales, seasonal = T)
autoplot(w_arima$fitted, series = "Sarima") + autolayer(w_trsales, series = 'Actual')
w_arima$aic
w_arima$bic

# plot the Holtz-Winters model
w_holtz <- HoltWinters(x = w_trsales, alpha = 0.5, beta = 0.1, gamma = F)
autoplot(w_holtz$fitted[,1], series = "hw") +
  autolayer(w_trsales, series = "Actual")

# Forecast both models with a 24 month period
w_forecast <- forecast(w_arima, h = 24)
w_forecast1 <- forecast(w_holtz, h = 24)

# Plot the forecast models
w_trsales%>%
  autoplot() + autolayer(w_forecast, series = "ARIMA Forecast") +
  autolayer(w_tesales, series = "test data")

w_trsales%>%
  autoplot() + autolayer(w_forecast1, series = "Holtz Forecast") +
  autolayer(w_tesales, series = "test data")

# Ljung-Box test  
Box.test(w_forecast$residuals, type = "Ljung-Box")
Box.test(w_forecast1$residuals, type = "Ljung-Box")

plot(w_forecast$residuals, col = "red") 
plot(w_forecast1$residuals, col = "blue")

## Walmart 2

# TS plot
wts1 <- ts(walmart2$Weekly_Sales, start = c(2010,2,5), frequency = 12)
autoplot(wts1)

# Decompose the data
wts_dec1 <- decompose(wts1, type = "additive")
autoplot(wts_dec1)

# Split the dataset into a training and test datasets
w_tesales1 <- tail(wts1, 15)
w_trsales1 <- head(wts1, 139)

# Determine if the data is stationary or not
w_trsales1 %>%
  adf.test()

# Reduce the p-value less than 0.05 if necessary
w_trsales1 %>%
  diff()%>%
  adf.test()

# Plot the data
w_trsales1 %>%
  diff()%>%
  tsdisplay()

# Plot the ARIMA models
w_arima1 <- auto.arima(y = w_trsales1, seasonal = T)
autoplot(w_arima1$fitted, series = "Sarima") + autolayer(w_trsales1, series = 'Actual')

# plot the Holtz-Winters model
w_holtz1 <- HoltWinters(x = w_trsales1, alpha = 0.5, beta = 0.1, gamma = F)
autoplot(w_holtz1$fitted[,1], series = "hw") +
  autolayer(w_trsales1, series = "Holtz-Winters")

# Forecast both models with a 24 month period
w_forecast2 <- forecast(w_arima1, h = 24)
w_forecast3 <- forecast(w_holtz1, h = 24)

# Plot the forecast models
w_trsales1%>%
  autoplot() + autolayer(w_forecast2, series = "ARIMA Forecast") +
  autolayer(w_tesales1, series = "ARIMA")

w_trsales1%>%
  autoplot() + autolayer(w_forecast3, series = "Holtz-Winters Forecast") +
  autolayer(w_tesales1, series = "Holtz-Winters")

# Ljung-Box test  
Box.test(w_forecast2$residuals, type = "Ljung-Box")
Box.test(w_forecast3$residuals, type = "Ljung-Box")

# Plot the residuals 
plot(w_forecast2$residuals, col = "darkred")
plot(w_forecast3$residuals, col = "orange")


## Walmart 3

# TS plot
wts2 <- ts(walmart3$Weekly_Sales, start = c(2010,2,5), frequency = 12)
autoplot(wts2)

# Decompose the data
wts_dec2 <- decompose(wts2, type = "additive")
autoplot(wts_dec2)

# Split the dataset into a training and test datasets
w_tesales2 <- tail(wts2, 15)
w_trsales2 <- head(wts2, 139)

# Determine if the data is stationary or not
w_trsales2 %>%
  adf.test()

# Reduce the p-value less than 0.05 if necessary
w_trsales2 %>%
  diff()%>%
  adf.test()

# Plot the data
w_trsales2 %>%
  diff()%>%
  tsdisplay()

# Plot the ARIMA models
w_arima2 <- auto.arima(y = w_trsales2, seasonal = T)
autoplot(w_arima2$fitted, series = "Sarima") + autolayer(w_trsales2, series = 'Actual')

# Plot the Holtz-Winters models
w_holtz2 <- HoltWinters(x = w_trsales2, alpha = 0.5, beta = 0.1, gamma = F)
autoplot(w_holtz2$fitted[,1], series = "hw") +
  autolayer(w_trsales2, series = "Actual")

# Forecast both models with a 24 month period
w_forecast4 <- forecast(w_arima2, h = 24)
w_forecast5 <- forecast(w_holtz2, h = 24)

w_trsales2%>%
  autoplot() + autolayer(w_forecast4, series = "ARIMA Forecast") +
  autolayer(w_tesales2, series = "test data")

w_trsales2%>%
  autoplot() + autolayer(w_forecast5, series = "Holtz Forecast") +
  autolayer(w_tesales2, series = "test data")

# Ljung-Box test
Box.test(w_forecast4$residuals, type = "Ljung-Box")
Box.test(w_forecast5$residuals, type = "Ljung-Box")

# Forecast both residuals models with a 24 month period
plot(w_forecast4$residuals, col = "deeppink")
plot(w_forecast5$residuals, col = "forestgreen")

# Walmart 4

# TS plot
wts3 <- ts(walmart4$Weekly_Sales, start = c(2010,2,5), frequency = 12)
autoplot(wts3)

# Decompose the data
wts_dec3 <- decompose(wts3, type = "additive")
autoplot(wts_dec3)

# Split the dataset into a training and test datasets
w_tesales3 <- tail(wts3, 15)
w_trsales3 <- head(wts3, 139)

# Determine if the data is stationary or not
w_trsales3 %>%
  adf.test()

# Reduce the p-value less than 0.05 if necessary
w_trsales3 %>%
  diff()%>%
  adf.test()

# Plot the data
w_trsales3 %>%
  diff()%>%
  tsdisplay()

# Plot the ARIMA models
w_arima3 <- auto.arima(y = w_trsales3, seasonal = T)
autoplot(w_arima3$fitted, series = "Sarima") + autolayer(w_trsales3, series = 'Actual')

# Plot the Holtz-Winters models
w_holtz3 <- HoltWinters(x = w_trsales3, alpha = 0.5, beta = 0.1, gamma = F)
autoplot(w_holtz3$fitted[,1], series = "hw") +
  autolayer(w_trsales3, series = "Actual")

# Forecast both models with a 24 month period
w_forecast6 <- forecast(w_arima3, h = 24)
w_forecast7 <- forecast(w_holtz3, h = 24)

w_trsales3%>%
  autoplot() + autolayer(w_forecast6, series = "ARIMA Forecast") +
  autolayer(w_tesales3, series = "test data")

w_trsales3%>%
  autoplot() + autolayer(w_forecast7, series = "Holtz-Winters Forecast") +
  autolayer(w_tesales3, series = "test data")

# Ljung-Box test
Box.test(w_forecast6$residuals, type = "Ljung-Box")
Box.test(w_forecast7$residuals, type = "Ljung-Box")

# Forecast both residuals models with a 24 month period
plot(w_forecast6$residuals, col = "lightslateblue")
plot(w_forecast7$residuals, col = "cyan3")

## Walmart 5

# TS plot
wts5 <- ts(walmart5$Weekly_Sales, start = c(2010,2,5), frequency = 12)
autoplot(wts5)

# Decompose the data
wts_dec5 <- decompose(wts5, type = "additive")
autoplot(wts_dec5)

# Split the dataset into a training and test datasets
w_tesales5 <- tail(wts5, 15)
w_trsales5 <- head(wts5, 139)

# Determine if the data is stationary or not
w_trsales5 %>%
  adf.test()

# Reduce the p-value less than 0.05 if necessary
w_trsales5 %>%
  diff()%>%
  adf.test()

# Plot the data
w_trsales5 %>%
  diff()%>%
  tsdisplay()

# Plot the ARIMA models
w_arima5 <- auto.arima(y = w_trsales5, seasonal = T)
autoplot(w_arima5$fitted, series = "Sarima") + autolayer(w_trsales5, series = 'Actual')

# Plot the Holtz-Winters models
w_holtz5 <- HoltWinters(x = w_trsales5, alpha = 0.5, beta = 0.1, gamma = F)
autoplot(w_holtz5$fitted[,1], series = "hw") +
  autolayer(w_trsales5, series = "Actual")

# Forecast both models with a 24 month period
w_forecast8 <- forecast(w_arima5, h = 24)
w_forecast9 <- forecast(w_holtz5, h = 24)

w_trsales5%>%
  autoplot() + autolayer(w_forecast8, series = "ARIMA Forecast") +
  autolayer(w_tesales5, series = "test data")

w_trsales5%>%
  autoplot() + autolayer(w_forecast9, series = "Holtz-Winters Forecast") +
  autolayer(w_tesales5, series = "test data")

# Ljung-Box test
Box.test(w_forecast8$residuals, type = "Ljung-Box")
Box.test(w_forecast9$residuals, type = "Ljung-Box")

# Forecast both residuals models with a 24 month period
plot(w_forecast8$residuals, col = "maroon")
plot(w_forecast9$residuals, col = "steelblue3")


##################  Linear Regression

# linear regression model of each variable

# Walmart Temperature
regmodel <- lm(Weekly_Sales ~ Temperature, data = walmart)

# Calculate AIC, BIC, and R2
AIC(regmodel)
BIC(regmodel)
summary(regmodel)$r.squared

# Calculate MSE
walmart$predicted_sales <- predict(regmodel, newdata = walmart)
wlp <- mean((walmart$Weekly_Sales - walmart$predicted_sales)^2)
wlp

# Walmart 1 Temperature
regmodelt <- lm(Weekly_Sales ~ Temperature, data = walmart1)

# Calculate AIC, BIC, and R2
AIC(regmodelt)
BIC(regmodelt)
summary(regmodelt)$r.squared

# Calculate MSE
walmart1$predicted_sales <- predict(regmodelt, newdata = walmart1)
wlpt <- mean((walmart1$Weekly_Sales - walmart1$predicted_sales)^2)
wlpt

# Walmart 2 Temperature
regmodelt2 <- lm(Weekly_Sales ~ Temperature, data = walmart2)

# Calculate AIC, BIC, and R2
AIC(regmodelt2)
BIC(regmodelt2)
summary(regmodelt2)$r.squared

# Calculate MSE
walmart2$predicted_sales <- predict(regmodelt2, newdata = walmart2)
wlpt2 <- mean((walmart2$Weekly_Sales - walmart2$predicted_sales)^2)
wlpt2

# Walmart 3 Temperature
regmodelt3 <- lm(Weekly_Sales ~ Temperature, data = walmart3)

# Calculate AIC, BIC, and R2
AIC(regmodelt3)
BIC(regmodelt3)
summary(regmodelt3)$r.squared

# Calculate MSE
walmart3$predicted_sales <- predict(regmodelt3, newdata = walmart3)
wlpt3 <- mean((walmart3$Weekly_Sales - walmart3$predicted_sales)^2)
wlpt3

# Walmart 4 Temperature
regmodelt4 <- lm(Weekly_Sales ~ Temperature, data = walmart4)

# Calculate AIC, BIC, and R2
AIC(regmodelt4)
BIC(regmodelt4)
summary(regmodelt4)$r.squared

# Calculate MSE
walmart4$predicted_sales <- predict(regmodelt4, newdata = walmart4)
wlpt4 <- mean((walmart4$Weekly_Sales - walmart4$predicted_sales)^2)
wlpt4

# Walmart 5 Temperature
regmodelt5 <- lm(Weekly_Sales ~ Temperature, data = walmart5)

# Calculate AIC, BIC, and R2
AIC(regmodelt5)
BIC(regmodelt5)
summary(regmodelt5)$r.squared

# Calculate MSE
walmart5$predicted_sales <- predict(regmodelt5, newdata = walmart5)
wlpt5 <- mean((walmart5$Weekly_Sales - walmart5$predicted_sales)^2)
wlpt5

# Calculate the average of AIC, BIC, R2, and MSE
sum(summary(regmodelt)$r.squared,
    summary(regmodelt2)$r.squared,
    summary(regmodelt3)$r.squared,
    summary(regmodelt4)$r.squared,
    summary(regmodelt5)$r.squared)/5

sum(AIC(regmodelt),
    AIC(regmodelt2),
    AIC(regmodelt3),
    AIC(regmodelt4),
    AIC(regmodelt5))/5

sum(BIC(regmodelt),
    BIC(regmodelt2), 
    BIC(regmodelt3),
    BIC(regmodelt4),
    BIC(regmodelt5))/5

sum(wlpt,wlpt2,wlpt3,wlpt4,wlpt5)/5


# Walmart Fuel Price
regmodelff <- lm(Weekly_Sales ~ Fuel_Price, data = walmart)

# Calculate AIC, BIC, and R2
AIC(regmodelff)
BIC(regmodelff)
summary(regmodelff)$r.squared

# Calculate MSE
walmart$predicted_sales <- predict(regmodelff, newdata = walmart)
wlpff <- mean((walmart$Weekly_Sales - walmart$predicted_sales)^2)
wlpff

# Walmart 1 Fuel Price

regmodelf <- lm(Weekly_Sales ~ Fuel_Price, data = walmart1)

# Calculate AIC, BIC, and R2
AIC(regmodelf)
BIC(regmodelf)
summary(regmodelf)$r.squared

# Calculate MSE
walmart1$predicted_sales1 <- predict(regmodelf, newdata = walmart1)
wlpf <- mean((walmart1$Weekly_Sales - walmart1$predicted_sales1)^2)
wlpf

# Walmart 2 Fuel Price


regmodelf2 <- lm(Weekly_Sales ~ Fuel_Price, data = walmart2)

# Calculate AIC, BIC, and R2
AIC(regmodelf2)
BIC(regmodelf2)
summary(regmodelf2)$r.squared

# Calculate MSE
walmart2$predicted_sales2 <- predict(regmodelf2, newdata = walmart2)
wlpf2 <- mean((walmart2$Weekly_Sales - walmart2$predicted_sales2)^2)
wlpf2

# Walmart 3 Fuel Price

regmodelf3 <- lm(Weekly_Sales ~ Fuel_Price, data = walmart3)

# Calculate AIC, BIC, and R2
AIC(regmodelf3)
BIC(regmodelf3)
summary(regmodelf3)$r.squared

# Calculate MSE
walmart3$predicted_sales3 <- predict(regmodelf3, newdata = walmart3)
wlpf3 <- mean((walmart3$Weekly_Sales - walmart3$predicted_sales3)^2)
wlpf3

# Walmart 4 Fuel Price

regmodelf4 <- lm(Weekly_Sales ~ Fuel_Price, data = walmart4)

# Calculate AIC, BIC, and R2
AIC(regmodelf4)
BIC(regmodelf4)
summary(regmodelf4)$r.squared

# Calculate MSE
walmart4$predicted_sales4 <- predict(regmodelf4, newdata = walmart4)
wlpf4 <- mean((walmart4$Weekly_Sales - walmart4$predicted_sales4)^2)
wlpf4

# Walmart 5 Fuel Price

regmodelf5 <- lm(Weekly_Sales ~ Fuel_Price, data = walmart5)

# Calculate AIC, BIC, and R2
AIC(regmodelf5)
BIC(regmodelf5)
summary(regmodelf5)$r.squared

# Calculate MSE
walmart5$predicted_sales5 <- predict(regmodelf5, newdata = walmart5)
wlpf5 <- mean((walmart5$Weekly_Sales - walmart5$predicted_sales5)^2)
wlpf5

# Calculate the average of AIC, BIC, R2, and MSE
sum(summary(regmodelf)$r.squared,
    summary(regmodelf2)$r.squared,
    summary(regmodelf3)$r.squared,
    summary(regmodelf4)$r.squared,
    summary(regmodelf5)$r.squared)/5

sum(AIC(regmodelf),
    AIC(regmodelf2),
    AIC(regmodelf3),
    AIC(regmodelf4),
    AIC(regmodelf5))/5

sum(BIC(regmodelf),
    BIC(regmodelf2), 
    BIC(regmodelf3),
    BIC(regmodelf4),
    BIC(regmodelf5))/5

sum(wlpf,wlpf2,wlpf3,wlpf4,wlpf5)/5


# Walmart CPI
regmodel2c <- lm(Weekly_Sales ~ CPI, data = walmart)

# Calculate AIC, BIC, and R2
AIC(regmodel2c)
BIC(regmodel2c)
summary(regmodel2c)$r.squared

# Calculate MSE
walmart$predicted_sales2c <- predict(regmodel2c, newdata = walmart)
wlp2c <- mean((walmart$Weekly_Sales - walmart$predicted_sales2c)^2)
wlp2c

# Walmart 1 CPI
regmodel2c1 <- lm(Weekly_Sales ~ CPI, data = walmart1)

# Calculate AIC, BIC, and R2
AIC(regmodel2c1)
BIC(regmodel2c1)
summary(regmodel2c1)$r.squared

# Calculate MSE
walmart1$predicted_sales2c1 <- predict(regmodel2c1, newdata = walmart1)
wlp2c1 <- mean((walmart1$Weekly_Sales - walmart1$predicted_sales2c1)^2)
wlp2c1

# Walmart 2 CPI
regmodel2c2 <- lm(Weekly_Sales ~ CPI, data = walmart2)

# Calculate AIC, BIC, and R2
AIC(regmodel2c2)
BIC(regmodel2c2)
summary(regmodel2c2)$r.squared

# Calculate MSE
walmart2$predicted_sales2c2 <- predict(regmodel2c2, newdata = walmart2)
wlp2c2 <- mean((walmart2$Weekly_Sales - walmart2$predicted_sales2c2)^2)
wlp2c2

# Walmart 3 CPI
regmodel2c3 <- lm(Weekly_Sales ~ CPI, data = walmart3)

# Calculate AIC, BIC, and R2
AIC(regmodel2c3)
BIC(regmodel2c3)
summary(regmodel2c3)$r.squared

# Calculate MSE
walmart3$predicted_sales2c3 <- predict(regmodel2c2, newdata = walmart3)
wlp2c3 <- mean((walmart3$Weekly_Sales - walmart3$predicted_sales2c3)^2)
wlp2c3

# Walmart 4 CPI
regmodel2c4 <- lm(Weekly_Sales ~ CPI, data = walmart4)

# Calculate AIC, BIC, and R2
AIC(regmodel2c4)
BIC(regmodel2c4)
summary(regmodel2c4)$r.squared

# Calculate MSE
walmart4$predicted_sales2c4 <- predict(regmodel2c4, newdata = walmart4)
wlp2c4 <- mean((walmart4$Weekly_Sales - walmart4$predicted_sales2c4)^2)
wlp2c4

# Walmart 5 CPI
regmodel2c5 <- lm(Weekly_Sales ~ CPI, data = walmart5)

# Calculate AIC, BIC, and R2
AIC(regmodel2c5)
BIC(regmodel2c5)
summary(regmodel2c5)$r.squared

# Calculate MSE
walmart5$predicted_sales2c5 <- predict(regmodel2c5, newdata = walmart5)
wlp2c5 <- mean((walmart5$Weekly_Sales - walmart5$predicted_sales2c5)^2)
wlp2c5

# Calculate the average of AIC, BIC, R2, and MSE
sum(summary(regmodel2c1)$r.squared,
    summary(regmodel2c2)$r.squared,
    summary(regmodel2c3)$r.squared,
    summary(regmodel2c4)$r.squared,
    summary(regmodel2c5)$r.squared)/5

sum(AIC(regmodel2c1),
    AIC(regmodel2c2),
    AIC(regmodel2c3),
    AIC(regmodel2c4),
    AIC(regmodel2c5))/5

sum(BIC(regmodel2c1),
    BIC(regmodel2c2),
    BIC(regmodel2c3),
    BIC(regmodel2c4),
    BIC(regmodel2c5))/5

sum(wlp2c1,wlp2c2,wlp2c3,wlp2c4,wlp2c5)/5


# Walmart Unemployment
regmodel3 <- lm(Weekly_Sales ~ Unemployment, data = walmart)

# Calculate AIC, BIC, and R2
AIC(regmodel3)
BIC(regmodel3)
summary(regmodel3)$r.squared

# Calculate MSE
walmart$predicted_sales3 <- predict(regmodel3, newdata = walmart)
wlp3 <- mean((walmart$Weekly_Sales - walmart$predicted_sales3)^2)
wlp3

# Walmart 1 Unemployment
regmodel3u1 <- lm(Weekly_Sales ~ Unemployment, data = walmart1)

# Calculate AIC, BIC, and R2
AIC(regmodel3u1)
BIC(regmodel3u1)
summary(regmodel3u1)$r.squared

# Calculate MSE
walmart1$predicted_sales3u <- predict(regmodel3u1, newdata = walmart1)
wlp3u1 <- mean((walmart1$Weekly_Sales - walmart1$predicted_sales3u)^2)
wlp3u1

# Walmart 2 Unemployment
regmodel3u2 <- lm(Weekly_Sales ~ Unemployment, data = walmart2)

# Calculate AIC, BIC, and R2
AIC(regmodel3u2)
BIC(regmodel3u2)
summary(regmodel3u2)$r.squared

# Calculate MSE
walmart2$predicted_sales3u2 <- predict(regmodel3u2, newdata = walmart2)
wlp3u2 <- mean((walmart2$Weekly_Sales - walmart2$predicted_sales3u2)^2)
wlp3u2

# Walmart 3 Unemployment
regmodel3u3 <- lm(Weekly_Sales ~ Unemployment, data = walmart3)

# Calculate AIC, BIC, and R2
AIC(regmodel3u3)
BIC(regmodel3u3)
summary(regmodel3u3)$r.squared

# Calculate MSE
walmart3$predicted_sales3u3 <- predict(regmodel3u3, newdata = walmart3)
wlp3u3 <- mean((walmart3$Weekly_Sales - walmart3$predicted_sales3u3)^2)
wlp3u3

# Walmart 4 Unemployment
regmodel3u4 <- lm(Weekly_Sales ~ Unemployment, data = walmart4)

# Calculate AIC, BIC, and R2
AIC(regmodel3u4)
BIC(regmodel3u4)
summary(regmodel3u4)$r.squared

# Calculate MSE
walmart4$predicted_sales3u4 <- predict(regmodel3u4, newdata = walmart4)
wlp3u4 <- mean((walmart4$Weekly_Sales - walmart4$predicted_sales3u4)^2)
wlp3u4

# Walmart 5 Unemployment
regmodel3u5 <- lm(Weekly_Sales ~ Unemployment, data = walmart5)

# Calculate AIC, BIC, and R2
AIC(regmodel3u5)
BIC(regmodel3u5)
summary(regmodel3u5)$r.squared

# Calculate MSE
walmart5$predicted_sales3u5 <- predict(regmodel3u5, newdata = walmart5)
wlp3u5 <- mean((walmart5$Weekly_Sales - walmart5$predicted_sales3u5)^2)
wlp3u5

# Calculate the average of AIC, BIC, R2, and MSE
sum(summary(regmodel3u1)$r.squared,
    summary(regmodel3u2)$r.squared,
    summary(regmodel3u3)$r.squared,
    summary(regmodel3u4)$r.squared,
    summary(regmodel3u5)$r.squared)/5

sum(AIC(regmodel3u1),
    AIC(regmodel3u2),
    AIC(regmodel3u3),
    AIC(regmodel3u4),
    AIC(regmodel3u5))/5

sum(BIC(regmodel3u1),
    BIC(regmodel3u2),
    BIC(regmodel3u3),
    BIC(regmodel3u4),
    BIC(regmodel3u5))/5

sum(wlp3u1,wlp3u2,wlp3u3,wlp3u4,wlp3u5)/5


# Walmart Volume
regmodel4v1 <- lm(Weekly_Sales ~ Volume, data = walmart)

# Calculate AIC, BIC, and R2
AIC(regmodel4v1)
BIC(regmodel4v1)
summary(regmodel4v1)$r.squared

# Calculate MSE
walmart$predicted_sales4v <- predict(regmodel4v1, newdata = walmart)
wlp4v1 <- mean((walmart$Weekly_Sales - walmart$predicted_sales4v)^2)
wlp4v1

# Walmart 1 Volume
regmodel4v1 <- lm(Weekly_Sales ~ Volume, data = walmart1)

# Calculate AIC, BIC, and R2
AIC(regmodel4v1)
BIC(regmodel4v1)
summary(regmodel4v1)$r.squared

# Calculate MSE
walmart1$predicted_sales4v1 <- predict(regmodel4v1, newdata = walmart1)
wlp4v1 <- mean((walmart1$Weekly_Sales - walmart1$predicted_sales4v1)^2)
wlp4v1

# Walmart 2 Volume
regmodel4v2 <- lm(Weekly_Sales ~ Volume, data = walmart2)

# Calculate AIC, BIC, and R2
AIC(regmodel4v2)
BIC(regmodel4v2)
summary(regmodel4v2)$r.squared

# Calculate MSE
walmart2$predicted_sales4v2 <- predict(regmodel4v2, newdata = walmart2)
wlp4v2 <- mean((walmart2$Weekly_Sales - walmart2$predicted_sales4v2)^2)
wlp4v2

# Walmart 3 Volume
regmodel4v3 <- lm(Weekly_Sales ~ Volume, data = walmart3)

# Calculate AIC, BIC, and R2
AIC(regmodel4v3)
BIC(regmodel4v3)
summary(regmodel4v3)$r.squared

# Calculate MSE
walmart3$predicted_sales4v3 <- predict(regmodel4v3, newdata = walmart3)
wlp4v3 <- mean((walmart3$Weekly_Sales - walmart3$predicted_sales4v3)^2)
wlp4v3


# Walmart 4 Volume
regmodel4v4 <- lm(Weekly_Sales ~ Volume, data = walmart4)

# Calculate AIC, BIC, and R2
AIC(regmodel4v4)
BIC(regmodel4v4)
summary(regmodel4v4)$r.squared

# Calculate MSE
walmart4$predicted_sales4v4 <- predict(regmodel4v4, newdata = walmart4)
wlp4v4 <- mean((walmart4$Weekly_Sales - walmart4$predicted_sales4v4)^2)
wlp4v4


# Walmart 5 Volume
regmodel4v5 <- lm(Weekly_Sales ~ Volume, data = walmart5)

# Calculate AIC, BIC, and R2
AIC(regmodel4v5)
BIC(regmodel4v5)
summary(regmodel4v5)$r.squared

# Calculate MSE
walmart5$predicted_sales4v5 <- predict(regmodel4v5, newdata = walmart5)
wlp4v5 <- mean((walmart5$Weekly_Sales - walmart5$predicted_sales4v5)^2)
wlp4v5

# Calculate the average of AIC, BIC, R2, and MSE 
sum(summary(regmodel4v1)$r.squared,
    summary(regmodel4v2)$r.squared,
    summary(regmodel4v3)$r.squared,
    summary(regmodel4v4)$r.squared,
    summary(regmodel4v5)$r.squared)/5

sum(AIC(regmodel4v1),
    AIC(regmodel4v2),
    AIC(regmodel4v3),
    AIC(regmodel4v4),
    AIC(regmodel4v5))/5

sum(BIC(regmodel4v1),
    BIC(regmodel4v2),
    BIC(regmodel4v3),
    BIC(regmodel4v4),
    BIC(regmodel4v5))/5

sum(wlp4v1,wlp4v2,wlp4v3,wlp4v4,wlp4v5)/5


##################  Multilinear Regression

wfit <- lm(walmart$Weekly_Sales ~ walmart$Temperature + walmart$Fuel_Price + walmart$CPI
           + walmart$Unemployment + walmart$Volume, data = walmart)

# Calculate R2
summary(wfit)$r.squared
# Calculate MSE
mean(wfit$residuals^2)
# Calculate AIC
AIC(wfit)
# Calculate BIC 
BIC(wfit)


## Walmart 1

multwalmart1 <- lm(Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment + Volume, 
                   data = walmart1)

# Calculate R2
summary(multwalmart1)$r.squared
# Calculate MSE
mean(multwalmart1$residuals^2)
# Calculate AIC
AIC(multwalmart1)
# Calculate BIC
BIC(multwalmart1)

## Walmart 2

multwalmart2 <- lm(Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment + Volume, 
                   data = walmart2)
# Calculate R2 
summary(multwalmart2)$r.squared
# Calculate MSE
mean(multwalmart2$residuals^2)
# Calculate AIC
AIC(multwalmart2)
# Calculate BIC
BIC(multwalmart2)

## Walmart 3

multwalmart3 <- lm(Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment + Volume, 
                   data = walmart3)

# Calculate R2
summary(multwalmart3)$r.squared
# Calculate MSE
mean(multwalmart3$residuals^2)
# Calculate AIC
AIC(multwalmart3)
# Calculate BIC
BIC(multwalmart3)

## Walmart 4

multwalmart4 <- lm(Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment + Volume, 
                   data = walmart4)

# Calculate R2
summary(multwalmart4)$r.squared
# Calculate MSE
mean(multwalmart4$residuals^2)
# Calculate AIC
AIC(multwalmart4)
# Calculate BIC
BIC(multwalmart4)

## Walmart 5

multwalmart5 <- lm(Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment + Volume, 
                   data = walmart5)

# Calculate R2
summary(multwalmart5)$r.squared
# Calculate MSE
mean(multwalmart5$residuals^2)
# Calculate AIC
AIC(multwalmart5)
# Calculate BIC
BIC(multwalmart5)


##################  Logistic Regression

# reproduce the dataset 
set.seed(100)

# split the dataset into a training and test dataset
wsit <- sample(c(TRUE,FALSE), size = nrow(walmart), replace = TRUE, prob = c(0.7,0.3))
wtrains <- walmart[wsit,]
wtests <- walmart[!wsit,]

# create the logistic model
wlmodel <- glm(IsHoliday ~ Weekly_Sales + Temperature + Fuel_Price + CPI + Unemployment
               + Volume, family = "binomial", data = wtrains)

# reduction of scientifc notations
options(scipen = 999)

# summary of the model
summary(wlmodel)

# Calculate R2
pscl::pR2(wlmodel)["McFadden"]

# convert response to 0 and 1
threshold = 0.6
wpredict <- ifelse(predict(wlmodel, type = "response") > threshold,1,0)
wactual <- wlmodel$y

# Create confusion matrix
wconf <- table(wpredict, wactual)
wconf

# Create predict values
wpredicted <- predict(wlmodel, wtests, type = "response")

# Calculate MSE
mse(wactual,wpredict)
# Calculate AIC
AIC(wlmodel)
# Calculate BIC
BIC(wlmodel)

## Walmart 1 

# reproduce the dataset 
set.seed(100)

# split the dataset into a training and test dataset
ws <- sample(c(TRUE,FALSE), size = nrow(walmart1), replace = TRUE, prob = c(0.7,0.3))
wtrain <- walmart1[ws,]
wtest <- walmart1[!ws,]

# create the logistic model
wlmodel <- glm(IsHoliday ~ Weekly_Sales + Temperature + Fuel_Price + CPI + Unemployment
               + Volume, family = "binomial", data = wtrain)

# reduction of scientific notations
options(scipen = 999)

# summary of the model
summary(wlmodel)

# Calculate R2
pscl::pR2(wlmodel)["McFadden"]

# convert response to 0 and 1
threshold = 0.6
wpredict <- ifelse(predict(wlmodel, type = "response") > threshold,1,0)
wactual <- wlmodel$y

# Create confusion matrix
wconf <- table(wpredict, wactual)
wconf

# Create predict values
wpredicted <- predict(wlmodel, wtest, type = "response")

# Calculate MSE
mse(wactual,wpredict)

# Calculate AIC
AIC(wlmodel)

# Calculate BIC
BIC(wlmodel)

## Walmart 2

# reproduce the dataset
set.seed(100)

# split the dataset into a training and test dataset
ws1 <- sample(c(TRUE,FALSE), size = nrow(walmart2), replace = TRUE, prob = c(0.7,0.3))
wtrain1 <- walmart2[ws1,]
wtest1 <- walmart2[!ws1,]

# create the logistic model
wlmodel1 <- glm(IsHoliday ~ Weekly_Sales + Temperature + Fuel_Price + CPI + Unemployment
                + Volume, family = "binomial", data = wtrain1)

# reduction of scientifc notations
options(scipen = 999)

# summary of the model
summary(wlmodel1)

# Calculate R2
pscl::pR2(wlmodel1)["McFadden"]

threshold = 0.6
wpredict1 <- ifelse(predict(wlmodel1, type = "response") > threshold,1,0)
wactual1 <- wlmodel1$y

# Create confusion matrix
wconf1 <- table(wpredict1, wactual1)
wconf1

# Create predict values
wpredicted1 <- predict(wlmodel1, wtest1, type = "response")

# Calculate MSE
mse(wactual1,wpredict1)

# Calculate AIC
AIC(wlmodel1)

# Calculate BIC
BIC(wlmodel1)

## Walmart 3

# reproduce the dataset
set.seed(100)

# create the logistic model
ws2 <- sample(c(TRUE,FALSE), size = nrow(walmart3), replace = TRUE, prob = c(0.7,0.3))
wtrain2 <- walmart3[ws2,]
wtest2 <- walmart3[!ws2,]

# create the logistic model
wlmodel2 <- glm(IsHoliday ~ Weekly_Sales + Temperature + Fuel_Price + CPI + Unemployment
                + Volume, family = "binomial", data = wtrain2)

# reduction of scientifc notations
options(scipen = 999)

# summary of the model
summary(wlmodel2)

# Calculate R2
pscl::pR2(wlmodel2)["McFadden"]

# convert response to 0 and 1
threshold = 0.6
wpredict2 <- ifelse(predict(wlmodel2, type = "response") > threshold,1,0)
wactual2 <- wlmodel2$y

# Create confusion matrix
wconf2 <- table(wpredict2, wactual2)
wconf2

# Create predict values
wpredicted2 <- predict(wlmodel2, wtest2, type = "response")

# Calculate MSE
mse(wactual2,wpredict2)

# Calculate AIC
AIC(wlmodel2)

# Calculate BIC
BIC(wlmodel2)

## Walmart 4

# reproduce the dataset
set.seed(100)

# split the dataset into a training and test dataset
ws3 <- sample(c(TRUE,FALSE), size = nrow(walmart4), replace = TRUE, prob = c(0.7,0.3))
wtrain3 <- walmart4[ws3,]
wtest3 <- walmart4[!ws3,]

# create the logistic model
wlmodel3 <- glm(IsHoliday ~ Weekly_Sales + Temperature + Fuel_Price + CPI + Unemployment
                + Volume, family = "binomial", data = wtrain3)

# reduction of scientifc notations
options(scipen = 999)

# summary of the model
summary(wlmodel3)

# Calculate R2
pscl::pR2(wlmodel3)["McFadden"]

# convert response to 0 and 1
threshold = 0.6
wpredict3 <- ifelse(predict(wlmodel3, type = "response") > threshold,1,0)
wactual3 <- wlmodel3$y

# Create confusion matrix
wconf3 <- table(wpredict3, wactual3)
wconf3

# Create predict values
wpredicted3 <- predict(wlmodel3, wtest3, type = "response")

# Calculate MSE
mse(wactual3,wpredict3)

# Calculate AIC
AIC(wlmodel3)

# Calculate BIC
BIC(wlmodel3)

## Walmart 5

# reproduce the dataset
set.seed(100)

# split the dataset into a training and test dataset
ws4 <- sample(c(TRUE,FALSE), size = nrow(walmart5), replace = TRUE, prob = c(0.7,0.3))
wtrain4 <- walmart5[ws4,]
wtest4 <- walmart5[!ws4,]

# create the logistic model
wlmodel4 <- glm(IsHoliday ~ Weekly_Sales + Temperature + Fuel_Price + CPI + Unemployment
                + Volume, family = "binomial", data = wtrain4)

# reduction of scientifc notations
options(scipen = 999)

# summary of the model
summary(wlmodel4)

# Calculate R2
pscl::pR2(wlmodel4)["McFadden"]

# convert response to 0 and 1
threshold = 0.6
wpredict4 <- ifelse(predict(wlmodel4, type = "response") > threshold,1,0)
wactual4 <- wlmodel4$y

# Create confusion matrix
wconf4 <- table(wpredict4, wactual4)
wconf4

# Create predict values
wpredicted4 <- predict(wlmodel4, wtest4, type = "response")

# Calculate MSE
mse(wactual4,wpredict4)

# Calculate AIC
AIC(wlmodel4)

# Calculate BIC
BIC(wlmodel4)





