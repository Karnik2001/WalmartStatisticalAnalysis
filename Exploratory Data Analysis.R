# correlation Matrix
walmartc <- walmart[,c(1,2,3,4,5,6,7,8,9,10)]
ggcorr(walmartc, palette = "Blues", label = TRUE)

# Correlation Test
cor.test(walmart$Weekly_Sales, walmart$Temperature)
cor.test(walmart$Weekly_Sales, walmart$Fuel_Price)
cor.test(walmart$Weekly_Sales, walmart$CPI)
cor.test(walmart$Weekly_Sales,walmart$Unemployment)
cor.test(walmart$Weekly_Sales, walmart$Volume)

# Two Sample T Test
t.test(walmart$Weekly_Sales, walmart$Temperature, var.equal = TRUE)
t.test(walmart$Weekly_Sales, walmart$Fuel_Price, var.equal = TRUE)
t.test(walmart$Weekly_Sales, walmart$CPI, var.equal = TRUE)
t.test(walmart$Weekly_Sales, walmart$Unemployment, var.equal = TRUE)
t.test(walmart$Weekly_Sales, walmart$Volume, var.equal = TRUE)

# Welch Sample T-test
t.test(walmart$Weekly_Sales ~ walmart$IsHoliday)
t.test(walmart$Weekly_Sales, walmart$Temperature, paired = TRUE)
t.test(walmart$Weekly_Sales, walmart$Fuel_Price, paired = TRUE)
t.test(walmart$Weekly_Sales, walmart$CPI, paired = TRUE)
t.test(walmart$Weekly_Sales, walmart$Unemployment, paired = TRUE)
t.test(walmart$Weekly_Sales, walmart$Volume, paired = TRUE)

# One- Sample Test
t.test(walmart$Weekly_Sales, mu = 50000)
t.test(walmart$Temperature, mu = 80)
t.test(walmart$Fuel_Price, mu = 4)
t.test(walmart$CPI, mu = 224)
t.test(walmart$Unemployment, mu = 8.25)
t.test(walmart$Volume, mu = 25000000)

# Kolmogorov-Smirnov Test
ks.test(walmart$Weekly_Sales, "pnorm")
ks.test(walmart$Temperature, "pnorm")
ks.test(walmart$Fuel_Price, "pnorm")
ks.test(walmart$CPI, "pnorm")
ks.test(walmart$Unemployment, "pnorm")
ks.test(walmart$Volume, "pnorm")

# Mann Whitney U Test
wilcox.test(walmart$Weekly_Sales ~ walmart$IsHoliday, distribution = "exact")
wilcox.test(walmart$Weekly_Sales, walmart$Temperature, distribution = "exact")
wilcox.test(walmart$Weekly_Sales, walmart$Fuel_Price, distribution = "exact")
wilcox.test(walmart$Weekly_Sales, walmart$CPI, distribution = "exact")
wilcox.test(walmart$Weekly_Sales, walmart$Unemployment, distribution = "exact")
wilcox.test(walmart$Weekly_Sales, walmart$Volume, distribution = "exact")

# Cramer-Von Mises Test
cvm.test(walmart$Weekly_Sales)
cvm.test(walmart$Temperature)
cvm.test(walmart$Fuel_Price)
cvm.test(walmart$CPI)
cvm.test(walmart$Fuel_Price)
cvm.test(walmart$Volume)

# Kruskal-Wallis Test
kruskal.test(walmart$Weekly_Sales ~ walmart$Temperature)
kruskal.test(walmart$Weekly_Sales, walmart$Fuel_Price)
kruskal.test(walmart$Weekly_Sales, walmart$CPI)
kruskal.test(walmart$Weekly_Sales, walmart$Unemployment)
kruskal.test(walmart$Weekly_Sales, walmart$Volume)

# KPSS Test
kpss.test(walmart$Weekly_Sales, null = 'Trend')
kpss.test(walmart$Temperature, null = 'Trend')
kpss.test(walmart$Fuel_Price, null = 'Trend')
kpss.test(walmart$CPI, null = 'Trend')
kpss.test(walmart$Unemployment, null = 'Trend')
kpss.test(walmart$Volume, null = 'Trend')

# Granger-Causality Test
grangertest(Weekly_Sales~ Temperature, order = 1, data = walmart)
grangertest(Weekly_Sales~ Fuel_Price, order = 1, data = walmart)
grangertest(Weekly_Sales~ CPI, order = 1, data = walmart)
grangertest(Weekly_Sales~ Unemployment, order = 1, data = walmart)
grangertest(Weekly_Sales~ Volume, order = 1, data = walmart)

# Breusch-Godfrey Test
bgtest(Weekly_Sales ~ Temperature,data = walmart)
bgtest(Weekly_Sales ~ Fuel_Price,data = walmart)
bgtest(Weekly_Sales ~ CPI,data = walmart)
bgtest(Weekly_Sales ~ Unemployment,data = walmart)
bgtest(Weekly_Sales ~ Volume,data = walmart)

# Breusch-Pagan Test
bptest(regmodel)
bptest(regmodel1)
bptest(regmodel2)
bptest(regmodel3)
bptest(regmodel4)

# Anova test
wfit1 <- aov(walmart$Weekly_Sales ~ walmart$Temperature + walmart$Fuel_Price + walmart$CPI
             + walmart$Unemployment + walmart$Volume, data = walmart)
summary(wfit1)

# Anova individual test
waov1 <- aov(walmart$Weekly_Sales ~ walmart$IsHoliday, data = walmart)
waov2 <- aov(walmart$Weekly_Sales ~ walmart$Temperature, data = walmart)
waov3 <- aov(walmart$Weekly_Sales ~ walmart$Fuel_Price, data = walmart)
waov4 <- aov(walmart$Weekly_Sales ~ walmart$CPI, data = walmart)
waov5 <- aov(walmart$Weekly_Sales ~ walmart$Unemployment, data = walmart)
waov6 <- aov(walmart$Weekly_Sales ~ walmart$Volume, data = walmart)

# Summary of the individual anova tests
summary(waov1)
summary(waov2)
summary(waov3)
summary(waov4)
summary(waov5)
summary(waov6)

# Variation inflation factor
vif(wfit1)

# Confidence intervals 
confint(wfit1, level = 0.95)

# Plot the confidence Intervals
wc <- ggplot(walmart, aes(x = Temperature, y = Weekly_Sales)) + ggtitle("Weekly Sales vs Temperature") + geom_smooth(method = "lm", se = TRUE, level = 0.95) +
  theme(plot.title = element_text(hjust = 0.5))

wc1 <- ggplot(walmart, aes(x = Fuel_Price, y = Weekly_Sales)) + ggtitle("Weekly Sales vs Fuel Price") + geom_smooth(method = "lm", se = TRUE, level = 0.95) +
  theme(plot.title = element_text(hjust = 0.5))

wc2 <- ggplot(walmart, aes(x = CPI, y = Weekly_Sales)) + ggtitle("Weekly Sales vs CPI")+ geom_smooth(method = "lm", se = TRUE, level = 0.95) +
  theme(plot.title = element_text(hjust = 0.5))

wc3 <- ggplot(walmart, aes(x = Unemployment, y = Weekly_Sales)) + ggtitle("Weekly Sales vs Unemployment")+ geom_smooth(method = "lm", se = TRUE, level = 0.95) +
  theme(plot.title = element_text(hjust = 0.5))

wc4 <- ggplot(walmart, aes(x = Volume, y = Weekly_Sales)) + ggtitle("Weekly Sales vs Volume")+ geom_smooth(method = "lm", se = TRUE, level = 0.95) +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(wc,wc1,wc2,wc3,wc4)



