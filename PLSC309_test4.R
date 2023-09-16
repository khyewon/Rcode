options(scipen = 999)
library(ggplot2)
library(dplyr)
library(httr)
library(WDI)
library(stargazer)

# read data file
test_df <- read.csv("./PLSC309_test4.csv")

# DV = ffp_hf       human fight and brain drain
# IV = bci_bci      bayesian corruption indicator


# does perceived corruption lead to human flight/brain drain

# visualizing relationship
p1 <- ggplot(test_df, aes(x = bci_bci, y = ffp_hf)) +
     geom_point(size = 1.2, shape = 18, colour = 'tomato2', alpha = 0.5) +
     geom_smooth(method = 'lm', colour = 'steelblue', size = 1.5) +
     labs(title = 'Perceived Corruption and Brain Drain',
          subtitle = 'From QOG Data',
          caption = 'Source: QOG',
          x = 'Bayesian Corruption Indicator',
          y = 'Brain Drain')
# ggsave('plot1.png', width = 8, hiehgt = 6)
# p1: pretty clear linear relationship(positive relationship)

# ols regression model
f1 = lm(test_df$ffp_hf ~ test_df$bci_bci)
stargazer(f1, type = 'html',
          out = 'model1.html',
          covariate.labels = 'Corruption',
          dep.var.labels = '',
          dep.var.caption = 'Brain Drain',
          title = 'Corruption and Brain Drain')
BROWSE('model1.html')
# substantively significant at point 1 level because of the amount of dependent variable range : the amount of independent variable range
# p-value: probability of beta hat is less than 0.01 therefore, I'm going to reject the Null Hypothesis that there is no relationship
# Constant: y-intercept. hypothetical value where dv is going to 0.
# R^2: strength of correlation(how well is the data set is sit on the line) pretty well capturing the data
# Residual standard error: RMSE. specific estimate of the average error in regression line in here
# F Stat: statistically significant, reject Null hypothesis

# voh_gti, gii_gii
p2 <- ggplot(df, aes(x = voh_gti, y = gii_gii)) +
     geom_point(size = 1.2, shape = 18, colour = 'tomato2', alpha = 0.5) +
     geom_smooth(method = 'lm', colour = 'steelblue', size = 1.5) +
     labs(title = 'Gender Inequality and Terrorism',
          subtitle = 'From QOG Data',
          caption = 'Source: QOG',
          x = 'Global Terrorism Index',
          y = 'Gender Inequality Index')
ggsave('genter.png', p2, width = 8, height = 6)

df$GII2 <- df$gii_gii * 1000
f2 <- lm(df$GII2 ~ df$voh_gti)
stargazer(f2, type = 'html',
          out = 'genter.html',
          covariate.labels = 'Global Terrorism Index',
          dep.var.labels = '',
          dep.var.caption = 'Gender Inequality',
          title = 'Gender Inequality and Global Terrorism Index')
BROWSE('genter.html')


# x = IV = gti
# y = DV = gii
mean_gti <- mean(subdf2$gti)
mean_gii <- mean(subdf2$gii)
subdf2$dev_gti <- subdf2$gti - mean_gti
subdf2$dev_gii <- subdf2$gii - mean_gii
subdf2$cov_xy <- subdf2$dev_gti * subdf2$dev_gii
subdf2$var_gti <- subdf2$dev_gti ^ 2
subdf2$var_gii <- subdf2$dev_gii ^ 2
variation_gti <- sum(subdf2$var_gti)
variation_gii <- sum(subdf2$var_gii)
covxy <- sum(subdf2$cov_xy)
gbeta <- covxy / variation_gti
galpha <- mean_gii - (gbeta * mean_gti)
subdf2$yhat <- galpha + (gbeta * subdf2$gti)
subdf2$muhat <- subdf2$gii - subdf2$yhat
subdf2$muhatsq <- subdf2$muhat ^ 2
gRSS <- sum(subdf2$muhatsq)
gMSE <- gRSS / length(subdf2$gii)
gRMSE <- sqrt(gMSE)


# x = IV = bl_asym = The average years of schooling for males
# y = DV = voh_gti = Global Terrorism Index

x <- c(7, 12, 12, 11, 8, 10, 3, 6, 8, 12)
y <- c(0, 1, 1, 6, 2, 6, 3, 2, 3, 0)

df2 <- data.frame(x, y)
f <- lm(y~x)
mean_x <- mean(df2$x)
mean_y <- mean(df2$y)

df2$dev_x <-df2$x - mean_x
df2$dev_y <- df2$y - mean_y

df2$cov_xy <- df2$dev_x * df2$dev_y

df2$var_x <- df2$dev_x ^ 2
df2$var_y <- df2$dev_y ^ 2

variation_x <- sum(df2$var_x)
variance_x <- variation_x / (length(df2$x) - 1)
sd_x <- sd(df2$x)

variation_y <- sum(df2$var_y)
variance_y <- variation_y / (length(df2$y) - 1)
sd_y <- sd(df2$y)

covxy <- sum(df2$cov_xy)

beta <- covxy / variation_x
alpha <- mean_y - (beta * mean_x)

rbeta <- round(beta, 3)
ralpha <- round(alpha, 3)

df2$yhat <- ralpha + (rbeta * df2$x)
df2$muhat <- df2$y - df2$yhat
df2$muhatsq <- round((df2$muhat ^ 2), 2)

RSS <- round(sum(df2$muhatsq), 2)
MSE <- (RSS / length(df2$y))
RMSE <- round(sqrt(MSE), 1)

c1 <- cor(df2$y, df2$x)
rsquared <- round((c1 ^ 2), 3)

df2$mss <- (df2$yhat - mean_y) ^ 2
MSS <- sum(df2$mss)

MSmodel <- MSS / (length(f$coefficients) - 1)
MSresidual <- round((RSS / (length(df2$y) - length(f$coefficients))), 3)
Fstat <- MSmodel / MSresidual

sebeta <- round(sqrt(MSresidual / variation_x), 3)
tscore <- round(((rbeta - 0) / sebeta), 3)

quantile <- qt(0.025, length(df2$y) - 2)


p3 <- ggplot(df2, aes(x = x, y = y)) +
     geom_point(size = 1.2, shape = 18, colour = 'tomato2', alpha = 0.5) +
     geom_smooth(method = 'lm', colour = 'steelblue', size = 1.5)
