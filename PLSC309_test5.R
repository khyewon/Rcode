options(scipen = 999)
library(ggplot2)
library(dplyr)
library(httr)
library(stargazer)
library(readxl)
library(gridExtra)


# import the data
df <- read_excel("./PLSC309_test5data.xlsx")

# x = independent variable = primary, election2011
# y = dependent variable = election2016


# univariate visualization for the dependent variable
p1 <- ggplot(df, aes(x = election2016, stat(density))) +
     geom_histogram(colour = 'white', fill = 'cornflowerblue') +
     geom_density(colour = 'darkblue', size = 1.5) +
     geom_vline(aes(xintercept = mean(election2016)),
                colour = "red", size = 1.5) +
     labs(title = "Dependent Variable",
          caption = "Source: Tkacheva & Golosov Research",
          x = "Election 2016")
p2 <- ggplot(df, aes(x = election2016)) +
     geom_boxplot(outlier.colour = 'red', outlier.shape = 15, outlier.size = 3) +
     labs(title = "Dependent Variable",
          caption = "Source: Tkacheva & Golosov Research",
          x = "Election 2016")
p12 <- grid.arrange(p1, p2, ncol = 2)
ggsave('test5_dvunivariate.png', p12)


# univariate visualization for the independent variable
p3 <- ggplot(df, aes(x = primary, stat(density))) +
     geom_histogram(colour = 'white', fill = 'cornflowerblue') +
     geom_density(colour = 'darkblue', size = 1.5) +
     geom_vline(aes(xintercept = mean(primary)),
                colour = "red", size = 1.5) +
     labs(title = "Independent Variable",
          caption = "Source: Tkacheva & Golosov Research",
          x = "United Russia's Primaries")
p4 <- ggplot(df, aes(x = primary)) +
     geom_boxplot(outlier.colour = 'red', outlier.shape = 15, outlier.size = 3) +
     labs(title = "Independent Variable",
          caption = "Source: Tkacheva & Golosov Research",
          x = "United Russia's Primaries")
p34 <- grid.arrange(p3, p4, ncol = 2)
ggsave('test5_ivunivariate1.png', p34)

p5 <- ggplot(df, aes(x = election2011, stat(density))) +
     geom_histogram(colour = 'white', fill = 'cornflowerblue') +
     geom_density(colour = 'darkblue', size = 1.5) +
     geom_vline(aes(xintercept = mean(election2011)),
                colour = "red", size = 1.5) +
     labs(title = "Independent Variable",
          caption = "Source: Tkacheva & Golosov Research",
          x = "Election 2011")
p6 <- ggplot(df, aes(x = election2011)) +
     geom_boxplot(outlier.colour = 'red', outlier.shape = 15, outlier.size = 3) +
     labs(title = "Independent Variable",
          caption = "Source: Tkacheva & Golosov Research",
          x = "Election 2011")
p56 <- grid.arrange(p5, p6, ncol = 2)
ggsave('test5_ivunivariate2.png', p56)


# visualization
p7 <- ggplot(df, aes(x = primary, y = election2016)) +
     geom_point(size = 1.5, shape = 20, colour = 'green4') +
     geom_smooth(method = 'lm', colour = 'steelblue', size = 1.5) +
     labs(title = "United Russia's Primaries and the Strength of Political Machines",
          subtitle = "Evidence from the 2016 Duma Elections",
          caption = 'Source: Tkacheva & Golosov Research',
          x = "Regional voter turnout in the May 2016 United Russia's primaries",
          y = "The Sep 2016 national legislative elections")

p8 <- ggplot(df, aes(x = election2011, y = election2016)) +
     geom_point(size = 1.5, shape = 20, colour = 'steelblue') +
     geom_smooth(method = 'lm', colour = 'tomato2', size = 1.5) +
     labs(title = "Impact of the 2011 United Russia's vote and the Strength of Political Machines",
          subtitle = "Evidence from the 2016 Duma Elections",
          caption = 'Source: Tkacheva & Golosov Research',
          x = "The 2011 Duma elections",
          y = "The Sep 2016 national legislative elections")

ggsave('test5_primary2016.png', p7)
ggsave('test5_20112016.png', p8)


# regression model
f1 <- lm(df$election2016 ~ df$primary + df$election2011)
stargazer(f1, type = 'html',
          out = 'test5.html',
          title = "United Russia's Primaries and the Strength of Political Machines",
          dep.var.caption = '2016 Elections',
          dep.var.labels = '',
          covariate.labels = c('Primary',
                               '2011 Elections'))
plot(f1)


# calculation by hand
m_2016 <- mean(df$election2016)    # 51.4231
m_pri <- mean(df$primary)          # 9.8147

df$dev_2016 <- df$election2016 - m_2016
df$dev_pri <- df$primary - m_pri

df$dev2016_sq <- df$dev_2016 ^ 2
df$devpri_sq <- df$dev_pri ^ 2

variation_2016 <- sum(df$dev2016_sq)    # 16456.2456
variance_2016 <- variation_2016 / (length(df$election2016) - 1)  # 200.6859
sd_2016 <- sqrt(variance_2016)          # 14.1664

variation_pri <- sum(df$devpri_sq)      # 750.6145
variance_pri <- variation_pri / (length(df$primary) - 1)         # 9.1538
sd_pri <- sqrt(variance_pri)            # 3.0255

df$cov_16pri <- df$dev_2016 * df$dev_pri
cov16pri <- sum(df$cov_16pri)      # 2659.2901

beta_16pri <- cov16pri / variation_pri            # 3.5428
alpha_16pri <- m_2016 - (beta_16pri * m_pri)      # 16.6514

df$yhat_16pri <- alpha_16pri + (beta_16pri * df$primary)
df$mu_16pri <- df$election2016 - df$yhat_16pri
df$musq_16pri <- df$mu_16pri ^ 2

RSS <- sum(df$musq_16pri)
MSE <- RSS / length(df$election2016)
RMSE <- sqrt(MSE)

df$mss <- (df$yhat_16pri - m_2016) ^ 2
MSS <- sum(df$mss)
r <- MSS / variation_2016

ftest <- lm(df$election2016 ~ df$primary)

MSmodel <- MSS / (length(ftest$coefficients) - 1)
MSresidual <- RSS / (length(df$election2016) - length(ftest$coefficients))

Fstat <- MSmodel / MSresidual

sebeta <- sqrt(MSresidual / variation_pri)
tscore <- (beta_16pri - 0) / sebeta

# critical t
qt(0.025, length(df$election2016) - 2)            # -1.989686
1 - pt(beta_16pri, length(df$election2016) - 2)    # 0.000330078

plot(f1)
plot(ftest)
