rm(list = ls())
library(WDI)
library(tidyverse)
setwd("E:/University/PSU/spring2023/PLSC421")
options(scipen = 999)


# main iv/dv
Imports <- WDI(indicator = "NE.IMP.GNFS.CD")
ChildLabor <- read.csv("chid_labor.csv")
colnames(ChildLabor) <- c("country", "iso3c", "year", "clabor")

# control variables
Morality <- WDI(indicator = "SP.DYN.IMRT.IN")


# my data
my_data <- ChildLabor[-2] %>%
            full_join(Imports[-(2:3)]) %>%
            full_join(Morality[-(2:3)]) %>%
            na.omit()


colnames(my_data) <- c("country", "year", "clabor", "imports", "morality")

head(my_data, 10)

g20 <- c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", "Japan", "Mexico", "South Korea", "Russia", "Saudi Arabia", "South Africa", "Turkiye", "United Kingdom", "United States")

my_data <- my_data %>%
        filter(!country %in% g20)

# scatter plot of iv against dv
library(ggplot2)
ggplot(my_data, aes(imports, clabor)) +
    geom_point() +
    geom_smooth(method = "lm", se = T, color = "red") +
    theme(plot.title = element_text(hjust = .5)) +
    ylim(0, 75) +
    labs(title = "Scatter Plot", x = "Import rate", y = "Children Labor Rate")

#ggsave("final plot.png")

# linear model
ols.model <- lm(clabor ~ imports + morality, data = my_data, x = T)
summary(ols.model)

library(stargazer)

stargazer(ols.model, type = "html", out = 'final model.html',
          title = 'Children labor and Imports rate',
          dep.var.labels = "Children Labor Rate",
          covariate.labels = c("Imports rate", "Infant morality rate"))
httr::BROWSE('final model.html')


ols.model_iv <- lm(clabor ~ imports, data = my_data, x = T)
summary(ols.model_iv)
stargazer(ols.model_iv, type = "text", dep.var.labels = "Children Labor Rate",
          covariate.labels = "Imports rate")

ols.model_cv <- lm(clabor ~ imports * morality, data = my_data, x = T)
stargazer(ols.model_cv, type = "text", dep.var.labels = "Children Labor Rate",
          covariate.labels = c("Imports rate", "Infant morality rate", "Imports: Infant morality"))

# calculate cook's distance
my_data$cooksd <- cooks.distance(ols.model_cv)
cutoff <- 4 / (nrow(ols.model_iv$x) - length(ols.model_iv$coefficients) - 2)

# check influential obs
my_data[which(my_data$cooksd > cutoff),]    # 10 values, not a big difference


ols.model_out <- lm(clabor ~ imports, data = subset(my_data, cooksd <= cutoff))
stargazer(ols.model_out, type = "html", out = 'final model.html',
          title = 'Children labor and Imports rate', dep.var.labels = "Children Labor Rate",
          covariate.labels = "Imports rate")
httr::BROWSE('final model.html')

ols.model_cv_out <- lm(clabor ~ imports * morality, data = subset(my_data, cooksd <= cutoff))
stargazer(ols.model_cv_out, type = "html", out = 'final model with mortality.html',
          title = 'Children labor and Imports rate', dep.var.labels = "Children Labor Rate",
          covariate.labels = c("Imports rate", "Infant morality rate", "Imports: Infant morality"))
httr::BROWSE('final model with mortality.html')


ggplot(subset(my_data, cooksd <= cutoff), aes(imports, clabor)) +
    geom_point() +
    geom_smooth(method = "lm", se = T, color = "red") +
    theme(plot.title = element_text(hjust = .5)) +
    ylim(0, 75) +
    labs(title = "Scatter Plot", x = "Import rate", y = "Children Labor Rate")

#ggsave("final_plot_out.png")
