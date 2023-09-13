
# read dataset
survey_data <- read.csv("./survey_preliminary_data.csv")


###
# Independent variable: religious ID, christianity and catholicism, and religion importance
# Dependent variable: death penalty

# Null hypothesis: There is no relationship between religion and support for the death penalty
# Alternative hypothesis: There is a relationship between religion and support of the death penalty


# Assign variables
survey_data$catholic <- survey_data$Religion.ID == 2
survey_data$christian <- survey_data$Religion.ID < 5
deathPenalty <- survey_data$Death.Penalty

find_mode <- function(x) {
    u <- unique(x)
    tab <- tabulate(match(x, u))
    u[tab == max(tab)]
}


# Calculate central tendency by mean, median
mean(deathPenalty, na.rm = T)
mean(survey_data$Religion.importance, na.rm = T)
find_mode(survey_data$catholic)
find_mode(survey_data$christian)
find_mode(survey_data$Religion.ID)

    # The average feeling about the dependent variable, the death penalty among our survey sample is 2.559 which falls in the middle of "in favor" and "neutral."
    # Most respondents were not Christian or Catholic. The most common response for religious ID was "nothing in particular."
    # The average feeling about religious importance is around slightly important.


# Measure of dispersion for each variable by variation, standard deviation
prop.table(table(survey_data$catholic))
prop.table(table(survey_data$christian))

sd(survey_data$Death.Penalty, na.rm = T)
sd(survey_data$Religion.ID, na.rm = T)
sd(survey_data$Religion.importance, na.rm = T)

    # The standard deviation of feelings about the death penalty is 1.320857, which indicates the average amount of variability.
    # The standard deviation for Christianity is 0.48845 and for Catholicism, it is 0.4172881.
    # The greatest standard deviation is among religious ID at 4.389381. The standard deviation for religious importance is 1.252427


# Build regression models for each independent variable
library(httr)
library(stargazer)

f1 <- lm(Death.Penalty ~ christian + Religion.importance, data = survey_data)
summary(f1)

stargazer(f1, type = 'html',
          out = 'Christian Pre Model.html',
          covariate.labels = c('Christian', 'Religious importance'),
          dep.var.labels = '',
          dep.var.caption = 'Death Penalty',
          title = 'Death Penalty and Christianity')
BROWSE('Christian Pre Model.html')

    # Both Christianity and religious importance were significant at the 0.1 level.
    # Those who are Christians are .447 more likely to support the death penalty.
    # Those who consider religious important are 0.177 less likely to support the death penalty.


f2 <- lm(Death.Penalty ~ catholic + Religion.importance, data = survey_data)
summary(f2)

stargazer(f2, type = 'html',
          out = 'Catholic Pre Model.html',
          covariate.labels = c('Catholic', 'Religious importance'),
          dep.var.labels = '',
          dep.var.caption = 'Death Penalty',
          title = 'Death Penalty and Catholicism')
BROWSE('Catholic Pre Model.html')

    # Catholicism was even more significant at the 0.05 level with a p-value of 0.0212.
    # Those who are Catholic are 0.646 more likely to support the penalty. In this test, religious importance was not significant.


f3 <- lm(Death.Penalty ~ Religion.ID + Religion.importance, data = survey_data)
summary(f3)

stargazer(f3, type = 'html',
          out = 'Religion Pre Model.html',
          covariate.labels = c('Religion', 'Religious importance'),
          dep.var.labels = '',
          dep.var.caption = 'Death Penalty',
          title = 'Death Penalty and Religion')
BROWSE('Religion Pre Model.html')

    # Neither variables are significant
    # We can reject the null hypothesis at the 0.05 level and say that there is a relationship between identification as being Catholic and support for the death penalty.


# Summarize demographical survey results
survey_data$Race.Ethnicity2 <- as.integer(survey_data$Race.Ethnicity)

median(survey_data$Ideology, na.rm=T)
sd(survey_data$Ideology, na.rm=T)

mean(survey_data$Political.party, na.rm=T)
sd(survey_data$Political.party, na.rm=T)

prop.table(table(survey_data$Race.Ethnicity2))
prop.table(table(survey_data$Sex))
prop.table(table(survey_data$Gender))


# Visualize demographical survey results
par(mfrow = c(2, 3))
hist(survey_data$Ideology, na.rm=T)
hist(survey_data$Political.party, na.rm=T)
hist(survey_data$Race.Ethnicity2, na.rm=T)
hist(survey_data$Sex, na.rm=T)
hist(survey_data$Gender, na.rm=T)
