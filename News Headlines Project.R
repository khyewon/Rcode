library("lubridate")
library("dplyr")
library("stringi")
library("quanteda")
library("ggplot2")


setwd("E://University/PSU/fall 2022/PLSC498")

# read data
#@article{misra2022news,
#         title={News Category Dataset},
#         author={Misra, Rishabh},
#         journal={arXiv preprint arXiv:2209.11429},
#         year={2022}}
news_data <- read.csv("news_category.csv")

# word dictionaries of positive and negative words
hldict <- dictionary(file = "hu_liu.cat", format = "wordstat",encoding = "utf-8")

# change date category character into date value, and only save feasible data from 11-08-2015 to 11-08-2017
news_data$date <- mdy(news_data$date)
news_data <- na.omit(news_data)
news_data <- news_data[news_data$date >= "2015-11-08" & news_data$date <= "2017-11-08",]

summary(news_data)


# divide data into two groups - data for a year before Trump elected and data for a year after Trump elected
news_before <- news_data[news_data$date <= "2016-11-07",]
news_after <- news_data[news_data$date >= "2016-11-08",]

# find out whether the negative words were more used after Trump elected in political news
# sample data - 1000 samples
set.seed(1981L)
politics_samp_before <- news_before %>%
     filter(category %in% c("POLITICS")) %>%
     sample_n(1000) %>%
     select(headline)
politics_samp_before$headline <- stri_trans_general(politics_samp_before$headline, "latin-ascii")
politics_samp_before$headline <- gsub(" [A-z] ", " ", politics_samp_before$headline)

politics_samp_after <- news_after %>%
     filter(category %in% c("POLITICS")) %>%
     sample_n(1000) %>%
     select(headline)
politics_samp_after$headline <- stri_trans_general(politics_samp_after$headline, "latin-ascii")
politics_samp_after$headline <- gsub(" [A-z] ", " ", politics_samp_after$headline)

politics_corpus_before <- corpus(politics_samp_before$headline)
politics_corpus_after <- corpus(politics_samp_after$headline)

politics_corpus_before_summary <- summary(politics_corpus_before)
politics_corpus_after_summary <- summary(politics_corpus_after)

# creating dfm with dictionary
politics_dfm_before <- dfm(politics_corpus_before, remove_punct = TRUE, remove_numbers = TRUE, stem = TRUE, tolower = TRUE, remove = stopwords("english"), dictionary = hldict)
politics_dfm_after <- dfm(politics_corpus_after, remove_punct = TRUE, remove_numbers = TRUE, stem = TRUE, tolower = TRUE, remove = stopwords("english"), dictionary = hldict)

# calculate sentiment score
politics_matrix_before <- as.matrix(politics_dfm_before)
politics_score_before <- politics_matrix_before[,"POSITIVE"] - politics_matrix_before[, "NEGATIVE"]
politics_samp_before$sentiscore <- ifelse(politics_score_before > 0, "positive", ifelse(politics_score_before < 0, "negative", "neutral"))
prop.table(table(politics_samp_before$sentiscore))
     # neg: 0.233, neutral 0.458, pos: 0.309
par(mfrow = c(1, 2))
politics_histogram_before <- hist(politics_score_before,
                                   main = paste("Sentiment Scores on Politics before the election"),
                                   xlab = "Sentiment Score on Politics")

politics_matrix_after <- as.matrix(politics_dfm_after)
politics_score_after <- politics_matrix_after[,"POSITIVE"] - politics_matrix_after[, "NEGATIVE"]
politics_samp_after$sentiscore <- ifelse(politics_score_after > 0, "positive", ifelse(politics_score_after < 0, "negative", "neutral"))
prop.table(table(politics_samp_after$sentiscore))
     # neg: 0.244, neutral: 0.457, pos: 0.299
politics_histogram_after <- hist(politics_score_after,
                                  main = paste("Sentiment Scores on Politics after the election"),
                                  xlab = "Sentiment Score on Politics")


# find out whether business category has more negative words than sport category news after Trump elected
# # sample data - 400 samples
business_samp_after <- news_after %>%
     filter(category %in% c("BUSINESS")) %>%
     sample_n(400) %>%
     select(headline)
business_samp_after$headline <- stri_trans_general(business_samp_after$headline, "latin-ascii")
business_samp_after$headline <- gsub(" [A-z] ", " ", business_samp_after$headline)

business_corpus_after <- corpus(business_samp_after$headline)
summary(business_corpus_after)

# creating dfm with dictionary
business_dfm_after <- dfm(business_corpus_after, remove_punct = TRUE, remove_numbers = TRUE, stem = TRUE, tolower = TRUE, remove = stopwords("english"), dictionary = hldict)

# calculate sentiment score
business_matrix_after <- as.matrix(business_dfm_after)
business_score_after <- business_matrix_after[,"POSITIVE"] - business_matrix_after[, "NEGATIVE"]
business_samp_after$sentiscore <- ifelse(business_score_after > 0, "positive", ifelse(business_score_after < 0, "negative", "neutral"))
prop.table(table(business_samp_after$sentiscore))
     # neg: 0.3, neutral: 0.48, pos: 0.22
business_histogram_after <- hist(business_score_after,
                                  main = paste("Sentiment Scores on Business after the election"),
                                  xlab = "Sentiment Score on Business")



sports_samp_after <- news_after %>%
     filter(category %in% c("SPORTS")) %>%
     sample_n(400) %>%
     select(headline)
sports_samp_after$headline <- stri_trans_general(sports_samp_after$headline, "latin-ascii")
sports_samp_after$headline <- gsub(" [A-z] ", " ", sports_samp_after$headline)

sports_corpus_after <- corpus(sports_samp_after$headline)
summary(sports_corpus_after)

# creating dfm with dictionary
sports_dfm_after <- dfm(sports_corpus_after, remove_punct = TRUE, remove_numbers = TRUE, stem = TRUE, tolower = TRUE, remove = stopwords("english"), dictionary = hldict)

# calculate sentiment score
sports_matrix_after <- as.matrix(sports_dfm_after)
sports_score_after <- sports_matrix_after[,"POSITIVE"] - sports_matrix_after[, "NEGATIVE"]
sports_samp_after$sentiscore <- ifelse(sports_score_after > 0, "positive", ifelse(sports_score_after < 0, "negative", "neutral"))
prop.table(table(sports_samp_after$sentiscore))
     # neg: 0.2925, neutral: 0.4425, pos: 0.2650
business_histogram_after <- hist(sports_score_after,
                                 main = paste("Sentiment Scores on Sports after the election"),
                                 xlab = "Sentiment Score on Sports")
