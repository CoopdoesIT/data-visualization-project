install.packages("tidyr")
install.packages("dplyr")
install.packages("tidyverse") 
install.packages("ggplot2")
install.packages("wordcloud2")
install.packages("textdata")


# Load necessary libraries
library(tidytext)
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(wordcloud2)
library(textdata)
library(readr)  # For reading external files

data <- read_csv("Students Social Media Addiction (1).csv")
View(data)
summary(data)
summary(data2)

data2 <- read_csv("Sentimentdataset (3).csv")
View(data2)

ggplot(data, aes(x = Avg_Daily_Usage_Hours)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(
    title = "Daily Social Media Usage Among Students",
    x = "Hours Per Day",
    y = "Number of Students"
  ) +
  theme_minimal()

mean(data$Avg_Daily_Usage_Hours)

platform_counts <- data %>%
  count(Most_Used_Platform, sort = TRUE)

platform_counts

ggplot(platform_counts, aes(x = reorder(Most_Used_Platform, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Most Used Social Media Platforms",
    x = "Platform",
    y = "Number of Students"
  ) +
  theme_minimal()

ggplot(data, aes(x = Addicted_Score)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", boundary = 0) +
  scale_x_continuous(breaks = 1:9, limits = c(1, 9)) +
  labs(
    title = "Distribution of Social Media Addiction Scores",
    x = "Addiction Score (1–9)",
    y = "Number of Students"
  ) +
  theme_minimal()
ggplot(data, aes(x = Addicted_Score, y = Sleep_Hours_Per_Night)) +
  geom_point(color = "skyblue") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Relationship Between Addiction Score and Sleep Hours",
    x = "Addiction Score",
    y = "Hours of Sleep per Night"
  ) +
  theme_minimal()

data("stop_words")

tidy_comments <- data2 %>%
  unnest_tokens(word, Text) %>%   
  anti_join(stop_words)
word_freq <- tidy_comments %>%
  count(word, sort = TRUE)

top_words <- word_freq %>% filter(n >= 15)

ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Most Frequent Words in Sentiment Dataset",
       x = "Word",
       y = "Frequency") +
  theme_minimal()

wordcloud2(word_freq, size = 1)

bing_sent <- tidy_comments %>%
  inner_join(get_sentiments("bing"))

ggplot(bing_sent, aes(x = sentiment, fill = sentiment)) +
  geom_bar() +
  labs(title = "Positive vs Negative Sentiment (BING)",
       x = "Sentiment",
       y = "Count") +
  theme_minimal()

