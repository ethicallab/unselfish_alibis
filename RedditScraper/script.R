rm(list = ls())

library(tidyverse)

n <- 20

wc <- read_csv("Comments_WC.csv")
wc |>
  arrange(desc(freq), word) -> wc

wc <- wc[1:n,]

ggplot(wc, aes( x=freq, y = reorder(word, (freq)))) +
  geom_bar(stat="identity") +
  theme_classic() +
  xlab("Count") +
  ylab("Keywords") + 
  ggtitle("From Comments") +
  theme(axis.title = element_text(face="bold", size=10), axis.text = element_text(face="bold", size=10))

wc <- read_csv("Post_WC.csv")
wc |>
  arrange(desc(freq), word) -> wc

wc <- wc[1:n,]
ggplot(wc, aes( x=freq, y = reorder(word, (freq)))) +
  geom_bar(stat="identity") +
  theme_classic() +
  xlab("Count") +
  ylab("Keywords") + 
  ggtitle("From Posts") +
  theme(axis.title = element_text(face="bold", size=10), axis.text = element_text(face="bold", size=10))

x <- read_csv("Comments_Assertive.csv")
sentiment_comments <- read_csv("Comments_Assertive.csv") |>
  group_by(comment_id) |>
  mutate(score = ifelse(label == "POSITIVE", score, -score)) |>
  summarize(score = sum(score)) |>
  mutate(Sentiment = ifelse(score < 0, "NEGATIVE", "POSITIVE")) |>
  group_by(Sentiment) |>
  summarize( count = n())

ggplot(sentiment_comments, aes( x=count, y = Sentiment )) +
  geom_bar(stat="identity") +
  theme_classic() +
  ggtitle("From Comments")

sentiment_post <- read_csv("Posts_Assertive.csv") |>
  group_by(id) |>
  mutate(score = ifelse(label == "POSITIVE", score, -score)) |>
  summarize(score = sum(score)) |>
  mutate(Sentiment = ifelse(score < 0, "NEGATIVE", "POSITIVE")) |>
  group_by(Sentiment) |>
  summarize( count = n() )

ggplot(sentiment_post, aes( x = count, y = Sentiment )) +
  geom_bar(stat="identity") +
  theme_classic() +
  ggtitle("From Posts")
  