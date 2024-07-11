## ================================================================================================================
##                                DATA ANALYSIS | Reddit Data Scraping             
## ================================================================================================================
## clear workspace
rm(list = ls())

## Import Libraries
library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)

## ================================================================================================================
##                                Plot Word Frequency             
## ================================================================================================================

## Top 20 words
n <- 20

## Rearrange Keywords in a Descending Order
wc <- read_csv("./Data/KeywordCounts.csv")
wc |>
  arrange(desc(freq), word) -> wc

wc <- wc[1:n,]

ggplot(wc, aes( x=freq, y = reorder(word, (freq)))) +
  geom_bar(stat="identity") +
  theme_classic() +
  xlab("Count") +
  ylab("Keywords") + 
  theme(axis.title = element_text(face="bold", size=10), axis.text = element_text(face="bold", size=10))

ggsave("wc_frequencyplot.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

## ================================================================================================================
##                                Sentiment Analysis            
## ================================================================================================================

comments <- read_csv("./Data/Comments_Sentences.csv")

comments |>
  select(-post_id) |>
  rename(id = comment_id, sentences = sentence) |>
  mutate(source = "Comments") -> comments

posts <- read_csv("./Data/Posts_Sentences.csv") |>
  mutate(source = "Posts")

df <- rbind(comments, posts) |>
  filter(grepl("assertive", sentences))
  
df |>
  filter(grepl("assertive", sentences)) |>
  group_by(id, source) |>
  mutate(score = ifelse(label == "POSITIVE", score, -score)) |>
  summarize( score = sum(score) ) |>
  mutate(Sentiment = ifelse(score < 0, "NEGATIVE", "POSITIVE")) -> d

d |>
  group_by( Sentiment ) |>
  summarize( count = n() ) -> d

ggplot(d, aes( x = count, y = Sentiment )) +
  geom_bar(stat="identity") +
  theme_classic() + 
  xlab("Count") +
  ylab("Sentiment") + 
  theme(axis.title = element_text(face="bold", size=10), axis.text = element_text(face="bold", size=10))

ggsave("./Graphics/sentiment.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

##================================================================================================================
##                                LDA           
##================================================================================================================

comments <- read_csv("./Data/Comments_Sentences.csv")[,c("sentence", "cleaned_sentence", "label")]
posts <- read_csv("./Data/Posts_Sentences.csv")[,c("sentences", "cleaned_sentence", "label")]

posts |>
  rename(sentence = sentences) |> 
  rbind(comments) |>
  filter(grepl("assertive", sentence)) |>
  mutate( cleaned_sentence = removePunctuation(cleaned_sentence, ucp = TRUE) ) -> df

n <- 10

dtm <- DocumentTermMatrix(df[df$label == "NEGATIVE",]$cleaned_sentence)
lda <- LDA(dtm, k = 2, control = list(seed = 1))

word_topic <- tidy(lda, matrix = "beta")

word_topic |>
  filter(!grepl("assertive|like", term)) |>
  group_by(topic) |>
  slice_max(beta, n = n ) |>
  arrange(topic, beta) |>
  mutate( sentiment = "negative" ) -> word_topic_neg

dtm <- DocumentTermMatrix(df[df$label == "POSITIVE",]$cleaned_sentence)
lda <- LDA(dtm, k = 2, control = list(seed = 1))

word_topic <- tidy(lda, matrix = "beta")

word_topic |>
  filter(!grepl("assertive|like", term)) |>
  group_by(topic) |>
  slice_max(beta, n = n ) |>
  arrange(topic, beta) |>
  mutate( sentiment = "positive" ) -> word_topic_pos

wt <- rbind(word_topic_pos, word_topic_neg) |>
  mutate(topic = paste(sentiment, topic))

ggplot(wt, aes(beta, y = reorder(term, beta), fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  theme_classic()

ggsave("./Graphics/lda.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")
