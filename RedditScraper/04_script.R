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
library(quanteda)

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

ggsave("./Graphics/wc_frequencyplot.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

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
##                                Topic Modeling with LDA          
##================================================================================================================

## Read 
comments <- read_csv("./Data/Comments_Sentences.csv")
posts <- read_csv("./Data/Posts_Sentences.csv")

comments |>
  rename(sentences = sentence, id = comment_id) |>
  select(-post_id) |>
  rbind(posts) |>
  filter(grepl("assertive", sentences)) |>
  mutate( 
    cleaned_sentence = removePunctuation(cleaned_sentence, ucp = TRUE),
    cleaned_sentence = trimws(str_replace_all(cleaned_sentence, "[[:digit:]]", ""))) |> 
  filter(cleaned_sentence != "") -> df

negative_sentiments <- df[df$label == "NEGATIVE",]
positive_sentiments <- df[df$label == "POSITIVE",]

calculate_perplexity <- function(i, train, test) {
  lda <- LDA(train, method = "Gibbs", k = i,  control = list(alpha = 0.01, seed = 42))
  return(perplexity(lda, test))
}

# NEGATIVE Sentiments
set.seed(43)
train_split <- sample(negative_sentiments$id, floor(length(negative_sentiments$id) * 0.8))

train_negative <- negative_sentiments[negative_sentiments$id %in% train_split,]
test_negative <- negative_sentiments[!(negative_sentiments$id %in% train_split),]

topic_numbers <- 2:20
dtm_train_negative <- DocumentTermMatrix(train_negative$cleaned_sentence)
dtm_test_negative <- DocumentTermMatrix(test_negative$cleaned_sentence)

perplexity_scores <- sapply(topic_numbers, function(i) calculate_perplexity(i, dtm_train_negative, dtm_test_negative))

ggplot(data = NULL, aes(x = topic_numbers, y = perplexity_scores)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  ylab("Perplexity") +
  xlab("Number of Topics") +
  ggtitle("Negative Sentiments")

ggsave("./Graphics/perplexity_negative.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

# POSITIVE Sentiments
set.seed(42)
train_split <- sample(positive_sentiments$id, floor(length(positive_sentiments$id) * 0.8))

train_positive <- positive_sentiments[positive_sentiments$id %in% train_split,]
test_positive <- positive_sentiments[!(positive_sentiments$id %in% train_split),]

topic_numbers <- 2:20
dtm_train_positive <- DocumentTermMatrix(train_positive$cleaned_sentence)
dtm_test_positive <- DocumentTermMatrix(test_positive$cleaned_sentence)

perplexity_scores <- sapply(topic_numbers, function(i) calculate_perplexity(i, dtm_train_positive, dtm_test_positive))

ggplot(data = NULL, aes(x = topic_numbers, y = perplexity_scores)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  ylab("Perplexity") +
  xlab("Number of Topics") +
  ggtitle("Positive Sentiments")

ggsave("./Graphics/perplexity_positive.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")

## NEGATIVE at 2 topics
num_top_words <- 15
num_negative_topics <- 5

dtm_negative <- DocumentTermMatrix(negative_sentiments$cleaned_sentence)
lda_negative <- LDA(dtm_negative, method = "Gibbs", k = num_negative_topics,  control = list(alpha = 0.01, seed = 42))
word_topic_neg <- tidy(lda_negative, matrix = "beta")

word_topic_neg |>
  filter(!grepl("assertive|like", term)) |>
  group_by(topic) |>
  slice_max(beta, n = num_top_words, with_ties = F ) |>
  arrange(topic, beta) |>
  mutate( sentiment = "negative",
          topic = paste(sentiment, topic, sep = ":")) -> word_topic_neg

## POSITIVE at 2 topics
num_top_words <- 15
num_positive_topics <- 2

dtm_positive <- DocumentTermMatrix(positive_sentiments$cleaned_sentence)
lda_positive <- LDA(dtm_positive, method = "Gibbs", k = num_positive_topics, control = list(alpha = 0.01, seed = 42))
word_topic_pos <- tidy(lda_positive, matrix = "beta")

word_topic_pos |>
  filter(!grepl("assertive|like", term)) |>
  group_by(topic) |>
  slice_max(beta, n = num_top_words, with_ties = F ) |>
  arrange(topic, beta) |>
  mutate( sentiment = "positive", topic = paste(sentiment, topic, sep = ":") ) -> word_topic_pos

## Combine Both
word_topic <- rbind(word_topic_neg, word_topic_pos)

ggplot(word_topic, aes(beta, y = reorder(term, beta), fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 5) +
  theme_classic() +
  xlab("") +
  ylab("")

ggsave("./Graphics/lda.jpg", device = "jpg",width = 10, height = 5, units = "in")

## POSITIVE and AGGRESSIVE
positive_sentiments |>
  filter(grepl("aggressive", sentences)) -> pa

## NEGATIVE and AGGRESSIVE
negative_sentiments |>
  filter(grepl("aggressive", sentences)) -> na

aggressive <- rbind(pa,na)

write_csv(aggressive, "./Data/aggressive.csv")
