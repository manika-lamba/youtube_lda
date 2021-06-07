#Make dataframe of top N frequent terms for multiple corpora:https://stackoverflow.com/questions/15506118/make-dataframe-of-top-n-frequent-terms-for-multiple-corpora-using-tm-package-in
#Load Libraries
library(tm)
library(topicmodels)
library(LDAvis)
library(servr)
library(dplyr)
library(stringi)

#read text data
###First get the list of all txt files (including those in sub-folders)
data <- choose.dir()
filesnames <- list.files(getwd(), pattern = ".txt")
data <- lapply(filesnames, readLines)


#corpus
corpus <- Corpus(VectorSource(data))

#cleaning

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords,stopwords("english"))
corpus <- tm_map(corpus, removeWords,c("libraries", "total", "responses", "percent", "value", "library", "yes", "no", "mean", "percentage", "bmpl", "rlau", "like", "and", "table", "can", "study", "figure", "the", "just", "really", "right", "going", "get", "well", "lot", "actually", "new", "will", "much", "way", "see", "make", "look", "also", "able", "say", "back", "got", "take", "great", "many", "next", "using", "around", "thing", "two", "looking", "said", "kind", "come", "put", "yeah", "even", "still", "ago", "every", "three", "five", "gonna", "okay", "whether", "seen", "you", "six", "me", "there", "their", "but", "thats", "folks", "well", "sure", "run", "also", "total", "can", "table", "figure", "study", "research"))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)

#dtm
dtm <- DocumentTermMatrix(corpus)
dtm.mx <- as.matrix(dtm)
frequency <- colSums(dtm.mx)
frequency <- sort(frequency, decreasing=TRUE)
frequency[1:25]

#params
k = 5
seed = 2010

#LDA
lda_fit <- LDA(dtm, k=k, control = list(seed=seed))
lda_fit@alpha
topics(lda_fit, k)
terms(lda_fit, 5)

#save docstotopics
lda_fit.topics <- as.matrix(topics(lda_fit))
write.csv(lda_fit.topics,file=paste("docstotopics",k,"DocsToTopics.csv"))

#save topictoterms
lda_fit.terms <- as.matrix(terms(lda_fit,5))
write.csv(lda_fit.terms,file=paste("topicstoterms",k,"TopicsToTerms.csv"))
lda_fit.terms[1:5,]

#visualization
library(tidytext)
library(tidyverse)
library(ggplot2)
topics <- tidy(lda_fit, matrix = "beta")

top_terms <-
  topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  labs(title = "LDA of Indian ETDs", caption= "Top Terms") +
  ylab("") +
  xlab("") +
  coord_flip()


