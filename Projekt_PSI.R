# Ustawienia
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

# Biblioteki
library(tm)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(wordcloud)
library(RColorBrewer)

# Funkcja czyszcząca tekst
clean_corpus <- function(corpus) {
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))
  corpus <- tm_map(corpus, toSpace, "@")
  corpus <- tm_map(corpus, toSpace, "@\\w+")
  corpus <- tm_map(corpus, toSpace, "\\|")
  corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")
  corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")
  corpus <- tm_map(corpus, toSpace, "http\\w*")
  corpus <- tm_map(corpus, toSpace, "/")
  corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")
  corpus <- tm_map(corpus, toSpace, "www")
  corpus <- tm_map(corpus, toSpace, "~")
  corpus <- tm_map(corpus, toSpace, "â€“")
  corpus <- tm_map(corpus, toSpace, "negative")
  corpus <- tm_map(corpus, toSpace, "positive")
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Tworzenie i czyszczenie korpusów
corpus_negative <- VCorpus(VectorSource(data$Negative_Review)) %>% clean_corpus()
corpus_positive <- VCorpus(VectorSource(data$Positive_Review)) %>% clean_corpus()

# Funkcja LDA
top_terms_by_topic_LDA <- function(input_text, plot = TRUE, k = number_of_topics, title_prefix = "") {
  corpus <- VCorpus(VectorSource(input_text))
  DTM <- DocumentTermMatrix(corpus)
  unique_indexes <- unique(DTM$i)
  DTM <- DTM[unique_indexes,]
  lda <- LDA(DTM, k = k, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  top_terms <- topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  if(plot == TRUE){
    top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      labs(
        title = paste("Tematy LDA:", title_prefix),
        x = "Terminy", y = "β (ważność słowa w temacie)"
      ) +
      coord_flip() +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
  } else {
    return(top_terms)
  }
}


# Wczytaj dane
data <- read.csv("Hotel_Reviews.csv", sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
data <- data %>% select(Hotel_Name, Negative_Review, Positive_Review)


# Przygotowanie dokumentów do LDA
cleaned_neg_docs <- sapply(corpus_negative, as.character)
cleaned_pos_docs <- sapply(corpus_positive, as.character)

# Wybór liczby tematów i LDA
number_of_topics <- 4
top_terms_by_topic_LDA(cleaned_neg_docs, plot = TRUE, k = number_of_topics, title_prefix = "recenzje NEGATYWNE")
top_terms_by_topic_LDA(cleaned_pos_docs, plot = TRUE, k = number_of_topics, title_prefix = "recenzje POZYTYWNE")

# Częstość słów – NEGATIVE
tdm_neg <- TermDocumentMatrix(corpus_negative)
m_neg <- as.matrix(tdm_neg)
v_neg <- sort(rowSums(m_neg), decreasing = TRUE)
tdm_df_neg <- data.frame(word = names(v_neg), freq = v_neg) %>%
  filter(!word %in% c("negative", "positive"))

# Częstość słów – POSITIVE
tdm_pos <- TermDocumentMatrix(corpus_positive)
m_pos <- as.matrix(tdm_pos)
v_pos <- sort(rowSums(m_pos), decreasing = TRUE)
tdm_df_pos <- data.frame(word = names(v_pos), freq = v_pos) %>%
  filter(!word %in% c("negative", "positive"))

# 🌩️ Chmura słów – NEGATIVE
wordcloud(words = tdm_df_neg$word, freq = tdm_df_neg$freq, min.freq = 7,
          colors = brewer.pal(8, "Dark2"))
title("Top słowa w recenzjach NEGATYWNYCH")

# 🌩️ Chmura słów – POSITIVE
wordcloud(words = tdm_df_pos$word, freq = tdm_df_pos$freq, min.freq = 7,
          colors = brewer.pal(8, "Paired"))
title("Top słowa w recenzjach POZYTYWNYCH")


# Top 10 słów – NEGATIVE
cat("Top 10 słów (negative reviews):\n")
print(head(tdm_df_neg, 10))

# Top 10 słów – POSITIVE
cat("\nTop 10 słów (positive reviews):\n")
print(head(tdm_df_pos, 10))

