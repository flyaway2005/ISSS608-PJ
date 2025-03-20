# data_preprocessing.R
library(readr)
library(dplyr)
library(tidyr)
library(tidytext)
library(tm)
library(topicmodels)

# 讀取數據
Cleaned_GP <- read_csv("data/GovernmentProcurementviaGeBIZ.csv")

# 清理文本
stopwords_custom <- c("please", "refer", "another", "one", "two", "three", "framework", "edition", "related", "whole", "period", "government", "entities", "various", "including",
                      "requirement", "provide", "supply", "service", "procurement", "year", "option", "extend", "agreement", "singapore", "Singapore", "one", "two", "three")

Cleaned_GP <- Cleaned_GP %>%
  mutate(
    tender_clean = tender_description %>%
      tolower() %>%
      removePunctuation() %>%
      removeNumbers() %>%
      stripWhitespace() %>%
      removeWords(stopwords("en")) %>%
      removeWords(stopwords_custom)
  )

# 建立 Document-Term Matrix (DTM)
dtm <- Cleaned_GP %>%
  unnest_tokens(word, tender_clean) %>%
  count(tender_no, word) %>%
  cast_dtm(document = tender_no, term = word, value = n)

# 訓練 LDA 模型
lda_model <- LDA(dtm, k = 20, control = list(seed = 1234))
lda_results <- tidy(lda_model, matrix = "beta")

# 生成 Document-Topic 機率矩陣
doc_topic_matrix <- tidy(lda_model, matrix = "gamma") %>%
  spread(topic, gamma)

# 取得每個 Topic 的關鍵詞
topic_keywords <- lda_results %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  summarise(keywords = paste(term, collapse = ", "))  # 確保使用正確的單詞變數

# 確保 Topic 和 topic 類型一致
doc_topic_matrix <- doc_topic_matrix %>%
  mutate(Topic = as.character(Topic))

topic_keywords <- topic_keywords %>%
  mutate(topic = as.character(topic))

# 儲存處理後的數據，供 Shiny 使用
save(doc_topic_matrix, topic_keywords, file = "processed_data.RData")

