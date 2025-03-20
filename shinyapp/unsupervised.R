library(readr)
library(dplyr)
library(tidyr)
library(tidytext)
library(tm)
library(topicmodels)
library(ggplot2)
library(plotly)
library(reshape2)

# **📌 读取并清理数据**
Cleaned_GP <- read_csv("data/GovernmentProcurementviaGeBIZ.csv")

stopwords_custom <- c("please", "refer", "another", "one", "two", "three", "framework", "edition", "related", 
                      "whole", "period", "government", "entities", "various", "including", "requirement", 
                      "provide", "supply", "service", "procurement", "year", "option", "extend", "agreement", 
                      "singapore", "Singapore", "one", "two", "three")

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

# **📌 建立 LDA 模型**
dtm <- DocumentTermMatrix(Corpus(VectorSource(Cleaned_GP$tender_clean)))

lda_model <- LDA(dtm, k = 10, control = list(seed = 1234))  # 固定 10 個 topic

doc_topic_matrix <- posterior(lda_model)$topics %>%
  as.data.frame() %>%
  mutate(document = seq_len(nrow(.)))

# **📌 定義函數：進行 K-means 分群並繪圖**
topic_cluster_plot <- function(num_clusters) {
  req(num_clusters)  # 確保 K 值有效
  
  # **📌 確保 K 值不大於文件數**
  if (nrow(doc_topic_matrix) < num_clusters) {
    showNotification("K cannot be larger than available documents.", type = "error")
    return(NULL)
  }
  
  set.seed(1234)
  kmeans_result <- kmeans(doc_topic_matrix[,-ncol(doc_topic_matrix)], centers = num_clusters)
  
  clustered_matrix <- doc_topic_matrix  # 複製一份矩陣
  clustered_matrix$cluster <- factor(kmeans_result$cluster)  # 新增 K-means 群組
  
  doc_topic_melted <- melt(clustered_matrix, id.vars = c("document", "cluster"), 
                           variable.name = "Topic", value.name = "Probability")
  
  # **📌 繪製 K-means 群組圖**
  p <- ggplot(doc_topic_melted, aes(x = Topic, y = Probability, group = document, color = cluster)) +
    geom_line(alpha = 0.1, size = 0.3) +
    geom_point(size = 1, alpha = 0.5) +  # 增加點來區分群組
    facet_wrap(~ cluster, scales = "free_y") +
    scale_color_manual(values = RColorBrewer::brewer.pal(10, "Set3")) + # 指定顏色
    theme_minimal() +
    labs(title = paste("Topic Clustering with", num_clusters, "Clusters"),
         x = "Topic", y = "Probability")
  
  return(ggplotly(p))
}


###UI


