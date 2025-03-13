# TODO: Set your working directory
setwd('/path/to/source/directory')
options(stringsAsFactors = FALSE)
library(dplyr)
library(ggplot2)
library(quanteda)
library(topicmodels)
library(ldatuning)
library(udpipe)
library(LDAvis)
library("tsne")

### DATA PROCESSING ###

data <- read.csv("./dat/data.csv", sep = ",", encoding = "UTF-8")
col.id <- "anzeigennummer"
col.is_flinta <- "is_flinta"

# Choose the relevant columns
data$text <- paste(data$description_wg_life, data$description_misc, sep = " ")

# Corpus is Paragraphs and docnames are indices
corpus.wg_gesucht <- corpus(data$text, docnames = data[[col.id]])
# Build a dictionary of lemmas
lemma_data <- read.csv("./dat/lemmas.csv", encoding = "UTF-8")
# Stopword list
# stopwords_de <- readLines("./dat/stopwords_de_plain.txt", encoding = "UTF-8")
stopwords_custom <- readLines("./dat/stopwords_de_custom.txt", encoding = "UTF-8")
stopwords_en <- readLines("./dat/stopwords_en.txt", encoding = "UTF-8")
stopwords_de <- readLines("./dat/stopwords_de_full.txt", encoding = "UTF-8")

# Create a DTM
corpus_tokens <- corpus.wg_gesucht %>%
  tokens(
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE
  ) %>%
  tokens_replace(lemma_data$word, lemma_data$lemma, valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_de, padding = T) %>%
  tokens_remove(pattern = stopwords_custom, padding = T) %>%
  tokens_remove(pattern = stopwords_en, padding = T)
collocations <- quanteda.textstats::textstat_collocations(corpus_tokens, min_count = 5)
collocations <- collocations[1:100, ]
collocations <- na.omit(collocations)
corpus_tokens <- tokens_compound(corpus_tokens, collocations)

# Remove terms which occur in less than 1% of all documents
min_frequency <- 1 / 100
DTM <- corpus_tokens %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = floor(nrow(data) * min_frequency))
# Remove empty rows from the DTM and the metadata
sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
data <- data[sel_idx, ]
# Frequency of terms across all documents
term_frequencies <- as.data.frame(sort(colSums(DTM), decreasing = TRUE))

# Test model quality for different K values
optimal.topics <- FindTopicsNumber(
  DTM,
  topics = 2:20,
  metrics = c("Arun2010", "CaoJuan2009", "Griffiths2004", "Deveaud2014")
)

FindTopicsNumber_plot(optimal.topics)


# TOPIC MODELING ###

K <- 5
# compute the LDA model, inference via n iterations of Gibbs sampling
topicModel <- LDA(
  DTM,
  K,
  method = "Gibbs",
  control = list(
    iter = 500,
    seed = 1,
    verbose = 25,
    alpha = 0.02
  )
)


### TOPIC LABELING ###
# Note: If you change any data, the labels might change due to their indices

tmResult <- posterior(topicModel)
terms(topicModel, 10)
top5termsPerTopic <- terms(topicModel, 6)
topic_labels_top_6 <- apply(top5termsPerTopic, 2, paste, collapse = " ")
topic_labels_top_6

topic_labels <- c(
  "Englische Anzeige",
  "Bewerbungsprozess",
  "Alltag, Freizeit & WG-Leben",
  "WG-Ausstattung",
  "Nebenkosten"
)
topic_index <- seq_along(topic_labels)

topic_overview_df <- data.frame(
  Topic_Index = topic_index,
  Top_5_Terms = topic_labels_top_6,
  Topic_Label = topic_labels
)

print(topic_overview_df)


### RESULT OVERVIEW ###

beta <- tmResult$terms
theta <- tmResult$topics
svd_tsne <- function(x)
  tsne(svd(x)$u)
json <- createJSON(
  phi = beta,
  theta = theta,
  doc.length = rowSums(DTM),
  vocab = colnames(DTM),
  term.frequency = colSums(DTM),
  mds.method = svd_tsne,
  plot.opts = list(xlab = "", ylab = "")
)
serVis(json)


### TOPIC DISTRIBUTION ###

# Convert to dataframe
topic_df <- as.data.frame(theta)
topic_df <- cbind(topic_df, is_flinta = data[[col.is_flinta]])  # Merge subgroup information

# Overall Topic Distribution
overall_distribution <- colMeans(topic_df[, 1:ncol(theta)])
overall_distribution_df <- data.frame(Topic = names(overall_distribution),
                                      Proportion = overall_distribution)
overall_distribution_df$Topic <- factor(overall_distribution_df$Topic,
                                        levels = as.character(seq_along(topic_labels)),
                                        labels = topic_labels)
print(overall_distribution_df)

# Topic Distribution for Each Subgroup
flinta_distribution <- colMeans(topic_df[topic_df[[col.is_flinta]] == "True", 1:ncol(theta)])
non_flinta_distribution <- colMeans(topic_df[topic_df[[col.is_flinta]] == "False", 1:ncol(theta)])

flinta_df <- data.frame(
  Topic = names(flinta_distribution),
  Proportion = flinta_distribution,
  Group = "FLINTA*"
)
non_flinta_df <- data.frame(
  Topic = names(non_flinta_distribution),
  Proportion = non_flinta_distribution,
  Group = "Non-FLINTA*"
)

# Combine for comparison
subgroup_distribution_df <- rbind(flinta_df, non_flinta_df)
subgroup_distribution_df$Topic <- factor(subgroup_distribution_df$Topic,
                                         levels = as.character(seq_along(topic_labels)),
                                         labels = topic_labels)

print(subgroup_distribution_df)


### PLOTS ###

# Plot distributions
ggplot(subgroup_distribution_df,
       aes(x = Topic, y = Proportion, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Topic Distribution by Subgroup", y = "Proportion", x = "Topic") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Convert proportions into long format for ggplot
stacked_data <- subgroup_distribution_df %>%
  mutate(Topic = factor(Topic, levels = unique(Topic))) %>%
  group_by(Group) %>%
  mutate(Proportion = Proportion / sum(Proportion))  # Normalize within each group

ggplot(stacked_data, aes(x = Group, y = Proportion, fill = Topic)) +
  geom_bar(stat = "identity", position = "fill") +  # Stacked bars normalized
  scale_y_continuous(labels = scales::percent_format()) +  # Show as percentage
  theme_minimal() +
  labs(title = "Themenverteilung zwischen FLINTA* und Non-FLINTA*-WGs",
       x = "Group",
       y = "Proportion of Topics",
       fill = "Topic") +
  theme(axis.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text())

### WORD CLOUDS ###

require(wordcloud2)
topicToViz <- 1 
top40terms <- sort(beta[topicToViz, ], decreasing = TRUE)[1:40]
words <- names(top40terms)
probabilities <- sort(beta[topicToViz, ], decreasing = TRUE)[1:40]
wordcloud2(data.frame(words, probabilities), shuffle = FALSE)

topicToViz <- 2 
top40terms <- sort(beta[topicToViz, ], decreasing = TRUE)[1:40]
words <- names(top40terms)
probabilities <- sort(beta[topicToViz, ], decreasing = TRUE)[1:40]
wordcloud2(data.frame(words, probabilities), shuffle = FALSE)

topicToViz <- 3 
top40terms <- sort(beta[topicToViz, ], decreasing = TRUE)[1:40]
words <- names(top40terms)
probabilities <- sort(beta[topicToViz, ], decreasing = TRUE)[1:40]
wordcloud2(data.frame(words, probabilities), shuffle = FALSE)

topicToViz <- 4 
top40terms <- sort(beta[topicToViz, ], decreasing = TRUE)[1:40]
words <- names(top40terms)
probabilities <- sort(beta[topicToViz, ], decreasing = TRUE)[1:40]
wordcloud2(data.frame(words, probabilities), shuffle = FALSE)

topicToViz <- 5 
top40terms <- sort(beta[topicToViz, ], decreasing = TRUE)[1:40]
words <- names(top40terms)
probabilities <- sort(beta[topicToViz, ], decreasing = TRUE)[1:40]
wordcloud2(data.frame(words, probabilities), shuffle = FALSE)
