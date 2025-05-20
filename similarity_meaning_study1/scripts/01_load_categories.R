# Title: Load Categories
# Author: Layla Unger
# Last Updated: 2025-05-18
# R version: 4.3.2
# Packages: here, dplyr, stringr, tidyr, tidytext

################################################
################ DESCRIPTION ###################
################################################

# This script is the first step in assessing the degree to which words 
# in the same semantic category (e.g., words for body parts) co-occur 
# more reliably than random words matched in frequency.

# The steps implemented in this script are:
# (1) Load CHILDES corpora to identify unique words present in the corpora
# (2) Load scores that capture co-occurrence between each pair of words in
#     the corpora (calculated in a separate script).
# (3) Load key that identifies each semantic category and the words belonging to it
# (4) For each possible pair of words, identify whether they belong to the same or different
#     categories, and merge with t-scores


################################################
################# PACKAGES #####################
################################################

library(here)

library(stringr)
library(dplyr)
library(tidyr)
library(tidytext)

################################################
################# LOAD DATA ####################
################################################

corpora_file <- here("corpus_processing", "data", "CHILDES_corpora_processed.rds")
scores_file <- here("derive_cooccurrence", "data", "co_scores.rds")
category_file <- here("similarity_meaning_study1", "data", "categories.csv")

corpora_text <- readRDS(file = corpora_file)
co_scores <- readRDS(file = scores_file)
categories_raw <- read_csv(file = category_file, show_col_types = FALSE)

################################################
############ IDENTIFY CORPUS WORDS #############
################################################

# Unnest all words in corpus
all_words <- corpora_text %>%
  tidytext::unnest_tokens(word, Text)

# Count frequency of each word
count_words <- all_words %>%
  dplyr::count(word, sort = TRUE)

# Identify unique words
unique_words <- unique(all_words$word)

###############################################
############### LOAD CATEGORIES ###############
###############################################


# Only keep words that appear in the CHILDES corpora
categories_filter <- categories_raw %>%
  dplyr::filter(word %in% unique_words)


# The next steps generate each possible pair of words and 
# their respective categories

# First, add new column that pastes together a word with its category
categories_filter <- categories_filter %>%
  dplyr::mutate(word_category = paste(word, category, sep = "_"))


# Generate all unique word_category pairs
pairs <- expand_grid(word_category1 = categories_filter$word_category, 
                     word_category2 = categories_filter$word_category) %>%
         dplyr::filter(word_category1 < word_category2)

# Re-separate columns into word and category columns
pairs <- pairs %>%
  separate_wider_delim(word_category1, delim = "_", names = c("word1", "category1"), too_many = "merge") %>%
  separate_wider_delim(word_category2, delim = "_", names = c("word2", "category2"), too_many = "merge") 


# Add a column that indicates whether words in a pair belong to the same or
# different categories, and a column indicating the category
pairs <- pairs %>%
  dplyr::mutate(type = ifelse(category1 == category2, "same", "diff"),
                category = ifelse(type == "same", category1, "diff"))

###############################################
#### MERGE CATEGORIES AND CO-OCCUR SCORES #####
###############################################

# Add co-occurrence scores to word pair dataframe
category_scores <- left_join(pairs, co_scores)

# Identify names of columns that contain co-occurrence scores
score_columns <- names(co_scores)[grepl("_", names(co_scores))]

# If a word pair never co-occurred within a given window size, its co-occurrence score will be NA. 
# Set these values to 0. 
category_scores <- category_scores %>%
  dplyr::mutate(across(all_of(score_columns), ~ replace_na(.x, 0)))


# Save as .rds file
category_scores_file <- here("similarity_meaning_study1", "data", "category_scores.rds")
saveRDS(category_scores, file = category_scores_file)

count_words_file <- here("similarity_meaning_study1", "data", "count_words.rds")
saveRDS(count_words, file = count_words_file)
