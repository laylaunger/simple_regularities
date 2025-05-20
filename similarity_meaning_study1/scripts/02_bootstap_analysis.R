# Title: Bootstap Analysis
# Author: Layla Unger
# Last Updated: 2025-05-18
# R version: 4.3.2
# Packages: here, dplyr, stringr, tidyr, tidytext, furrr

################################################
################ DESCRIPTION ###################
################################################

# This script is the second step in assessing the degree to which words 
# in the same semantic category (e.g., words for body parts) co-occur 
# more reliably than random words matched in frequency.

# The steps implemented in this script are:
# (1) Define and use a series of functions to conduct a boostrap comparison
#     between (A) co-occurrence of words in a category and (B) co-occurrence 
#     of the same number of randomly sampled words matched in frequency
# (2) Store the bootstrap comparisons in an .rds file


################################################
################# PACKAGES #####################
################################################

library(here)

library(stringr)
library(dplyr)
library(tidyr)
library(tidytext)
library(furrr)

################################################
################# LOAD DATA ####################
################################################

category_scores_file <- here("similarity_meaning_study1", "data", "category_scores.rds")
count_words_file <- here("similarity_meaning_study1", "data", "count_words.rds")

category_scores <- readRDS(category_scores_file)
count_words <- readRDS(count_words_file)

################################################
########### WORD FREQUENCY MATCHING ############
################################################

# In the analysis below, co-occurrence between words within the same category
# is compared to co-occurrence between sets of words sampled from across 
# categories that consist of the same number of words as the category and are 
# matched in frequency to words in the category. 
# This section calculates word frequencies and defines a function for sampling
# a set of words matched in frequency to words in a category.

# Identify frequency and frequency quantile of each word to use 
# for matching random words to category words on frequency
word_freq <- count_words %>%
  dplyr::filter(word %in% category_scores$word1 | word %in% category_scores$word2) %>%
  dplyr::mutate(log_n = log(n),
                quantile = ntile(log_n, 5))


################################################
######## BOOTSTAP CO-OCCURRENCE DENSITY ########
################################################

# This section designs a series of functions that will
# be used to compare: (1) the degree to which words within
# a category co-occur more reliably than a specified cutoff
# level, versus (2) the degree to which randomly sampled words
# matched to words in the category in frequency co-occur more
# reliably than a specified cutoff level.


# Function that takes the counts of words in each quantile in a category
# and samples a set of words from across categories that match these counts
sample_matched <- function(input_quantile) {
  matched_quantile <- sample(word_freq$word[word_freq$quantile == input_quantile$quantile], 
                             size = input_quantile$count)
  return(matched_quantile)
}


# Function that calculates co-occurrence density in a category
# (proportion of words that co-occur above a given cutoff value)
density_category <- function(input_category, score, cutoff_score) {
  
  scores <- input_category %>%
    dplyr::select(all_of(score)) %>%
    flatten_dbl()
  
  density <- length(scores[scores > cutoff_score]) / length(scores)
  
  return(density)
}

# Function that calculates co-occurrence density of words sampled
# randomly across categories, but matched in frequency to words in a category
density_random <- function(input_category, score, cutoff_score) {
  
  # Get the unique words in the categry
  category_words <- unique(c(input_category$word1, input_category$word2))
  
  # Count number of words in each frequency quantile within the category
  quantile_count <- word_freq %>%
    dplyr::filter(word %in% category_words) %>%
    group_by(quantile) %>%
    dplyr::summarise(count = length(word))
  
  # Use the sample_matched function defined above to sample a random
  # set of words that are matched to the category words in terms of both
  # total number of words and their frequency
  matched_words <- quantile_count %>%
    group_by(quantile) %>%
    group_split() %>%
    map(sample_matched) %>%
    flatten_chr()
  
  # Get co-occurrence between randomly sampled words
  scores <- category_scores %>%
    dplyr::filter(word1 %in% matched_words & word2 %in% matched_words) %>%
    dplyr::select(all_of(score)) %>%
    flatten_dbl()
  
  # Calculate co-occurrence density for sampled words
  density <- length(scores[scores > cutoff_score]) / length(scores)
  
  return(density)
}


# Function that conducts a bootstrap test of co-occurrence density in a category.
# This bootstrap test calculates co-occurrence density for a category, and compares
# it to density for n_random matched sets of random words

density_category_random <- function(input_category, score, cutoff_score, n_random) {
  
  # Calculate density for the category
  category_density <- density_category(input_category, score = score, cutoff_score = cutoff_score)
  
  # Generate n_random samples random words matched in frequency to category words
  # and store the density of these samples in a tibble
  # This is the bootstrap distribution of density
  random_density_metrics <- map_dfr(1:n_random,
    ~ tibble(density = density_random(input_category, score = score, cutoff_score = cutoff_score))
  )
  
  # Add smoothed version of the bootstrapped distribution
  random_density_sd = sd(random_density_metrics$density)
  
  random_density_metrics <- random_density_metrics %>%
    dplyr::mutate(density_smooth = density + rnorm(n(), mean = 0, sd = random_density_sd))
  
  # Compare category density to random density
  diff_metrics <- data.frame(density = category_density - random_density_metrics$density,
                             density_smooth = category_density - random_density_metrics$density_smooth)
  
  # Return difference between category and random networks
  return(diff_metrics)
}


# Function that conducts bootstrap comparison for each category for a given
# set of values for score and cutoff_score
density_calculate_comparison <- function(input_values) {
  
  # For each category, compare density within the category to density in
  # randomly sampled words matched in frequency using the density_categry_random
  # function
  categories_comparison <- category_scores %>%
    filter(category != "diff") %>%
    group_by(category) %>%
    group_modify(~ {
      density_category_random(
        input_category = .x,
        score = input_values$score,
        cutoff_score = input_values$cutoff_score,
        n_random = input_values$n_random
      )
    }) %>%
    ungroup()
  
  #categories_comparison$sd_level <- input_values$sd_level
  
  return(categories_comparison)
}

################################################
########### CONDUCT BOOTSTRAP TEST #############
################################################

# Co-occurrence between words will be compared to a cutoff
# value that is either 1 or 2 standard deviations above
# the mean co-occurrence score. These cutoffs are calculated
# separately for each co-occurrence score.

# Function to identify 1SD and 2SD cutoff values for scores in a given window size
score_cutoffs <- function(score, n_random) {
  
  score_values <- category_scores %>%
    dplyr::select(all_of(score)) %>%
    flatten_dbl()
  
  score_mean <-  mean(score_values)
  
  cutoff_score <- c(round(score_mean + sd(score_values), 2), round(score_mean + 2*sd(score_values), 2))
  sd_level <- c(1,2)
  
  output <- data.frame(score, sd_level, cutoff_score, n_random)
  
  return(output)
}

# Identify score columns
score_columns <- names(category_scores)[grepl("_", names(category_scores))]

# Identify 1SD and 2SD cutoff values for each window size score
comparison_values <- map_dfr(score_columns,
                             ~ score_cutoffs(score = .x, n_random = 1000)
)

# Compare categories to matched randomly sampled words for each set of scores and cutoff_scores
# (this takes a long time, so save results and reload later)

plan(multisession)

categories_comparisons <- comparison_values %>%
  transpose() %>%
  future_map(~ bind_cols(as_tibble(.x), density_calculate_comparison(.x)), .options = furrr_options(seed = 123)) %>%
  list_rbind()


categories_comparisons_file <-  here("similarity_meaning_study1", "data", "category_comparisons.rds")

saveRDS(categories_comparisons, file = categories_comparisons_file)




