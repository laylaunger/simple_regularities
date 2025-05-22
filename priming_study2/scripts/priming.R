# Title: Predict Priming
# Author: Layla Unger
# Last Updated: 2025-05-18
# R version: 4.3.2
# Packages: here, readr, dplyr, tidyr, purrr, stringr, ggplot2


################################################
################ DESCRIPTION ###################
################################################

# This script assesses the degree to which semantically related items
# co-occurred more reliably than unrelated items in studies of infant
# and toddler semantic knowledge

# Studies followed one of three designs:
# Design 1: Compared conditions in which items were semantically related vs unrelated
#           (referred to here as relunrel)
# Design 2: Provided participants with a target, a related item, and 1+ unrelated
#           distactor items. Measured preference for related item,
#           (referred to here as reldistract)
# Design 3: Provided participants with a prime that was related or unrelated to
#           a target, a target, a related item, and 1+ unrelated distractor items.
#           Measured whether preference for related item was stronger when given
#           related vs unrelated prime.
#           (referred to here as tardistract)

# The steps implemented in this script are:
# (1) Load item sets used in prior studies, grouped by study design
# (2) Merge item sets with scores that capture co-occurrence between each pair 
#     of items (calculated in a separate script).
# (4) Run permutation analyses that compare co-occurrence in Rel vs Unrel in
#     real stim sets to permuted stim sets in which Rel and Unrel labels are shuffled
# (5) Plot results

################################################
################# PACKAGES #####################
################################################

library(here)
library(readr)

library(dplyr)
library(tidyr)
library(purrr)

library(stringr)

library(ggplot2)

options(dplyr.summarise.inform = FALSE)


################################################
############ CHOOSE PERMUTATIONS ###############
################################################


# Choose the number of permutations to run in permutation analyses

n_permutations = 1000

################################################
################# METADATA #####################
################################################

# Identify names of prior studies and whether they controlled for association strength

study_metadata <- tibble::tribble(
  ~study, ~study_label, ~study_subtitle,
  "rss", "Räma et al. (2013)", "Assoc Strength Control",
  "sr",  "Sirri & Räma (2015)", "Assoc Strength Control",
  "dl",  "Delle Luche et al. (2013)", "Assoc Strength Control",
  "ba",  "Bergelson & Aslin (2017)", "No Control",
  "w",   "Willits et al. (2013)", "No Control",
  "cdp", "Chow et al. (2017", "Assoc Strength Control",
  "n", "Nguyen (2007)", "No Control",
  "ap", "Arias-Trejo & Plunkett (2013)",  "Assoc Strength Control",
  "sp", "Styles & Plunkett (2009)", "No Control"
)

################################################
################# LOAD DATA ####################
################################################

# Rel vs Unrel studies
prime_relunrel <- list.files(pattern = "relunrel", recursive = T) %>%
  map_dfr(~ read_csv(.x, col_select = c("Cond_Prime", "word1", "word2"), show_col_types = F) %>% 
            mutate(study = str_extract(basename(.x), "[A-Za-z]+(?=\\.csv$)")))


# Rel Target vs Unrel Distractor studies
prime_reldistract <- list.files(pattern = "reldistract", recursive = T) %>%
  map_dfr(~ read_csv(.x, show_col_types = F) %>% 
            mutate(study = str_extract(basename(.x), "[A-Za-z]+(?=\\.csv$)")))


# Unrel vs Rel Prime with Named Target vs Distractor studies
prime_tardistract <- list.files(pattern = "tardistract", recursive = T) %>%
  map_dfr(~ read_csv(.x, show_col_types = F) %>% 
            mutate(study = str_extract(basename(.x), "[A-Za-z]+(?=\\.csv$)")))


# Add metadata
prime_relunrel <- prime_relunrel %>%
  left_join(study_metadata, by = "study") %>%
  mutate(study_facet = paste0(study_label, "\n", study_subtitle))

prime_reldistract <- prime_reldistract %>%
  left_join(study_metadata, by = "study") %>%
  mutate(study_facet = paste0(study_label, "\n", study_subtitle))

prime_tardistract <- prime_tardistract %>%
  left_join(study_metadata, by = "study") %>%
  mutate(study_facet = paste0(study_label, "\n", study_subtitle))



# Identify unique words across studies
# First combine all studies in a list
all_primes <- list(prime_relunrel, prime_reldistract, prime_tardistract)

# Then get just the words from the studies
words <- all_primes %>%
  map_dfr(~ select(.x, word1, word2)) %>%
  pivot_longer(cols = everything(), values_to = "word", names_to = NULL) %>%
  distinct(word) %>%
  pull(word)

#############################################
######## COMBINE STIM WITH CO-OCCUR #########
#############################################

# Get the co-occurrence scores
scores_file <- here::here("derive_cooccurrence", "data", "co_scores.rds")
co_scores <- readRDS(file = scores_file)

# Filter to include only co-occurrence btwn stim used in studies
# (this reduces memory demands by reducing the size of the co_scores object)
co_scores <- co_scores %>%
  dplyr::select(matches("word|tscore")) %>%
  dplyr::filter(word1 %in% words & word2 %in% words)

# Define function to combine stim with co-occurrence scores
format_pred <- function(input_data) {
  
  pred <- left_join(input_data, co_scores)
  
  # Replace NA scores with 0
  pred <- pred %>%
    mutate(across(
      .cols = contains("_"),
      .fns = ~ replace_na(., 0)
    ))
  
  
  pred <- pivot_longer(pred, cols = matches("_[0-9]{1,2}"), names_to = "score_window", values_to = "score")
  
  pred <- pred %>%
    dplyr::mutate(score_type = str_extract(score_window, "^[[:alnum:]]+"),
                  window = str_extract(score_window, "[[:digit:]]{1,2}$"),
                  window = factor(window, levels = c(3, 7, 11)))
  
  return(pred)
}

# Rel vs Unrel Studies
prime_relunrel_pred <- format_pred(prime_relunrel)

# Rel Target vs Unrel Distractor studies
prime_reldistract_pred <- format_pred(prime_reldistract)

# Unrel vs Rel Prime with Named Target vs Distractor studies
prime_tardistract_pred <- format_pred(prime_tardistract)

#############################################
########### CALCULATE LUCE CHOICE ###########
#############################################

# For studies in which participants were presented with one item
# as a prompt along with a semantically related target and 1 or more
# unrelated distractors, the outcome measure was some form of preference
# for the target over the distractor(s) (e.g., greater looking). 
# For these studies, use the Luce choice rule to calculate the probability 
# of preferring the semantically related target over the distractor(s) based 
# on the degree to which the target co-occurred with the prompt item more
# than the distractor(s) 


# Define a function that calculates probability of a preference for the
# target based on the degree to which it co-occurred with the prompt item 
# within a given window more than the distractor(s) 
luce_choice <- function(input_data) {
  
  choice <- input_data %>%
    group_by(word1) %>%
    dplyr::summarise(chance_level = 1/length(score), # chance probability = 1/number of choice options (target + distractor(s))
                     choice_ratio = score[Cond_Item == "target"] / sum(score), # ratio of co-occurrence between prompt & target out of summed choice co-occurrence
                     choice_prob = ifelse(is.na(choice_ratio), chance_level, choice_ratio)) # if no choices co-occurred, choice_ratio is NA. convert to chance level
  
  return(choice)
}


##############################################
########### PERMUTATION ANALYSIS #############
##############################################

################ REL VS UNREL ################

# Studies that compare a measure of semantic relatedness
# (e.g., N400) for related vs unrelated items

# Helper function:
# Calculates t-statistic for difference between Rel and Unrel
# when labels shuffled once
shuffle_rel_unrel <- function(input_data) {
  
  # Shuffle Rel and Unrel condition labels
  input_data$Cond_Prime <- sample(input_data$Cond_Prime, nrow(input_data))
  
  # Calculate t-statistic for difference in co-occurrence between Rel and Unrel 
  # when labels shuffled
  t_test <- t.test(input_data$score[input_data$Cond_Prime == "Rel"],
                   input_data$score[input_data$Cond_Prime == "Unrel"])
  t_stat <- as.numeric(t_test$statistic)
  return(t_stat)
}



# Helper function:
# Calculates t-statistics for difference between Rel and Unrel
# when labels shuffled n_shuffles times, and returns these t-statistics along
# with the t-statistic for the difference in the real stim set
permute_rel_unrel <- function(input_data, n_shuffles = 10) {
  
  # Calculate t-statistic for difference in co-occurrence between Rel and Unrel
  # for real stim set
  real_t_test <- t.test(input_data$score[input_data$Cond_Prime == "Rel"],
                        input_data$score[input_data$Cond_Prime == "Unrel"])
  real_t_stat <- as.numeric(real_t_test$statistic)
  
  # Calculate t-statistic for difference in co-occurrence between Rel and Unrel
  # when labels shuffled for n_shuffles permuted stim sets
  sim_t_stat <- replicate(n_shuffles, shuffle_rel_unrel(input_data))
  
  output <- tibble(real_t_stat, sim_t_stat)
  
  return(output)
}


# Helper function:
# Takes permuted t-statistics as input, and calculates
# significance both corrected and uncorrected for multiple comparisons
# (num_comparisons = 3 bc 3 windows used)
sig_t_stat <- function(input_permutations, num_comparisons = 3) {
  
  # Get t-statistic for difference in real stim set
  real_t_stat <- unique(input_permutations$real_t_stat)
  
  # Calculate how many permuted samples had t-statistic greater than real stim set
  p <- mean(input_permutations$sim_t_stat > real_t_stat)
  
  p <- format(p, scientific = F, digits = 4)
  p_adjust <- p.adjust(p, method = "holm", n = num_comparisons)
  
  # Generate symbols to use for plotting significance in graphs
  sig_level <- case_when(
    p_adjust < 0.001 ~ "***",
    p_adjust < 0.01  ~ "**",
    p_adjust < 0.05  ~ "*",
    p_adjust < 0.1   ~ "~",
    TRUE             ~ ""
  )
  
  one_sided <- data.frame(p, p_adjust, sig_level)
  
  return(one_sided)
}


# Combine helper functions into an analyze_study function
# that tests whether the "related" words in a study co-occurred
# more reliably than the "unrelated" words in a study
analyze_relunrel <- function(input_data, n_shuffles = 10, num_comparisons = 3) {
  
  permutations <- permute_rel_unrel(input_data, n_shuffles)
  output <- sig_t_stat(permutations, num_comparisons)
  
  output$score <- input_data %>%
    dplyr::filter(Cond_Prime == "Rel") %>%
    dplyr::summarise(sig_pos = mean(score) + (sd(score)/2)) %>%
    pull(sig_pos)
  
  return(output)
    
}

# Controlled for FA
# Rama Sirri & Serensen
# Sirri & Rama
# Delle Luche et al

# Not controlled for FA
# Bergelson & Aslin
# Willits et al.

sig_relunrel <- prime_relunrel_pred %>%
  group_by(study, study_facet, score_type, window) %>%
  group_modify(~ analyze_relunrel(.x, n_shuffles = n_permutations)) 


############ TARGET VS DISTRACTOR ############

# Studies in which outcome measure was some form of preference
# for a related target vs unrelated distractor(s)

# Function that takes the stim items in a trial in one study, and 
# shuffles the labels of the target and distractor(s)
shuffle_trial_target_distractor <- function(input_trial) {
  
  input_trial$Cond_Item <- sample(input_trial$Cond_Item, nrow(input_trial))
  
  return(input_trial)
}

# Function that takes the stim items in all trials in a study, and 
# shuffles labels for all trials, and compares luce choice preference
# for target to chance
shuffle_luce <- function(input_data) {
  
  # Shuffle trial data
  shuffled_items <- input_data %>%
    group_by(word1) %>%
    group_modify(~ shuffle_trial_target_distractor(.x)) %>%
    ungroup()
  
  # Calculate luce choice prob for each trial in shuffled data
  choice <- luce_choice(shuffled_items)
  
  # Chance level for luce choice prob (1 / number of items presented in a trial)
  chance_level <- 1/nrow(input_data[input_data$word1 == unique(input_data$word1),])
  
  # Get t-statistic for comparison of luce choice prob to chance for shuffled data
  choice_t_test <- t.test(choice$choice_prob, mu = chance_level)
  choice_t_stat <- as.numeric(choice_t_test$statistic)
  
  return(choice_t_stat)
}



# Function that calculates t-statistics luce choice prob vs chance
# when labels shuffled n_shuffles times, and returns these t-statistics along
# with the t-statistic in the real stim set
permute_luce <- function(input_data, n_shuffles = 10) {
  
  # Luce choice prob for real data
  real_choice <- luce_choice(input_data)
  
  # Chance level for luce choice prob (1 / number of items presented in a trial)
  chance_level <- 1 / nrow(input_data[input_data$word1 == unique(input_data$word1),])
  
  # Get t-statistic for comparison of luce choice prob to chance for real data
  real_t_test <- t.test(real_choice$choice_prob, mu = chance_level)
  real_t_stat <- as.numeric(real_t_test$statistic)
  
  # Get t-statistics for comparison of luce choice prob to chance for shuffled data
  sim_t_stat <- replicate(n_shuffles, shuffle_luce(input_data))
  
  return(tibble(real_t_stat, sim_t_stat))
}


analyze_reldistract <- function(input_data, n_shuffles = 10, num_comparisons = 3) {
  permutations <- permute_luce(input_data, n_shuffles)
  output <- sig_t_stat(permutations, num_comparisons)
  
  choice_data <- luce_choice(input_data)

  output$choice_prob <- choice_data %>%
    dplyr::summarise(sig_pos = mean(choice_prob) + (sd(choice_prob)/2),
                     sig_pos = ifelse(sig_pos > .99, .99, sig_pos)) %>%
    pull(sig_pos)
  
  return(output)
  
}

# Target vs Distractor
# Chow Davies Plunkett
# Nguyen
sig_reldistract <- prime_reldistract_pred %>%
  group_by(study, study_facet, Cond, score_type, window) %>%
  group_modify(~  analyze_reldistract(.x, n_shuffles = n_permutations))


##### TARGET VS DISTRACTOR REL VS UNREL ######

# Studies that compare looking at a NAMED target vs distractor
# when preceded by Rel vs Unrel prime

# Function that calculates t-statistic for difference in Luce choice prob 
# between Rel and Unrel when labels shuffled once
shuffle_rel_unrel_luce <- function(input_choice) {
  
  # Shuffle Rel and Unrel condition labels
  input_shuffled <- input_choice
  
  input_shuffled$Cond_Prime <- sample(input_shuffled$Cond_Prime, nrow(input_shuffled))

  
  t_test <- t.test(input_shuffled$choice_prob[input_shuffled$Cond_Prime == "Rel"],
                   input_shuffled$choice_prob[input_shuffled$Cond_Prime == "Unrel"])
  t_stat <- as.numeric(t_test$statistic)
  return(t_stat)
}

# Function that calculates t-statistics for difference in Luce choice prob 
# between Rel and Unrel when labels shuffled n_shuffles times, and returns 
# these t-statistics along with the t-statistic for the difference in the real stim set
permute_rel_unrel_luce <- function(input_choice, n_shuffles) {
  
  real_t_test <- t.test(input_choice$choice_prob[input_choice$Cond_Prime == "Rel"],
                        input_choice$choice_prob[input_choice$Cond_Prime == "Unrel"])
  real_t_stat <- as.numeric(real_t_test$statistic)
  sim_t_stat <- replicate(n_shuffles, shuffle_rel_unrel_luce(input_choice))
  
  return(data.frame(real_t_stat, sim_t_stat))
}


analyze_tardistract <- function(input_data, n_shuffles = 10, num_comparisons = 3) {
  
  input_choice <- input_data %>%
    group_by(Cond_Prime) %>%
    group_modify(~ luce_choice(.))
  
  permutations <- permute_rel_unrel_luce(input_choice, n_shuffles)
  output <- sig_t_stat(permutations, num_comparisons)
  
  output$choice_prob <- input_choice %>%
    ungroup() %>%
    dplyr::summarise(sig_pos = mean(choice_prob[Cond_Prime == "Rel"]) + (sd(choice_prob[Cond_Prime == "Rel"])/2),
                     sig_pos = ifelse(sig_pos > .99, .99, sig_pos)) %>%
    pull(sig_pos)
  
  return(output)
  
}

# Controlled for FA
# Arias-Trejo & Plunkett 2013 

# Not controlled for FA
# Styles & Plunkett

sig_tardistract <- prime_tardistract_pred %>%
  group_by(study, study_facet, Cond, score_type, window) %>%
  group_modify(~ analyze_tardistract(.x, n_shuffles = n_permutations))


#######################################
################ PLOT #################
#######################################


# Rel vs Unrel (design 1)
plot_relunrel_faceted <- function(input_data, sig_data) {
  ggplot(input_data, aes(x = window, y = score)) +
    stat_summary(aes(group = Cond_Prime, color = Cond_Prime), fun = "mean", geom = "line") + 
    stat_summary(aes(group = Cond_Prime, color = Cond_Prime), fun.data = "mean_se", geom = "pointrange", size = .4) +
    geom_text(data = sig_data, 
              aes(x = window, y = score, label = sig_level), 
              inherit.aes = FALSE, 
              
              size = 3.5) +
    facet_wrap(~ study_facet, scales = "free_y") +
    scale_x_discrete(name = "Window Size") +
    scale_y_continuous(name = "Co-Occurrence\nbetween Words") +
    scale_color_manual(name = "Condition", labels = c("Related", "Unrelated"), values = c("blue", "gray")) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 9),
          strip.background = element_blank())
}



plot_relunrel_faceted(prime_relunrel_pred, sig_relunrel)



# Target vs Distractor (design 2)
plot_luce_faceted <- function(input_data, sig_data) {
  
  choice_data <- input_data %>%
    dplyr::filter(Cond != "Assoc") %>%
    group_by(study, study_facet, Cond, score_type, window) %>%
    group_modify(~ luce_choice(.x))
  
  
  sig_data <- sig_data %>%
    dplyr::filter(Cond != "Assoc")
  
  ggplot(choice_data, aes(x = window, y = choice_prob)) +
    stat_summary(aes(group = 1), fun = "mean", geom = "line", color = "blue") + 
    stat_summary(aes(group = 1), fun.data = "mean_se", geom = "pointrange", size = .4, color = "blue") +
    geom_hline(aes(yintercept = chance_level), linetype = "dashed") +
    geom_text(data = sig_data, 
              aes(x = window, y = choice_prob, label = sig_level), 
              inherit.aes = FALSE,
              size = 3.5) +
    facet_wrap(~ study_facet, scales = "free_y") +
    scale_x_discrete(name = "Window Size") +
    scale_y_continuous(name = "Co-Occur Preference\nfor Target", limits = c(0, 1)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 9),
          strip.background = element_blank())
}

plot_luce_faceted(prime_reldistract_pred, sig_reldistract)


# Rel vs Unrel Target vs Distractor (design 3)
plot_relunrel_luce_faceted <- function(input_data, sig_data) {
  
  choice_data <- input_data %>%
    dplyr::filter(is.na(Cond) | Cond != "Assoc") %>%
    group_by(study, study_facet, Cond, Cond_Prime, score_type, window) %>%
    group_modify(~ luce_choice(.x))
  
  
  sig_data <- sig_data %>%
    dplyr::filter(is.na(Cond) | Cond != "Assoc")
  
  ggplot(choice_data, aes(x = window, y = choice_prob)) +
    stat_summary(aes(group = Cond_Prime, color = Cond_Prime), fun = "mean", geom = "line") + 
    stat_summary(aes(group = Cond_Prime, color = Cond_Prime), fun.data = "mean_se", geom = "pointrange", size = .4) +
    geom_hline(aes(yintercept = chance_level), linetype = "dashed") +
    geom_text(data = sig_data, 
              aes(x = window, y = choice_prob, label = sig_level), 
              inherit.aes = FALSE,
              size = 3.5) +
    facet_wrap(~ study_facet, scales = "free_y") +
    scale_x_discrete(name = "Window Size") +
    scale_y_continuous(name = "Co-Occur Preference\nfor Target", limits = c(0, 1)) +
    scale_color_manual(name = "Prime Type", values = c("blue", "gray")) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 9),
          strip.background = element_blank())
}


plot_relunrel_luce_faceted(prime_tardistract_pred, sig_tardistract)

# Optional: Save figures to files