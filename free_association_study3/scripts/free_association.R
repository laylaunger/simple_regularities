# Title: Predict Free Association
# Author: Layla Unger
# Last Updated: 2025-05-18
# R version: 4.3.2
# Packages: here, readr, dplyr, tidyr, stringr, tidytext, ggplot2, lme4, lmerTest, broom.mixed

################################################
################ DESCRIPTION ###################
################################################

# This script assesses the degree to which co-occurrence
# accounts for spontaneous free associations between words in
# a developmental sample (Wojcik & Kandhadai). 
# The outcome variable of interest is association strength between
# words: I.e., the degree to which participants provide a given 
# response word when prompted with a cue word. The goal of analyses
# is to test whether association strength between cue and response
# words is predicted by their co-occurrence above and beyond the
# frequency of the response words. 

# The steps implemented in this script are:
# (1) Load free association (FA) data, scores that capture co-occurrence between each
#     pair of words in the corpora (calculated in a separate script), and the corpora
#     (used for calculating the frequencies of each response word)
# (2) Preprocess FA data (e.g., to correct misspellings)
# (3) Calculate association strength from FA data
# (4) Standardize word forms in co-occurrence scores to match word forms present
#     in FA data
# (5) Merge FA data, co-occurrence scores, and response word frequencies
# (6) Generate plots that depict co-occurrence between FA words
# (7) Run analyses that test whether co-occurrence scores predict association
#     strength


################################################
################# PACKAGES #####################
################################################

library(here)
library(readr)

library(dplyr)
library(tidyr)

library(stringr)
library(tidytext)

library(ggplot2)

library(lme4)
library(lmerTest)
library(broom.mixed)

#######################################
############# LOAD DATA ###############
#######################################


# Load co-occurrence scores, corpus, FA data and substitutions used to match 
# word forms between FA data and co-occurrence data

scores_file <- here("derive_cooccurrence", "data", "co_scores.rds")
corpora_file <- here("corpus_processing", "data", "CHILDES_corpora_processed.rds")
fa_file <- here("free_association_study3", "data", "Wojcik_data_formatted.txt")
sub_file <- here("free_association_study3", "data", "fa_word_substitution.csv")

co_scores <- readRDS(file = scores_file)
corpora_text <- readRDS(file = corpora_file)
FA_original <- read.table(fa_file, header=TRUE, fill=TRUE, row.names=NULL) 
sub <- read_csv(sub_file, show_col_types = FALSE)


#######################################
####### GET WORD FREQUENCIES ##########
#######################################

# Subsequent analyses will look at whether free association responses -
# i.e., word2 - are predicted by their frequency. 

# Unnest all words in corpus
all_words <- corpora_text %>%
  tidytext::unnest_tokens(word, Text)

# Here, calculate the frequencies and log frequencies of words 
# from the corpus, and name the columns "word2" so that they can be 
# linked to word2 later
count_words <- all_words %>%
  dplyr::rename(word2 = word) %>%
  dplyr::count(word2, sort = TRUE, name = "word2_freq") %>%
  dplyr::mutate(word2_logfreq = log(word2_freq))


#######################################
########### FORMAT ASSOC ##############
#######################################

# Remove age 8 data, which contains only a single participant
assoc_raw <- FA_original %>%
  dplyr::filter(Age != 8)

# Filter data to focus on columns of interest: 
# Subject ID
# Age group ("Age2" in original, "Age_Group" here)
# Cue word ("Experimenter_Word" in original, "word1" here)
# Response word ("Child_Word" in original, "word2" here)
# Response category ("ResponseCat" in original, "code" here) - paradigmatic, syntagmatic, etc.
# Class ("partofspeech" in original, "class" here) - noun, verb, etc
assoc_raw <- assoc_raw %>%
  dplyr::select(Subject, Age2, Experimenter_Word, Child_Word , ResponseCat, partofspeech) %>%
  dplyr::rename(ID = Subject,
                Age_Group = Age2,
                word1 = Experimenter_Word,
                word2 = Child_Word,
                code = ResponseCat,
                class = partofspeech) 

# Convert to lower case
assoc_raw$word2 <- tolower(assoc_raw$word2)

# Identify and remove cue words that were used with very few participants
# in an age group
cue_count <- assoc_raw %>%
  group_by(Age_Group, word1) %>%
  dplyr::summarise(cue_count = length(unique(ID)))


hist(cue_count$cue_count, main = " Histogram of Cue Counts", 
     xlab = "Num of Participants with which Cue was Used") 

assoc_raw <- assoc_raw %>%
  dplyr::filter(!(word1 %in% cue_count$word1 [cue_count$cue_count < 3]) ) 


#######################################
######## WORD SUBSTITUTIONS ###########
#######################################

# Free Association subsitutions
# Substitute some misspelled words with correctly spelled words
# and plural words with singular forms
assoc_sub <- sub %>%
  dplyr::filter(substitution_assoc != "") %>%
  dplyr::select(original_assoc, substitution_assoc)

assoc_sub_vector <- assoc_sub$substitution_assoc
names(assoc_sub_vector) <- assoc_sub$original_assoc 

assoc_raw <- assoc_raw %>%
  dplyr::mutate(word1 = str_replace_all(word1, assoc_sub_vector),
                word2 = str_replace_all(word2, assoc_sub_vector))


# Co-occurrence word substitutions
# Substitute some word forms to match those in free association data
co_sub <- sub %>%
  dplyr::filter(substitution_direct != "") %>%
  dplyr::select(original_direct, substitution_direct)

co_sub_vector <- co_sub$substitution_direct
names(co_sub_vector) <- co_sub$original_direct 

co_scores <- co_scores %>%
  dplyr::mutate(word1 = str_replace_all(word1, co_sub_vector),
                word2 = str_replace_all(word2, co_sub_vector))


#######################################
######### FORMAT CO-SCORES ############
#######################################

# Focus just on co-occurrence between words in FA data
# to reduce object size
co_scores <- co_scores %>%
  dplyr::filter((word1 %in% assoc_raw$word1 | word1 %in% assoc_raw$word2) &
                (word2 %in% assoc_raw$word1 | word2 %in% assoc_raw$word2))


#######################################
########## ASSOC STRENGTH #############
#######################################

# For each age group and cue word, identify the number of times each response word was used
assoc_count <- assoc_raw %>%
  group_by(Age_Group, word1, word2, code, class) %>%
  dplyr::summarise(count = length(unique(ID)))

# Calculate association strength for each cue word in each age group
assoc_strength <- assoc_count %>%
  group_by(Age_Group, word1) %>%
  dplyr::mutate(prop = count / sum(count))


########################################
## MERGE ASSOC STRENGTH WITH CO-OCCUR ##
########################################


# Merge assoc_strength with co_scores and word frequency
assoc_pred <- left_join(assoc_strength, co_scores, relationship = "many-to-many") 

# Set score of words that never co-occurred to 0
assoc_pred <- assoc_pred %>%
  mutate(across(
    .cols = matches("_[0-9]{1,2}|freq"),
    .fns = ~ replace_na(., 0)
  ))


# Convert co-occurrence from wide to long format
assoc_pred <- pivot_longer(assoc_pred, cols = matches("_[0-9]{1,2}"), names_to = "score_window", values_to = "score")

# Add columns indicating score type and window
assoc_pred <- assoc_pred %>%
  dplyr::mutate(score_type = str_extract(score_window, "^[[:alnum:]]+"),
                window = str_extract(score_window, "[[:digit:]]{1,2}$"),
                window = factor(window, levels = c(3, 7, 11)))


# In some cases, these substitutions changed a word form to 
# match one that was already present in co_scores. E.g.,
# there might have originally been separate scores for dog-swim and
# dog-swims, but when swims is changed to swim, there are now two
# scores for dog-swim. Average these together.
assoc_pred <- assoc_pred %>%
  group_by(Age_Group, word1, word2, code, class, count, prop, score_type, window) %>%
  dplyr::summarise(score = mean(score))


# Add frequency information and replace any NA frequencies with 0
assoc_pred <- left_join(assoc_pred, count_words)

assoc_pred <- assoc_pred %>%
  mutate(across(
    .cols = matches("freq"),
    .fns = ~ replace_na(., 0)
  ))


# Order the Age_Group factor by age
assoc_pred$Age_Group <- factor(assoc_pred$Age_Group, levels = c("Younger", "Older", "Adult"))

# Add column that calculates a co-occurrence strength value for each response
# word in a manner analogous to association strength
assoc_pred <- assoc_pred %>%
  group_by(Age_Group, word1, score_type, window) %>%
  dplyr::mutate(co_prop = score / sum(score))






#######################################
############## ANALYSIS ###############
#######################################

# Predict association strength based on co-occurrence

# Scale predictor variables
assoc_pred <- assoc_pred %>%
  group_by(score_type, window) %>%
  dplyr::mutate(score_scale = as.vector(scale(score)),
                logfreq_scale = as.vector(scale(word2_logfreq)))


#######################################
### ANALYSIS: ALL WORDS AND SUBSETS ###
#######################################

############### ALL FA ################

fit_model_chunk <- function(input_data) {
  model <- lmer(prop ~ logfreq_scale + score_scale + (1 | word1), 
                data = input_data)
  
  output <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
    dplyr::filter(term != "(Intercept)")
  
  output <- output %>%
    dplyr::mutate(effect_present = ifelse(p.value < .05, 1,0),
                  co_effect = ifelse(p.value[term == "score_scale" ] < .05, 1, 0))
  
  return(output)
  
}

fa_all <- assoc_pred %>%
  dplyr::filter(score_type == "tscore") %>%
  group_by(Age_Group, window) %>%
  group_modify(~ fit_model_chunk(.x))

print(fa_all, n = 18)


############ PARADIG ONLY ############

fa_paradig <- assoc_pred %>%
  group_by(Age_Group, window) %>%
  dplyr::filter(code == "P" & score_type == "tscore") %>%
  group_modify(~ fit_model_chunk(.x))


print(fa_paradig, n = 18)

#######################################
################ PLOT #################
#######################################

# Visualize the degree to which association strength is predicted
# by frequency and co-occurrence

fa_plot <- ggplot(fa_all, aes(estimate, term, xmin = conf.low, xmax = conf.high, height = 0)) +
  facet_wrap(~Age_Group) +
  geom_point(aes(shape = window, color = term), show.legend = F,position = position_dodge(width = .2)) +
  geom_errorbarh(aes(group = window, color = term), show.legend = F,position = position_dodge(width = .2)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(name = "Relationship with Association Strength") +
  scale_y_discrete(name ="", labels = c("Frequency", "Co-Occurrence")) +
  coord_cartesian(xlim = c(-.01, 0.08)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        strip.text = element_text(size = 12))

fa_plot

