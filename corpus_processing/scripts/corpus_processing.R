# Title: CHILDES Corpus Preprocessing
# Author: Layla Unger
# Last Updated: 2025-05-18
# R version: 4.3.2
# Packages: here, childesr, dplyr, stringr, tidytext, readr



#####################################
########### DESCRIPTION #############
#####################################

# This script preprocesses the CHILDES corpus datasets for North American 
# English. These datasets contain transcribed natural language input
# to children.

# The goal of preprocessing steps is to align the word forms in the corpus
# to word forms that appear in other child language datasets, including
# the MacArthur-Bates Communicative Development Inventories (MCDI) datasets
# of word age of acquisition and a dataset of child free associations between
# words.

# The preprocessing steps include:
# (1) Load CHILDES datasets (These are already saved as a single .RDS file that contains
#     many CHILDES datasets)
# (2) Convert all words to lower case and remove some special characters
# (3) Replace some word forms to match forms in MCDI norms, stimuli in priming studies, or
#     words in free association data (e.g., "ice cream" is converted to "icecream")
# (4) Divide transcripts into a tibble in which each row is a single word from a single transcript.
#     The order of rows is the same as the order in which words appeared in utterances. E.g., for
#     an utterance, "How was your day...", there will be rows for "how", "was", "your", "day" etc
# (5) Remove rare words and "words" for noises (e.g., "uh")
# (6) Lemmatize by using a stemming package to stem the words (e.g., "baking"/"baked" -> "bak")
#     and replace all morphological variants of a root word with the most common variant - the lemma
# (7) Remove a small set of high-frequency stop words
# (8) Recombine processed words back into utterances in transcripts

#####################################
############# PACKAGES ##############
#####################################

library(here)
library(childesr)
library(readr)
library(dplyr)
library(stringr)
library(tidytext)

#####################################
############### FILES ###############
#####################################

# Store the names of files that will be used in this script,
# including the file containing the corpus data, a file
# containing word substitutions, some corrections applied during
# the lemmatization process, and a file including stopwords
# that are present in the free association dataset

corpora_file <- here("corpus_processing", "data","CHILDES_corpora.RDS")
substitutions_file <- here("corpus_processing", "data", "corpus_substitutions.csv")
stem_overrides_file <- here("corpus_processing", "data", "stem_overrides.csv")
stemcomp_overrides_file <- here("corpus_processing", "data", "stemcomp_overrides.csv")
stopwords_fa_file <- here("corpus_processing", "data", "corpus_stopwords.csv")

#####################################
########### LOAD CORPORA ############
#####################################

############# CHILDES ###############

# You can use the CHILDES_corpora.RDS file included in this project and 
# skip straight to loading it as "corpora" below.

# If you want to re-extract CHILDES corpora, you need to use the package "childsr"
# and run the commented-out step below.
# This step extracts only English North American corpora and eliminates speech produced
# by child to focus on input to child
# corpora <- get_utterances(collection = "Eng-NA",
#                           corpus = c("Brown","Bates","Bernstein", "Bliss", "Bloom70","Bloom73","Bohannon",
#                                      "Braunwald","Brent","Carterette","Clark","Cornell","Davis","Demetras1","Demetras2","Evans",
#                                      "Garvey","Gathercole","Gelman", "Gleason","Goad", "Hall","Higginson",
#                                      "HSLLD","Inkelas","Jimmy","MacWhinney","McCune","McMillan","NewEngland", "NewmanRatner","Post",
#                                      "Providence","Rollins","Sawyer","Soderstrom","Suppes","Tardif", "VanKleek","Warren",
#                                      "Weist"),
#                           role = NULL, role_exclude = c("Target_Child"),
#                           db_version = "current")
# Save for future use
# saveRDS(corpora,file=corpora_file)

corpora_childes <- readRDS(corpora_file)

# In the corpus, each row is an utterance which is transcribed in the column
# "gloss". Additional columns provide information such as a unique ID for the 
# utterance ("id"), the age of the child
head(corpora_childes)

# Remove any rows in which the utterance was recorded as NA
corpora_childes <- corpora_childes %>%
  dplyr::filter(!is.na(gloss))

#####################################
####### SPECIAL CHARACTERS ##########
#####################################

# Define and use a function to remove special characters 
# between compound words and possessives and convert to lower case

clean_special_characters <- function(corpus) {
  corpus %>%
    mutate(gloss = str_replace_all(gloss, '\\+', '')) %>%
    mutate(gloss = str_replace_all(gloss, '_', ' ')) %>%
    mutate(gloss = str_replace_all(gloss, "'s", "")) %>%
    mutate(gloss = tolower(gloss))
}

cleaned_childes <- clean_special_characters(corpora_childes)


#####################################
########## SUBSTITUTIONS ############
#####################################

# Perform some substitutions, including substituting common compound
# words (e.g., rocking chair) with single words (rockingchiair),
# irregular plurals with singular forms (teeth as tooth)

substitute_words <- function(corpus, substitutions_file) {
  substitutions <- read_csv(substitutions_file, show_col_types = FALSE)
  
  substitution_vector <- substitutions$sub
  names(substitution_vector) <- substitutions$original
  
  corpus %>%
    mutate(gloss = str_replace_all(gloss, substitution_vector))
}


substituted_childes <- substitute_words(cleaned_childes, substitutions_file)


#####################################
############## UNNEST ###############
#####################################


# The following steps work with a version of the corpus that
# has been unnested so that each word is a row
unnested <- substituted_childes %>% 
  dplyr::select(transcript_id, gloss) %>%
  unnest_tokens(word, gloss) %>%
  dplyr::rename(id = transcript_id)


#####################################
######## RARE & NOISE WORDS #########
#####################################

# Remove rare words and "words" that are actually transcribed noises
# (e.g., "aah")
# You can set a cutoff for the frequency that is used to remove
# rare words. The default is that words that occur 3 times or fewer
# are removed.

filter_rare_noise <- function(unnested_corpus, rare_cutoff = 3) {
  
  #Count the number of times each word occurs across corpora
  word_count <- unnested_corpus %>%
    dplyr::count(word, sort = TRUE)
  
  #Identify words that occur very rarely (<=3x here, can be changed)
  rare_words <- word_count$word[word_count$n <= rare_cutoff]
  
  # Identify and remove noise annotations that appear often in CHILDES transcripts
  noises <- c("xxx","huh","ya","hm","mhm","uhhuh","um","uh","em",
              "www","cha","ta","yyy","uhuh","ah","eh","mm",
              "awoh","hmhm","�", "aa", "aaaaaaaaaaa","aaaa","aah",
              "aaah","oh","ooh","aw","uhhum","hum")
  
  unnested_corpus %>%
    dplyr::filter(!(word %in% noises | word %in% rare_words)) %>%
    filter(!str_detect(word, "[^ -~]"))
}


unnested_filter <- filter_rare_noise(unnested)



#####################################
##### LEMMATIZE TO COMMON FORM ######
#####################################

# Lemmatize words by identifying word stems, identifying
# the most common form associated with a stem, and replacing
# different forms with the most common form.


lemmatize_words <- function(unnested_corpus) {
  
  # Step 1: Generate a "stemmed" object that contains the stemmed
  # versions of the words in the corpora
  stemmed <- unnested_corpus %>%
    mutate(stem = SnowballC::wordStem(word)) %>%
    dplyr::rename(unstemmed = word)
  
  
  # Step 2: Apply stem corrections from CSV
  stem_overrides <- read_csv(stem_overrides_file, show_col_types = FALSE)
  
  stemmed <- stemmed %>%
    left_join(stem_overrides, by = "unstemmed") %>%
    mutate(stem = coalesce(corrected_stem, stem)) %>%
    select(-corrected_stem)
  
  # Step 3: Identify the completed stem (stemcomp) as the most common form 
  completed <- stemmed %>%
    group_by(stem) %>%
    count(unstemmed, sort = TRUE) %>%
    slice_max(n, with_ties = FALSE) %>%
    ungroup() %>%
    select(unstemmed, stem, stemcomp = unstemmed)
  
  unnested_stemmed <- stemmed %>%
    left_join(completed, by = c("stem"))
  
  # Step 4: Apply stemcomp corrections from CSV
  stemcomp_overrides <- read_csv(stemcomp_overrides_file, show_col_types = FALSE)
  
  unnested_stemcomp <- unnested_stemmed %>%
    left_join(stemcomp_overrides, by = "unstemmed") %>%
    mutate(stemcomp = coalesce(stemcomp.y, stemcomp.x)) %>%
    select(-stemcomp.x, -stemcomp.y)
  
  # Format to have one column for the original version of
  # a word, and one column for the lemma
  unnested_lemmas <- unnested_stemcomp %>%
    dplyr::select(id, unstemmed, stemcomp) %>%
    dplyr::rename(word = unstemmed, lemma = stemcomp)
  
  return(unnested_lemmas)
}



unnested_lemmas <- lemmatize_words(unnested_filter)


#####################################
######### REMOVE STOPWORDS ##########
#####################################

# Commonly used stop words
common_stop <- get_stopwords()

# Load stop words *not* to remove because they are used in the free association dataset
skip_stop <- read_csv(file = stopwords_fa_file, show_col_types = FALSE)

# Amend common.stop to exclude words that appear in free association dataset
common_stop <- common_stop[!(common_stop$word %in% skip_stop$word),]

# Remove stop words from unnested corpus
unnested_lemmas_filter <- unnested_lemmas %>%
  anti_join(common_stop)


#####################################
######### RECOMBINE CORPORA #########
#####################################

# Combine words within each transcript back together.

corpora_processed <- unnested_lemmas_filter %>%
  group_by(id) %>%
  dplyr::summarise(Text = paste(lemma, collapse=" "))


# Save preprocessed corpus as an .rds file
processed_file <- here("corpus_processing", "data", "CHILDES_corpora_processed.rds")
saveRDS(corpora_processed, file = processed_file)
