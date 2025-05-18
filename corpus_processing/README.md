The corpus_processing.R script preprocesses the CHILDES corpus datasets for North American English. These datasets contain transcribed natural language input to children. 

The goal of preprocessing steps is to align the word forms in the corpus to word forms that appear in other child language datasets, including the MacArthur-Bates Communicative Development Inventories (MCDI) datasets of word age of acquisition and a dataset of child free associations betweenwords. This folder contains additional .csv files used to align word forms across datasets. 

The preprocessing steps include:

(1) Load CHILDES datasets (These are already saved as a single .RDS file that contains many CHILDES datasets)

(2) Convert all words to lower case and remove some special characters

(3) Replace some word forms to match forms in MCDI norms, stimuli in priming studies, or words in free association data (e.g., "ice cream" is converted to "icecream")

(4) Divide transcripts into a tibble in which each row is a single word from a single transcript. The order of rows is the same as the order in which words appeared in utterances. E.g., for an utterance, "How was your day...", there will be rows for "how", "was", "your", "day" etc

(5) Remove rare words and "words" for noises (e.g., "uh")

(6) Lemmatize by using a stemming package to stem the words (e.g., "baking"/"baked" -> "bak") and replace all morphological variants of a root word with the most common variant - the lemma

(7) Remove a small set of high-frequency stop words

(8) Recombine processed words back into utterances in transcripts
