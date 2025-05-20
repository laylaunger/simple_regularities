# Title: Bootstap Results
# Author: Layla Unger
# Last Updated: 2025-05-18
# R version: 4.3.2
# Packages: here, readr, dplyr, stringr, tidyr, ggplot2, ggridges

################################################
################ DESCRIPTION ###################
################################################

# This script is the third step in assessing the degree to which words 
# in the same semantic category (e.g., words for body parts) co-occur 
# more reliably than random words matched in frequency.

# The steps implemented in this script are:
# (1) Load the bootstrap comparison between (A) co-occurrence of words 
#     in a category and (B) co-occurrence of the same number of randomly 
#     sampled words matched in frequency
# (2) Visualize the results of the bootstrap comparison


################################################
################# PACKAGES #####################
################################################

library(here)
library(readr)

library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)

################################################
################# LOAD DATA ####################
################################################

count_words_file <- here::here("similarity_meaning_study1", "data", "count_words.rds")
category_file <- here::here("similarity_meaning_study1", "data", "categories.csv")
categories_comparisons_file <-  here::here("similarity_meaning_study1", "data", "category_comparisons.rds")

categories_comparisons <- readRDS(file = categories_comparisons_file)
count_words <- readRDS(count_words_file)
categories_raw <- read_csv(file = category_file, show_col_types = FALSE)

################################################
############## FORMAT COLUMNS ##################
################################################

# Format score type (e.g., t-score, ppmi), window size (3, 7, 11), and sd_level factors
categories_comparisons <- categories_comparisons %>%
  dplyr::mutate(score_type = str_extract(score, "^[[:alnum:]]+"),
                window = str_extract(score, "[[:digit:]]{1,2}$"),
                window = factor(window, levels = c(3, 7, 11)),
                sd_level = factor(sd_level, levels = c(1,2)))

# For each category, score type, window size score, and cutoff, calculate the proportion
# of comparisons in which co-occurrence within a category (referred to here as "density")
# was greater than co-occurrence in randomly sampled words matched in frequency (i.e., 
# density value above 0)
# This proportion is the bootstrapped p-value
comparison_prop <- categories_comparisons %>%
  group_by(score_type, window, sd_level, category) %>%
  dplyr::summarise(prop = mean(density > 0),
                   prop_smooth = mean(density_smooth > 0),
                   p_smooth = 1 - prop_smooth,
                   p_smooth_label = ifelse(p_smooth < .001, "< .001", as.character(round(p_smooth, 3)) ),
                   p_smooth_sig = ifelse(p_smooth < .05, 1, 0),
                   p_smooth_adjust = p.adjust(p_smooth, method = "holm", n = 3))


###############################################
########### CATEGORY DESCRIPTIONS #############
###############################################

# Used for plotting below: Format category names and identify 
# examples of words in each category

# Function that identifies 3 example words for each category
category_description <- function(input_category) {
  
  category_words_freq <- input_category %>%
    dplyr::select(word) %>%
    left_join(count_words) %>%
    dplyr::arrange(desc(n))
  
  num_words <- nrow(input_category)
  most_freq <- paste(category_words_freq$word[1:3], collapse = " ")
  
  caption <- paste0("(", num_words, "; ", most_freq, ")", collapse = "")
  
  description <- data.frame(num_words, most_freq, caption)
  return(description)
}

# Generate example words for each category
category_descriptions <- categories_raw %>%
  group_by(category) %>%
  group_modify(~ category_description(.x)) %>%
  ungroup()


# Format names of categories and examples use as labels in plots below
category_descriptions <- category_descriptions %>%
  dplyr::mutate(category = tools::toTitleCase(gsub("_", " ", category)))


category_descriptions <- tidyr::unite(category_descriptions, col = "label", c(category, caption), sep = " ", remove = F)


################################################
############## VISUALIZATIONS ##################
################################################

# Plot distributions of differences in category density between categories 
# and randomly sampled frequency-matched words
comparison_plot <- categories_comparisons %>%
  filter(sd_level == 1, score_type == "tscore") %>%
  ggplot(aes(x = density_smooth, y = rev(factor(category)))) +
  ggridges::geom_density_ridges(
    aes(fill = factor(category), alpha = window),
    scale = 3.5,
    rel_min_height = 0.01,
    #linewidth = .3
    color = rgb(0,0,0,0)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_alpha_manual(values = c(0.3, 0.6, 0.8), name = "Window Size", labels = c(3, 7, 11)) +
  scale_x_continuous(limits = c(-0.2, 1)) +
  scale_y_discrete(
    name = "Semantic Category (number of words; examples)",
    labels = rev(category_descriptions$label),
    expand = expansion(add = c(0.2, 4))
  ) +
  labs(
    x = "Reliable Co-Occurrence between Same-Category Words vs\nRandomly Sampled Frequency-Matched Words"
  ) +
  guides(fill = "none") +
  coord_cartesian(xlim = c(-.2, 1)) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 8 / 1.5),
    axis.title = element_text(size = 9 / 1.5),
    legend.title = element_text(size = 8 / 1.5),
    legend.text = element_text(size = 7 / 1.5),
    legend.key.size = unit(0.03, "npc"),
    legend.margin = margin(0, 0, 0, 0),
    panel.grid = element_blank()
  )


comparison_plot

# Plot significance for each category/window size
sd_labs <- c("Cutoff: 1 SD","Cutoff: 2 SD")
names(sd_labs) <- c(1, 2)

comparison_stats <- ggplot(comparison_prop[comparison_prop$score_type == "tscore",], aes(window, rev(factor(category)))) + 
  facet_wrap(~sd_level, labeller = as_labeller(sd_labs)) +
  geom_tile(aes(fill = factor(p_smooth_sig)), colour = "white", show.legend = F) + 
  geom_text(aes(label = p_smooth_label), size = 1.75) +
  scale_x_discrete(name = "Window Size", labels = c(3, 7, 11)) +
  scale_y_discrete(name = "Semantic Category", labels = rev(category_descriptions$category)) +
  scale_fill_manual(values = c("lightgray", "white")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        text = element_text(size = 6),
        strip.text = element_text(size = 7, face = "bold"))

comparison_stats

################################################
########### EXPORT VISUALIZATIONS ##############
################################################

comparison_plot_file <- here::here("similarity_meaning_study1", "figures", "Co-Occurrence in Categories.pdf")

pdf(file = comparison_plot_file, width = 4.75, height = 4)
comparison_plot
dev.off()

png(file = "Co-Occurrence in Categories - Stats.png", width = 900, height = 750, type = "cairo", res = 210)
comparison_stats
dev.off()

