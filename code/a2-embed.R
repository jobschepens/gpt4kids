set.seed(1234)

# install.packages(c("ggplot2", "Rtsne", "text2vec"))
# install.packages("reticulate") # for BERT if using Python-based embeddings
# install.packages("umap")
library(umap)
library(udpipe)
library(ggplot2)
library(dplyr)

# Load necessary libraries
library(data.table)
library(tidyverse)
library(ggplot2)
library(Rtsne)
library(FNN)

# 1️⃣ Load pretrained FastText embeddings
fasttext_file <- "data-original/cc.de.300.vec"  # Download from fasttext.cc
# fasttext_vectors <- fread(fasttext_file, skip = 1, header = FALSE, quote = "")

# Rename first column as 'word' and others as vector dimensions
colnames(fasttext_vectors) <- c("word", paste0("V", 1:300))

# from wider_again.R
load("data-processed/all_wider_again_subt_ch_plus1_prob.RData", verbose = T)

getPlot <- function(words_of_interest, title) {
  word_subset_sample <- words_of_interest[sample(length(words_of_interest), sample_size)]
  # Filter for selected words
  word_subset <- fasttext_vectors[fasttext_vectors$word %in% word_subset_sample, ]
  
  # To handle large datasets better, we'll use a random subset 
  word_subset_sample <- word_subset[sample(nrow(word_subset), sample_size/3), ]
  word_vectors_sample <- as.matrix(word_subset_sample[, -1, with = FALSE]) # remove words
  
  # Apply UMAP on your embeddings (assuming fasttext_vectors is a dataframe with embeddings)
  umap_result <- umap(word_vectors_sample)  
  
  # Create a data frame for UMAP components and words
  umap_df <- data.frame(comp1 = umap_result$layout[, 1], 
                        comp2 = umap_result$layout[, 2], 
                        Word = word_subset_sample$word)
  
  # load("data-original/udpipe-de.RData", verbose = T)
  # udmodel_german <- udpipe_load_model(file = de$file_model)
  pos_result <- udpipe_annotate(udmodel_german, x = word_subset_sample$word)
  pos_df <- as.data.frame(pos_result)
  umap_df$POS <- pos_df$upos  
  
  # Perform t-SNE
  # tsne_result <- Rtsne(word_vectors_sample, dims = 2, pca = TRUE, check_duplicates = FALSE)
  
  # Create a dataframe for plotting the t-SNE results
  # tsne_df <- data.frame(
  #   Word = word_subset_sample$word,
  #   comp1 = tsne_result$Y[, 1],  # First t-SNE component
  #   comp2 = tsne_result$Y[, 2]   # Second t-SNE component
  # )
  
  umap_df 
}

words_of_interest <- all_wider_againl %>% filter(value_chht < .1 & value_chcount > .1) %>% pull(merged_type)
# words_of_interest <- all_wider_againl %>% filter(norm_chcount > 50 & norm_chht < 10) %>% pull(merged_type)
sample_size <- 300
chc <- getPlot(all_wider_againl %>% filter(value_chht < .1 & norm_chcount > .1) %>% pull(merged_type))
llm <- getPlot(all_wider_againl %>% filter(value_chcount < .1 & value_chht > .1) %>% pull(merged_type))
ch2 <- getPlot(all_wider_againl %>% filter(norm_chht > 50 & norm_chcount < 10) %>% pull(merged_type))
ll2 <- getPlot(all_wider_againl %>% filter(norm_chcount > 50 & norm_chht < 10) %>% pull(merged_type))
# sha <- getPlot(all_wider_againl %>% filter(norm_chht > 100) %>% pull(merged_type))
# hfr <- getPlot(all_wider_againl %>% filter(norm_chcount > 100) %>% pull(merged_type))

chc$c <- "LLM only"
llm$c <- "ChildLex only"
ch2$c <- "LLM > 50, ChildLex < 10"
ll2$c <- "ChildLex > 50, LLM < 10"
all <- rbind(chc, llm, ll2, ch2)
all$c <- factor(all$c, levels = c("LLM only", "ChildLex only", "LLM > 50, ChildLex < 10", "ChildLex > 50, LLM < 10"))

# 4️⃣ Visualize
a <- ggplot(all, aes(x = comp1, y = comp2, label = Word, color = POS)) +
  facet_wrap(vars(c)) +
  # geom_point(color = "blue", size = 1, alpha = .5) +
  # geom_bin2d() +  # or use geom_hex()
  geom_text(aes(label = Word), size = 3, vjust = -0.5, check_overlap = TRUE) +
  # geom_text(data = sampled_points, aes(label = Word), size = 3, vjust = -0.5, check_overlap = TRUE) +
  theme_minimal() +
  labs(#title = paste(title),
       # labs(title = paste(title)) +
       x = "",
       y = "") +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

# Save the plot with A4 size
ggsave(a, filename = "figures/embed100.pdf", width = 8, height = 7, dpi = 300)


