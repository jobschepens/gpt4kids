library(data.table)
library(tidyverse)
library(umap)
library(udpipe)
library(fastTextR)
source("a2-embed-config.R")
source("a2-embed-translation_utils.R")
set.seed(RANDOM_SEED)

# Load data
cat("Loading FastText model (this may take a moment)...\n")
if (grepl("\\.bin$", FASTTEXT_FILE)) {
  # Binary format - use fastTextR
  fasttext_model <- fastTextR::ft_load(FASTTEXT_FILE)
  use_binary_format <- TRUE
} else {
  # Text format - use data.table with proper column handling
  fasttext_vectors <- fread(FASTTEXT_FILE, quote = "", header = FALSE)
  # Set first column as word, rest as embedding dimensions
  setnames(fasttext_vectors, c("word", paste0("dim_", 1:(ncol(fasttext_vectors)-1))))
  use_binary_format <- FALSE
}
load(DATA_FILE)

# Main plotting function
getPlot <- function(words_of_interest, use_english = FALSE, stored_sample = NULL) {
  # Sample words
  if (is.null(stored_sample)) {
    word_sample <- words_of_interest[sample(length(words_of_interest), SAMPLE_SIZE)]
  } else {
    word_sample <- intersect(stored_sample, words_of_interest)
  }
  
  # Get embeddings from FastText model
  if (use_binary_format) {
    # Binary format - use fastTextR
    word_vectors <- fastTextR::ft_word_vectors(fasttext_model, word_sample)
    # Remove words that don't have embeddings
    valid_indices <- !is.na(rowSums(word_vectors))
    word_vectors <- word_vectors[valid_indices, , drop = FALSE]
    word_sample <- word_sample[valid_indices]
  } else {
    # Text format - use data.table
    word_subset <- fasttext_vectors[fasttext_vectors$word %in% word_sample, ]
    if (nrow(word_subset) == 0) {
      return(data.frame())
    }
    word_vectors <- as.matrix(word_subset[, -1, with = FALSE])
    word_sample <- word_subset$word
  }
  
  umap_result <- umap(word_vectors)
  
  # Create dataframe
  umap_df <- data.frame(
    comp1 = umap_result$layout[, 1], 
    comp2 = umap_result$layout[, 2], 
    Word_DE = word_sample
  )
  
  # Add translations if requested
  if (use_english) {
    translation_result <- translate_german_words(word_sample, use_english = TRUE)
    umap_df$Word <- translation_result$words
    umap_df$Translated <- translation_result$translated
  } else {
    umap_df$Word <- umap_df$Word_DE
    umap_df$Translated <- rep(FALSE, nrow(umap_df))
  }
  
  # Add POS tags
  udmodel <- udpipe_load_model(file = UDPIPE_MODEL)
  pos_result <- udpipe_annotate(udmodel, x = word_sample)
  pos_df <- as.data.frame(pos_result)
  
  # Get the first POS tag for each word (in case of tokenization splits)
  pos_tags <- pos_df %>%
    group_by(doc_id) %>%
    slice(1) %>%
    pull(upos)
  
  # Ensure we have the right number of POS tags
  if (length(pos_tags) != length(word_sample)) {
    pos_tags <- rep("UNKNOWN", length(word_sample))
  }
  
  umap_df$POS <- pos_tags
  
  return(umap_df)
}

# Load or generate word samples
if (file.exists(SAMPLES_FILE)) {
  load(SAMPLES_FILE)
} else {
  set.seed(RANDOM_SEED)
  words_chc <- all_wider_againl %>% filter(value_chht < .1 & norm_chcount > .1) %>% pull(merged_type)
  words_llm <- all_wider_againl %>% filter(value_chcount < .1 & value_chht > .1) %>% pull(merged_type)
  words_ch2 <- all_wider_againl %>% filter(norm_chht > 50 & norm_chcount < 10) %>% pull(merged_type)
  words_ll2 <- all_wider_againl %>% filter(norm_chcount > 50 & norm_chht < 10) %>% pull(merged_type)
  
  sample_chc <- sample(words_chc, min(SAMPLE_SIZE, length(words_chc)))
  sample_llm <- sample(words_llm, min(SAMPLE_SIZE, length(words_llm)))
  sample_ch2 <- sample(words_ch2, min(SAMPLE_SIZE, length(words_ch2)))
  sample_ll2 <- sample(words_ll2, min(SAMPLE_SIZE, length(words_ll2)))
  
  save(sample_chc, sample_llm, sample_ch2, sample_ll2, file = SAMPLES_FILE)
}

# Generate plots for both languages
for (lang in c("german", "english")) {
  use_english <- (lang == "english")
  
  # Create plots for each category
  chc <- getPlot(all_wider_againl %>% filter(value_chht < .1 & norm_chcount > .1) %>% pull(merged_type), 
                 use_english, sample_chc)
  llm <- getPlot(all_wider_againl %>% filter(value_chcount < .1 & value_chht > .1) %>% pull(merged_type), 
                 use_english, sample_llm)
  ch2 <- getPlot(all_wider_againl %>% filter(norm_chht > 50 & norm_chcount < 10) %>% pull(merged_type), 
                 use_english, sample_ch2)
  ll2 <- getPlot(all_wider_againl %>% filter(norm_chcount > 50 & norm_chht < 10) %>% pull(merged_type), 
                 use_english, sample_ll2)
  
  # Skip if any category is empty
  if (nrow(chc) == 0 || nrow(llm) == 0 || nrow(ch2) == 0 || nrow(ll2) == 0) {
    cat("Skipping", lang, "- insufficient data for one or more categories\n")
    next
  }
  
  # Add category labels
  chc$c <- CATEGORIES[1]
  llm$c <- CATEGORIES[2] 
  ch2$c <- CATEGORIES[3]
  ll2$c <- CATEGORIES[4]
  
  # Combine data
  all_data <- rbind(chc, llm, ll2, ch2)
  all_data$c <- factor(all_data$c, levels = CATEGORIES)
  
  # Create plot
  p <- ggplot(all_data, aes(x = comp1, y = comp2, label = Word, color = POS)) +
    facet_wrap(vars(c)) +
    geom_point(alpha = 0) +
    geom_text(size = 3, vjust = -0.5, check_overlap = TRUE, show.legend = FALSE) +
    theme_minimal() +
    labs(x = "", y = "") +
    theme(axis.text = element_blank(), axis.ticks = element_blank()) +
    guides(color = guide_legend(override.aes = list(shape = 15, size = 3, alpha = 1, label = "")))
  
  # Save plot
  lang_suffix <- if(use_english) "EN" else "DE"
  format_suffix <- if(use_binary_format) "bin" else "vec"
  filename <- paste0("../figures/a2-embed-embed100-2-", format_suffix, "-", lang_suffix, ".pdf")
  ggsave(p, filename = filename, width = FIGURE_WIDTH, height = FIGURE_HEIGHT, dpi = 300)
  
  cat("Saved:", filename, "\n")
  
  # Print translation stats for English version
  if (use_english) {
    translated_count <- sum(all_data$Translated, na.rm = TRUE)
    total_count <- nrow(all_data)
    cat("Translation success:", translated_count, "/", total_count, 
        "(", round(100 * translated_count / total_count, 1), "%)\n")
  }
}
