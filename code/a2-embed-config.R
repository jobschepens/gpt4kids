# Key settings
SAMPLE_SIZE <- 500
RANDOM_SEED <- 1234
FIGURE_WIDTH <- 8
FIGURE_HEIGHT <- 7

# File paths
FASTTEXT_FILE <- "../data-original/cc.de.300.bin" # or ../data-original/cc.de.300.vec
DATA_FILE <- "../data-processed/all_wider_again_subt_ch_plus1_prob.RData"
UDPIPE_MODEL <- "../data-original/german-hdt-ud-2.5-191206.udpipe"
CACHE_FILE <- "../data-processed/translation_cache.RData"
SAMPLES_FILE <- "../data-processed/word_samples.RData"

# Plot categories
CATEGORIES <- c("LLM only", "ChildLex only", "LLM > 50, ChildLex < 10", "ChildLex > 50, LLM < 10")
