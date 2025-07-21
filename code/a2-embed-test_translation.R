# =============================================================================
# Test German-English Translation
# =============================================================================

source("a2-embed-config.R")
source("a2-embed-translation_utils.R")

# Test with common German words
test_words <- c("Haus", "Auto", "Hund", "Katze", "Wasser", "Brot", "Schule")
result <- translate_german_words(test_words, use_english = TRUE)

cat("=== TRANSLATION TEST ===\n")
for (i in 1:length(test_words)) {
  status <- if (result$translated[i]) "✓" else "✗"
  cat(sprintf("%-10s -> %-15s [%s]\n", test_words[i], result$words[i], status))
}

success_rate <- round(100 * sum(result$translated) / length(test_words), 1)
cat("\nSuccess rate:", success_rate, "%\n")
