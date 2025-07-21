library(jsonlite)

#' Decode HTML entities to proper UTF-8 characters
decode_html_entities <- function(words) {
  # Convert common German HTML entities to UTF-8
  words <- gsub("<e4>", "ä", words, fixed = TRUE)  # a-umlaut
  words <- gsub("<f6>", "ö", words, fixed = TRUE)  # o-umlaut  
  words <- gsub("<fc>", "ü", words, fixed = TRUE)  # u-umlaut
  words <- gsub("<c4>", "Ä", words, fixed = TRUE)  # A-umlaut
  words <- gsub("<d6>", "Ö", words, fixed = TRUE)  # O-umlaut
  words <- gsub("<dc>", "Ü", words, fixed = TRUE)  # U-umlaut
  words <- gsub("<df>", "ß", words, fixed = TRUE)  # eszett
  
  # Additional common HTML entities
  words <- gsub("&amp;", "&", words, fixed = TRUE)
  words <- gsub("&lt;", "<", words, fixed = TRUE)
  words <- gsub("&gt;", ">", words, fixed = TRUE)
  words <- gsub("&quot;", '"', words, fixed = TRUE)
  words <- gsub("&#39;", "'", words, fixed = TRUE)
  
  return(words)
}

#' Clean words to ensure UTF-8 compatibility
clean_utf8_words <- function(words) {
  # Remove or replace problematic characters
  cleaned <- sapply(words, function(word) {
    # Convert to UTF-8 and remove invalid bytes
    word_utf8 <- iconv(word, to = "UTF-8", sub = "")
    if (is.na(word_utf8)) return("")
    
    # Keep only basic Latin, Latin Extended, and common German characters
    word_clean <- gsub("[^a-zA-Z0-9 äöüÄÖÜß-]", "", word_utf8)
    # Remove excessive whitespace
    word_clean <- trimws(word_clean)
    # Return empty string if cleaning resulted in very short or empty string
    if (nchar(word_clean) < 2) return("")
    return(word_clean)
  }, USE.NAMES = FALSE)
  
  # Filter out empty strings
  cleaned <- cleaned[nchar(cleaned) >= 2]
  return(cleaned)
}

#' Translate German words to English with caching
translate_german_words <- function(german_words, use_english = FALSE) {
  if (!use_english) {
    return(list(words = german_words, translated = rep(FALSE, length(german_words))))
  }
  
  # First decode HTML entities to fix corruption
  german_words_decoded <- decode_html_entities(german_words)
  
  # Clean words for UTF-8 compatibility but maintain original length
  german_words_clean <- sapply(german_words_decoded, function(word) {
    # Convert to UTF-8 and remove invalid bytes
    word_utf8 <- iconv(word, to = "UTF-8", sub = "")
    if (is.na(word_utf8)) return(word)
    
    # Keep only basic Latin, Latin Extended, and common German characters
    word_clean <- gsub("[^a-zA-Z0-9 äöüÄÖÜß-]", "", word_utf8)
    # Remove excessive whitespace
    word_clean <- trimws(word_clean)
    # Return original if cleaning resulted in very short or empty string
    if (nchar(word_clean) < 2) return(word)
    return(word_clean)
  }, USE.NAMES = FALSE)
  
  # Load cache
  if (file.exists(CACHE_FILE)) {
    load(CACHE_FILE)
  } else {
    translation_cache <- list()
  }
  
  # Find words needing translation (using cleaned versions)
  words_to_translate <- setdiff(unique(german_words_clean), names(translation_cache))
  # Additional safety: only translate words that are valid
  words_to_translate <- words_to_translate[nchar(words_to_translate) >= 2]
  
  # Translate new words
  if (length(words_to_translate) > 0) {
    cat("Translating", length(words_to_translate), "new words...\n")
    
    # Use simple Python translation with better encoding handling
    temp_file <- tempfile(fileext = ".json")
    # Convert to ASCII-safe JSON
    json_content <- jsonlite::toJSON(words_to_translate, auto_unbox = FALSE, force = TRUE)
    # Write with explicit UTF-8 encoding
    writeLines(json_content, temp_file, useBytes = FALSE)
    
    # Try translation with error handling
    tryCatch({
      # Suppress warnings about UTF-8 conversion
      result_json <- suppressWarnings(
        system(paste0('python "a2-embed-translate_simple.py" "', temp_file, '"'), 
               intern = TRUE, ignore.stderr = TRUE)
      )
      unlink(temp_file)
      
      if (length(result_json) > 0 && !any(grepl("Error|Traceback", result_json))) {
        # Try to parse JSON with multiple fallback strategies
        json_parsed <- FALSE
        
        # Strategy 1: Direct parsing
        tryCatch({
          result <- jsonlite::fromJSON(paste(result_json, collapse = ""))
          json_parsed <- TRUE
        }, error = function(e) {
          # Strategy 2: Clean and try again
          tryCatch({
            clean_json <- suppressWarnings(
              iconv(paste(result_json, collapse = ""), to = "UTF-8", sub = "")
            )
            if (!is.na(clean_json)) {
              result <<- jsonlite::fromJSON(clean_json)
              json_parsed <<- TRUE
            }
          }, error = function(e2) {
            # Strategy 3: Try with different encoding
            tryCatch({
              clean_json <- paste(result_json, collapse = "")
              # Remove any problematic characters
              clean_json <- gsub("[^\\x20-\\x7E\\u00A0-\\u00FF\\u0100-\\u017F\\u1E00-\\u1EFF]", "", clean_json)
              result <<- jsonlite::fromJSON(clean_json)
              json_parsed <<- TRUE
            }, error = function(e3) {
              cat("JSON parsing failed after multiple attempts\n")
            })
          })
        })
        
        # If parsing succeeded, update cache
        if (json_parsed && exists("result") && !is.null(result$german) && !is.null(result$english)) {
          for (i in 1:length(result$german)) {
            translation_cache[[result$german[i]]] <- result$english[i]
          }
          save(translation_cache, file = CACHE_FILE)
        }
      }
    }, error = function(e) {
      cat("Translation error:", e$message, "\n")
      unlink(temp_file)
    })
  }
  
  # Apply translations (using cleaned words for lookup but maintaining original order)
  translations <- sapply(1:length(german_words), function(i) {
    clean_word <- german_words_clean[i]
    if (clean_word %in% names(translation_cache)) {
      gsub("\\(\\?\\)", "?", translation_cache[[clean_word]])
    } else {
      # Return original word if no translation found
      german_words[i]
    }
  })
  
  translated_flags <- sapply(1:length(german_words), function(i) {
    clean_word <- german_words_clean[i]
    clean_word %in% names(translation_cache) && 
      translation_cache[[clean_word]] != clean_word
  })
  
  return(list(words = translations, translated = translated_flags))
}
