# setup ---- 



wordtype = "types"
# wordtype = "lemma"
# conditions = c("adlt", "adht", "chlt", "chht"); exp = "exp2" # for output file
# conditions = c("chlt", "chht", "dslo", "lllo")
# conditions = c("lllo", "llsh", "dslo", "dssh"); exp = "exp3" # for output file
conditions = c("lll2", "lls2", "dsl2", "dss2"); exp = "exp3-2" # for output file
# conditions = c("lls2")
# conditions = c("dss2")

MIN_FREQUENCY = 0
MIN_FREQUENCY_JOINED = 0
MIN_FREQUENCY_LABEL = 17
MIN_FREQUENCY_LABEL = 5
MIN_FREQUENCY_COR <- 0
MAX_FREQUENCY = 70001 # for plotting
knitr::opts_chunk$set(echo = TRUE)

# readRenviron("../RProfile.site")
# Sys.setenv(openai_secret_key = Sys.getenv("OPENAI_API_KEY"))

# library("koRpus.lang.de")
# library(textstem)
library(tm)
library(readtext)
library(SnowballC)
library(tidyverse)
library(scales)
library(lexicon)
library(stringi)

# Load LLM frequency list ----

library(tm)
library(stringi)
library(dplyr)

removeMoreThings <- function(freq_df) {
  # freq_df
  freq_df$addcleaned <- freq_df$cleaned
  
  # Normalize and remove punctuation/special characters
  # freq_df$addcleaned <- stri_trans_general(freq_df$addcleaned, "Latin-ASCII")
  freq_df$addcleaned <- gsub("[[:punct:]„“”‘’‚…«»]", " ", freq_df$addcleaned)
  freq_df$addcleaned <- gsub("[^A-Za-zÄÖÜäöüßé ]", "", freq_df$addcleaned)
  freq_df$addcleaned <- gsub("\\s+", " ", freq_df$addcleaned)
  freq_df$addcleaned <- trimws(freq_df$addcleaned)
  
  # Remove entries that are just numbers or empty
  freq_df <- freq_df[!grepl("^[0-9]+$", freq_df$addcleaned), ]
  freq_df <- freq_df[freq_df$addcleaned != "", ]
  
  print("remove words that are only numbers and words that now only consist of spaces after removing punctiuation them")
  print(sum(freq_df$ai_freq))
  print(length(freq_df$ai_freq))
  
  # freq_df$addcleaned <- freq_df$cleaned
  freq_df$something <- freq_df$addcleaned
  freq_df$something[freq_df$addcleaned == freq_df$cleaned] <- NA
  
  # aben <- freq_df$cleaned[freq_df$addcleaned == "zurückkehren"]
  # paste(unique(aben), collapse = " ")
  
  # Combine frequencies for duplicates and preserve all columns
  freq_df$addcleaned <- as.factor(freq_df$addcleaned)
  freq_df <- freq_df %>%
    dplyr::group_by(addcleaned) %>%
    dplyr::summarize(
      keep = first(cleaned),       # Keep the first occurrence of the original word
      otherfreq = sum(ai_freq) - first(ai_freq),
      ai_freq = sum(ai_freq),            # Sum the frequencies for duplicates
      n = n(),
      remwords = paste(cleaned, collapse = " "),
      .groups = "drop"
    ) %>%
    dplyr::arrange(addcleaned)           # Sort alphabetically by original word
  # freq_df
  print("after removing duplicates")
  print(format(sum(freq_df$ai_freq)), big.mark = ",", scientific = FALSE)
  print(length(freq_df$ai_freq))
  
  return(freq_df)
}

getFreqMat <- function(df, opts = list()) {
  # df <- df_l

  # Step 1: Create a corpus from cleaned text
  corpus <- Corpus(VectorSource(df$text))
  tdm <- TermDocumentMatrix(corpus, control = opts)
  mat <- as.matrix(tdm) # takes long, 2.4 gb
  cleaned_freq <- rowSums(mat)
  
  # Create data frame of cleaned frequency list
  freq_df <- data.frame(
    cleaned = names(cleaned_freq),
    ai_freq = as.integer(cleaned_freq),
    stringsAsFactors = FALSE
  )
  
  print("after applying pipeline as in exp 1 and 2")
  print(sum(freq_df$ai_freq))
  print(length(freq_df$ai_freq))
  # remove(mat, tdm, corpus, cleaned_freq, df)
  
  # remove more things
  clean_table <- removeMoreThings(freq_df) # adds additionally cleaned words as new column "addcleaned"
  
  return(clean_table)
}

opts <- list(removeNumbers = T,
						removePunctuation = T,
						# preserve_intra_word_contractions = TRUE,
						# preserve_intra_word_dashes = TRUE,
						stopwords = F,
						stemming = FALSE, 
						tolower=FALSE,
						wordLengths = c(2, Inf))


# all_data.csv: Make and save lemma or types f tables ----

# make a list to store the frequency table for each condition
ftables <- list()

for(cond in conditions) {
  
  # cond = "dsl2"
  print(cond)
  
  # when cond is chlt, add "/exp2" to path
  path <- case_when(
    cond == "chlt" ~ {path = paste0("exp2/", cond)},
    cond == "chht" ~ {path = paste0("exp2/", cond)},
    cond == "adlt" ~ {path = paste0("exp2/", cond)},
    cond == "adht" ~ {path = paste0("exp2/", cond)},
    cond == "lllo" ~ {path = paste0("exp3/", cond)},
    cond == "llsh" ~ {path = paste0("exp3/", cond)},
    cond == "dslo" ~ {path = paste0("exp3/", cond)},
    cond == "dssh" ~ {path = paste0("exp3/", cond)},
    cond == "lll2" ~ {path = paste0("exp3/", "lllo")},
    cond == "lls2" ~ {path = paste0("exp3/", "llsh")},
    cond == "dsl2" ~ {path = paste0("exp3/", "dslo")},
    cond == "dss2" ~ {path = paste0("exp3/", "dssh")})
  # path
  
  if (wordtype == "types") {
  	# read texts and save as rdata corpus
    # df_l <- readtext::readtext("data-original/", exp, "/gpt3_5_texts/gen_texts*/*.txt")
    # df_l <- readtext::readtext("data-original/", exp, "/gpt_4_texts/gen_texts*/*.txt"); cond = "gpt4"
  	df_l <- readtext::readtext(paste0("data-original/", path, "/*.txt"))
  	print("fininshed reading")
  	save(df_l, file = paste0("data-processed/corpus-", cond, ".RData")) # used for udpipe
  	freqmat_l <- getFreqMat(df_l, opts) # types
  	print("fininshed processing and counting")
  } else {
    # load("data-processed/tidycorpus-text-3.5-ch.RData", verbose = T) # texts on lines (udpipe)
  	load(paste0("data-processed/tidycorpus-text-", cond, ".RData"), verbose = T) # texts on lines (udpipe)
    freqmat_l <- getFreqMat(text_df2, opts) # lemma
  }
  
  # apply normalisation like in childLex
  freqmat_l$ai_norm <- 1000000 * freqmat_l$ai_freq / (sum(freqmat_l$ai_freq))
  freqmat_l$ai_normplus1 <- 1000000 * (freqmat_l$ai_freq + 1) / (sum(freqmat_l$ai_freq))
  freqmat_l$ai_norm_log <- log(freqmat_l$ai_normplus1)
  save(freqmat_l, file = paste0("data-processed/freqmat_l-", wordtype, "-", cond, ".RData")) # used for table
  write.csv(freqmat_l, file = paste0("data-processed/freqmat_l-", wordtype, "-", cond, ".csv")) # used for table
  ftables[[cond]] <- freqmat_l 
}

# write a csv for each condition with frequency table
# for(cond in conditions) {
#   # round
#   ftables[[cond]] %>% mutate(ai_norm = round(ai_norm, digits = 2),
#                              ai_norm_log = round(ai_norm_log, digits = 4)) %>%
# 	rename(word = type, 
# 	       count = ai_freq,
# 	       frequency_per_million = ai_norm,
# 	       log_frequency_per_million = ai_norm_log) %>% 
#     
# 	write.csv(paste0("data-processed/", cond,".csv"), row.names = F)
# }

# make a df
ftables_with_source <- Map(function(tbl, cond) {
  tbl$Source <- cond  
  return(tbl)
}, ftables, conditions)


# write a csv with all frequency tables
all_data <- do.call(rbind, ftables_with_source)
all_data %>% mutate(ai_norm = round(ai_norm, digits = 2),
										ai_norm_log = round(ai_norm_log, digits = 4)) %>%
	rename(word = addcleaned,
										count = ai_freq,
										frequency_per_million = ai_norm,
										log_frequency_per_million = ai_norm_log) %>% 
	write.csv(paste0("data-processed/all_data", exp, ".csv"), row.names = F, fileEncoding = "UTF-8")

