library(tidyverse)
# conditions = c("adlt", "adht", "chlt", "chht")
# conditions = c("chlt", "chht", "dslo", "lllo")
# conditions = c("lllo", "llsh", "dslo", "dssh")
conditions = c("lll2", "lls2", "dsl2", "dss2")

# load downloaded devel RData file ----

# Example for a German UTF-8 locale, replace with your actual one if different
# Sys.setlocale("LC_CTYPE", "de_DE.UTF-8")
# load("data-original/devel/DeveL_Words.RData", verbose = T) # DeveL Dataset
# # Or for a US English UTF-8 locale
# Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
# item$word
# write.csv(item, "data-processed/devel-item.csv")

# result <- ld.rt %>% 
#   left_join(nam.on, by = "word") %>% 
#   left_join(ld.acc, by = "word") %>% 
#   left_join(nam.dur, by = "word") %>% 
#   left_join(nam.acc, by = "word") %>%
#   left_join(item, by = "word")
# colnames(result)

# write.csv(result, "data-processed/devel-result.csv")
result <- read_csv("data-processed/devel-result.csv")
# result$word

# result
# result <- result %>% mutate(rt.avg.m = rowMeans(across(c(rt.g1.m, rt.g2.m, rt.g3.m, rt.g4.m, rt.g6.m)), na.rm = T))
result <- result %>% mutate(rt.avg.m = rowMeans(across(c(rt.g2.m, rt.g3.m, rt.g4.m, rt.g6.m)), na.rm = T)) # grade 1 has missing values
# summary(result)

df <- result %>% 	
  select(contains("m") & contains("rt") | 
           word | child.type.freq | dwds.type.freq | child.old20 | aoa | 
           letter.cnt | unigram | bigram | trigram | sfreq) %>%
  select(where(~ !is.character(.)) | word)
# tibble(df)
# summary(df)
# colnames(df)
# save(df, file = "data-processed/devel-avg.RData")
# load(file = "data-processed/devel-avg.RData", verbose = T)

# df$child.type.freq2 = df$child.type.freq * 7.352941
# df$child.type.freq2[df$word == "Obstbaum"]
# df$child.type.freq2 = df$child.type.freq * 9.850786
# summary(df$child.type.freq2)

df <- df %>% 
  mutate(norm_logcountplus1_dwds = log(1 + dwds.type.freq)) %>% # already normalized
  mutate(norm_logcountplus1_chcount_devel = log(1 + child.type.freq)) %>% # not used
  # select(rt.avg.m, rt.g1.m, rt.g2.m, word, norm_log_dwds)
  select(rt.avg.m, rt.g1.m, rt.g2.m, 
         word, 
         norm_logcountplus1_dwds, 
         # norm_logcountplus1_chcount_devel,
         rt.g2.m, rt.g3.m, rt.g4.m, rt.g6.m,
         rt.ya.m, rt.oa.m, 
         # sfreq, # subjective frequency
         child.old20, aoa, letter.cnt, unigram, bigram, trigram)

# tibble(df)
# summary(df$dwds.type.freq)
# summary(df$norm_logcountplus1_dwds)
# summary(df$norm_log_dwds)


## --> check for double words (capitalization issues)
# df <- df %>%
# mutate(word_lower = tolower(word))
# 
# # Create a frequency table of words
# word_freq <- table(df$word_lower)
# 
# # Convert the table to a data frame for easier manipulation
# word_freq_df <- as.data.frame(word_freq, stringsAsFactors = FALSE)
# 
# # Rename the columns for clarity
# colnames(word_freq_df) <- c("Word", "Frequency")
# 
# # Sort the data frame by frequency in descending order
# word_freq_df <- word_freq_df %>%
# 	arrange(desc(Frequency))
# 	# arrange(Frequency)
# 
# # Print the top 10 most frequent words
# print(head(word_freq_df, 10))


# devel1: join with all_wider_againl from 0-wider_again.R ---- 

# load("data-processed/all_wider_again_subt.RData", verbose = T)
# load("data-processed/all_wider_again_subt_ch_plus1.RData", verbose = T)
# load("data-processed/all_wider_again_exp3.RData", verbose = T)
# load("data-processed/all_wider_again_exp3_all.RData", verbose = T)
load("data-processed/all_wider_again_exp3-2_all.RData", verbose = T)
# colnames(all_wider_againl)

# b <- all_wider_againl %>% 
#   filter(norm_chcount != 0)
# summary(b$norm_chcount)
# length(b$norm_chcount)
# aap <- b %>% filter(norm_chcount < 10)
# 100 * (nrow(aap)) / nrow(b)

all_wider_againls <- all_wider_againl %>% select(!contains("subtlex"))
# colnames(all_wider_againls)

# Remove rows where all columns starting with 'value_' are 0
# all_wider_againls2 <- all_wider_againls %>%
# filter(!rowSums(select(., starts_with("value_")) == 0) == length(select(., starts_with("value_"))))

devel1 <- left_join(df, all_wider_againls, join_by(word == merged_type))
# save(devel1, file = "data-processed/devel-avg-merged.RData")
# save(devel1, file = "data-processed/devel-avg-merged-nochildlexerr-plus1vars.RData")
# save(devel1, file = "data-processed/devel-avg-merged-lower.RData")

# colnames(devel1)
# summary(devel1$norm_logcountplus1_chcount)
# summary(df$norm_logcountplus1_dwds)
# summary(devel1$norm_logcountplus1_dwds)
# df$word
# all_wider_againls$merged_type


# devel13: add subtlex ---- 

du <- read_delim(file = "data-original/SUBTLEX-DE_cleaned_with_Google00.txt", 
                 delim = "\t", 
                 locale = locale(decimal_mark = ",")) 
du <- du %>% select(CUMfreqcount, Word) %>% 
  rename(subtlex = CUMfreqcount)
du2 <- du %>% mutate(wordlower = tolower(Word))

devel12 <- devel1 %>% mutate(wordlower = tolower(word))
devel13 <- left_join(devel12, du2, join_by(wordlower == wordlower))
# colnames(devel12)
# summary(devel13)
# summary(du2)

# all_longerl <- all_longer %>% group_by(group) %>% 
devel13 <- devel13 %>% 
  mutate(across(subtlex, ~replace_na(., 0))) %>%
  rename(value_subtlex = subtlex) %>%
  # mutate(norm_subtlex = 1000000 * value_subtlex / (sum(value_subtlex))) %>%
  mutate(normplus1_subtlex = 1000000 * (value_subtlex + 1) / (sum(value_subtlex))) %>%
  # mutate(normplus1prob_subtlex = (value_subtlex + 1) / (sum(value_subtlex) + 2)) %>%
  # mutate(norm_log_subtlex = log(1 + norm_subtlex)) %>% 
  mutate(norm_logcountplus1_subtlex = log( normplus1_subtlex)) 
# mutate(norm_logcountplus1prob_subtlex = -log( normplus1prob_subtlex)) %>%
# mutate(norm_lognonorm_subtlex = log( 1 + value_subtlex ))
# summary(devel13$norm_logcountplus1_dwds)

# aap <- devel13 %>% filter(norm_chcount > 5)
# aap <- devel13 %>% filter(norm_chcount < 10)
# aap <- devel13 %>% filter(norm_chcount < 5)
# nrow(aap)
# (nrow(devel13) - nrow(aap)) / nrow(devel13)
# 
# aap <- devel13 %>% filter(norm_chcount < 10)
# 100 * (nrow(aap)) / nrow(devel13)
# 
# aap <- devel13 %>% filter(value_chcount > 10)
# nrow(devel13) - nrow(aap)


colnames(devel13)
# save(devel13, file = "data-processed/devel-avg-merged-nochildlexerr-plus1vars-prob.RData")
# save(devel13, file = "data-processed/devel-avg-merged-nochildlexerr-plus1vars-prob-exp3.RData")
save(devel13, file = "data-processed/devel-avg-merged-nochildlexerr-plus1vars-prob-exp3-2.RData")


# make devel-LLM_Experiment_1.RData -----

# load("data-processed/devel-avg-merged-nochildlexerr-plus1vars-prob.RData", verbose = T)

# 1. Define fixed components

# Columns always selected and NOT renamed
fixed_select_only_cols <- c(
  "rt.g1.m", "rt.g2.m", "word", "rt.g3.m", "rt.g4.m", "rt.g6.m",
  "rt.ya.m", "rt.oa.m", "child.old20", "aoa", "letter.cnt",
  "unigram", "bigram"
)

# Base names of fixed columns that ARE always selected and renamed
fixed_rename_bases <- c(
  "ai_freq", "dwds", "chcount", "subtlex"
)

# Create a mapping of ALL potential base names to their NEW names
# (Include commented-out ones from your original code if they might appear in 'conditions' later)
rename_mapping <- c(
  # Exp3
  lll2 = "log_frequency_per_million_ll_l2",
  dsl2 = "log_frequency_per_million_ds_l2",
  lls2 = "log_frequency_per_million_ll_s2",
  dss2 = "log_frequency_per_million_ds_s2",
  # Exp3
  lllo = "log_frequency_per_million_ll_lo",
  dslo = "log_frequency_per_million_ds_lo",
  llsh = "log_frequency_per_million_ll_sh",
  dssh = "log_frequency_per_million_ds_sh",
  # Exp2
  chlt = "log_frequency_per_million_j_kids_lo_t_j",
  chht = "log_frequency_per_million_j_kids_hi_t_j",
  adlt = "log_frequency_per_million_j_adults_lo_t_j",
  adht = "log_frequency_per_million_j_adults_hi_t_j",
  # Exp1
  ai_freq = "log_frequency_per_million_b_LLM_exp1_j",
  # Existing
  dwds = "log_frequency_per_million_b_dwds_j",
  chcount = "log_frequency_per_million_b_childlex_j",
  subtlex = "log_frequency_per_million_b_subtlex_j"
)

# 2. Generate dynamic components based on 'conditions'

# Identify all base names that need selecting and renaming in this run
# (Combine the current conditions with the fixed ones)
all_rename_bases_this_run <- unique(c(conditions, fixed_rename_bases))

# Generate the full OLD column names for selection/renaming
cols_to_rename_old_names <- paste0("norm_logcountplus1_", all_rename_bases_this_run)

# Filter the main mapping to get only the relevant entries for this run
relevant_rename_mapping <- rename_mapping[names(rename_mapping) %in% all_rename_bases_this_run]

# Ensure the order of old names matches the order of new names from the filtered map
# It's safer to generate the final rename vector using the filtered map directly
# Create the named vector for rename: new_name = old_name
rename_vector <- setNames(
  paste0("norm_logcountplus1_", names(relevant_rename_mapping)), # old names (values)
  relevant_rename_mapping                                       # new names (names)
)

# 3. Combine and Execute

# Combine columns to select: fixed-select-only + old-names-to-be-renamed
cols_to_select <- c(cols_to_rename_old_names, fixed_select_only_cols)

# Apply select and rename
# Using all_of() ensures errors if columns are missing
colnames(devel13)
results_merged <- devel13 %>%
  select(all_of(cols_to_select)) %>%
  rename(!!!rename_vector)          # Use !!! to splice the named vector

# --- Verification ---
print(colnames(results_merged))
# print(head(results_merged)) # Optional: view first few rows



# save(results_merged, file = "data-processed/devel-LLM_Experiment_1.RData")
# save(results_merged, file = "data-processed/devel-LLM_Experiment_1_repr.RData") # also includes exp2
# save(results_merged, file = "data-processed/devel-LLM_Experiment_1_exp3.RData") # intermediate data 
# save(results_merged, file = "data-processed/devel-LLM_Experiment_1_exp3all.RData") # all exp3 data
save(results_merged, file = "data-processed/devel-LLM_Experiment_1_exp3all-2.RData") # all exp3 data

# load("data-processed/devel-LLM_Experiment_1.RData", verbose = T) 
# load("data-processed/devel-LLM_Experiment_2.RData", verbose = T) # only difference is no subtlex

# 2 and 3 together
load("data-processed/devel-LLM_Experiment_1_exp3all-2.RData", verbose = T)
exp3 = results_merged

load("data-processed/devel-LLM_Experiment_1_repr.RData", verbose = T) 

# add 
# sel <- exp3 %>% select(word, 
#                        log_frequency_per_million_ll_lo, 
#                        log_frequency_per_million_ds_lo, 
#                        log_frequency_per_million_ll_sh, 
#                        log_frequency_per_million_ds_sh)

sel <- exp3 %>% select(word, 
                       log_frequency_per_million_ll_l2, 
                       log_frequency_per_million_ds_l2, 
                       log_frequency_per_million_ll_s2, 
                       log_frequency_per_million_ds_s2)

sel
results_merged <- left_join(results_merged, sel, by = "word")
colnames(results_merged)
# save(results_merged, file = "data-processed/devel-LLM_Experiment_1_exp2and3all.RData") # all exp2 and exp3 data
save(results_merged, file = "data-processed/devel-LLM_Experiment_1_exp2and3all-2.RData") # all exp2 and exp3 data


# 2, 3, 3-2
load("data-processed/devel-LLM_Experiment_1_exp3all-2.RData", verbose = T)
exp32 = results_merged

load("data-processed/devel-LLM_Experiment_1_exp3all.RData", verbose = T)

sel <- exp32 %>% select(word, 
                       log_frequency_per_million_ll_l2, 
                       log_frequency_per_million_ds_l2, 
                       log_frequency_per_million_ll_s2, 
                       log_frequency_per_million_ds_s2)

sel
results_merged <- left_join(results_merged, sel, by = "word")
colnames(results_merged)
cor(results_merged$log_frequency_per_million_ll_l2, results_merged$log_frequency_per_million_ll_lo)
cor(results_merged$log_frequency_per_million_ds_l2, results_merged$log_frequency_per_million_ds_lo)
cor(results_merged$log_frequency_per_million_ll_s2, results_merged$log_frequency_per_million_ll_sh)
cor(results_merged$log_frequency_per_million_ds_s2, results_merged$log_frequency_per_million_ds_sh)

# colnames(results_merged)
# round all numeric values
results_merged2 <- results_merged %>% 
  # mutate(across(where(is.numeric), ~ round(., digits = 2)),
         mutate(diff = log_frequency_per_million_ll_lo - log_frequency_per_million_ll_l2,
                diff = log_frequency_per_million_ds_lo - log_frequency_per_million_ds_l2) %>%
  select(log_frequency_per_million_ds_lo, log_frequency_per_million_ds_l2, word, diff)
