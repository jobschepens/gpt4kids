library(stringdist)
library(ggplot2)
library(reshape2)
library(textstem)
library(ggrepel)
library(tm)
library(readtext)
library(SnowballC)
library(ggplot2)
library(tidyverse)
library(scales)
library(lexicon)
library(corrplot)
library(lme4)
library(viridis)

# create merged data ----

# used instaed of merged_df-type

# conditions = c("chlt", "chht", "dslo", "lllo"); exp = "exp2" # for output file
conditions = c("chlt", "chht", "adlt", "adht"); exp = "exp2" # for output file
# conditions = c("lllo", "llsh", "dslo", "dssh"); exp = "exp3" # for output file
# conditions = c("lll2", "lls2", "dsl2", "dss2"); exp = "exp3-2" # for output file


## load exp 2 and 3 data from tm.R ----

all_data <- read_csv(file = "data-processed/all_dataexp2.csv")
# all_data <- read_csv(file = "data-processed/all_dataexp3.csv")
# all_data <- read_csv(file = "data-processed/all_dataexp3-2.csv")

all_wide <- all_data %>% 
  pivot_wider(
    id_cols = word, 
    names_from = Source, 
    values_from = c(count),
    values_fn = sum) # sum duplicates
# summary(all_wide)

unique(all_data$Source)

# t(all_wide[all_wide$word == "Zeitraum",])
# t(all_wide[all_wide$word == "zanken",])
# t(all_wide[all_wide$word == "verbuddeln",])
# t(all_wide[all_wide$word == "Unterhemd",])
# t(all_wide[all_wide$word == "Apfelsine",])
# t(all_wide[all_wide$word == "Automatik",])

## join childlex ----

library(readxl)
getChildLex <- function(sheet = 1){
  childLex <- read_xlsx(xlsx, sheet = sheet, range = cell_cols("A:I"))
  childLexAgg <- childLex %>% 
    summarise(.by = type,
              # summarise(.by = lemma,
              # childlex_norm = lemma.norm[1],					
              # childlex_norm = type.norm[1],
              childlex_norm = sum(atype.norm),
              n_pos = n())
  MIN_FREQUENCY = 0
  childLexAgg <- childLexAgg %>% filter(childlex_norm > MIN_FREQUENCY)
  # childLexAgg <- rename(childLexAgg, type = lemma)
}

getChildLexLemma <- function(sheet = 1){
  childLex <- read_xlsx(xlsx, sheet = sheet, range = cell_cols("A:I"))
  childLexAgg <- childLex %>% 
    # summarise(.by = type,
    summarise(.by = lemma,
              childlex_norm = lemma.norm[1],
              # childlex_norm = type.norm[1],
              # childlex_norm = sum(atype.norm),
              n_pos = n())
  childLexAgg <- childLexAgg %>% filter(childlex_norm > MIN_FREQUENCY)
  # childLexAgg <- rename(childLexAgg, type = lemma)
}

MIN_FREQUENCY = 0
xlsx = "data-original/childLex/childLex_0.17.01c_2018-12-24_schr.xlsx"
# read_xlsx(xlsx, sheet = 1, range = cell_cols("A:I"))
# https://drive.google.com/open?id=1OKdYErf1q3RzNXx_WyOnA4jq3uZqu6l_
wordtype = "type"

if (wordtype == "type") { # 164k in excel file and 182k in ms table?
  # childLexAgg <- getChildLex(excel_sheets(xlsx)[1]) # 158,398
  # childLexAgg6 <- getChildLex(excel_sheets(xlsx)[2])
  # childLexAgg9 <- getChildLex(excel_sheets(xlsx)[3])
  # childLexAgg11 <- getChildLex(excel_sheets(xlsx)[4])
} else {
  # childLexAgg <- getChildLexLemma(excel_sheets(xlsx)[1]) # 96204
  # childLexAgg6 <- getChildLexLemma(excel_sheets(xlsx)[2])
  # childLexAgg9 <- getChildLexLemma(excel_sheets(xlsx)[3])
  # childLexAgg11 <- getChildLexLemma(excel_sheets(xlsx)[4])
}
# save(childLexAgg, file = "data-processed/childLexAgg.RData")

# childLexAgg[childLexAgg$type == "hatte",]
# childLexAgg[childLexAgg$type == "Offenbar",]
# childLexAgg[childLexAgg$type == "offenbar",]


# target <- "Skater"
# childLexAgg$distance <- stringdist::stringdist(childLexAgg$type, target, method = "lv")
# print(similar_words <- childLexAgg[childLexAgg$distance <= 2, ], n = 50)
# childLexAgg[childLexAgg$type == "Schulvampire",]

load(file = "data-processed/childLexAgg.RData", verbose = T)
childLexAgg$chcount1 <- ((childLexAgg$childlex_norm * 9850786) / 1000000) - 1
childLexAgg$chcount2 <- ((childLexAgg$childlex_norm * 9850786) / 1000000)
# number taken from paper
childLexAgg$chcount3 <- ((childLexAgg$childlex_norm * 7352941) / 1000000) - 1
childLexAgg$chcount4 <- ((childLexAgg$childlex_norm * 7352941) / 1000000) 
# number taken from original childlex corpus

childLexAgg$chcount <- childLexAgg$chcount2 # most plausible

# summary(childLexAgg$chcount3) # assuming they did  count + 1
# summary(childLexAgg$chcount4) # assuming they did not do count + 1
# min(childLexAgg$childlex_norm * 7.352941)

chlex <- childLexAgg %>% select(type, chcount)
save(chlex, file = "data-processed/freqmat_l-types-chlex.RData") 

all_wide_ch <- full_join(chlex, all_wide, join_by(type == word)) # used for plotting

# head(litkey2)
# litkey[litkey$target == "beginnen",]
# freqmat_l[freqmat_l$type == "beginnen",]
# merged_lit[merged_lit$target == "beginnen",]
# litkeya[litkeya$target == "beschlossen", ]
# load("data-processed/litkey.RData", verbose = T)
# litkey <- litkey %>% distinct()
# 


## join old ----

# exp 1 data from tm.R
load(file = "data-processed/freqmat_l-types-3.5-PUNC.RData", verbose = T)
old <- freqmat_l
# load("data-processed/merged_df-type-3.5-full.RData", verbose = T)
# old <- merged_dfx
old <- old %>% rename(merged_type = type)
old <- old %>% select(merged_type, ai_freq)
summary(old)
# old[old$merged_type == "In",]
# old[old$merged_type == "in",]

all_wide_ch_old <- full_join(old, all_wide_ch, join_by(merged_type == type)) # used for plotting


## join subtlex ----

# cleaned up version
# if cap and non-cap words exist, only the most frequent has an entry
# converted to utf8 with notepad++
du <- read_delim(file = "data-original/SUBTLEX-DE_cleaned_with_Google00.txt", 
                 delim = "\t", 
                 locale = locale(decimal_mark = ",")) 

# du2 <- read_xlsx(path = "data-original/SUBTLEX-DE cleaned version with Zipf values.xlsx", 
# 								sheet = 1)

# du$Word[du$Word == "Brücke"]
# du$Word[du$Word == "brücke"]
# du$Word[du$Word == "Obstbaum"]
# du$Word[du$Word == "obstbaum"] 

sum_subtlex_norm <- sum(du$SUBTLEX, na.rm = T) # 984,689.1
sum_raw_subtlex <- sum(du$CUMfreqcount, na.rm = T) # 25,006,561 # case independent WF

# lgSUBTLEX: log10(CUMfreqcount+1)
# SUBTLEX: CUMfreqcount/scaling_factor

scaling_factor <- 25.399 # official scaling factor

# du$subtlex_norm <- du$CUMfreqcount / subtlex_value

# dus <- du %>% rename(merged_type = Word) %>% select(Google00pm, SUBTLEX, merged_type)
du <- du %>% select(CUMfreqcount, Word) %>% 
  rename(subtlex = CUMfreqcount)
# save(du, file = "data-processed/du.RData")
# mutate(norm_log_subtlex = log(1 + norm_subtlex)) %>%
# mutate(subtlex = (norm_subtlex * sum_subtlex_norm) / 1000000) %>% # wrong
# mutate(subtlex = (norm_subtlex * sum_raw_subtlex) / 1000000) %>%
# mutate(subtlex = (norm_subtlex * scaling_factor) ) %>%
# select(!norm_subtlex)

joined <- full_join(all_wide_ch_old, du, join_by(merged_type == Word)) # used for plotting


## DO NOT collapse all to lower ---- 

# joined <- joined %>%
# 	mutate(merged_type_lower = tolower(merged_type))
# summary(joined) # many NAs


## Replace all NA values in specific columns ----
fixed_cols <- c("ai_freq", "chcount", "subtlex")
cols_to_modify_ignore_missing <- c(fixed_cols, conditions)
joined4_ignore <- joined %>%
  mutate(across(any_of(cols_to_modify_ignore_missing), ~ replace_na(., 0)))


## longer and group_by word----

all_longer <- joined4_ignore %>%
  pivot_longer(
    cols = any_of(cols_to_modify_ignore_missing),  # Adjust the cols argument as needed
    names_to = "group")
# all_longer
# save(all_longer, file = "data-processed/all_longer.RData")

sums <- all_longer %>% group_by(group) %>% 
  summarize(sum = sum(value))
# sums

# all_longer <- all_longer %>% filter(value > 10)


# # Create a frequency table of words
# word_freq <- table(all_longer$merged_type_lower)
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
# 
# # Print the top 10 most frequent words
# print(head(word_freq_df, 10))
# 
# word_freq_summary <- word_freq_df %>%
# 	group_by(Word) %>%
# 	summarize(Total_Frequency = sum(Frequency)) %>%
# 	arrange(desc(Total_Frequency))

#
all_longer2 <- all_longer %>% 
  # group_by(merged_type_lower, group) %>% 
  group_by(merged_type, group) %>% 
  summarise(value_sum = sum(value)) %>%
  rename(value = value_sum)
# all_longer2


## LAPLACE TRANSFORMATION ----
# log(1 + x) - missing values are still zero

# all_longerl <- all_longer %>% group_by(group) %>% 
all_longerl <- all_longer2 %>% group_by(group) %>% 
  mutate(norm = 1000000 * value / (sum(value))) %>%
  mutate(normplus1 = 1000000 * (value + 1) / (sum(value))) %>%
  mutate(normplus1prob = (value + 1) / (sum(value) + 2)) %>%
  mutate(norm_log = log(1 + norm)) %>% 
  mutate(norm_logcountplus1 = log( normplus1)) %>% # final measure
  mutate(norm_logcountplus1prob = -log( normplus1prob)) %>%
  mutate(norm_lognonorm = log( 1 + value ))
summary(all_longerl)

sums2 <- all_longer2 %>% group_by(group) %>% 
  summarize(sum = sum(value))
# sums2

all_wider_againl <- all_longerl %>%
  pivot_wider(
    id_cols = merged_type,
    # id_cols = merged_type_lower,
    names_from = group,
    values_from = c(value, 
                    norm, 
                    normplus1,
                    norm_log, 
                    norm_logcountplus1prob,
                    norm_logcountplus1,
                    norm_lognonorm))

# summary(all_wider_againl)
# save(all_wider_againl, file = "data-processed/all_wider_again_subt_lower.RData")
# save(all_wider_againl, file = "data-processed/all_wider_again_subt_ch_plus1.RData")
# save(all_wider_againl, file = "data-processed/all_wider_again_subt_ch_plus1_childlex2.RData")
# save(all_wider_againl, file = "data-processed/all_wider_again_subt_ch_plus1_prob.RData") # 
save(all_wider_againl, file = "data-processed/all_wider_again_exp2.RData") # 
# save(all_wider_againl, file = "data-processed/all_wider_again_exp3.RData") # incomplete exp3 corpora
# save(all_wider_againl, file = "data-processed/all_wider_again_exp3_all.RData") # complete exp3 corpora
# save(all_wider_againl, file = "data-processed/all_wider_again_exp3-2_all.RData") # remove more diacritics
# updated subtlex, childlex error, and plus1 variables

