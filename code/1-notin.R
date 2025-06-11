Sys.setlocale("LC_CTYPE", "German_Germany.UTF-8")
library(xtable)
library(tidyverse)
# conditions = c("adlt", "adht", "chlt", "chht")
# conditions = c("ai_freq", "adlt", "adht", "chlt", "chht"); exp = "exp2"
conditions = c("ai_freq", "dsl2", "dss2", "lls2", "lll2"); exp = "exp3"
# conditions = c("ai_freq"); exp = "exp1"
conditions = c("dsl2"); exp = "exp3" # test

# experiment 1 merged data from tm.R
# load(file = "data-processed/freqmat_l-types-3.5-PUNC.RData", verbose = T)
# freqmat_l$norm_logcountplus1_llmplus1 <- 1000000 * (freqmat_l$ai_freq + 1) / (sum(freqmat_l$ai_freq))
# freqmat_l$norm_logcountplus1 = log(freqmat_l$norm_logcountplus1_llmplus1)
# colnames(freqmat_l)
# df <- freqmat_l

# experiment 2 merged data from wider-again.R
# load("data-processed/all_wider_again_exp2.RData", verbose = T)
# load("data-processed/all_wider_again_exp23.RData", verbose = T)
# load("data-processed/all_wider_again_exp3_all.RData", verbose = T) # used for plotting
load("data-processed/all_wider_again_exp3-2_all.RData", verbose = T) # used for plotting
# head(all_wider_againl)
colnames(all_wider_againl)

all_wider_againl <- all_wider_againl %>% rename(type = merged_type)

# target <- "Skater"
# all_wider_againl$distance <- stringdist::stringdist(all_wider_againl$type, target, method = "lv")
# print(similar_words <- all_wider_againl[all_wider_againl$distance <= 2, ], n = 50)

# t(all_wider_againl[all_wider_againl$type == "Schulvampire",])

df <- all_wider_againl
colnames(df)

# t(df[df$type == "Hoffentlich",])
# t(df[df$type == "In",])

for(cond in conditions) {
  # cond = "adht"
	print(cond)
	df$norm_logcountplus1_llm <- df[[paste0("norm_logcountplus1_", cond)]]
	df$value_llm <- df[[paste0("value_", cond)]]
	# colnames(df)
	### low in #### 
	
	df <- df %>%
		mutate(word_length = nchar(type)) %>%
		arrange(desc(word_length))

	pattern <- paste0("norm_logcountplus1_", paste(conditions, collapse = "|"), "")
	pattern2 <- paste0("value_", paste(conditions, collapse = "|"), "")
	pattern3 <- paste0("normplus1_", paste(conditions, collapse = "|"), "")
	
	# df$norm_logcountplus1_llm
	# top F childlex words that occur the least often in ADHT 
	c <- df %>% 
		filter(value_llm != 0 & value_chcount != 0) %>%
	  arrange(desc(norm_logcountplus1_chcount)) %>%
		arrange(norm_logcountplus1_llm) %>%
		slice_head(n = 10) %>%
		# select(type, norm_logcountplus1_llm) %>% 
	  # select(type, norm_logcountplus1_chcount, matches("norm_logcountplus1_(" %+% paste(conditions, collapse = "|") %+% ")"))
	  mutate("F" = round(norm_logcountplus1_chcount, 1)) %>%
	  select(type, "F") %>%
	  # select(type, F, norm_logcountplus1_adht) %>%
	         # matches(pattern)) %>%
	         # value_chcount, matches(pattern2), 
	         # normplus1_chcount, matches(pattern3)) %>% 
		rename("childLex" = type)
	c
	d <- df %>% 
	  filter(value_llm != 0 & value_chcount != 0) %>%
		arrange(desc(norm_logcountplus1_chcount)) %>% 
		arrange(norm_logcountplus1_llm) %>% 
		filter(word_length > 10) %>% 
		slice_head(n = 10) %>% 
	  mutate("F" = round(norm_logcountplus1_chcount, 1)) %>%
	  select(type, "F") %>%
	  rename("childLex >10" = type)
	d
	a <- df %>% 
	  filter(value_llm != 0 & value_chcount != 0) %>%
	  arrange(desc(norm_logcountplus1_llm)) %>% 
		arrange(norm_logcountplus1_chcount) %>% 
		slice_head(n = 10) %>% 
	  mutate("F" = round(norm_logcountplus1_llm, 1)) %>%
	  select(type, "F") %>%
		rename(!!cond := type)
	a
	new_colname  <- sym(paste0(cond, " >10"))
	b <- df %>% 
	  filter(value_llm != 0 & value_chcount != 0) %>%
	  arrange(desc(norm_logcountplus1_llm)) %>% 
		arrange(norm_logcountplus1_chcount) %>% 
		filter(word_length > 10) %>% 
		slice_head(n = 10) %>%
	  mutate("F" = round(norm_logcountplus1_llm, 1)) %>%
	  select(type, "F") %>%
	  rename(!!new_colname  := type)
	b
	# sample(b$`dsl2 >10`, size = 20)
	
	
	lowin <- cbind(c, d, a, b)
	lowin
	
	
	#### not in #### 
	
	# df$norm_logcountplus1_llm[is.na(df$norm_logcountplus1_llm)] = .1
	
	# df$norm_logcountplus1_chcount[is.na(df$norm_logcountplus1_chcount)] = .1
	# df <- df %>% slice_sample(n = 300)
	
	# col 1, not in LLM
	c <- df %>% 
		arrange(desc(norm_logcountplus1_chcount)) %>% 
		arrange(value_llm) %>% 
		slice_head(n = 10) %>% 
	  mutate("F" = round(norm_logcountplus1_chcount, 1)) %>%
	  # select(type, "F", norm_logcountplus1_llm, value_llm) %>%
	  select(type, "F") %>%
	  rename("childLex" = type)
	c
	# col 2, childLex length  > 10
	d <- df %>% 
		arrange(desc(norm_logcountplus1_chcount)) %>% 
		arrange(value_llm) %>% 
		filter(word_length > 10) %>% 
		slice_head(n = 10) %>% 
	  mutate("F" = round(norm_logcountplus1_chcount, 1)) %>%
	  select(type, "F") %>%
	  rename("childLex >10" = type)
	d
	# col 3, not in childLex
	a <- df %>% 
		arrange(desc(norm_logcountplus1_llm)) %>% 
		arrange(norm_logcountplus1_chcount) %>% 
		slice_head(n = 10) %>% 
	  mutate("F" = round(norm_logcountplus1_llm, 1)) %>%
	  select(type, "F") %>%
		rename(!!cond := type)
	a
	
	# col 4, LLM length > 10
	new_colname  <- sym(paste0(cond, " >10"))
	
	b <- df %>% 
		arrange(desc(norm_logcountplus1_llm)) %>% 
		arrange(norm_logcountplus1_chcount) %>% 
		filter(word_length > 10) %>% 
		slice_head(n = 10) %>%
	  mutate("F" = round(norm_logcountplus1_llm, 1)) %>%
	  select(type, "F") %>%
	  rename(!!new_colname  := type)
	
	  # summarise(Words = str_flatten(type, collapse = ", ")) %>%
		# mutate("Only in" = "GPT3.5",
					 # "Word length" = ">10", .before = 1)
	b
	# sample(b$`dsl2 >10`, size = 20)
	notin <- cbind(c, d, a, b)
	notin
	
	# xtable(notin)
	# xtable(lowin)
	
	con <- file(description = paste0("data-processed/notin", ".tex"), encoding = "UTF-8", open = "a")
	
	cat(paste("\n\nCondition:", cond, "-", exp, "\n"),
				 file = con, append = T)
	
	print(xtable(notin, digits = rep(1, ncol(notin) + 1)),
				include.rownames = F,
				type = "latex",
				file = paste0("data-processed/notin", ".tex"), append = T)
	
	close(con)
	
	con <- file(description = paste0("data-processed/lowin", ".tex"), encoding = "UTF-8", open = "a")
	
	cat(paste("\n\nCondition:", cond, "-", exp, "\n"),
				file = con, append = T)
	
	print(xtable(lowin, digits = rep(1, ncol(notin) + 1)),
				include.rownames = F,
				type = "latex",
				file = paste0("data-processed/lowin", ".tex"), append = T)
	
	close(con)
}
