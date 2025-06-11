# load data from cormat.R
load("data-processed/devel-avg-merged-lower.RData", verbose = T)
# load("data-processed/devel-avg-merged.RData", verbose = T)
tibble(devel1)

long_merged_df <- devel1 %>%
	pivot_longer(
		cols = starts_with("rt."),
		names_to = "group",
		values_to = "rt",
		names_pattern = "rt\\.(g\\d+|ya|oa)\\.m"
	) %>%
	mutate(
		# Assign age based on group identifiers
		age = case_when(
			group == "g1" ~ 1,
			group == "g2" ~ 2,
			group == "g3" ~ 3,
			group == "g4" ~ 4,
			group == "g6" ~ 6,
			group == "ya" ~ 20,
			group == "oa" ~ 50,
			TRUE ~ NA_real_  # For 'avg', age is not applicable
		),
		# Recode group based on the pivoted names
		group = case_when(
			group == "oa" ~ "Adults",
			group == "ya" ~ "Adults",
			group %in% c("g1", "g2", "g3", "g4", "g6") ~ "Kids",
			TRUE ~ group
		)
	)
table(long_merged_df$group)
table(long_merged_df$age)

long_merged_df <- long_merged_df %>% 
	rename(log_frequency_per_million_lo_kids = norm_log_chlt,
				 log_frequency_per_million_hi_kids = norm_log_chht)

m0a = lm(rt~child.old20+aoa+letter.cnt+unigram+bigram+trigram+age,
				 data=subset(long_merged_df,long_merged_df$group=="Adults"))
mfltkk = lm(rt~child.old20+aoa+letter.cnt+unigram+bigram+trigram+age+log_frequency_per_million_lo_kids,
						data=subset(long_merged_df,long_merged_df$group=="Kids"))
mfhtkk = lm(rt~child.old20+aoa+letter.cnt+unigram+bigram+trigram+age+log_frequency_per_million_hi_kids,
						data=subset(long_merged_df,long_merged_df$group=="Kids"))

# AIC(m0k) - AIC(mfltkk)
# AIC(m0k) - AIC(mfhtkk)
AIC(m0a) - AIC(mfhtkk)
