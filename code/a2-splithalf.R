library(ggh4x)
library(tidyverse)
library(ggrepel)
library(tm)
library(readtext)

# data from getsplits.R
load("data-processed/splithalf.RData", verbose = T)
# reps_withi_df

summary_df <- reps_withi_df %>%
	group_by(Source) %>%
	summarize(corm = mean(cor),
						corsm = mean(cors),
						kl_divm = mean(kl_div),
						js_divm = mean(js_div),
						n2 = n(),
						sd = sd(cor),
						sds = sd(cors),
						sdkl = sd(kl_div),
						sdjs = sd(js_div))

summary_df

# Transform the data to a long format suitable for ggplot
tidy_reps_withi_df <- reps_withi_df %>%
	pivot_longer(cols = c(cor, cors, kl_div, js_div),
							 names_to = "measure",
							 values_to = "value")


tidy_reps_withi_df <- tidy_reps_withi_df %>%
	mutate(measure = recode(measure,
													cor = "Pearson",
													cors = "Spearman",
													kl_div = "KL divergence",
													js_div = "JS divergence"),
				 category = case_when(
				 	measure %in% c("Pearson", "Spearman") ~ "Correlation",
				 	measure %in% c("KL divergence", "JS divergence") ~ "Divergence"
				 ))



# Create a dummy variable for the x-axis
tidy_reps_withi_df <- tidy_reps_withi_df %>%
	mutate(dummy = 1,
				 Source = recode(Source,
				 								"adht" = "Adult high temp.",
				 								"adlt" = "Adult low temp.",
				 								"chht" = "Child high temp.",
				 								"chlt" = "Child low temp."),
				 reverse_y = measure %in% c("measure1", "measure2")) # specify which measures to reverse)

# Create the boxplot
ggplot(tidy_reps_withi_df %>% 
			 	filter(measure == "Pearson"), 
			 aes(x = factor(dummy), y = value, fill = Source)) +
	geom_boxplot() +
	labs(
		# title = "Boxplots of Different Measures by Source",
		x = "",
		y = "Correlation"
	) +
	theme_minimal() + 
	# facet_wrap(. ~ measure, scales = "free_y", drop = T, ncol = 4) +
	# facet_wrap(~ measure, scales = "free_y", strip.position = "bottom",  ncol = 4) +
	# ggh4x::facetted_pos_scales(y = list(
		# measure %in% c("KL divergence", "JS divergence") ~ scale_y_reverse()
	# )) +
	theme(
		axis.text.x = element_blank(),
		axis.ticks.x = element_blank(),
		strip.background = element_blank(),
		strip.placement = "outside"
	)
# ggsave("figures/boxplot-split.pdf", width = 10, height = 3, dpi = 300)
ggsave("figures/boxplot-split-pearson.pdf", width = 4, height = 3, dpi = 300)

