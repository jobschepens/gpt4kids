#########Models for lexical richness###########
library(languageR)
library(zipfR)
library(tidyverse)
library(tidytext)

#### test ###
# test2 <- text_df_all$word[100001:200000]
# test <- text_df_all$word[1:length(test2)]
# compare.richness.fnc(test, test2)
# fitting word frequency models to each text, and selecting for each text the model with the best goodness of fit. (Models with a better goodness of fit have a lower chi-squared value). Given the estimates of the required variances, Z scores are obtained that evaluate the difference between the number of types in the first and the second text.

exp <- "exp3"
ext <- 50; maxy <- 130000 # exp2, for ~ 20m corpora
ext <- 20; maxy <- 200000 # exp3, including dslo
# ext <- 5; maxy <- 80000 # for ~ 5m corpora
maxext <- ext * 1000000
plo = F
extrapolation_size <- 10e7

pdf(paste0("figures/vgc_plots_grid_2", exp, "_", ext, ".pdf"), width = 7, height = 7)
default_par <- par()
par(mfrow = c(2, 2))

# conditions = c("3.5")
conditions = c("chlex")
# conditions = c("adlt", "adht", "chlt", "chht")
# conditions = c("lllo", "dslo", "chlt", "chht")
# conditions = c("lllo", "llsh", "dslo", "dssh")
# conditions = c("lll2", "lls2", "dsl2", "dss2")

results_df <- data.frame(
	Corpus = character(),
	Original_Tokens = numeric(),
	Original_Types = numeric(),
	# Model_Fit_P_Value = numeric(),
	# Alpha = numeric(),
	# Population_Size_S = numeric(),
	Extrapolated_Types_at_10M = numeric(),
	stringsAsFactors = FALSE
)

# conditions = c("dslo")

for(cond in conditions) {
	# print(cond)
	# cond <- "3.5"
	# cond <- "dslo"
	# cond <- "dsl2"
	# pdf(paste0("data-processed/zipfR-plot", cond, ".pdf"))
	
	# corpus from 0-tm.R
	# load(paste0(file = "data-processed/corpus-", cond, ".RData"), verbose = T) 
	# text_types <- tibble(df_l)
	# text_df_all <- text_types %>% # raw texts
	# 	unnest_tokens(word, text) # defaults to word tokenizatiopn, using lowercase = true
	# # text_df_all <- text_df_all[1:10000,]
	# text_df_all_cnt <- text_df_all %>% count(word, sort = TRUE)
	# summary(text_df_all_cnt)
	
	# freqmat
	load(paste0("data-processed/freqmat_l-types-", cond, ".RData"), verbose = T) 
	head(freqmat_l)
	freqmat_l <- chlex
	freqmat_l$chcount <- round(freqmat_l$chcount, 0)
	text_df_all_cnt <- freqmat_l %>% 
		rename(word = type, n = chcount) %>% # childlex
		# rename(word = type, n = ai_freq) %>% # 1
		# select(-n) %>% rename(word = keep, n = ai_freq) %>% # 2, 3
		select(word, n) 
	summary(text_df_all_cnt)
	
	df_tfl_t <- tfl(text_df_all_cnt$n, type=text_df_all_cnt$word)
	df_spc_t <- tfl2spc(df_tfl_t)
	model <- lnre("fzm", df_spc_t) #  finite Zipf-Mandelbrot (fZM)
	
	extrapolated_types <- EV(model, extrapolation_size)
	
	results_df[nrow(results_df) + 1, ] <- list(
		cond,
		original_tokens,
		original_types,
		# fit_p_value,
		# alpha_param,
		# s_param,
		extrapolated_types
	)
	

	if(plo == T) {
		
		# load(paste0("data-processed/word_types-", cond, ".RData"), verbose = T) # types on lines all, text_df_all
		all <- text_df_all$word
		
		### takes some time
		# df.growth = growth.fnc(text = all, size = length(all)/40, nchunks = 40)
		# save(df.growth, file = paste0("data-processed/df.growth-", cond, ".RData"))
		load(file = paste0("data-processed/df.growth-", cond, ".RData"), verbose = T)
		df.vgc = growth2vgc.fnc(df.growth)
	 
		# maxext <- N(model)*2
	
		# ext.model = lnre.vgc(model, seq(N(model), maxext, length = 20), m.max = 3, variances=TRUE)
		ext.model = lnre.vgc(model, seq(min(N(model), maxext), maxext, length = 20), m.max = 3, variances=TRUE)
		int.model = lnre.vgc(model, seq(0,        N(model),   length = 20), m.max = 3, variances=TRUE)
		
		
		# par(default_par)
		# plot(df.vgc, add.m = 1:3, main = " ")
		options(scipen = .5)
		# options(scipen = 10)
		label = case_when(
			cond == "adlt" ~ "Adults low temperature",
			cond == "adht" ~ "Adults high temperature",
			cond == "chlt" ~ "Children low temperature",
			cond == "chht" ~ "Children high temperature",
			cond == "dssh" ~ "DeepSeek short",
			cond == "dslo" ~ "DeepSeek long",
			cond == "llsh" ~ "LLama short",
			cond == "lllo" ~ "LLama long",
			cond == "gpt4" ~ "GPT 4"
		)
		# label
		plot.vgc(int.model, ext.model, df.vgc, add.m = 1:3, 
						 main = label,
						 N0 = N(model), 
						 conf.style = "ticks", 
						 # legend = c("Interp.", "Extrap.", "Obs."), 
						 ylab = "Number of types (V)",
						 xlab = "Sample size",
						 ylim = c(0, maxy),  # Set the y-axis limits from 0 to 12000
						 xlim = c(0, maxext),  # Set the y-axis limits from 0 to 12000
						 # yaxt = 'n',  # Suppress the default y-axis
						 # xaxt = 'n',  # Suppress the default y-axis
						 lwd = c(3, 3, 3), 
						 col = c("#808080", "#D65F5F", "#6ACC65"), 
						 lty = c("solid", "solid", "longdash"))
		grid(lwd = 1.5, col = "grey")
		# axis(1, at = seq(0, maxext, by = 10000000), labels = FALSE)
		# axis(2, at = seq(0, 130000, by = 20000), labels = FALSE)
		
		# axis(1, at = c(10000000, 20000000, 30000000, 40000000, 50000000), 
		# 		 labels = c("10m", "20m", "30m", "40m", "50m"),
		# 		 las = 0)  # Ensure labels are parallel (change `las` as needed)
		# axis(2, at = c(20000, 40000, 60000, 80000, 100000, 120000), 
		# 		 labels = c("20000", "40000", "60000", "80000", "10000", "120000"),
		# 		 # las = 1)  # Ensure labels are horizontal (change `las` as needed)
		# 		 las = 0)  # Ensure labels are parallel (change `las` as needed)
		# 		 # las = 2)  # Ensure labels are perpendicular  (change `las` as needed)
		# 		 # las = 3)  # Ensure labels are vertical  (change `las` as needed)
		# dev.copy2pdf(out.type = "pdf", file = paste0("figures/df.growth.intrextr", cond, ".pdf"), width = 5, height = 4)
		# dev.off()
	}
}
par(mfrow = c(1, 1))
dev.off()


print(results_df, digits = 4)

# aap = rbind(aap, results_df)
# write.csv(aap, "data-processed/extrap.csv")



ggplot(aap, aes(x = reorder(Corpus, Extrapolated_Types_at_10M), 
								y = Extrapolated_Types_at_10M, 
								fill = Corpus)) +
	# Create the bar chart
	geom_col() +
	
	labs(
		x = "Corpus",
		y = "Predicted Types at 10M Tokens"
	) +
	
	# Use a clean theme and improve text readability
	theme_minimal() +
	theme(
		legend.position = "none", # Hide legend, as x-axis is clear
		axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels
	)
ggsave("figures/zipfR_extrapolation.pdf", width = 8, height = 5)
# dev.off()
