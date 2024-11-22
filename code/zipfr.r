#########Models for lexical richness###########
library(languageR)
library(zipfR)
library(tidyverse)
library(tidytext)

# not used
# load("data-processed/tidycorpus-3.5-ch.RData", verbose = T) # lemmas on lines, text_df
# load("data-processed/word_rem-types-3.5.RData", verbose = T) # types on lines, text_df_rem, stop words removed
# load("data-processed/word_types-3.5.RData", verbose = T) # types on lines all, text_df_all

#### test ###
# test2 <- text_df_all$word[100001:200000]
# test <- text_df_all$word[1:length(test2)]
# compare.richness.fnc(test, test2)
# fitting word frequency models to each text, and selecting for each text the model with the best goodness of fit. (Models with a better goodness of fit have a lower chi-squared value). Given the estimates of the required variances, Z scores are obtained that evaluate the difference between the number of types in the first and the second text.


pdf("figures/vgc_plots_grid.pdf", width = 7, height = 7)
default_par <- par()
par(mfrow = c(2, 2))
conditions = c("adlt", "adht", "chlt", "chht")
for(cond in conditions) {
	# print(cond)
	# cond <- "chlt"
	# pdf(paste0("data-processed/zipfR-plot", cond, ".pdf"))
	
	# from tm_ben.R
	load(paste0(file = "data-processed/corpus-", cond, ".RData"), verbose = T) 
	text_types <- tibble(df_l)
	
	text_df_all <- text_types %>% # raw texts
		unnest_tokens(word, text)
	# text_df_all <- text_df_all[1:10000,]
	
	text_df_all_cnt <- text_df_all %>% count(word, sort = TRUE)
	
	# load(paste0("data-processed/word_types-", cond, ".RData"), verbose = T) # types on lines all, text_df_all
	# all <- text_df_all$word

	### takes some time
	# df.growth = growth.fnc(text = all, size = length(all)/40, nchunks = 40) 
	# Herdan’s C [Herdan, 1960] looks interesting
	# save(df.growth, file = "data-processed/df.growth-3.5-ch.RData")
	# save(df.growth, file = paste0("data-processed/df.growth-", cond, ".RData"))
	load(file = paste0("data-processed/df.growth-", cond, ".RData"), verbose = T)
	df.vgc = growth2vgc.fnc(df.growth)
	
	df_tfl_t <- tfl(text_df_all_cnt$n, type=text_df_all_cnt$word)
	df_spc_t <- tfl2spc(df_tfl_t)
	fzm <- lnre("fzm", df_spc_t) #  finite Zipf-Mandelbrot (fZM)
	model = fzm # zm # gigp
	# maxext <- N(model)*2
	maxext <- 50000000
	ext.model = lnre.vgc(model, seq(N(model), maxext, length = 20), m.max = 3, variances=TRUE)
	int.model = lnre.vgc(model, seq(0,        N(model),   length = 20), m.max = 3, variances=TRUE)
	
	
	# par(default_par)
	# plot(df.vgc, add.m = 1:3, main = " ")
	options(scipen = .5)
	# options(scipen = 10)
	label = case_when(
		cond == "adlt" ~ "Adults low temperature",
		cond == "adht" ~ "Adults high temperature",
		cond == "chlt" ~ "Children low temperature",
		cond == "chht" ~ "Children high temperature"
	)
	# label
	plot.vgc(int.model, ext.model, df.vgc, add.m = 1:3, 
					 main = label,
					 N0 = N(model), 
					 conf.style = "ticks", 
					 # legend = c("Interp.", "Extrap.", "Obs."), 
					 ylab = "Number of types (V)",
					 xlab = "Sample size",
					 ylim = c(0, 130000),  # Set the y-axis limits from 0 to 12000
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
par(mfrow = c(1, 1))
dev.off()


####### plot growth #######

# load("data-processed/df.growth-3.5-ch.RData", verbose = T)
# languageR::plot.growth(df.growth)
plot(df.growth)
# dev.copy2pdf(out.type = "pdf", file = "figures/df.growth.all-3.5-ch.pdf")

df.g = df.growth@data$data
head(df.g)
plot(log(df.g$Tokens), log(df.g$Types))
df.g.lm = lm(log(df.g$Types) ~ log(df.g$Tokens))
abline(df.g.lm, col="darkgrey")

plot(log(df.g$Tokens), resid(df.g.lm))
abline(h=0)


### models #### 

# load("data-processed/word_types_cnt.RData", verbose = T)
df_tfl_t <- tfl(text_df_all_cnt$n, type=text_df_all_cnt$word)
df_spc_t <- tfl2spc(df_tfl_t)

# Baayen’s P = estimate for slope of VGC
100*Vm(df_spc_t, 1) / N(df_spc_t)
# 100*Vm(df_spc_l, 1) / N(df_spc_l) # lemmas
plot(df_spc_t, log="x")
plot(df_spc_t)
# with(df_spc_t, plot(m, Vm, main="Frequency Spectrum"))
# dev.copy2pdf(out.type = "pdf")

zm <- lnre("zm",df_spc_t) # The Zipf-Mandelbrot (ZM) LNRE model (see lnre.zm for details).
zm
fzm <- lnre("fzm", df_spc_t) #  finite Zipf-Mandelbrot (fZM)
fzm
gigp = lnre("gigp", df_spc_t) # Generalized Inverse Gauss-Poisson
gigp
plot(zm)
plot(fzm)
plot(gigp)
plot(df_spc_t, lnre.spc(gigp, attr(df_spc_t, "N")))
# for 1: observed larger than expected
# for 2: observed smaller than expected
fzm.spc <-	lnre.spc(fzm, N(fzm))
plot(df_spc_t, fzm.spc, legend=c("observed", "fzm"))
plot(df_spc_t, fzm.spc, legend=c("observed", "fzm"), m.max=10)

# length(text_df_all$word)


###### zipf.fnc #########

z = zipf.fnc(all, plot = T)
plot(log(z$rank), log(z$frequency), type = "S")
tail(z)
z.lm = lm(log(z$frequency) ~ log(z$rank))
abline(z.lm, col = "darkgrey")
plot(log(z$rank), resid(z.lm))
abline(h=0)


##### lnre.vgc from zipfR ####### 

zm.vgc <- lnre.vgc(zm,(1:100)*70, variances=TRUE)
summary(zm.vgc)
plot(zm.vgc)
# plot(zm.vgc,add.m=1)


### gigp #####

# extrapol
seq(N(gigp), N(gigp)*2, length = 20)
seq(0,       N(gigp),   length = 20)
# V       V1       V2       V3



# dev.copy2pdf(out.type = "pdf", file = "figures/df.growth.intrextr-3.5.pdf", width = 5, height = 4)
# par()