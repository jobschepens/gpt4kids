#########Models for lexical richness###########
library(languageR)
# load("data-processed/tidycorpus.RData", verbose = T) # lemmas on lines
load("data-processed/tidycorpus-3.5.RData", verbose = T) # lemmas on lines
load("data-processed/tidycorpus-3.5-ch.RData", verbose = T) # lemmas on lines

# load("data-processed/word_rem-types.RData", verbose = T) # types on lines, text_df_rem
load("data-processed/word_rem-types-3.5.RData", verbose = T) # types on lines, text_df_rem

# load("data-processed/word_types.RData", verbose = T) # types on lines all, text_df_all
load("data-processed/word_types-3.5.RData", verbose = T) # types on lines all, text_df_all

test <- text_df_rem$word[1:25000]
test <- text_df_all$word[1:50000]
test2 <- text_df_all$word[100001:200000]
test <- text_df_all$word[1:length(test2)]
compare.richness.fnc(test, test2)
# significant
# fitting word frequency models to each text, and selecting for each text the model with the best goodness of fit. (Models with a better goodness of fit have a lower chi-squared value). Given the estimates of the required variances, Z scores are obtained that evaluate the difference between the number of types in the first and the second text.
all <- text_df_all$word
text_df_all_cnt <- text_df_all %>% count(word, sort = TRUE)

# load("data-processed/word_types_cnt.RData", verbose = T)
df_tfl_t <- tfl(text_df_all_cnt$n, type=text_df_all_cnt$word)
df_spc_t <- tfl2spc(df_tfl_t)

# Baayen’s P = estimate for slope of VGC
100*Vm(df_spc_t, 1) / N(df_spc_t)
100*Vm(df_spc_l, 1) / N(df_spc_l)
plot(df_spc_t, log="x")
plot(df_spc_t)
# with(df_spc_t, plot(m, Vm, main="Frequency Spectrum"))
# dev.copy2pdf(out.type = "pdf")

zm <- lnre("zm",df_spc_t) # The Zipf-Mandelbrot (ZM) LNRE model (see lnre.zm for details).
zm
fzm <- lnre("fzm", df_spc_t)
fzm
gigp = lnre("gigp", df_spc_t)
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
# for 1: observed larger than expected
# for 2: observed smaller than expected

# length(text_df_rem$word)
# length(text_df_all$word)

### takes some time
df.growth = growth.fnc(text = all, size = length(all)/40, nchunks = 40) 
# Herdan’s C [Herdan, 1960] looks interesting
# save(df.growth, file = "data-processed/df.growth.RData")
# save(df.growth, file = "data-processed/df.growth-3.5.RData")
# save(df.growth, file = "data-processed/df.growth-3.5-ch.RData")
# load("data-processed/df.growth.RData", verbose = T)
# load("data-processed/df.growth-3.5.RData", verbose = T)
load("data-processed/df.growth-3.5-ch.RData", verbose = T)

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

z = zipf.fnc(all, plot = T)
plot(log(z$rank), log(z$frequency), type = "S")
tail(z)
z.lm = lm(log(z$frequency) ~ log(z$rank))
abline(z.lm, col = "darkgrey")
plot(log(z$rank), resid(z.lm))
abline(h=0)

zm.vgc <- lnre.vgc(zm,(1:100)*70, variances=TRUE)
summary(zm.vgc)
plot(zm.vgc)
# plot(zm.vgc,add.m=1)

# extrapol
seq(N(gigp), N(gigp)*2, length = 20)
seq(0,       N(gigp),   length = 20)
# V       V1       V2       V3
model = fzm # zm # gigp
ext.model = lnre.vgc(model, seq(N(model), N(model)*2, length = 20), m.max = 3, variances=TRUE)
int.model = lnre.vgc(model, seq(0,        N(model),   length = 20), m.max = 3, variances=TRUE)
df.vgc = growth2vgc.fnc(df.growth)
plot(df.vgc, add.m = 1:3, main = " ")

options(scipen = .5)
plot.vgc(int.model, ext.model, df.vgc, add.m = 1:3, 
								main = " ",
								N0 = N(model), 
								conf.style = "ticks", 
								# legend = c("Interp.", "Extrap.", "Obs."), 
								ylab = "Number of types (V)",
								xlab = "Sample size",
								lwd = c(3, 3, 3), 
								col = c("#808080", "#D65F5F", "#6ACC65"), 
								lty = c("solid", "solid", "longdash"))
# dev.copy2pdf(out.type = "pdf", file = "figures/df.growth.intrextr-3.5.pdf", width = 5, height = 4)
# par()