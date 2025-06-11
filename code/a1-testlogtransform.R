library(ggplot2)

# Simulate a dataset of word frequencies
set.seed(42)
words <- paste0("word", 1:1000)
raw_freq <- sample(1:10000, 10000, replace = TRUE)
freq_per_million <- raw_freq / sum(raw_freq) * 1e6
# 4m words

# Calculate log transformations
log_raw_freq <- log(raw_freq + 1)
log_freq_per_million <- log(freq_per_million + 1)

# Create a data frame for plotting
df <- data.frame(
	words = words,
	raw_freq = raw_freq,
	freq_per_million = freq_per_million,
	log_raw_freq = log_raw_freq,
	log_freq_per_million = log_freq_per_million
)

# Plot the distributions
p1 <- ggplot(df, aes(x = raw_freq, y = log_raw_freq)) +
	geom_point() +
	labs(title = "Log(Raw Frequency + 1)",
			 x = "Raw Frequency",
			 y = "Log(Raw Frequency + 1)") +
	theme_minimal()

p2 <- ggplot(df, aes(x = freq_per_million, y = log_freq_per_million)) +
	geom_point() +
	labs(title = "Log(Frequency per Million + 1)",
			 x = "Frequency per Million",
			 y = "Log(Frequency per Million + 1)") +
	theme_minimal()
p1
p2
# Arrange the plots side by side
library(patchwork)
# grid.arrange(p1, p2, ncol = 2)
p1+p2

p <- ggplot(df, aes(x = log_raw_freq, y = log_freq_per_million)) +
	geom_point() +
	labs(title = "Log(Frequency per Million + 1)",
	  	 x = "Log(Raw Frequency + 1)",
  			y = "Log(Frequency per Million + 1)") +
	# add a diagonal line
	geom_abline(intercept = 0, slope = 1, color = "red") +
	stat_smooth(method = "lm", se = FALSE, color = "blue") +
	theme_minimal() +
	
	# make the graph square
	coord_fixed(ratio = 1, xlim = c(0, 10), ylim = c(0, 10))
p

