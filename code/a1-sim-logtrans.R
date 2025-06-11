# Generate normalized values between 0 and 1
normalized_values <- seq(0.01, 100, length.out = 1000)  # starting from 0.01 to avoid log(0)

# Calculate log(1 + normalized_value) and log(normalized_value)
log1p_normalized_values <- log1p(normalized_values)
log_normalized_values <- log(normalized_values)

# Create scatter plot
plot(log_normalized_values, log1p_normalized_values,
		 xlab = "log(normalized_value)",
		 ylab = "log(1 + normalized_value)",
		 main = "Scatter Plot of log(normalized_value) vs log(1 + normalized_value)",
		 col = "black",
		 pch = 20)
grid()

# Visualize transformations
par(mfrow = c(2, 1))
par(mfrow = c(1, 1))
plot(normalized_values, log1p_normalized_values, type = 'l',
		 col = 'blue', ylab = 'log1p(normalized_value)', xlab = 'normalized_value',
		 main = 'Log1p Transformation')
grid()

plot(normalized_values, log_normalized_values, type = 'l',
		 col = 'red', ylab = 'log(normalized_value)', xlab = 'normalized_value',
		 main = 'Log Transformation')
grid()


# Simulate skewed data
set.seed(42)
original_values <- c(rlnorm(50, meanlog=0, sdlog=1), rlnorm(50, meanlog=2, sdlog=1))

# Apply log1p transformation
log1p_x_values <- log1p(original_values)
log1p_y_values <- log1p(original_values + rnorm(100, mean=0, sd=0.5))  # adding some noise for realism

# Create scatter plot
plot(log1p_x_values, log1p_y_values,
		 xlab = "log1p(original_values)",
		 ylab = "log1p(original_values + noise)",
		 main = "Scatter Plot of log1p vs log1p with Skewed Data",
		 col = "black",
		 pch = 20)
grid()
