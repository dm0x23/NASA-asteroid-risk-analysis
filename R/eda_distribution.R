library(tidyverse)

if (!dir.exists("output")) dir.create("output")

neo <- read.csv("data/raw/neo.csv")

# Standardize column names to match the guide
neo <- neo %>%
  rename_with(~ "estimated_diameter_max", any_of(c("est_diameter_max", "estimated_diameter_max.km."))) %>%
  rename_with(~ "is_potentially_hazardous", any_of(c("is_hazardous", "hazardous")))

# 1. Summary Statistics
print(summary(neo$estimated_diameter_max))
diam_sd  <- sd(neo$estimated_diameter_max, na.rm = TRUE)
diam_iqr <- IQR(neo$estimated_diameter_max, na.rm = TRUE)

cat("Standard Deviation:", diam_sd, "\n")
cat("IQR:", diam_iqr, "\n")

# 2. Visualizations
p_hist <- ggplot(neo, aes(x = estimated_diameter_max)) +
  geom_histogram(bins = 35, fill = "#2b5c8f", color = "white") +
  scale_x_log10() +
  labs(title = "Asteroid Maximum Diameter Distribution", 
       x = "Diameter (km, log10)", 
       y = "Count") +
  theme_minimal()
ggsave("output/diameter_histogram.png", plot = p_hist, width = 7, height = 4.5)

p_box <- ggplot(neo, aes(x = as.factor(is_potentially_hazardous), 
                         y = relative_velocity, 
                         fill = as.factor(is_potentially_hazardous))) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Relative Velocity by Hazard Status", 
       x = "Potentially Hazardous", 
       y = "Velocity (km/h)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("output/speed_by_hazard_boxplot.png", plot = p_box, width = 6, height = 4.5)

# 3. Normal Distribution (Velocity)
vel_mean <- mean(neo$relative_velocity, na.rm = TRUE)
vel_sd   <- sd(neo$relative_velocity, na.rm = TRUE)

p_norm <- ggplot(neo, aes(x = relative_velocity)) +
  geom_density(fill = "#5bc0de", alpha = 0.4) +
  stat_function(fun = dnorm, 
                args = list(mean = vel_mean, sd = vel_sd), 
                color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Velocity Density vs Normal Curve", 
       x = "Relative Velocity (km/h)", 
       y = "Density") +
  theme_minimal()
ggsave("output/velocity_density_normal.png", plot = p_norm, width = 7, height = 4.5)

png("output/velocity_qqplot.png", width = 600, height = 500)
qqnorm(neo$relative_velocity, main = "Q-Q Plot: Relative Velocity")
qqline(neo$relative_velocity, col = "red", lwd = 2)
dev.off()

# 4. Binomial Distribution
p_hat <- mean(neo$is_potentially_hazardous == TRUE, na.rm = TRUE)
prob_exact_3 <- dbinom(x = 3, size = 20, prob = p_hat)
cat("P(X = 3 | n = 20):", prob_exact_3, "\n")

# 5. Poisson Distribution
lambda_rate <- nrow(neo) / 12
k_range <- 0:ceiling(lambda_rate * 2)
poisson_df <- data.frame(events = k_range, prob = dpois(k_range, lambda = lambda_rate))

p_pois <- ggplot(poisson_df, aes(x = events, y = prob)) +
  geom_col(fill = "#5cb85c", color = "blue") +
  labs(title = paste("Poisson Distribution (lambda =", round(lambda_rate, 2), ")"),
       x = "Events", y = "Probability") +
  theme_minimal()
ggsave("output/poisson_distribution.png", plot = p_pois, width = 7, height = 4.5)