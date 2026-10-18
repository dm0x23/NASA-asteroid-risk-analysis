library(ggplot2)
neo <- readRDS("data/neo_clean.rds")
model_slr <- lm(estimated_diameter_max ~ absolute_magnitude, data = neo)
summary_slr <- summary(model_slr)
print(summary_slr)
r2_slr <- summary_slr$r.squared
cat("\nSLR R-squared:", round(r2_slr, 4), "\n")
p1 <- ggplot(neo, aes(x = absolute_magnitude, y = estimated_diameter_max)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "SLR: Diameter vs Absolute Magnitude",
       x = "Absolute Magnitude",
       y = "Estimated Diameter Max (km)")
print(p1)
model_mlr <- lm(relative_velocity ~ absolute_magnitude + estimated_diameter_max + miss_distance, data = neo)
summary_mlr <- summary(model_mlr)
print(summary_mlr)
cat("\nMLR R-squared:", round(summary_mlr$r.squared,4), "\n")
cat("MLR Adjusted R-squared:", round(summary_mlr$adj.r.squared,4), "\n")
png("output/mlr_diagnostics.png", width = 800, height = 800)
par(mfrow = c(2,2))
plot(model_mlr)
dev.off()
cat("\nPerson 3 analysis complete.\n")
