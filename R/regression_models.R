library(ggplot2)

if (file.exists("data/neo_clean.rds")) {
  neo <- readRDS("data/neo_clean.rds")
  if ("estimated_diameter_max" %in% names(neo)) neo$est_diameter_max <- neo$estimated_diameter_max
} else {
  neo <- read.csv("data/raw/neo.csv")
}

dir.create("output", showWarnings = FALSE)

model_slr <- lm(est_diameter_max ~ absolute_magnitude, data = neo)
summary_slr <- summary(model_slr)
print(summary_slr)
r2_slr <- summary_slr$r.squared
cat("\nSLR R-squared:", round(r2_slr, 4), "\n")

p1 <- ggplot(neo, aes(x = absolute_magnitude, y = est_diameter_max)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "SLR: Diameter vs Absolute Magnitude - Gayatri",
       x = "Absolute Magnitude",
       y = "Estimated Diameter Max (km)")
print(p1)
ggsave("output/slr_plot.png", plot = p1, width = 7, height = 5)

model_mlr <- lm(relative_velocity ~ absolute_magnitude + est_diameter_max + miss_distance, data = neo)
summary_mlr <- summary(model_mlr)
print(summary_mlr)
cat("\nMLR R-squared:", round(summary_mlr$r.squared,4), "\n")
cat("MLR Adjusted R-squared:", round(summary_mlr$adj.r.squared,4), "\n")

png("output/mlr_diagnostics.png", width = 800, height = 800)
par(mfrow = c(2,2))
plot(model_mlr)
dev.off()

cat("\nPerson 3 analysis complete - Gayatri\n")
