library(tidyverse)



if (file.exists("data/processed/neo_clean.rds")) {
  neo_data <- readRDS("data/processed/neo_clean.rds")
} else if (file.exists("data/raw/neo.csv")) {
  neo_data <- read_csv("data/raw/neo.csv", show_col_types = FALSE) %>% na.omit()
} else {
  message("Dataset not found in local path. Generating mock sample for testing...")
  set.seed(42)
  n <- 500
  neo_data <- tibble(
    relative_velocity = rnorm(n, mean = 48000, sd = 15000),
    is_potentially_hazardous = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.12, 0.88)),
    estimated_diameter_min = runif(n, 0.05, 0.8),
    estimated_diameter_max = estimated_diameter_min + runif(n, 0.02, 0.3),
    orbit_class = sample(c("Apollo", "Aten", "Amor"), n, replace = TRUE, prob = c(0.55, 0.15, 0.30)),
    close_approach_month = sample(month.name, n, replace = TRUE)
  )
}

neo_data <- neo_data %>%
  mutate(is_potentially_hazardous = as.logical(is_potentially_hazardous))

cat("\n==================================================================\n")
cat("         HYPOTHESIS TESTING SUITE: NASA NEO DATASET               \n")
cat("==================================================================\n\n")


cat("--- 1. ONE-SAMPLE MEAN TEST (t-test) ---\n")
mu_0 <- 45000
test_1_res <- t.test(neo_data$relative_velocity, 
                     mu = mu_0, 
                     alternative = "greater", 
                     conf.level = 0.95)

print(test_1_res)
cat("Interpretation:\n")
if (test_1_res$p.value < 0.05) {
  cat("Reject H0 (p < 0.05). Significant evidence that mean velocity exceeds", mu_0, "km/h.\n\n")
} else {
  cat("Fail to reject H0 (p >= 0.05). Insufficient evidence that mean velocity exceeds", mu_0, "km/h.\n\n")
}



cat("--- 2. ONE-SAMPLE PROPORTION TEST ---\n")
k_hazardous <- sum(neo_data$is_potentially_hazardous == TRUE)
n_total     <- nrow(neo_data)
p_0         <- 0.10

test_2_res <- prop.test(x = k_hazardous, 
                        n = n_total, 
                        p = p_0, 
                        alternative = "two.sided", 
                        correct = TRUE)

print(test_2_res)
cat("Interpretation:\n")
if (test_2_res$p.value < 0.05) {
  cat("Reject H0 (p < 0.05). True hazard proportion significantly differs from 10%.\n\n")
} else {
  cat("Fail to reject H0 (p >= 0.05). True hazard proportion does not significantly differ from 10%.\n\n")
}




cat("--- 3. TWO-SAMPLE INDEPENDENT t-TEST ---\n")
test_3_res <- t.test(relative_velocity ~ is_potentially_hazardous, 
                     data = neo_data, 
                     var.equal = FALSE)

print(test_3_res)
cat("Interpretation:\n")