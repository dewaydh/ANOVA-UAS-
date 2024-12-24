# Load necessary libraries
library(readxl)
library(ggplot2)
library(car)
library(dplyr)

# 1. Load the data
data <- read_excel("C:/Users/LENOVO/Downloads/pupuk_pertanian.xlsx")

# Preview the data
head(data)

# Convert 'Jenis Pupuk' (Fertilizer Type) to a factor
data$`Jenis Pupuk` <- as.factor(data$`Jenis Pupuk`)

# Assumption Tests
## 1. Normality Test (Shapiro-Wilk Test)
shapiro_results <- data %>%
  group_by(`Jenis Pupuk`) %>%
  summarise(shapiro_p = shapiro.test(`Hasil Tanaman (kg)`)$p.value)

print("Normality Test Results:")
print(shapiro_results)

## 2. Homogeneity of Variance Test (Levene's Test)
levene_result <- leveneTest(`Hasil Tanaman (kg)` ~ `Jenis Pupuk`, data = data)

print("Homogeneity of Variance Test Results:")
print(levene_result)

## 3. Independence Test (If there is grouping by time or location, add this test if relevant)
# Not applied here since the data lacks time/location information.

# ANOVA Analysis
anova_result <- aov(`Hasil Tanaman (kg)` ~ `Jenis Pupuk`, data = data)
summary(anova_result)

# Visualization
## Boxplot of plant yield by fertilizer type
ggplot(data, aes(x = `Jenis Pupuk`, y = `Hasil Tanaman (kg)`, fill = `Jenis Pupuk`)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Plant Yield by Fertilizer Type", x = "Fertilizer Type", y = "Plant Yield (kg)")

## Histogram of plant yield distribution
ggplot(data, aes(x = `Hasil Tanaman (kg)`)) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black") +
  theme_minimal() +
  facet_wrap(~`Jenis Pupuk`) +
  labs(title = "Histogram of Plant Yield", x = "Plant Yield (kg)", y = "Frequency")

# Interpretation
# Interpretation of the Normality Test (Shapiro-Wilk Test)
# All p-values > 0.05 for each fertilizer type, indicating that data in each group satisfies the normality assumption.
# Therefore, no data transformation is needed.

# Interpretation of the Homogeneity of Variance Test (Levene's Test)
# p-value = 0.1883 > 0.05, indicating that the assumption of homogeneity of variances is satisfied.
# Therefore, the variances between fertilizer groups are considered equal, and ANOVA can proceed without adjustments.

# Interpretation of ANOVA
# p-value < 2e-16, indicating a statistically significant difference in plant yield between fertilizer types.
# The F-value (195.3) is very high, suggesting that the effect of fertilizer type on plant yield is very strong.
