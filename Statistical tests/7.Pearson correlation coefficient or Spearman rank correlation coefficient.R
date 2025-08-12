### ----- PERFORM Pearson correlation coefficient test manually -----

# Assumes both variables are normally distributed (or at least approximately so).
# To check: use Shapiro–Wilk test (shapiro.test) or visual checks (histograms, Q-Q plots).
# If either variable is not normal, use Spearman instead (or Kendall’s τ for small samples).

# Question 1: Is there a linear relationship between patient age and tumor size?
# • H0: ρ (R) = 0 (no linear correlation between age and tumor size)
# • H1: ρ (R) ≠ 0 (a linear correlation exists between age and tumor size) (either + or -)
# i. Perform a Pearson correlation test in R.

cor.test(data$Age, data$Tumor_Size_mm, method = "pearson")

library(ggplot2)
ggplot(data, aes(x = Age, y = Tumor_Size_mm)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Age vs Tumor Size (Pearson Correlation)")


# Question 2: Do countries with higher cancer incidence rates have higher healthcare costs?
# • H0: ρ (R) = 0 (no linear correlation between healthcare costs and incidence rate)
# • H1: ρ (R) ≠ 0 (a linear correlation exists between healthcare costs and incidence rate)
# i. Perform a Pearson correlation test in R.

cor.test(data$Healthcare_Costs, data$Incidence_Rate_per_100K, method = "pearson")


### ----- PERFORM Spearman rank correlation coefficient test manually -----

# No normality assumption — works for skewed data, ordinal variables, 
# or when the relationship is monotonic but not linear.
# Can be used as the “safe” choice if you’re unsure about distribution.

# Question 1: Is there a monotonic relationship between patient age and mortality rate per 100K?
# • H0: ρs = 0 (no monotonic correlation between age and mortality rate)
# • H1: ρs ≠ 0 (a monotonic correlation exists between age and mortality rate)
# i. Perform a Spearman rank correlation test in R.

cor.test(data$Age, data$Mortality_Rate_per_100K, method = "spearman")

ggplot(data, aes(x = Age, y = Mortality_Rate_per_100K)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "loess", color = "orange") +
  labs(title = "Age vs Mortality Rate (Spearman Correlation)")


# Question 4: Is larger tumor size associated with higher mortality rate per 100K, 
# without assuming linearity?
# • H0: ρs = 0 (no monotonic correlation between tumor size and mortality rate)
# • H1: ρs ≠ 0 (a monotonic correlation exists between tumor size and mortality rate)
# i. Perform a Spearman rank correlation test in R.

cor.test(data$Tumor_Size_mm, data$Mortality_Rate_per_100K, method = "spearman")


