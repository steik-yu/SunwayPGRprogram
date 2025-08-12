### ----- PERFORM Chi-Square Goodness-of-Fit Test manually -----

# Question 1: Is the proportion of male and female patients significantly 
# different from an equal split?
# • H0:  Male and female proportions are 50% each.
# • H1:  Proportions differ from 50/50.
# i. Perform a Chi-Square Test of Independence in R.

# Create observed counts
obs_gender <- table(data$Gender)

# Run Chi-Square Goodness-of-Fit Test
chisq.test(obs_gender, p = c(0.5, 0.5))

# Visualization
library(ggplot2)
ggplot(data, aes(x = Gender)) +
  geom_bar(fill = "skyblue") +
  geom_hline(yintercept = sum(obs_gender) * 0.5, 
             linetype = "dashed", color = "red") +
  labs(title = "Gender Distribution vs Expected 50/50", y = "Count")


# Question 2: Is the early detection rate significantly different 
# from the target goal of 70%?
# • H0: Early detection proportion is 70% (Yes) and 30% (No).
# • H1: Early detection proportion differs from 70% (Yes) and 30% (No).
# i. Perform a Chi-Square Goodness-of-Fit Test in R.

obs_detection <- table(data$Early_Detection)
obs_detection
obs_detection <- obs_detection[c("Yes", "No")]  # reorder
obs_detection
chisq.test(obs_detection, p = c(0.70, 0.30))  # Yes, No


### ----- PERFORM Chi-Square Test of Independence manually -----

# Question 1: Is there a significant association between a patient’s gender (Male/Female) and 
# their cancer stage (Localized, Regional, Distant, etc.)?
# • H0: Gender and Cancer Stage are independent.
# • H1: Gender and Cancer Stage are not independent.
# i. Perform a Chi-Square Test of Independence in R.
# This could reveal whether certain cancer stages are more prevalent in one gender.

# Create contingency table
table_gender_stage <- table(data$Gender, data$Cancer_Stage)

# Chi-square test
chisq.test(table_gender_stage)

ggplot(data, aes(x = Cancer_Stage, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Cancer Stage by Gender", y = "Count")


# Question 2: Is a country's economic classification (Developed/Developing) 
# linked to healthcare access level (High/Moderate/Low)?
# • H0: Economic classification and healthcare access are independent.
# • H1: They are associated.
# i. Perform a Chi-Square Goodness-of-Fit Test in R.
# This could confirm socio-economic disparities in healthcare services.

table_econ_access <- table(data$Economic_Classification, data$Healthcare_Access)
chisq.test(table_econ_access)


### ----- PERFORM chi-square test of independence manually with binning -----

# Question: Is there an association between tumor size category and mortality rate category?
# • H0: Tumor size category and mortality rate category are independent.
# • H1: Tumor size category and mortality rate category are not independent.
# i. Perform a Chi-Square Test of Independence in R after binning variables.

library(dplyr)
summary(data$Tumor_Size_mm)
summary(data$Mortality_Rate_per_100K)

# Binning Plan
## Tumor Size (mm)
# Small: ≤ 22 (1st quartile)
# Medium: 23–60 (middle range)
# Large: ≥ 61 (above 3rd quartile)

## Mortality Rate per 100K
# Low: ≤ 11 (1st quartile)
# Medium: 12–23 (middle range)
# High: ≥ 24 (above 3rd quartile)


# Bin tumor size into categories
data <- data %>%
  mutate(Tumor_Size_cat = cut(Tumor_Size_mm,
                              breaks = c(-Inf, 22, 60, Inf),
                              labels = c("Small", "Medium", "Large")))

# Bin mortality rate into categories
data <- data %>%
  mutate(Mortality_cat = cut(Mortality_Rate_per_100K,
                             breaks = c(-Inf, 11, 23, Inf),
                             labels = c("Low", "Medium", "High")))

# Now run chi-square test of independence
table_size_mortality <- table(data$Tumor_Size_cat, data$Mortality_cat)
chisq.test(table_size_mortality)


df_tab <- as.data.frame(table_size_mortality)
colnames(df_tab) <- c("Tumor_Size_cat", "Mortality_cat", "Freq")
ggplot(as.data.frame(df_tab), aes(x = Tumor_Size_cat, y = Freq, fill = Mortality_cat)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of Mortality Categories by Tumor Size Category",
       y = "Proportion", x = "Tumor Size Category") +
  theme_minimal()

