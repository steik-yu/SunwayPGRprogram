### ----- PERFORM one-way ANOVA with post-hoc test manually -----

# Research Question: Does physical activity level affect tumor size?
# • H0: No difference in tumor size across physical activity levels.
# • H1: Significant difference.
# i. Perform a one-way ANOVA.
# ii. Check assumptions (normality & homogeneity of variance).
# iii. If significant, conduct a Tukey post-hoc test and interpret.


##   Physical_Activity     n mean_size median_size    sd   IQR
##   <chr>             <int>     <dbl>       <dbl> <dbl> <dbl>
## 1 High              50004      41.9          42  21.6    38
## 2 Low               50471      42.0          42  21.7    38
## 3 Moderate          67022      42.1          42  21.7    38


# Check your normality and variance with perform_normality_tests(data) or
# perform_levene_test(data) using the function or,
# manually as shown below.

HighPA_TS <- data$Tumor_Size_mm[data$Physical_Activity == "High"]
ModeratePA_TS <- data$Tumor_Size_mm[data$Physical_Activity == "Moderate"]
LowPA_TS <- data$Tumor_Size_mm[data$Physical_Activity == "Low"]

shapiro.test(HighPA_TS)
shapiro.test(ModeratePA_TS)
shapiro.test(LowPA_TS)

leveneTest(data$Tumor_Size_mm ~ data$Physical_Activity)


# If ANOVA assumption met (normality and variance)
ANOVAresult <- aov(Tumor_Size_mm ~ Physical_Activity, data = data)
summary(ANOVAresult)
## Post-hoc test if significant aov (p < 0.05)
tukey_results <- TukeyHSD(ANOVAresult)
print(tukey_results)


# If ANOVA assumption NOT met (normality and variance)
ANOVAresult <- kruskal.test(Tumor_Size_mm ~ Physical_Activity, data = data)
print(ANOVAresult)
## Posthoc test if significant Kruskal-Wallis (p < 0.05)
pairwise_results <- pairwise.wilcox.test(
  x = data$Tumor_Size_mm,
  g = data$Physical_Activity,
  p.adjust.method = "bonferroni"  # bonferroni or BH
)
print(pairwise_results)

# 1. Use Bonferroni (bonferroni) When:
#   You have ≤5 group comparisons (e.g., 3 groups → 3 pairwise tests)
#   Your field requires ultra-strict error control (e.g., clinical trials)
#   You want to minimize false positives at all costs
# 2. Use Benjamini-Hochberg (BH) When:
#   You have >5 group comparisons (e.g., 6 groups → 15 pairwise tests)
#   You're doing exploratory analysis (common in biology/omics research)
#   You want to balance discovering true effects vs. controlling false positives

