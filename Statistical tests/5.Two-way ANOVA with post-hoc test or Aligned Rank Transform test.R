### ----- PERFORM two-way ANOVA with post-hoc test manually -----

# Research Question: Does gender and smoking history jointly influence tumor size?
# • H0: No interaction effect between gender and smoking history.
# • H1: There is an interaction effect.
# i. Perform a two-way ANOVA in R.
# ii. Check interaction effects and main effects.


##   Gender Smoking_History     n mean_size median_size    sd   IQR
##   <chr>  <chr>           <int>     <dbl>       <dbl> <dbl> <dbl>
## 1 F      No              39726      42.1          42  21.7    38
## 2 F      Yes             27085      42.2          42  21.7    38
## 3 M      No              60415      41.9          42  21.7    38
## 4 M      Yes             40271      42.0          42  21.7    38


# Check your normality and variance with perform_normality_tests(data) or
# perform_levene_test(data) using the function or,
# manually as shown below.

MaleSmoke <- data$Tumor_Size_mm[data$Gender == "M" &
                                  data$Smoking_History == "Yes"]
MaleNOSmoke <- data$Tumor_Size_mm[data$Gender == "M" &
                                    data$Smoking_History == "No"]
FemaleSmoke <- data$Tumor_Size_mm[data$Gender == "F" &
                                    data$Smoking_History == "Yes"]
FemaleNOSmoke <- data$Tumor_Size_mm[data$Gender == "F" &
                                      data$Smoking_History == "No"]
shapiro.test(MaleSmoke)
shapiro.test(MaleNOSmoke)
shapiro.test(FemaleSmoke)
shapiro.test(FemaleNOSmoke)

leveneTest(Tumor_Size_mm ~ Gender*Smoking_History, data = data)


# Check package
packages_needed <- c("ARTool", "emmeans")
# Function to check/install packages
check_and_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing missing package: ", pkg)
    install.packages(pkg)
  } else {
    message("Package already installed: ", pkg)
  }
}
invisible(sapply(packages_needed, check_and_install))
invisible(lapply(packages_needed, library, character.only = TRUE))


# If ANOVA assumption met (normality and variance)
ANOVAresult <- aov(Tumor_Size_mm ~ Gender*Smoking_History, data = data)
summary(ANOVAresult)
## Post-hoc test if significant aov (p < 0.05)
tukey_results <- TukeyHSD(ANOVAresult)
print(tukey_results)


# If ANOVA assumption NOT met (normality and variance)
data$Gender <- as.factor(data$Gender)
data$Smoking_History <- as.factor(data$Smoking_History)
art_model <- art(Tumor_Size_mm ~ Gender*Smoking_History, data = data)
anova_art <- anova(art_model)
print(anova_art)
## Post-hoc test only if ANOVA shows significance
art.con(art_model, 
        formula = ~ Gender * Smoking_History,
        adjust = "holm")  # Recommended adjustment


# Use Holm for:
#   ART post-hoc tests
#   Any non-parametric pairwise comparisons
#   When you want strong error control without excessive power loss
# Use Bonferroni for:
#   Very few comparisons (≤3)
#   When any false positive is unacceptable


# Holm progressively relaxes criteria after first rejection
# More likely to detect true effects than Bonferroni

# Bonferroni: Reject if p < 0.01 (0.05/5)  
# Holm: Reject smallest p if p < 0.01, next if p < 0.0125, etc.

