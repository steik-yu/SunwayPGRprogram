### ----- PERFORM two sample t test manually -----

# Research Question: Is there a significant change in tumor size from the time of diagnosis to 6 months after treatment?
# • H0: No difference in mean tumor size between diagnosis and 6 months post-treatment.
# • H1: Significant difference in tumor size.
# i. Perform a paired sample t-test.
# ii. Check normality; if violated, use paired sample Wilcoxon rank sum test instead.


# Step 1: Calculate differences and normality
data$Tumor_Size_Diff <- data$Tumor_Size_6mo - data$Tumor_Size_mm
shapiro.test(data$Tumor_Size_Diff)

# If Tumor_Size_Diff normally distributed
t.test(data$Tumor_Size_6mo, data$Tumor_Size_mm, paired = TRUE)

# If Tumor_Size_Diff NOT normally distributed
wilcox.test(data$Tumor_Size_6mo, data$Tumor_Size_mm, paired = TRUE)


