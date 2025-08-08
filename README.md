# Sunway post-graduate R program
This page aims to provide read-to-use R program for students who are performing basic biostatistics in R with figures. 

## Data input format
To make our life easier onwards, make your data looking great like the format below. It is not as clean as your scientific table with all those redundant information, but R reads better this way. (ps: yes, ofc you can transform the data yourself in R, but it will become complicated here if we introduce that here, so)

Example Data 1. Colorectal Cancer Dataset
| Attempt | #1    | #2    |
| :-----: | :---: | :---: |
| Seconds | 301   | 283   |

Example Data 2. 

## Statistical Tests
0. *Assumption checking tests: Shapiro-Wilk test / Levene's test*
1. One sample t-test / One sample Wilcoxon rank sum test
2. Independent two sample t-test / Mann-Whitney U test
3. Paired samples t-test / Paired sample Wilcoxon rank sum test
4. One-way ANOVA with post-hoc test / Kruskal-Wallis rank sum test
5. Two-way ANOVA with post-hoc test / Aligned Rank Transform test
6. Chi-Square Goodness-of-Fit Test
7. Pearson correlation coefficient / Spearman rank correlation coefficient
8. Simple linear regression
9. Multiple linear regression
10. Logistic regression

## R graphs or formula for the moment
### Antioxidant assays (DPPH/ABTS)
- Line graph of % Radical Scavenging Activity over Concentration (same unit across samples and positive control)
- Line graph of % Radical Scavenging Activity over Concentration (different unit across samples and positive control)
- Linear model for EC50
- Bar chart of EC50 over Assays

### Phytochemical content (TPC/TFC)
- Bar chart of Total Phenolic/Flavonoid Content over Treatments

### Cell viability assay (Cell-Titer Blue®)
- Line graph of % Cell Viability over Concentration
- Bar chart of % Cell Viability over Concentration (with + and - controls) for one treatment
- Bar chart of % Cell Viability over Concentration (with + and - controls) for all treatments
- Linear model for IC50
