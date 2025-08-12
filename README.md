# Sunway post-graduate R program
This page aims to provide read-to-use R program for students who are performing basic biostatistics in R with figures. 

## Data input format
To make your life easier moving forward, format your data like the example below. It might not be as clean as the scientific table with all the redundant information, but R reads it better this way (better if replace space with _). (P.S. Yes, of course you can transform the data yourself in R, but introducing that here would make things more complicated, so)

**Example Data 1.** Colorectal Cancer Patient Raw Dataset.
| Patient_ID | Country    | Age    | Gender    | Cancer_Stage    | Tumor_Size_mm    | Family_History    | Smoking_History    | Alcohol_Consumption    | ...    |
| :-----: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| 1 | UK   | 77   | M   | Localized   | 69   | No   | No   | Yes   | ...   |
| 2 | Japan   | 59   | M   | Localized   | 33   | No   | Yes   | No   | ...   |
| 3 | USA   | 66   | F   | Regional   | 17   | No   | No   | No   | ...   |
| 4 | Japan   | 83   | F   | Metastatic   | 14   | No   | Yes   | No   | ...   |
| 5 | UK   | 66   | M   | Localized   | 34   | Yes   | No   | No   | ...   |
| 6 | France   | 69   | M   | Regional   | 71   | Yes   | No   | Yes   | ...   |
| ... | ...   | ...   | ...   | ...   | ...   | ...   | ...   | ...   | ...   | 

**Example Data 2.** Cell Viability Absorbance Reading Raw Dataset. (MelastomaW: Melastoma water extract)
| Treatment | Concentration | Absorbance |
| :-----: | :---: | :---: |
| Untreated | 0   | 6271   |
| Untreated | 0   | 6465   |
| Untreated | 0   | 6516   |
| MelastomaW | 200   | 2286   |
| MelastomaW | 200   | 3364   |
| MelastomaW | 200   | 2578   |
| MelastomaW | 150   | 5504   |
| MelastomaW | 150   | 5632   |
| MelastomaW | 150   | 6302   |
| ... | ...   | ...   |

## Statistical Tests
0. *Data input + Assumption checking tests: Shapiro-Wilk test / Levene's test*
1. One sample t-test / One sample Wilcoxon rank sum test
2. Independent two sample t-test / Mann-Whitney U test
3. Paired samples t-test / Paired sample Wilcoxon rank sum test
4. One-way ANOVA with post-hoc test / Kruskal-Wallis rank sum test
5. Two-way ANOVA with post-hoc test / Aligned Rank Transform test
6. Chi-Square Goodness-of-Fit Test / Test of Independence
7. Pearson correlation coefficient / Spearman rank correlation coefficient
8. Simple linear / Multiple linear regression / Logistic regression

## R graphs or formula for the moment
### Antioxidant assays (DPPH/ABTS)
1. Line graph of % Radical Scavenging Activity over Concentration (same unit across samples and positive control)
2. Line graph of % Radical Scavenging Activity over Concentration (different unit across samples and positive control)
3. Linear model for EC50
4. Bar chart of EC50 over Assays

### Phytochemical content (TPC/TFC)
1. Bar chart of Total Phenolic/Flavonoid Content over Treatments

### Cell viability assay (Cell-Titer Blue®)
1. Line graph of % Cell Viability over Concentration
2. Bar chart of % Cell Viability over Concentration (with + and - controls) for one treatment
3. Bar chart of % Cell Viability over Concentration (with + and - controls) for all treatments
4. Linear model for IC50
