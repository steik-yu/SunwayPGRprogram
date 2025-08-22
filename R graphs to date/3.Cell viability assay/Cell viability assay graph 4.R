### ----- Linear model for IC50 ----- 

library(ggplot2)
library(tidyverse) 

# Use 0.Data input... to import Cell_viability.xlsx, sheet 2
# once you have data, run the following
head(data) # small check
Cellvia <- data


# ----- Data normalization -----

Cellvia_2 <- Cellvia %>% filter(Treatment != "Untreated") %>% filter(Treatment != "α-Mangostin") %>% 
  group_by(Treatment) %>%
  mutate(Control_Mean = mean(Absorbance[Concentration == 0 | Concentration == 60], na.rm = TRUE)) %>%
  mutate(CV_P = ((Absorbance / Control_Mean) * 100))

Cellvia_3 <- Cellvia %>% filter(Treatment == "Untreated" | Treatment == "α-Mangostin") %>%
  mutate(Control_Mean = mean(Absorbance[Treatment == "Untreated"], na.rm = TRUE)) %>%
  mutate(CV_P = ((Absorbance / Control_Mean) * 100))

Cellvia_final <- rbind(Cellvia_2, Cellvia_3)

Cellvia_final <- Cellvia_final %>% group_by(Treatment, Concentration) %>%
  summarise(
    Mean_CV_P = mean(CV_P, na.rm = TRUE),
    Std_CV_P = sd(CV_P, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(c(Mean_CV_P, Std_CV_P), 
                ~ ifelse(abs(.) < 0.005, 0, round(., 2))))


# ----- linear regression -----

IC50data <- Cellvia_final %>% filter(Treatment != "Untreated") %>% filter(Treatment != "α-Mangostin")

IC50 <- function(i){
  x <- IC50data %>% filter(Treatment == i)
  model <- lm(data = x, Mean_CV_P ~ Concentration)
  m <- summary(model)$coefficients["Concentration", "Estimate"] %>% round(4)
  c <- summary(model)$coefficients["(Intercept)", "Estimate"] %>% round(4)
  z <- ((50 - c) / m) %>% round(4)
  print(paste0("lm model equal to y = ", m, "x + ", c, ", IC50 = ", z))
}

IC50("MelastomaW") #y = -0.2011x + 99.87, IC50 = 247.9861
IC50("MelastomaM") #y = -0.0189x + 91.976, IC50 = 2220.9524
IC50("MelastomaE") #y = -0.209x + 92.402, IC50 = 202.8804


# Example for a single treatment
plot_IC50 <- function(treatment_name) {
  x <- IC50data %>% filter(Treatment == treatment_name)
  model <- lm(Mean_CV_P ~ Concentration, data = x)
  
  ggplot(x, aes(x = Concentration, y = Mean_CV_P)) +
    geom_point(size = 3, alpha = 0.7, shape = 18) +
    geom_smooth(method = "lm", formula = y ~ x, span = 1, alpha = 0.1) + # regression line
    geom_hline(yintercept = 50, linetype = "dashed", color = "black") +      # IC50 reference line
    labs(
      title = paste("Linear Fit for", treatment_name),
      subtitle = paste0("IC50 ≈ ", round((50 - coef(model)[1]) / coef(model)[2], 2)),
      x = "Concentration (µg/mL)",
      y = "Mean % CV"
    ) +
    theme_bw()
}

# Example usage:
plot_IC50("MelastomaE")
