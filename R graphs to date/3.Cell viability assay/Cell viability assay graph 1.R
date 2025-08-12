### ----- Line graph of % Cell Viability over Concentration -----

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


# ----- Generate line graph -----

plot_data1 <- Cellvia_final %>% filter(Treatment != "Untreated") %>% filter(Treatment != "α-Mangostin")

plot1 <- ggplot(plot_data1, aes(Concentration, Mean_CV_P), color = Treatment) +
  geom_line(aes(color = Treatment), size = 1, alpha = 0.5) +
  geom_point(aes(shape = Treatment, color = Treatment)) +
  ylim(30, 110) +
  theme_bw() +
  theme(legend.position = c(.05, .05),
        legend.justification = c("left", "bottom"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6)) +
  labs(x = "MMLE (µg/mL)",
       y = "% Cell Viability") +
  scale_color_brewer(palette = "Set2") +
  
  geom_errorbar(aes(color = Treatment, 
                    ymin = Mean_CV_P - Std_CV_P, 
                    ymax = Mean_CV_P + Std_CV_P),
                width = 0.3)
plot1
