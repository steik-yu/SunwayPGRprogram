### ----- Bar chart of % Cell Viability over Concentration -----
### ----- (with + and - controls) for one treatment -----

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


# ----- Generate bar chart -----

plot_data1 <- Cellvia_final %>% filter(Treatment != "MelastomaM") %>%
  filter(Treatment != "MelastomaW")
plot_data1$Concentration[plot_data1$Treatment == "Untreated"] <- "-"
plot_data1$Concentration[plot_data1$Treatment == "α-Mangostin"] <- "+"
plot_data1$Concentration[plot_data1$Concentration == 0] <- "V"

plot1 <- plot_data1 %>%
  ggplot(aes(Concentration, Mean_CV_P)) +
  geom_bar(aes(fill = Treatment), stat = "identity", width = 0.7) +
  scale_x_discrete(limits = c("V", "50", "100", "150", "200", "+", "-")) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  labs(x = "",
       y = "% Cell Viability") +
  
  annotate("text", x = 3.5, y = -16, label = "MMLE (µg/mL)") +
  coord_cartesian(ylim = c(0, 105), clip = "off") +
  annotate("segment", x = 1.8, y = -12.9, xend = 5.2, yend = -12.9) +
  guides(fill = "none") +
  geom_errorbar(aes(ymin = Mean_CV_P - Std_CV_P, ymax = Mean_CV_P + Std_CV_P),
                width = 0.3)
plot1


plot_data2 <- Cellvia_final %>% filter(Treatment != "MelastomaE") %>%
  filter(Treatment != "MelastomaW")
plot_data2$Concentration[plot_data2$Treatment == "Untreated"] <- "-"
plot_data2$Concentration[plot_data2$Treatment == "α-Mangostin"] <- "+"
plot_data2$Concentration[plot_data2$Concentration == 0] <- "V"

plot2 <- plot_data2 %>%
  ggplot(aes(Concentration, Mean_CV_P)) +
  geom_bar(aes(fill = Treatment), stat = "identity", width = 0.7) +
  scale_x_discrete(limits = c("V", "50", "100", "150", "200", "+", "-")) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  labs(x = "",
       y = "% Cell Viability") +
  
  annotate("text", x = 3.5, y = -16, label = "MMLE (µg/mL)") +
  coord_cartesian(ylim = c(0, 105), clip = "off") +
  annotate("segment", x = 1.8, y = -12.9, xend = 5.2, yend = -12.9) +
  guides(fill = "none") +
  geom_errorbar(aes(ymin = Mean_CV_P - Std_CV_P, ymax = Mean_CV_P + Std_CV_P),
                width = 0.3)
plot2


plot_data3 <- Cellvia_final %>% filter(Treatment != "MelastomaE") %>%
  filter(Treatment != "MelastomaM")
plot_data3$Concentration[plot_data3$Treatment == "Untreated"] <- "-"
plot_data3$Concentration[plot_data3$Treatment == "α-Mangostin"] <- "+"
plot_data3$Concentration[plot_data3$Concentration == 0] <- "V"

plot3 <- plot_data3 %>%
  ggplot(aes(Concentration, Mean_CV_P)) +
  geom_bar(aes(fill = Treatment), stat = "identity", width = 0.7) +
  scale_x_discrete(limits = c("V", "50", "100", "150", "200", "+", "-")) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  labs(x = "",
       y = "% Cell Viability") +
  
  annotate("text", x = 3.5, y = -16, label = "MMLE (µg/mL)") +
  coord_cartesian(ylim = c(0, 105), clip = "off") +
  annotate("segment", x = 1.8, y = -12.9, xend = 5.2, yend = -12.9) +
  guides(fill = "none") +
  geom_errorbar(aes(ymin = Mean_CV_P - Std_CV_P, ymax = Mean_CV_P + Std_CV_P),
                width = 0.3)
plot3


ggarrange(plot1, plot2, plot3,
          labels = c("A)", "B)", "C)"),
          ncol = 2, nrow = 2)
