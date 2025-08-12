### ----- Line graph of % Radical Scavenging Activity over Concentration -----
### ----- (same unit across samples and positive control) -----

library(tidyverse)

# Use 0.Data input... to import DDPH_TPCTFC.xlsx, sheet 1
# once you have data, run the following
head(data) # small check
DPPHABTS <- data


# ----- Normalize DPPH+ABTS data using concentration 0 respectively ----- 

### IMPORTANT! Change your column name accordingly, for example, my concentration
### column is Treatment, Blank, Group, Same_unit_concentration,
### Not_same_unit_concentration, & Absorbance for illustration purposes.
### RENAME as --- Concentration will be fine 👍

# Dont run the following, run only if you are using
# the dataset DDPH_TPCTFC.xlsx provided ---

DPPHABTS$Concentration <- DPPHABTS$Same_unit_concentration

# Dont run until here ---

DPPH <- DPPHABTS %>% filter(Group == "DPPH")
DPPH$Absorbance <- DPPH$Absorbance - DPPH$Blank

# Step 1: Normalize individual absorbance using control group mean (conc = 0)
# For example, get mean from concentration = 0 for ascorbic acid, then
# all raw data of ascorbic acid (including concentration 0), divide by the mean
# then will get % DPPH individually.
NormDPPH_temp <- DPPH %>% dplyr::select(-Blank) %>%
  group_by(Group, Treatment) %>%
  mutate(Control_Mean = mean(Absorbance[Concentration == 0], na.rm = TRUE)) %>%
  mutate(ScavengingActivity_P = ((Control_Mean - Absorbance) / Control_Mean) * 100) 


# Step 2: Calculate summary statistics
# Then from all the data, get mean and standard deviation.
NormDPPH <- NormDPPH_temp %>% group_by(Group, Treatment, Concentration) %>%
  summarise(
    MeanAbs = mean(Absorbance, na.rm = TRUE),
    Mean_ScavengingActivity_P = mean(ScavengingActivity_P, na.rm = TRUE),
    Std_ScavengingActivity_P = sd(ScavengingActivity_P, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(c(MeanAbs, Mean_ScavengingActivity_P, Std_ScavengingActivity_P), 
                ~ ifelse(abs(.) < 0.005, 0, round(., 2))))


ABTS <- DPPHABTS %>% filter(Group == "ABTS")
ABTS$Absorbance <- ABTS$Absorbance/ABTS$Blank

NormABTS <- ABTS  %>% dplyr::select(-Blank) %>%
  # Step 1: Normalize individual absorbance using control group mean (conc = 0)
  # For example, get mean from concentration = 0 for ascorbic acid, then
  # all raw data of ascorbic acid (including concentration 0), divide by the mean
  # then will get % DPPH individually.
  group_by(Group, Treatment) %>%
  mutate(ScavengingActivity_P = 100 - (Absorbance * 100)) %>%
  
  # Step 2: Calculate summary statistics
  # Then from all the data, get mean and standard deviation.
  group_by(Group, Treatment, Concentration) %>%
  summarise(
    MeanAbs = mean(Absorbance, na.rm = TRUE),
    Mean_ScavengingActivity_P = mean(ScavengingActivity_P, na.rm = TRUE),
    Std_ScavengingActivity_P = sd(ScavengingActivity_P, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(c(MeanAbs, Mean_ScavengingActivity_P, Std_ScavengingActivity_P), 
                ~ ifelse(abs(.) < 0.005, 0, round(., 2))))

NormDPPHABTS <- rbind(NormDPPH, NormABTS)


# ----- Generate plot (assume ascorbic acid is same unit) ----- 

plot1 <- ggplot(NormDPPH, aes(x = Concentration, y = Mean_ScavengingActivity_P, 
                                 color = Treatment)) +
  geom_line(aes(color = Treatment), size = 1, alpha = 0.5) +
  geom_point(aes(shape = Treatment, color = Treatment)) +
  theme_bw() +
  labs(
    x = "Concentration (g/mL) [Treatment]",
    y = "% of RSA (DPPH)"
  ) +
  # scale_x_continuous(
  #   sec.axis = sec_axis(
  #     trans = ~ . / scaling_factor,  # Reverse scaling for ascorbic axis
  #     name = "Concentration for Ascorbic Acid (µg/mL)",
  #     breaks = seq(0, 200, by = 50)  # Original ascorbic concentrations
  #   )) + 
  theme(legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6)) +
  geom_errorbar(aes(color = Treatment, ymin = Mean_ScavengingActivity_P - Std_ScavengingActivity_P, 
                    ymax = Mean_ScavengingActivity_P + Std_ScavengingActivity_P), width = 0.3)
plot1


plot2 <- ggplot(NormABTS, aes(x = Concentration, y = Mean_ScavengingActivity_P, 
                                color = Treatment)) +
  geom_line(aes(color = Treatment), size = 1, alpha = 0.5) +
  geom_point(aes(shape = Treatment, color = Treatment)) +
  theme_bw() +
  labs(
    x = "Concentration (g/mL) [Treatment]",
    y = "% of RSA (ABTS)"
  ) +
  # scale_x_continuous(
  #   sec.axis = sec_axis(
  #     trans = ~ . / scaling_factor,  # Reverse scaling for ascorbic axis
  #     name = "Concentration for Ascorbic Acid (µg/mL)",
  #     breaks = seq(0, 200, by = 50)  # Original ascorbic concentrations
  #   )) + 
  theme(legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6)) +
  geom_errorbar(aes(color = Treatment, ymin = Mean_ScavengingActivity_P - Std_ScavengingActivity_P, 
                    ymax = Mean_ScavengingActivity_P + Std_ScavengingActivity_P), width = 0.3)
plot2
