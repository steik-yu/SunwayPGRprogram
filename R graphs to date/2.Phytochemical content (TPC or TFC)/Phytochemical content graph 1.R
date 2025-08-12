### ----- Line graph of % Radical Scavenging Activity over Concentration -----
### ----- (different unit across samples and positive control) -----

library(tidyverse)

# Use 0.Data input... to import DDPH_TPCTFC.xlsx, sheet 2
# once you have data, run the following
head(data) # small check
TPCTFC <- data


NormTPCTFC <- TPCTFC %>%
  group_by(Group, Treatment) %>%
  summarise(
    MeanAbs = mean(Absorbance, na.rm = TRUE),
    `MeanContent/g` = mean(`Content/g`, na.rm = TRUE),
    `StdContent/g` = sd(`Content/g`, na.rm = TRUE),
    .groups = "drop"
  )

plot1 <- NormTPCTFC %>% 
  ggplot(aes(Treatment, `MeanContent/g`, fill = Group)) +
  geom_bar(aes(),
           stat = "identity",
           position = position_dodge(width = 0.7),  # Side-by-side bars
           width = 0.7
  ) +
  theme_bw() +
  labs(x = "Treatment (10 g/mL)",
       y = "Total content") +
  geom_errorbar(aes(ymin = `MeanContent/g` - `StdContent/g`, 
                    ymax = `MeanContent/g` + `StdContent/g`), 
                width =0.2,  # Width of error bar caps
                position = position_dodge(width = 0.7),  # MUST match bar dodge width
                color = "black"
  ) +
  scale_fill_brewer(
    palette = "Set2",
    name = "Group",  # Change legend title
    labels = c("TFC" = "Total flavonoid content (mgQE/g)", 
               "TPC" = "Total phenolic content (mgGAE/g)")  # Rename items
  ) 
plot1
