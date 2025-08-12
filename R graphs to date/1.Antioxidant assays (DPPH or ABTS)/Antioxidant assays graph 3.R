### ----- Bar chart of EC50 over Assays -----

library(tidyverse)

# Use 0.Data input... to import DDPH_TPCTFC.xlsx, sheet 3
# once you have data, run the following
head(data) # small check
EC50 <- data


# ----- Generate bar chart without significant * -----

EC50_graph <- EC50 %>%
  group_by(Group, Treatment) %>%
  summarise(MeanEC50 = mean(EC50, na.rm = TRUE),
            StdEC50 = sd(EC50, na.rm = TRUE),
            .groups = "drop")

plot1 <- EC50_graph %>% 
  ggplot(aes(Group, MeanEC50, fill = Treatment)) +
  geom_bar(aes(),
           stat = "identity",
           position = position_dodge(width = 0.7),  # Side-by-side bars
           width = 0.7
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  labs(x = "Antioxidant assays",
       y = expression(EC[50]~~(g/mL))) +
  geom_errorbar(aes(ymin = MeanEC50 - StdEC50, 
                    ymax = MeanEC50 + StdEC50), 
                width =0.2,  # Width of error bar caps
                position = position_dodge(width = 0.7),  # MUST match bar dodge width
                color = "black"
  ) +
  ylim(0,8)
plot1


# ----- Generate bar chart with significant * manually-----

EC50_graph <- EC50 %>%
  group_by(Group, Treatment) %>%
  summarise(MeanEC50 = mean(EC50, na.rm = TRUE),
            StdEC50 = sd(EC50, na.rm = TRUE),
            .groups = "drop")

plot2 <- EC50_graph %>% 
  ggplot(aes(Group, MeanEC50, fill = Treatment)) +
  geom_bar(aes(),
           stat = "identity",
           position = position_dodge(width = 0.7),  # Side-by-side bars
           width = 0.7
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  labs(x = "Antioxidant assays",
       y = expression(EC[50]~~(g/mL))) +
  geom_errorbar(aes(ymin = MeanEC50 - StdEC50, 
                    ymax = MeanEC50 + StdEC50), 
                width =0.2,  # Width of error bar caps
                position = position_dodge(width = 0.7),  # MUST match bar dodge width
                color = "black"
  ) +
  ylim(0,8) +
  
  # In annotate, change your x, xend, y, yend for the line
  # In geom_text, change you x, y for the * position
  
  annotate("segment", x = 0.75, y = 5, xend = 1, yend = 5) +
  geom_text(x = 0.875, y = 5.2, label = "**", size = 5.5) +
  
  annotate("segment", x = 1, y = 6.5, xend = 1.25, yend = 6.5) +
  geom_text(x = 1.125, y = 6.7, label = "***", size = 5.5) +
  
  annotate("segment", x = 0.75, y = 7.3, xend = 1.25, yend = 7.3) +
  geom_text(x = 1, y = 7.5, label = "***", size = 5.5) +
  
  annotate("segment", x = 1.75, y = 7.4, xend = 2, yend = 7.4) +
  geom_text(x = 1.875, y = 7.6, label = "***", size = 5.5) 
plot2
