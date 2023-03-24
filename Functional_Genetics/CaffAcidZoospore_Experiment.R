#----------------------
# This code analyzes LC-MS/MS data from an experiment testing cacao's metabolite secretions upon P. palmivora challenge
# Noah Winters
# Dec, 2021
#----------------------

# Load libraries
library(tidyverse)
library(cowplot)
library(scales)

# Load data
dat <- read.csv("CaffAcidZoospore_Experiment_Data.csv",
              header = TRUE) %>%
  setNames(., nm = c("Treatment", "Sample", "Intensity"))

# Plot results
pdf("output.pdf", height = 4.5, width = 4.5)
ggplot(dat, aes(x=Treatment, y=Intensity, shape = Treatment)) + 
  geom_jitter(alpha = 0.7, size = 5, width = 0.2) +
  stat_summary(fun = "mean", size = 1, shape = 5, show.guide = FALSE) +
  scale_x_discrete(labels = c("Water\n + Leaf", "Zoospore\n + Leaf", "Zoospore\nOnly")) +
  scale_y_continuous(labels = label_comma()) +
  theme_minimal_grid() +
    theme(
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(),
      axis.title.x = element_text(),
      axis.title.y = element_text(),
      legend.title = element_blank(),
      legend.text = element_blank(), 
      legend.position = "none"
    ) + 
  labs(x = NULL, y = "Intensity")
dev.off()

# Statistics
pairwise.t.test(dat$Intensity, dat$Treatment, p.adjust.method = "none")
mean(subset(dat, Treatment == "zoospore_only")$Intensity)
mean(subset(dat, Treatment == "water_leaf")$Intensity)
mean(subset(dat, Treatment == "zoospore_leaf")$Intensity)
mean(subset(dat, Treatment == "water_leaf")$Intensity) - mean(subset(dat, Treatment == "zoospore_only")$Intensity)
