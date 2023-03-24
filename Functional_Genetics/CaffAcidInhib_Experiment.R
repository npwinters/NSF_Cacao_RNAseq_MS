#-------------------
# This code analyzes an experiment testing the caffeic acid inhibition of P. palmivora
# Noah Winters
# Dec, 2021
#-------------------

# Load libraries
library(tidyverse)

# Load data
dat <- read.csv("CaffAcidInhib_Experiment_Data.csv",
              header = TRUE) %>%
  setNames(., nm = c("Treatment", "Sample", "Diameter_cm"))

# Calculate area from diameter
dat$Area <- pi*((dat$Diameter_cm/2)^2)
dat$Area <- as.numeric(dat$Area)
dat$Treatment <- factor(dat$Treatment, levels = c("V8_Control", "Caffeic_Acid"))
                        
# Plot results
pdf("output.pdf", width = 3.5, height = 3.5)
ggplot(dat, aes(x=Treatment, y=Area, shape = Treatment)) + 
  geom_jitter(alpha = 0.7, size = 5, width = 0.2) +
  stat_summary(fun = "mean", size = 1, shape = 5, show.guide = FALSE) +
  scale_x_discrete(labels = c("V8 Control", "2mM\n Caffeic Acid")) +
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
  scale_y_continuous(limits=c(10,30)) +
  labs(x = NULL, y = expression("Area (cm" ^2 ~ ")" ))
dev.off()

# Statistics
pairwise.t.test(dat$Area, dat$Treatment, p.adjust.method = "none")
mean(subset(dat, Treatment == "Caffeic_Acid")$Area)
mean(subset(dat, Treatment == "V8_Control")$Area)
