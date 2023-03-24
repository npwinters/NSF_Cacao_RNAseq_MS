#---------------------
# This code plots results from the CSE overexpression experiments in the NSF cacao RNA-seq manuscript
# Noah Winters
# Dec, 2021
#--------------------

# Load libraries
library(tidyverse)
library(scales)
library(Hmisc)
library(cowplot)

# Read in data
cse_allPeaks_diffReport <- read.table("CSE_overExpr_CaffAcid_data.txt", header = TRUE )

# Plot results
pdf("output.pdf", 
    width = 6.5, 
    height = 4)

cse_allPeaks_diffReport %>%
  subset(ID == "FT00269") %>% # This is the caffeic acid metabolite
  add_column(Vector = ifelse(grepl("CSE", .$Sample) == TRUE, "CSE", "Empty Vector")) %>%
  #  subset(., mzmed > 179 & mzmed < 180) %>%
  ggplot(., aes(x = Sample, y = Intensity, shape = Vector)) +
  geom_jitter(alpha = 0.7, size = 5, width = 0.1) + 
  stat_summary(fun = "mean", size = 1, shape = 5, show.guide = FALSE) + 
  theme_minimal_grid() +
  labs(x = NULL) +
  scale_x_discrete(labels = c("35S:TcCSE\n 48 hpi", 
                              "35S:TcCSE\n 96 hpi", 
                              "Empty Vector\n 48 hpi",
                              "Empty Vector\n 96 hpi")) +
  theme(
    axis.text.x = element_text(size = 13, color = "black"),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = comma)
  
dev.off()

# Statistical tests
dat <- 
  cse_allPeaks_diffReport %>%
  #  subset(., mzmed > 179 & mzmed < 180) %>%
  subset(ID == "FT00269") %>%
  add_column(Vector = ifelse(grepl("CSE", .$Sample) == TRUE, "CSE", "Empty Vector"))

pairwise.t.test(dat$Intensity, dat$Sample, p.adjust.method ="none")

dat <- 
  cse_allPeaks_diffReport %>%
  subset(ID == "FT00269") %>%
  add_column(Vector = ifelse(grepl("CSE", .$Sample) == TRUE, "CSE", "Empty Vector"))

mean(subset(dat, Sample == "CSE48_Into")$Intensity) - mean(subset(dat, Sample == "EV48_Into")$Intensity)
mean(subset(dat, Sample == "CSE96_Into")$Intensity) - mean(subset(dat, Sample == "EV96_Into")$Intensity)
