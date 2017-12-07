#
# Transform and summarize data frames #
#

# GOAL: To transform a dataframe from wide to long format in order to summarize and graph the data

# Raw data: Measurements of several plant functional traits (SLA, Leaf area, LDMC, Leaf fresh mass, and Leaf dry mass) on multiple individuals of four different species

# --- #

# Get data

load("TraitData_CodingClub.RData")

# Wide data is useful for comparing within-individual correlations
# Trait-trait correlation plot

library(corrplot)

(correlation <- corrplot(cor(data[,2:6], use = "pairwise.complete.obs")))

# Save the plot in your working directory
png(filename = "trait_correlation.png", width = 600, height = 600)
(correlation <- corrplot(cor(data[,2:6], use = "pairwise.complete.obs")))
dev.off()

# Convert wide data to long for summarizing and graphing

library(reshape2)

dlong <- melt(data, id = "SpeciesName", variable.name = "Trait")

# Summarize trait data to get mean, max, min, range and quantiles

library(plyr)

dsumm <- ddply(dlong, c("SpeciesName","Trait"), summarise,
               mean = mean(value, na.rm = T),
               max = max(value, na.rm = T),
               min = min(value, na.rm = T),
               q2.5 = quantile(value, 0.025),
               q97.5 = quantile(value, 0.975),
               range = max-min)

# Graph raw trait data behind mean +/- 95% CI's and save the file

(trait.plot <- ggplot()+
  geom_point(data = dlong, mapping = aes(x = SpeciesName, y = value, colour = Trait), alpha = 0.1) +
  geom_errorbar(data = dsumm, mapping = aes(x = SpeciesName, ymin = q2.5, ymax = q97.5, group = Trait), width = 0.3) +
  geom_point(data = dsumm, mapping = aes(x = SpeciesName, y = mean, group = Trait), size = 4, colour = "black") +
  geom_point(data = dsumm, mapping = aes(x = SpeciesName, y = mean, colour = Trait), size = 3) +
  facet_wrap(~Trait, scales = "free_y")+
  theme_classic() +
  scale_x_discrete(labels = c("Dryas", "Eriophorum", "Oxyria", "Salix")) +
  ylab("Trait Value") +
  xlab("Species"))

# We can save plots made using ggplot2 with ggsave, which is just one line of code
ggsave(trait.plot, filename = "traits.png", height = 5, width = 10)
