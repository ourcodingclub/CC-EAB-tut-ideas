# Packages ----
library(ggplot2)
library(dplyr)
library(readr)

# Set working directory ----
setwd("/Users/johngodlee/Downloads/congo_diam")

# Import data ----
# Tree locations and diameters
tree_loc_diam_1_2_summ <- read.csv("tree_loc_diam_1_2.csv")

# Plot bounding boxes
plot_bbox_1 <- read_csv("plot_bb_1.csv")
plot_bbox_2 <- read_csv("plot_bb_2.csv")

# Plot tree dbh as point size ----
ggplot(tree_loc_diam_1_2_summ, aes(x = dec_lon, 
																	 y = dec_lat, 
																	 size = dbh_cm, 
																	 colour = species)) + 
	geom_point(alpha = 0.5) + 
	theme(aspect.ratio = 1) +  # coord_map() doesn't work with facet_wrap()
	facet_wrap(~ plot, scales = "free") 

# Is there an effect of elephants on tree morphology? ----
# dbh boxplots
ggplot(tree_loc_diam_1_2_summ, aes(x = plot, y = dbh_cm)) + 
	geom_boxplot()

# height boxplots
ggplot(tree_loc_diam_1_2_summ, aes(x = plot, y = height_m)) + 
	geom_boxplot()

# Is there an effect of elephants on spatial clustering of trees? ----
# Split data frames
tree_loc_diam_1_summ <- tree_loc_diam_1_2_summ %>%
	filter(plot == "elephants")

tree_loc_diam_2_summ <- tree_loc_diam_1_2_summ %>%
	filter(plot == "no_elephants")

# Create heatmaps based on density of trees
elephant_plot <- ggplot(tree_loc_diam_1_summ, aes(x = dec_lon, y = dec_lat)) + 
	stat_density2d(aes(fill = ..level..), geom = "polygon") +
	scale_fill_gradient(low = "blue", high = "green") + 
	geom_polygon(data = plot_bbox_1, aes(x = dec_lon, y = dec_lat), fill = NA, colour = "black") + 
	geom_point() + 
	xlim(min(plot_bbox_1$dec_lon) - 0.001, max(plot_bbox_1$dec_lon) + 0.001) + 
	ylim(min(plot_bbox_1$dec_lat) - 0.001, max(plot_bbox_1$dec_lat) + 0.001) + 
	xlab("Decimal Longitude") +
	ylab("Decimal Latitude") +
	theme_classic() 

no_elephant_plot <- ggplot(tree_loc_diam_2_summ, aes(x = dec_lon, y = dec_lat)) + 
	stat_density2d(aes(fill = ..level..), geom = "polygon") +
	scale_fill_gradient(low = "blue", high = "green") + 
	geom_polygon(data = plot_bbox_2, aes(x = dec_lon, y = dec_lat), fill = NA, colour = "black") + 
	geom_point() + 
	xlim(min(plot_bbox_2$dec_lon) - 0.001, max(plot_bbox_2$dec_lon) + 0.001) + 
	ylim(min(plot_bbox_2$dec_lat) - 0.001, max(plot_bbox_2$dec_lat) + 0.001) + 
	xlab("Decimal Longitude") +
	ylab("Decimal Latitude") +
	theme_classic() 

ggsave(filename = "img/elephant_plot.png", plot = elephant_plot)
ggsave(filename = "img/no_elephant_plot.png", plot = no_elephant_plot)
