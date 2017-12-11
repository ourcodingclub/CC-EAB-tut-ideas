# Do elephants influence the spatial clustering of trees in a dry savannah?
# John Godlee
# johngodlee@gmail.com

# Packages ----
library(ggplot2)
library(dplyr)
library(readr)
library(viridis)

# Set working directory to the directory this file is in! ----
setwd("")

# Import data ----
# Tree locations and diameters
tree_loc_diam_1_2_summ <- read_csv("tree_loc_diam_1_2.csv")

# Plot bounding boxes
plot_bbox_1 <- read_csv("plot_bb_1.csv")  # Elephant plot
plot_bbox_2 <- read_csv("plot_bb_2.csv")  # No elephant plot

# Is there an effect of elephants on spatial clustering of trees? ----
# Split data frames to make two graphs
tree_loc_diam_1_summ <- tree_loc_diam_1_2_summ %>%
	filter(plot == "elephants")

tree_loc_diam_2_summ <- tree_loc_diam_1_2_summ %>%
	filter(plot == "no_elephants")

# Create heatmaps based on density of trees
(elephant_plot <- ggplot(tree_loc_diam_1_summ, aes(x = dec_lon, y = dec_lat)) + 
	stat_density2d(aes(fill = ..level..), geom = "polygon") +
	scale_fill_viridis() + 
	geom_polygon(data = plot_bbox_1, aes(x = dec_lon, y = dec_lat), fill = NA, colour = "black") + 
	geom_point() + 
	xlim(min(plot_bbox_1$dec_lon) - 0.001, max(plot_bbox_1$dec_lon) + 0.001) + 
	ylim(min(plot_bbox_1$dec_lat) - 0.001, max(plot_bbox_1$dec_lat) + 0.001) + 
	xlab("Decimal Longitude") +
	ylab("Decimal Latitude") +
	labs(fill = "Tree Density") +
	theme_classic())

(no_elephant_plot <- ggplot(tree_loc_diam_2_summ, aes(x = dec_lon, y = dec_lat)) + 
	stat_density2d(aes(fill = ..level..), geom = "polygon") +
	scale_fill_viridis() + 
	geom_polygon(data = plot_bbox_2, aes(x = dec_lon, y = dec_lat), fill = NA, colour = "black") + 
	geom_point() + 
	xlim(min(plot_bbox_2$dec_lon) - 0.001, max(plot_bbox_2$dec_lon) + 0.001) + 
	ylim(min(plot_bbox_2$dec_lat) - 0.001, max(plot_bbox_2$dec_lat) + 0.001) + 
	xlab("Decimal Longitude") +
	ylab("Decimal Latitude") +
	labs(fill = "Tree Density") +
	theme_classic())

# Save the plots
ggsave(filename = "elephant_plot.png", plot = elephant_plot)
ggsave(filename = "no_elephant_plot.png", plot = no_elephant_plot)
