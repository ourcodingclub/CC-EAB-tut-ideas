# Downloading and mapping species occurrence data from GBIF
# Gergana Daskalova
# gndaskalova@gmail.com

# Set working directory ----

# Load libraries ----
library(rgdal)
library(maps)
library(maptools)
library(geosphere)
library(ggplot2)
library(ggthemes)
library(mapdata)
library(rgbif)

# Download data from GBIF ----

# Here is the code so that you know how to do it, but please don't run it now as it takes a while
# We have downloaded the data in advance and saved it as a .RData file
# arctic_fox <- occ_search(scientificName = "Vulpes lagopus", limit = 20000, hasCoordinate = TRUE, return = "data")

load("arctic_fox.Rdata")

# Make a map ----
# Get global spatial data
world <- map_data("world")

# Plot top down view of the world and arctic fox occurrence
(map <- ggplot(world, aes(x = long, y = lat, group = group)) + 
    geom_polygon(fill = '#d3dddc', colour = 'gray45', size = 0.1) + 
    theme(panel.background = element_rect(fill = 'white'),
          axis.line = element_line(color = NA), 
          axis.text = element_blank(), axis.ticks = element_blank(), 
          axis.title = element_blank()) + 
    coord_map("ortho", orientation = c(90, 0, 0), ylim = c(0, 90)) + 
    geom_point(data = arctic_fox, aes(x = decimallongitude, y = decimallatitude, group = NULL), 
               fill = "#5bbcd6", colour = "black", shape = 21, size = 4, alpha = 0.6))

# Save map ----
ggsave(map, filename = "fox_map.png", width = 10, height = 10)
ggsave(map, filename = "fox_map.pdf")

# Turn the world around to reveal some unexpected arctic fox occurrences, possible errors in GBIF
(map2 <- ggplot(world, aes(x = long, y = lat, group = group)) + 
    geom_polygon(fill = '#d3dddc', colour = 'gray45', size = 0.1) + 
    theme(panel.background = element_rect(fill = 'white'),
          axis.line = element_line(color = NA), 
          axis.text = element_blank(), axis.ticks = element_blank(), 
          axis.title = element_blank()) + 
    coord_map("ortho", orientation = c(0, -60, 0)) + 
    geom_point(data = arctic_fox, aes(x = decimallongitude, y = decimallatitude, group = NULL), 
               fill = "#5bbcd6", colour = "black", shape = 21, size = 4, alpha = 0.6))

# Interesting arctic fox sightings in South America and in the South Atlantic ocean!

# Save map ----
ggsave(map2, filename = "fox_map2.png", width = 10, height = 10)
ggsave(map2, filename = "fox_map2.pdf")
