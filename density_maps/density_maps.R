#########################################################
# Density maps of red squirrel occurrences
# Francesca Mancini
# Date created: 2017-11-19
# Date modified: 
#########################################################

# Load required packages
library(rgbif)
library(raster)
library(sp)
library(ggplot2)
library(viridis)

# Download occurrence data from the GBIF website through R
# This takes a while to download, as we are getting 25000 occurrences
# Here is the code just so that you know how we got the data
# You can load the RData file we have prepared in advance and try out downloading from GBIF later on

# Retrieve isocode for UK
# UK_code <- isocodes[grep("United Kingdom", isocodes$name), "code"]

# Search for red squirrel occurrences in GBIF from 2005 to 2010
# Download only occurrences that have geographic coordinates and up to 25000 occurrences
# and return data, this takes a few minutes
# occur <- occ_search(scientificName = "Sciurus vulgaris", country = UK_code, 
#                    hasCoordinate = TRUE, limit = 25000, year = '2005,2010', return = "data")

# Load data
load("occur.RData")

# Explore data
str(occur)

# Download shapefile of UK boundaries
UK <- raster::getData("GADM", country = "GB", level = 0)  
# there is a function with the same name in the nlme package, so specify that you want the one from the raster package

# Make the occurrences data spatial
coordinates(occur) <- c("decimalLongitude", "decimalLatitude")

# Assign coordinate system
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(occur) <- crs.geo                             # assign the coordinate system

# Project both spatial objects to utm
UK_proj <- spTransform(UK, CRS("+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
occurr_proj <- spTransform(occur, CRS("+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

# Transform data in ggplot2 format 
UK.Df <- fortify(UK_proj, region = "ID_0")
occurr.points <- fortify(cbind(occurr_proj@data, occurr_proj@coords))

# Plot occurrences
(plot.occurr <- ggplot(data = occurr.points, aes(x = decimalLongitude, y = decimalLatitude)) +  # plot the data
    geom_polygon(data = UK.Df,aes(x = long, y = lat, group = group),                             # plot the UK
                 color = "black", fill = "gray82") + coord_fixed() +    # coord_fixed() ensures that one unit on the x-axis is the same length as one unit on the y-axis
    geom_point(color = "black", shape = "."))               # graphical parameters for points

# Plot density map
(plot.density <- plot.occurr +                   
    stat_density2d(aes(x = decimalLongitude,                           # create the density layer based on where the points are
                       y = decimalLatitude,  
                       fill = ..level.., alpha = ..level..),           # colour and transparency depend on density
                   geom = "polygon") +                                 # graphical parameters for the density layer
    scale_fill_viridis(breaks = c(0.00000000002, 0.00000000005, 0.00000000008),  # set colour palette for density layer
                       labels = c("Low", "Medium", "High")) +          # specify scale breaks and labels                                  
    scale_alpha(range = c(0.6, 1), guide = FALSE) +                    # set transparency for the density layer 
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),   # don't display x and y axes labels, titles and tickmarks 
          axis.ticks.x = element_blank(), axis.title.y = element_blank(),   
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          text = element_text(size = 18),                                # size of text 
          panel.grid.major = element_blank(),                            # eliminates grid lines from background
          panel.background = element_blank()) +                          # set white background
    labs(fill = "Density\n"))

# Save map
ggsave(plot.density, filename = "density_rs.pdf", width = 5, height = 5)
ggsave(plot.density, filename = "density_rs.png", width = 5, height = 5)
