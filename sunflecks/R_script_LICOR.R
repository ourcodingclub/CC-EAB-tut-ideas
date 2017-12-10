# In this tutorial, we learn to work with pipes and other dplyr functions, multipanel plots and plots with two Y-axes (ggplot2 package). 
# We will apply these functionalities to explore the importance of sunflecks for carbon assimilation in an understorey herb 
# based on a LICOR dataset (leaf-level gas exchange measurements) with a temporal resolution of 5 seconds, gathered on a sunny day in June 2017. 
# Measurements include, amongst other variables, assimilation rate ('Photo)' and incoming photosynthetic active radiation (PAR) ('PARi').
# After quantifying the importance of sunflecks we will visually explore time lags between assimilation rates and incoming irradiation.
                                                                      
# Load packages
library(dplyr)
library(ggplot2)
library(Rmisc)

# Load dataset
setwd("path_to_data")
dataset <- read.csv('LICOR.csv')

# Reformat time notation
dataset <- mutate(dataset, Time = strptime(HHMMSS, format = '%H:%M:%S') - 13*60)

# Explore column names
glimpse(dataset)


# Task 1: quantify the relative importance of sunflecks in daily incoming PAR and daily carbon assimilation, make use of dplyr pipes
#----------------------------------------------------------------------------------------------------------------------------------

# Sunflecks are brief increases in solar irradiance that occur in understories 
# of an ecosystem when sunlight is able to directly reach the ground. 

# Label sunflecks in additional column
dataset <- mutate(dataset, sunfleck = ifelse(dataset$PARi > 12, "S", ""))

# Percentage of time within sunfleck - do we need these? The numbers are not used later on.
dataset %>% filter(sunfleck == "S") %>% nrow %>% `/`(nrow(dataset))
# Relative PAR received during sunfleck periods
dataset %>% filter(sunfleck == "S") %>% dplyr::select(PARi) %>% sum %>% `/`(sum(dataset$PARi))
#relative carbon assimilation during sunfleck periods
dataset %>% filter(sunfleck == "S") %>% dplyr::select(Photo) %>% sum %>% `/`(sum(dataset$Photo))

# Task 2: Overlay assimilation rate and incoming PAR graphs to explore time lags in the photosynthetic response
#-------------------------------------------------------------------------------------------------------------
# Select a time interval with some clear sunflecks
dataset_subset <- filter(dataset,format(`Time`, "%H:%M") >= "11:00" & format(`Time`, "%H:%M") <= "13:00")

(par_plot <- ggplot(dataset_subset, aes(x = Time)) + 
  geom_line(aes(y = PARi, colour = "PAR"), size = 1) + 
  # rescale to improve visual interpretation
  geom_line(aes(y = Photo*60, colour = "Assimilation rate"), size = 1) + 
  # make sure to rescale second axes as well
  scale_y_continuous(sec.axis = sec_axis( ~./60 , name = (bquote('Assimilation ('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')')))) + 
  labs(x = "Time", y = (bquote('PAR ('*mu~ 'mol' ~ m^-2~s^-1*')')), colour = "Parameter") + 
  theme_classic() +
  theme(legend.position = c(0.2, 0.8)))

# Save plot
ggsave(par_plot, filename = "PAR_assimilation.png", width = 8, height = 5)

# Task 3: Visualise the trajectory of of assimilation rates within one selected sunfleck
#--------------------------------------------------------------------------------------
# Label individual sunflecks (fast implementation, probably lots of other, more efficient options here)
sunfleckID = 1
dataset$Sunfleck_ID = 0
for (r in (1:(nrow(dataset) - 1))){
  if(dataset$sunfleck[r+1] == "S"){
    if(dataset$sunfleck[r] == "S"){
      dataset$Sunfleck_ID[r + 1] <- sunfleckID
    }
    else{
      sunfleckID = sunfleckID + 1
      dataset$Sunfleck_ID[r + 1] <- sunfleckID
    }
  }
}

# Plot the trajectories 
# (left pane: changes in PAR during one sunfleck, right pane: photosynthetic response during one sunfleck, use colours to indicate time)
sunfleck_data <- filter(dataset, Sunfleck_ID == 27)

(irradiation <- ggplot(sunfleck_data, aes(x = Time, y = PARi)) + 
  geom_line(size = 1) + 
  labs(x ="Time", y = "Photosynthetic active radiation [?mol/m?.s]") +
  theme_classic() +
  ggtitle("(a) Irradiation\n"))

(photosynthesis <- ggplot(sunfleck_data, aes(x = PARi, y = Photo, colour = Time)) + 
  geom_point() + 
  labs(x = "PAR [?mol/m?.s]", y = "Assimilation rate [?mol CO2/m?.s]") + 
  ggtitle("(b) Photosynthesis\n") +
  theme_classic() +
  theme(legend.justification = c(1,-0.05), legend.position = c(0.98,0)))

(panel <- multiplot(irradiation, photosynthesis, cols = 2))
ggsave(panel, filename = "PAR_panel.png", width = 10, height = 5)
