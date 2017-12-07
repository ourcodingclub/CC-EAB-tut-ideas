
# -----------------------------------#

# Visualising time series in GGPLOT2 #

# -----------------------------------#

# Goal: produce a line graph or time series plot with the mean daily temperature
# pluss erros using GGPLOT2. Similarly produce a second graph of daliy temperature 
# fluctuations. Finally, plot and save the two figures together.


# Libraries
library(ggplot2)
library(gridExtra)

# Raw data: 
# Temperature from June 2016 to Jan 2017 from 36 unique loggers


# Get data

setwd("your_filepath")
dat <- read.csv("temperatureTimeSeries.csv")


length(unique(dat$unique_ID)) # Good - 36 loggers


# Create a plot of daily temperature fluctuations.
daily <- dat

# first we get the max and min values per day and per logger
daily2 <-   aggregate(data = daily,
              Temp~unique_ID+Date,
              FUN = function(x) c(max = max(x), min = min(x)))
daily2 <- do.call(data.frame, daily2)


# them we calculate the difference
daily2$diff <- daily2$Temp.max-daily2$Temp.min

daily3 <- aggregate(data = daily2,
                    diff~Date,
                    FUN = mean)


# housekeeping - formatting the date and time zone
daily3$Date2 <- as.POSIXct(daily3$Date, format="%Y-%m-%d", tz = "GMT")  
head(daily3)


# Make the plot
(temp.fluct <-  ggplot(data = daily3, 
                   aes(x = Date2, 
                       y = diff)) +
    geom_line(size = 1)+
    geom_smooth(method = "loess", span = 0.2, colour = "darkred", fill = "darkred") +
    xlab("Date") +
    ylab(expression(atop(paste("Daily Temperature Fluctuations"), ( degree~C)))) +
    theme_classic() +
    theme(text = element_text(size = 15)) +
    theme(legend.position = "none")
)



# Save figure
ggsave(filename = "Temperature daily fluctuations.pdf",
       plot = temp.fluct, height = 5, width = 5)

ggsave(filename = "temp_fluctuations.png",
       plot = temp.fluct, height = 5, width = 5)
