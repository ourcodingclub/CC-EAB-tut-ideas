
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



# First we plot the temperature time series:

# Aggregate to get mean per day for each logger. Otherwise the plot gets to messy.
dat2 <- aggregate(data = dat,
                  Temp~unique_ID+Date,
                  FUN = mean)

# then we get mean per date, and also the SD
dat3 <- aggregate(data = dat2,
                  Temp~Date,
                  FUN = function(x) c(mn = mean(x), sd = sd(x)))

head(dat3)
str(dat3)
# Obs, the output from the function doesn't show as columns

dat4 <- do.call(data.frame, dat3)
str(dat4)
# Good



# housekeeping - formatting the date
dat4$Date2 <- as.POSIXct(dat4$Date, format = c("%Y-%m-%d"))



# Plot temperature against date and add 1SD
(temp.series <-  ggplot(data = dat4, 
              aes(x = Date2, 
                  y = Temp.mn)) +
  geom_ribbon(aes(x = Date2, 
                    ymax = Temp.mn+Temp.sd, 
                    ymin = Temp.mn-Temp.sd, alpha = 0.2)) +
  geom_line(size = 1) +
  xlab("Date") +
  ylab(expression(atop(paste("Temperature "), ( degree~C)))) +
  theme_classic() +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "none")
  )



# NEXT: Create a plot of daily temperature fluctuations.
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



# Finally, plot the two figures together and export
ggsave(filename = "Temperature time series and daily fluctuations.pdf",
       plot = grid.arrange(temp.series, temp.fluct, nrow = 2),
             height = 10, width = 10)

ggsave(filename = "temp_timeseries.png",
       plot = grid.arrange(temp.series, temp.fluct, nrow = 2),
       height = 10, width = 10)
