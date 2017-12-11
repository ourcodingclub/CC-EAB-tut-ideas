#==============================================#
# SPATIAL MOVEMENT: VISUALIZATION AND ANALYSIS #
#==============================================#

# goal: handle a time series of spatial coordinates; extract useful informations; plot them

# data: a series of coordinates for individuals from different setups

library(adehabitatLT)
library(lme4)
library(nlme)
library(car)
library(ggplot2)

# Grab your data
Dispersion <- read.csv("Dispersion.txt", sep="")

# Take a look at them: what is what?
View(Dispersion)

# The individuals represent mites from different treatments that were put on a plastic arena 
# and monitored for 10 minutes to highlight differences in their movement patterns

# create a variable with the date sequence
date <- as.POSIXct(strptime(as.character(Dispersion$time), format = "%s"),tz = "GMT")

# create a trace file with all the coordinate sequences: take a good look at the syntax
trace <- as.ltraj(xy=Dispersion[,c("X", "Y")], date = date, id = Dispersion$ID)

trace

is.regular(trace)

summ1 <- summary.ltraj(trace)
subset(summ1, nb.reloc < 60)

# irregular burst: add missing values

refda <- strptime("2017-01-01", "%Y-%m-%d", tz = "GMT") # set a reference date

trace2 <- setNA(trace, refda, 1, units = "day") # check if every value is separated from refdate by a multiple of our unit

is.regular(trace2)
is.sd(trace2)     # to check if, in a regular trace, all bursts have the same duration

# Visualise path
plot(trace2[5])    # path; start = BLUE, last used observation = RED

ind <- trace2[5]

# Check for autocorrelation and visualise results
wawotest(ind)                  # test for autocorrelation of x, y and dist: low P values mean strong autocorrelation
acfdist.ltraj(ind, lag = 20, which = "dist") # autocorrelation function (Dray, 2010): the black dots are significantly autocorrelated  at 95%
