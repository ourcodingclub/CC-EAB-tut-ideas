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

# other example datasets:
# data(puechabonsp)
# data(ibexraw)
# data(capreochiz)
# data(porpoise)
# data(bear)

# Take a look at them: what is what?
View(Dispersion)


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

# the distribution of NAs can be checked using the function 'runsNAltraj'
# data(bear)
# runsNAltraj(bear)

# if the time intervals are not constant, function 'sett0' rounds them to a set value

is.regular(trace2)
is.sd(trace2)     # to check if, in a regular trace, all bursts have the same duration



plot(trace2[5])    # path; start = BLUE, last used observation = RED
plotltr(trace2[5]) # distances

ind <- trace2[5]

# Description of movement:
plotltr(ind,"dist")            # same as before
plot(redisltraj(ind, 50))      # discretize the steps at a certain length; some authors advise to do it when checking angles
plotltr(ind,"sqrt(R2n)")       # net displacement
plotltr(ind,"cos(rel.angle)")  # relative angles

wawotest(ind)                  # test for autocorrelation of x, y and dist: low P values mean strong autocorrelation
acfdist.ltraj(ind, lag = 20, which = "dist") # autocorrelation function (Dray, 2010): the black dots are significantly autocorrelated  at 95%
testang.ltraj(ind, "relative") # test for autocorrelations of relative angles
acfang.ltraj(ind, lag = 20)    # visual representation of the autocorrelation of angles



# ==== EXPORT FOR MORE ANALYSIS ====

ind1 <- ld(ind)    # from ltraj to dataframe
View(ind1)
str(ind1)
summary(ind1)

dist_1 <- cumsum(ind1$dist)
length(dist_1)
dist_1[59] # last number: total distance covered



#  LOOP =======================

l <- length(trace2)

info <- data.frame(ID = numeric(0),
                   Dist_tot = numeric(0),
                   Dist_mean = numeric(0),
                   Dist_var = numeric(0),
                   Angle_mean = numeric(0),
                   Angle_Var = numeric(0))
for (i in 1:l) {
  ind <- trace[i]
  ind <- ld(ind)                            # from list to data frame; the opposite is 'ld'
  ID <- as.numeric(levels(ind$id[1]))
  dist_tot <- as.numeric(cumsum(ind$dist)[59])
  dist_mean <- as.numeric(mean(ind$dist, na.rm = TRUE))
  dist_var <- as.numeric(var(ind$dist, na.rm = TRUE))
  angle_mean <- as.numeric(mean(ind$rel.angle, na.rm = TRUE))
  angle_var <- as.numeric(var(ind$rel.angle, na.rm = TRUE))
  data <- c(ID, dist_tot, dist_mean, dist_var, angle_mean, angle_var)
  info <- rbind(info, data)
}

rm(data, ind, ID, dist_tot, dist_mean, dist_var, angle_mean, angle_var, i, l)

names(info) <- c("ID", "Dist_tot", "Dist_mean", "Dist_var", "Angle_mean", "Angle_var")

info$Treat <- substr(info$ID, 1,1)
info$Bridge <- substr(info$ID, 2,2)
info$was.disp <- substr(info$ID, 3,3)

# =============== END OF THE LOOP

View(info)

# =========== ANALYSIS AND PLOTS ====================

boxplot(info$Dist_tot ~ info$Treat)
boxplot(info$Dist_tot ~ info$Treat*info$was.disp)
ggplot(info, aes(x = Treat, y = Dist_tot)) + 
  geom_boxplot(aes(fill=was.disp)) + 
  theme_classic()

mod <- lmer(Dist_tot~Treat*was.disp+(1|Treat:Bridge), data = info)
summary(mod)
plot(residuals(mod))

info_res <- subset(info, was.disp == 0)
mod2 <- lmer(Dist_tot~Treat+(1|Treat:Bridge), data = info_res)

info_disp <- subset(info, was.disp == 1)
mod3 <- lmer(Dist_tot~Treat+(1|Treat:Bridge), data = info_disp)


# ==== FANCY STUFF ====

sliwinltr(redisltraj(ind, 50), function(x) mean(cos(x$rel.angle)), type="locs", step=5)
# something a bit more fancy: a sliding window approach (needs a discrete series)
# smoothened values close to 0.5 mean a tortuos trajectory; closer to 1 means more linear

# other fancy stuff: interactive graph
trajdyn(trace2, burst = attr(trace2[[5]], "burst"), hscale = 1, vscale = 1, recycle = TRUE)
