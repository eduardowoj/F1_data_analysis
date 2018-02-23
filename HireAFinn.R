getwd()
setwd("/home/eduardo/Documents/UFSC/R/F1")
library(ggplot2)
library(jpeg)
library(grid)

# Load some data frames
constructors <- read.csv("constructors.csv")
drivers <- read.csv("drivers.csv")
driverStandings <- read.csv("driverStandings.csv")
races <- read.csv("races.csv")
results <- read.csv("results.csv")

# Some inspection
head(results,150)
str(drivers)

# Filter just each winner
filter_winner <- (results$positionText=="1")
winners <- results[filter_winner,]

# Merging two data frames to obtain a new data frame
# with just the winners and their nationalities
results.by.driver <- merge(x=winners[,c("raceId", "driverId","position")], y=drivers[,c("driverId","driverRef","nationality")], by="driverId")
races.by.driver <- merge(x=results[,c("raceId","driverId","position")], y=drivers[,c("driverId","driverRef","nationality")], by="driverId")

head(drivers)
# Inspect the new data frame
head(results.by.driver,10)

# How many races each country won?
plt1 <- ggplot(data=results.by.driver, aes(x=nationality))
plt1 + geom_bar(fill="DarkBlue",alpha=0.5) + xlab("Nationality") + ylab("Number of races won") +
  ggtitle("Number of races won by nationality") + scale_y_continuous(breaks=seq(0,300,50)) +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.25))

# But how many times drivers from each country raced?
plt2 <- ggplot(data=races.by.driver, aes(x=nationality))
plt2 + geom_bar(fill="DarkBlue",alpha=0.5) + xlab("Nationality") + ylab("Number of races won") +
  ggtitle("Number of races won by nationality of the driver") + scale_y_continuous(breaks=seq(0,5000,500)) +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.25), plot.title = element_text(hjust=0.5))

# Obviously there are a lot of races won by British drivers because
# there are lots of races raced by British drivers. Let's normalize,
# then, the amount of wins by the amount of races by nationality

# Build a new data frame with the amount of wins and amount
# of races by country. Somewhat dirty implementation

wins.by.country <- summary(results.by.driver$nationality)
races.by.country <- summary(races.by.driver$nationality)

win.percentage.by.country = data.frame(wins.by.country,races.by.country)
# Add the nationality column
win.percentage.by.country$nationality <- rownames(win.percentage.by.country)
# Add the percentage column
win.percentage.by.country$percentage <- 100*(win.percentage.by.country$wins.by.country)/(win.percentage.by.country$races.by.country)
# Remove countries with no wins (that came from the races.by.country)
filter2 <- win.percentage.by.country$wins.by.country > 0
win.percentage.by.country <- win.percentage.by.country[filter2,]
head(win.percentage.by.country) # Uncomment to check the new data frame

# Plot the amount of percentage of won races by country

plt3 <- ggplot(data=win.percentage.by.country, aes(x=reorder(nationality,-percentage), y=percentage))
plt3 <- plt3 + geom_bar(stat="identity", fill="#8cd98c", color="Black") + xlab("Nationality") + ylab("Win ratio (%)") + ggtitle("Number of races won by nationality") +
  ylim(0,11.9) + theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.25), plot.title = element_text(hjust=0.5)) 

# Call the flags to superimpose

arg <- readJPEG("./flags/argentina.jpg")
ger <- readJPEG("./flags/germany.jpg")
grb <- readJPEG("./flags/uk.jpg")
ast <- readJPEG("./flags/austria.jpg")
can <- readJPEG("./flags/canada.jpg")
col <- readJPEG("./flags/colombia.jpg")
aus <- readJPEG("./flags/australia.jpg")
bra <- readJPEG("./flags/brazil.jpg")
fin <- readJPEG("./flags/finland.jpg")
spa <- readJPEG("./flags/spain.jpg")
saf <- readJPEG("./flags/south-africa.jpg")
nze <- readJPEG("./flags/new-zealand.jpg")
fra <- readJPEG("./flags/france.jpg")
usa <- readJPEG("./flags/usa.jpg")
swe <- readJPEG("./flags/sweden.jpg")
bel <- readJPEG("./flags/belgium.jpg")
swi <- readJPEG("./flags/switzerland.jpg")
pol <- readJPEG("./flags/poland.jpg")
ita <- readJPEG("./flags/italy.jpg")
ven <- readJPEG("./flags/venezuela.jpg")
net <- readJPEG("./flags/netherlands.jpg")
mex <- readJPEG("./flags/mexico.jpg")


# Plot with flags
plt3 + annotation_raster(arg, ymin = 10.5,ymax=11,xmin=0.5,xmax=1.5) +
  annotation_raster(ger, ymin = 8.0,ymax=8.4,xmin=1.6,xmax=2.4) +
  annotation_raster(grb, ymin = 6.5,ymax=6.9,xmin=2.6,xmax=3.4) +
  annotation_raster(ast, ymin = 6.0,ymax=6.4,xmin=3.6,xmax=4.4) +
  annotation_raster(can, ymin = 6.0,ymax=6.35,xmin=4.6,xmax=5.4) +
  annotation_raster(col, ymin = 5.7,ymax=6.1,xmin=5.6,xmax=6.4) +
  annotation_raster(aus, ymin = 5.7,ymax=6.1,xmin=6.6,xmax=7.4) +
  annotation_raster(bra, ymin = 5.3,ymax=5.7,xmin=7.6,xmax=8.4) +
  annotation_raster(fin, ymin = 5.2,ymax=5.6,xmin=8.6,xmax=9.4) +
  annotation_raster(spa, ymin = 5.0,ymax=5.4,xmin=9.6,xmax=10.4) +
  annotation_raster(saf, ymin = 4.9,ymax=5.3,xmin=10.6,xmax=11.4) +
  annotation_raster(nze, ymin = 3.3,ymax=3.7,xmin=11.6,xmax=12.4) +
  annotation_raster(fra, ymin = 3.0,ymax=3.4,xmin=12.6,xmax=13.4) +
  annotation_raster(usa, ymin = 2.7,ymax=3.1,xmin=13.6,xmax=14.4) +
  annotation_raster(swe, ymin = 2.55,ymax=2.95,xmin=14.6,xmax=15.4) +
  annotation_raster(bel, ymin = 2.0,ymax=2.4,xmin=15.6,xmax=16.4) +
  annotation_raster(swi, ymin = 1.5,ymax=2.1,xmin=16.6,xmax=17.4) +
  annotation_raster(pol, ymin = 1.4,ymax=1.8,xmin=17.6,xmax=18.4) +
  annotation_raster(ita, ymin = 1.35,ymax=1.75,xmin=18.6,xmax=19.4) +
  annotation_raster(ven, ymin = 0.9,ymax=1.3,xmin=19.6,xmax=20.4) +
  annotation_raster(net, ymin = 0.9,ymax=1.3,xmin=20.6,xmax=21.4) +
  annotation_raster(mex, ymin = 0.7,ymax=1.1,xmin=21.6,xmax=22.4) 

