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

# How many races each driver won?
# This is a very polluted plot.

plt1 <- ggplot(data=results.by.driver, aes(x=driverRef))
plt1 + geom_bar(fill="DarkBlue",alpha=0.5) + xlab("Driver") + ylab("Number of races won") +
  ggtitle("Number of races won by driver") + scale_y_continuous(breaks=seq(0,300,50)) +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.25))

# But how many races each driver raced?
# This is a much polluted plot, since many drivers haven't
# won anything
plt2 <- ggplot(data=races.by.driver, aes(x=driverRef))
plt2 + geom_bar(fill="DarkBlue",alpha=0.5) + xlab("Driver") + ylab("Number of races") +
  ggtitle("Number of races by each driver") + scale_y_continuous(breaks=seq(0,5000,500)) +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.25), plot.title = element_text(hjust=0.5))

# This is not the best way of comparing driver's quality
# so let's normalize the amount of wins by the amount of races

# Build a new data frame with the amount of wins and amount
# of races by driver. Somewhat dirty implementation

wins.by.winners <- summary(results.by.driver$driverRef,1000)
races.by.winners <- summary(races.by.driver$driverRef,1000)

win.percentage.by.driver = data.frame(wins.by.winners,races.by.winners)
head(win.percentage.by.driver,10)

# Add the driver's name column
win.percentage.by.driver$driverId <- rownames(win.percentage.by.driver)
# Add the percentage column
win.percentage.by.driver$percentage <- 100*(win.percentage.by.driver$wins.by.winners)/(win.percentage.by.driver$races.by.winners)

# Create a new data frame with the win percentage
# for the 20 best drivers

top.drivers <- win.percentage.by.driver[order(win.percentage.by.driver$percentage,decreasing = T),]
top.drivers <- head(top.drivers,20)

# Finally, let's plot it!
plt3 <- ggplot(data=top.drivers, aes(x=reorder(driverId,-percentage), y=percentage))

# Creates a vector with custom colors just to 
# color some bars of the plot
bar.colors <- c("Gold", "Gray", "#B35900", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699"
                , "#006699", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699")

# Plot the results 
plt3 + geom_bar(stat="identity", fill=bar.colors, color="Black",alpha=0.75) + xlab("") +
  ylab("Win ratio (%)") + ggtitle("Win ratio of the top 20 drivers") +
  scale_x_discrete(labels=c("Lee Wallard", "Juan Fangio", "Bill Vukovich", 
                            "Alberto Ascari", "Jim Clark", "Lewis Hamilton",
                            "Michael Schumacher", "Jackie Stewart",
                            "Ayrton Senna", "Alain Prost", "Sebastian Vettel",
                            "Stirling Moss", "Bob Sweikert", "Damon Hill", 
                            "Pat Flaherty", "Nigel Mansell", "Tony Brooks",
                            "Niki Lauda","Nino Farina", "Luigi Fagioli")) +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.25), 
        plot.title = element_text(hjust=0.5))
  
# There is something strange. Juan Manuel Fangio is known for the best win ratio 
# of all time. Checking on historical F1 data, Lee Wallard and Bill Vukovich 
# didn't competed entire seasons, just raced the Indy 500 at the time it was
# part of the F1 Grand Prix season. So, let's filter just drivers with 10
# or more races, roughly a complete season.

top.drivers <- win.percentage.by.driver[order(win.percentage.by.driver$percentage,decreasing = T),]
filter_10races <- (top.drivers$races.by.winners >= 10)

# However, top.drivers is a data frame with 20 entries. Let's recreate 
# the data frame to include 20 drivers, but ignoring those with
# less than 10 race

top.drivers.10races <- top.drivers[filter_10races,]
top.drivers.10races <- head(top.drivers.10races,20)

head(top.drivers.10races,20)

# Once we filtered drivers with at least 10 races, let's plot again
plt4 <- ggplot(data=top.drivers.10races, aes(x=reorder(driverId,-percentage), y=percentage))

plt4 + geom_bar(stat="identity", fill=bar.colors, color="Black",alpha=0.75) + xlab("") +
  ylab("Win ratio (%)") + ggtitle("Win ratio of the top 20 drivers with at least 10 races") +
  scale_x_discrete(labels=c("Juan Fangio", "Alberto Ascari", "Jim Clark",
                            "Lewis Hamilton", "Michael Schumacher", "Jackie Stewart",
                            "Ayrton Senna", "Alain Prost", "Sebastian Vettel",
                            "Stirling Moss", "Bob Sweikert", "Damon Hill", 
                            "Pat Flaherty", "Nigel Mansell", "Tony Brooks",
                            "Niki Lauda","Nino Farina", "Mika Hakkinen",
                            "Nelson Piquet","Fernando Alonso")) +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.25), 
        plot.title = element_text(hjust=0.5))
