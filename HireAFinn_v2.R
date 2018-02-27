getwd()
setwd("/home/eduardo/Documents/UFSC/R/F1")
library(ggplot2)

# Load some data frames
drivers <- read.csv("drivers.csv")
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


# Creates a vector with custom colors just to 
# color some bars of the plot
bar.colors <- c("Gold", "Gray", "#B35900", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699"
                , "#006699", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699", "#006699")

# Plot the amount of percentage of won races by country

plt3 <- ggplot(data=win.percentage.by.country, aes(x=reorder(nationality,-percentage), y=percentage))
plt3 + geom_bar(stat="identity", fill=bar.colors, color="Black") + xlab("Nationality of the driver") + ylab("Win ratio (%)") +
  ggtitle("Win ratio of each nationality") +
  ylim(0,10.5) + theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.25), plot.title = element_text(hjust=0.5)) 


