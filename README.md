# If you want to win, hire a Finn. Is this right?

Quick and dirty analysis of the Formula 1 dataset from Kaggle. This my first attempt at using R for anything, so I decided to give motorsports a go and tried to check whether the maxim "if you want to win, hire a Finn" is true.

The dataset was pulled from Kaggle, and comprises loads of information from the 50's to 2017: Lap times, race winners, top speeds, you name it.

On this quick analysis, I filtered the data to select just the winners from each race, cross-matched the drivers IDs to their names and nationalities and normalized the amount of wins by the total amount of races for each country.

It turns out that the popular saying isn't true at all! For all the countries, Finland is the 9th best source of drivers! In the first place, however, a big surprise: Despite Michael Schumacher's meteoric career, Argentina has the best win ratio (in %) of all countries! However, this is clearly an outlier, because despite having 25 drivers racing in Formula 1 throughout the ages, the only   truly successful Argentinian driver was Juan Emanuel Fangio, which won more than half of his races, pushing Argentina's score way higher.

In this repository there are all the files needed to run the code and produce the plot.

Running the code, you will notice Argentina is in the top spot. To check why this (even though if you already know it's due to Fangio's stellar career), run the bestdrivers.r script. On this script we build a plot for checking the drivers with the best win ratio.
