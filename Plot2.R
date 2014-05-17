## In this assignment we analyze data on fine particulate matter (PM2.5)
## and develop a set of graphs in png format displaying 
## The data was provided as part of the Coursera Course Exploratory
## Data Analaysis from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
## which was originally downloaded on May 9, 2014.
## The data was collected from the EPA for the years 1999, 2002, 2005 and 2008.
## See README.md in the repository for further information and references about the
## data and background.

library(ggplot2)
library(utils)
library(plyr)

# Read the data
datafileName <- "summarySCC_PM25.rds"
classfileName <- "Source_Classification_Code.rds"

# See if we have the main data file and if not, download them and uzip
if (! file.exists(datafileName)) {
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip" 
  destfile <- "exdata-data-NEI_data.zip"
  # only download if the zip file not in local directory
  if (! file.exists(destfile)) download.file(url, destfile = destfile, mode = "wb")
  unzip(destfile)
}

# Read the two data files.  NEI has the data and SCC has infomration about the SCC code
# for each observation.
NEI <- readRDS(datafileName)
SCC <- readRDS(classfileName)
# We will remove "." and "_" in SCC names to keep it tidy
names(SCC) <- gsub("[._]","",names(SCC))

# Make many columns factors since we don't need to process them as strings or numeric
colsToConvert <- names(NEI) != "Emissions"
NEI[ ,colsToConvert] <- lapply(NEI[ ,colsToConvert], factor)

# Basic Stats
dim(NEI)
dim(SCC)
summary(NEI)
unique(NEI$year)

# See if we have any missing data
sum(is.na(NEI))
# See if we have any emissions < 0 which would be inaccurate
sum((NEI$Emissions<0))
# Plot 2
BCNEI <- subset(NEI, fips == "24510")
BCTotalEmissions <- ddply(BCNEI, "year", summarize, Total_Emissions=sum(Emissions))

# Build an empty plot and then add attributes to it

with(BCTotalEmissions, {
  bp <- barplot(names.arg=year, Total_Emissions, 
                main = bquote("Baltimore City Total PM"[2.5]~"Emissions by Year"),
                ylab = bquote("PM"[2.5]~"emissions (tons)"),
                xlab = "Year",
                ylim = c(0, max(Total_Emissions)*1.05)) # simple scale to look better
  # Note bp now holds the centers of each bar chart x axis
  lines(bp, Total_Emissions, col = "blue", lwd = 2)
  points(bp, Total_Emissions, col = "blue", pch = "X", lwd =2)
})
