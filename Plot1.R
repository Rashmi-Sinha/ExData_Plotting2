
+library(ggplot2)
+library(utils)
+library(plyr)
+
  +# Read the data
  +datafileName <- "summarySCC_PM25.rds"
+classfileName <- "Source_Classification_Code.rds"
+
  +# See if we have the main data file and if not, download them and uzip
  +if (! file.exists(datafileName)) {
    +  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip" 
    +  destfile <- "exdata-data-NEI_data.zip"
    +  # only download if the zip file not in local directory
      +  if (! file.exists(destfile)) download.file(url, destfile = destfile, mode = "wb")
    +  unzip(destfile)
    +}
+# Read the two data files.  NEI has the data and SCC has infomration about the SCC code
  +# for each observation.
  +NEI <- readRDS(datafileName)
+SCC <- readRDS(classfileName)
+# We will remove "." and "_" in SCC names to keep it tidy
  +names(SCC) <- gsub("[._]","",names(SCC))
+
  +# Make many columns factors since we don't need to process them as strings or numeric
  +colsToConvert <- names(NEI) != "Emissions"
+NEI[ ,colsToConvert] <- lapply(NEI[ ,colsToConvert], factor)
+
  +# Basic Stats
  +dim(NEI)
+dim(SCC)
+summary(NEI)
+unique(NEI$year)
+
  +# See if we have any missing data
  +sum(is.na(NEI))
+# See if we have any emissions < 0 which would be inaccurate
  +sum((NEI$Emissions<0))
+
  +
  +# Plot 1
  +# Calculate the total emissions by year
  +TotalEmissions <- ddply(NEI, "year", summarize, Total_Emissions=sum(Emissions))
+
  +# Build an empty plot and then add attributes to it
  +
  +with(TotalEmissions, {
    +  bp <- barplot(names.arg=year, Total_Emissions, 
                     +                main = bquote("US Total PM"[2.5]~"Emissions by Year"),
                     +                ylab = bquote("PM"[2.5]~"emissions (tons)"),
                     +                xlab = "Year",
                     +                ylim = c(0, max(Total_Emissions)*1.05))  # simple scale to look better
    +  # Note bp now holds the centers of each bar chart x axis
      +  lines(bp, Total_Emissions, col = "blue", lwd = 2)
    +  points(bp, Total_Emissions, col = "blue", pch = "X", lwd =2)
    +})
