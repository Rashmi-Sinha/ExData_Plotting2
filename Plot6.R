# read RDS files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#extract Baltimore City (fips: 24510) and LA (fips: 06037) motor vehicle emissions
motorEmi = subset(NEI, (type=="ON-ROAD" & (fips=="24510" | fips=="06037")))

#total emissions in 1999, 2002, 2005, 2008
totalEmi = aggregate(Emissions ~ year + fips, data=motorEmi, sum)

# Which city has seen greater changes over time in motor vehicle emissions?
# Strategy: calculate the absolute change in each period a) 1999 - 2002; b) 2002 - 2005; c) 2005 - 2009

# create a new data diffEmi
diffEmi = totalEmi
diffEmi['diff'] = 0 #form a new column

#calculate the absolute change / diff in each period
diffEmi$diff[2:4] = abs(diff(totalEmi$Emissions[1:4]))
diffEmi$diff[6:8] = abs(diff(totalEmi$Emissions[5:8]))

#remove irrevlant row and columns
diffEmi = diffEmi[-c(1, 5),] #row 
diffEmi = diffEmi[, -c(1, 3)] #col

#add the period column
diffEmi['period'] = rep(c("1999-2002", "2002-2005", "2005-2008"), 2)

#graph
library(ggplot2)
png(filename="plot6.png", width=480, height=480, units="px")
p = ggplot(diffEmi, aes(x=fips, y=diff, fill=period))
p + geom_bar(stat="identity")+
  xlab("Location") +
  ylab(expression(Absolute~Change~of~PM[2.5]~Emissions~(`in`~tons)))+
  ggtitle(expression(Absolute~Change~of~PM[2.5]~ Emission))+
  scale_x_discrete(breaks=c("06037", "24510"), labels=c("Los Angeles County", "Baltimore City"))+
  scale_fill_discrete(name="Period")
dev.off()