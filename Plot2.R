NEI <- readRDS("E:/Coursera/Data_Science/Assignments/4.4/summarySCC_PM25.rds")
SCC <- readRDS("E:/Coursera/Data_Science/Assignments/4.4/Source_Classification_Code.rds")

#HYPOTHESIS 2: Have total emissions from PM2.5 decreased in 
#the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.

NEI.Maryland <- NEI[which(NEI$fips == "24510"),]

Maryland.1999 <- NEI.Maryland[which(NEI.Maryland$year == 1999),]
Maryland.2002 <- NEI.Maryland[which(NEI.Maryland$year == 2002),]
Maryland.2005 <- NEI.Maryland[which(NEI.Maryland$year == 2005),]
Maryland.2008 <- NEI.Maryland[which(NEI.Maryland$year == 2008),]
Maryland.total.1999 <- sum(Maryland.1999[,4])
Maryland.total.2002 <- sum(Maryland.2002[,4])
Maryland.total.2005 <- sum(Maryland.2005[,4])
Maryland.total.2008 <- sum(Maryland.2008[,4])

Maryland.total <- c(Maryland.total.1999, Maryland.total.2002, Maryland.total.2005, Maryland.total.2008)
years <- c("1999", "2002", "2005", "2008")

#Plotting
png("plot2.png")
barplot(Maryland.total, xlab = "Years", col = "brown", adj = 0.5, names.arg = c("1999", "2002", "2005", "2008"), ylab = "Emissions (tons)", main = "Emissions in Baltimore City in 1999-2008")
dev.off()