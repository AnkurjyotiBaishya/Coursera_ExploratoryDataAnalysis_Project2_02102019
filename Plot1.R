#READING DATA
NEI <- readRDS("E:/Coursera/Data_Science/Assignments/4.4/summarySCC_PM25.rds")
SCC <- readRDS("E:/Coursera/Data_Science/Assignments/4.4/Source_Classification_Code.rds")

#######################################################################################################################################################
#######################################################################################################################################################
#######################################################################################################################################################

#HYPOTHESIS 1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
#for each of the years 1999, 2002, 2005, and 2009

#Collecting Data
NEI.1999 <- NEI[which(NEI$year == 1999),]
NEI.2002 <- NEI[which(NEI$year == 2002),]
NEI.2005 <- NEI[which(NEI$year == 2005),]
NEI.2008 <- NEI[which(NEI$year == 2008),]
total.1999 <- sum(NEI.1999[,4])
total.2002 <- sum(NEI.2002[,4])
total.2005 <- sum(NEI.2005[,4])
total.2008 <- sum(NEI.2008[,4])

total.Emission.data <- c(total.1999, total.2002, total.2005, total.2008)
total.Emission.data <- total.Emission.data/10^6
years <- c("1999", "2002", "2005", "2008")

#Plotting
png("plot1.png")
barplot(total.Emission.data, xlab = "Years", col = "yellow", adj = 0.5, names.arg = c("1999", "2002", "2005", "2008"), ylab = "Emissions (10^6 tons)", main = "Total Emissions in 1999-2008")
dev.off()