#READING DATA
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

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

#######################################################################################################################################################
#######################################################################################################################################################
#######################################################################################################################################################

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

#######################################################################################################################################################
#######################################################################################################################################################
#######################################################################################################################################################

#HYPOTHESIS 3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
#which of these four sources have seen decreases in emissions 
#from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008? 
#Use the ggplot2 plotting system to make a plot answer this question.
NEI.BaltimoreCity <- NEI.Maryland
NEI.BaltimoreCity.Point <- NEI.BaltimoreCity[which(NEI.BaltimoreCity$type == "POINT"),]
NEI.BaltimoreCity.Nonpoint <- NEI.BaltimoreCity[which(NEI.BaltimoreCity$type == "NONPOINT"),]
NEI.BaltimoreCity.Onroad <- NEI.BaltimoreCity[which(NEI.BaltimoreCity$type == "ON_ROAD"),]
NEI.BaltimoreCity.Nonroad <- NEI.BaltimoreCity[which(NEI.BaltimoreCity$type == "NON_ROAD"),]
types <- as.factor(NEI.BaltimoreCity$type)

library(ggplot2)

#Plotting
png("plot3.png")
ggplot(NEI.BaltimoreCity, aes(factor(year), Emissions, fill = type)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(.~as.factor(NEI.BaltimoreCity$type)) +
  labs(title = "Type-wise Emissions in Baltimore City in 1999-2008", x = "Years", y = "Emissions") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

#######################################################################################################################################################
#######################################################################################################################################################
#######################################################################################################################################################

#HYPOTHESIS 4: Across the United States, how have emissions from coal combustion-related sources 
#changed from 1999-2008?

combustionRelated <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
coalRelated <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
coalCombustion2 <- (combustionRelated & coalRelated)
combustionSCC <- SCC[coalCombustion,]$SCC
NEI.CoalCombustion <- NEI[NEI$SCC %in% combustionSCC,]

#Plotting
png("plot4.png")
ggplot(NEI.CoalCombustion, aes(factor(year), Emissions/10^5)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(x = "Years", y = "Emissions (tons)", title = "Emissions from Coal - Related Sources in 1999-2008") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

#######################################################################################################################################################
#######################################################################################################################################################
#######################################################################################################################################################

#HYPOTHESIS 5: How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

vehicleRelated <- grepl("vehicles", SCC$SCC.Level.Two, ignore.case=TRUE)
Vehicles.SCC <- SCC[vehicleRelated,]$SCC
NEI.Vehicles <- NEI[NEI$SCC %in% Vehicles.SCC,]
NEI.Vehicles.BaltimoreCity <- NEI.Vehicles[which(NEI.Vehicles$fips == "24510"),]

#Plotting
png("plot5.png")
ggplot(NEI.Vehicles.BaltimoreCity, aes(factor(year), Emissions)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Years", y = "Emissions", title = "Emissions from Motor Vehicle Sources \nin Baltimore City in 1999-2008") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

#######################################################################################################################################################
#######################################################################################################################################################
#######################################################################################################################################################

#HYPOTHESIS 6: Compare emissions from motor vehicle sources in Baltimore City 
#with emissions from motor vehicle sources in Los Angeles County, California (fips=="06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?

NEI.Vehicles.LA <- NEI.Vehicles[NEI.Vehicles$fips == "06037",]
H6.data <- rbind(NEI.Vehicles.BaltimoreCity, NEI.Vehicles.LA)

#Plotting
png("plot6.png")
ggplot(H6.data, aes(factor(year), Emissions, fill = fips)) +
  geom_bar(stat = "identity") +
  labs(x = "Years", y = "Emissions", title = "Emissions from Motor Vehicle Sources \nin Baltimore City and Los Angeles in 1999-2008") +
  scale_fill_discrete(name = "Cities", breaks = c("06037","24510"), labels = c("Los Angeles", "Baltimore City")) + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

#######################################################################################################################################################
#######################################################################################################################################################














