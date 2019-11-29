NEI <- readRDS("E:/Coursera/Data_Science/Assignments/4.4/summarySCC_PM25.rds")
SCC <- readRDS("E:/Coursera/Data_Science/Assignments/4.4/Source_Classification_Code.rds")

#HYPOTHESIS 6: Compare emissions from motor vehicle sources in Baltimore City 
#with emissions from motor vehicle sources in Los Angeles County, California (fips=="06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?

vehicleRelated <- grepl("vehicles", SCC$SCC.Level.Two, ignore.case=TRUE)
Vehicles.SCC <- SCC[vehicleRelated,]$SCC
NEI.Vehicles <- NEI[NEI$SCC %in% Vehicles.SCC,]
NEI.Vehicles.LA <- NEI.Vehicles[NEI.Vehicles$fips == "06037",]
NEI.Vehicles.BaltimoreCity <- NEI.Vehicles[which(NEI.Vehicles$fips == "24510"),]
H6.data <- rbind(NEI.Vehicles.BaltimoreCity, NEI.Vehicles.LA)

#Plotting
png("plot6B.png")
ggplot(H6.data, aes(factor(year), Emissions, fill = fips)) +
  geom_bar(stat = "identity") +
  labs(x = "Years", y = "Emissions", title = "Emissions from Motor Vehicle Sources \nin Baltimore City and Los Angeles in 1999-2008") +
  scale_fill_discrete(name = "Cities", breaks = c("06037","24510"), labels = c("Los Angeles", "Baltimore City")) + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

#######################################################################################################################################################
#######################################################################################################################################################

