NEI <- readRDS("E:/Coursera/Data_Science/Assignments/4.4/summarySCC_PM25.rds")
SCC <- readRDS("E:/Coursera/Data_Science/Assignments/4.4/Source_Classification_Code.rds")

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
