NEI <- readRDS("E:/Coursera/Data_Science/Assignments/4.4/summarySCC_PM25.rds")
SCC <- readRDS("E:/Coursera/Data_Science/Assignments/4.4/Source_Classification_Code.rds")

#HYPOTHESIS 3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
#which of these four sources have seen decreases in emissions 
#from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008? 
#Use the ggplot2 plotting system to make a plot answer this question.
NEI.Maryland <- NEI[which(NEI$fips == "24510"),]
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
