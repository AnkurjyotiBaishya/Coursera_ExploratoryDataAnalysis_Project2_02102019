NEI <- readRDS("E:/Coursera/Data_Science/Assignments/4.4/summarySCC_PM25.rds")
SCC <- readRDS("E:/Coursera/Data_Science/Assignments/4.4/Source_Classification_Code.rds")

#HYPOTHESIS 4: Across the United States, how have emissions from coal combustion-related sources 
#changed from 1999-2008?

combustionRelated <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
coalRelated <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
coalCombustion2 <- (combustionRelated & coalRelated)
combustionSCC <- SCC[coalCombustion2,]$SCC
NEI.CoalCombustion <- NEI[NEI$SCC %in% combustionSCC,]

#Plotting
png("plot4.png")
ggplot(NEI.CoalCombustion, aes(factor(year), Emissions/10^5)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(x = "Years", y = "Emissions (tons)", title = "Emissions from Coal - Related Sources in 1999-2008") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
