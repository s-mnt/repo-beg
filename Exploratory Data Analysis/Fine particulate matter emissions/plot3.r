# Working directory set as "C:/"
# Download the data from the weblink
con <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(con, "C:/dataset.zip")
unzip("C:/dataset.zip", exdir = "C:/dataset")
list.files("C:/dataset")

# Read the data
NEI <- readRDS("C:/dataset/summarySCC_PM25.rds")
SCC <- readRDS("C:/dataset/Source_Classification_Code.rds")

# Convert year and type to factor variables
NEI <- transform(NEI, year = factor(year), type = factor(type))

# Subsetting on Baltimore, Maryland
NEI2 <- subset(NEI, fips == "24510")

# Summing the emissions by year and type
library(dplyr)
NEI2 <- group_by(NEI2, year, type)
NEI2 <- summarize(NEI2, Emissions = sum(Emissions, na.rm = TRUE))

# Plot showing the total PM2.5 emission from 4 sources(type = point, nonpoint, onraod, nonroad) in Baltimore City for 1999, 2002, 2005, and 2008
library(ggplot2)
g <- ggplot(NEI2, aes(year, Emissions))
p <- g + facet_grid(.~type) + geom_bar(aes(fill = type), stat = "identity") + labs(title = "PM2.5 emission by source type in Baltimore City")
print(p)
dev.copy(png, file = "C:/plot3.png")
dev.off()


 
