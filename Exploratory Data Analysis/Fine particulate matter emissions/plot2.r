# Working directory set as "C:/"
# Download the data from the weblink
con <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(con, "C:/dataset.zip")
unzip("C:/dataset.zip", exdir = "C:/dataset")
list.files("C:/dataset")

# Read the data
NEI <- readRDS("C:/dataset/summarySCC_PM25.rds")
SCC <- readRDS("C:/dataset/Source_Classification_Code.rds")

# Convert year to a factor variable
NEI <- transform(NEI, year = factor(year))

# Subsetting on Baltimore, Maryland
NEI2 <- subset(NEI, fips == "24510")

# Summing the emissions by year
library(dplyr)
NEI2 <- group_by(NEI2, year)
NEI2 <- summarize(NEI2, Emissions = sum(Emissions, na.rm = TRUE))

# Plot showing the total PM2.5 emission from all sources in Baltimore City, Maryland for each of the years 1999, 2002, 2005, and 2008
barplot(height = NEI2$Emissions, names.arg = NEI2$year, main = "Total PM2.5 emissions in Baltimore City, Maryland", xlab = "Year", ylab = "Total PM2.5 emissions in Baltimore City, Maryland")
dev.copy(png, file = "C:/plot2.png")
dev.off()
