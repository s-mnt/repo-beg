# Working directory set as "C:/"
# Download the data from the weblink
con <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(con, "C:/dataset.zip")
unzip("C:/dataset.zip", exdir = "C:/dataset")
list.files("C:/dataset")

# Read the data
NEI <- readRDS("C:/dataset/summarySCC_PM25.rds")
SCC <- readRDS("C:/dataset/Source_Classification_Code.rds")

# Convert year to factor variable
NEI <- transform(NEI, year = factor(year))

# Extracting SCC for vehicles from SCC dataset
vehicle <- grep("Vehicle", SCC[["SCC.Level.Two"]], ignore.case = TRUE)
SCC2 <- SCC[vehicle,"SCC"]

# Subsetting NEI dataset by SCC for vehicles in Baltimore City
NEIvehicles <- NEI[NEI$SCC %in% SCC2, ]
NEIvehicles <- subset(NEIvehicles, fips == "24510")

# Summing the emissions by year
library(dplyr)
NEIvehicles <- group_by(NEIvehicles, year)
NEIvehicles <- summarize(NEIvehicles, Emissions = sum(Emissions, na.rm = TRUE))

# Plot showing the PM2.5 emission from motor vehicle sources in Baltimore City for 1999, 2002, 2005, and 2008
barplot(height = NEIvehicles$Emissions, names.arg = NEIvehicles$year, main = "Total PM2.5 emissions from motor vehicles in Baltimore City", xlab = "Year", ylab = "Total PM2.5 emissions from motor vehicles in Baltimore City")
dev.copy(png, file = "C:/plot5.png")
dev.off()
