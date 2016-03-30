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

# Extracting SCC for coal related sources from SCC dataset
coalrelated <- grep("Coal", SCC$Short.Name, ignore.case = TRUE)
SCC2 <- SCC[coalrelated,"SCC"]

# Subsetting NEI dataset by SCC for coal related sources
NEIcoal <- NEI[NEI$SCC %in% SCC2, ]

# Summing the emissions by year
library(dplyr)
NEIcoal <- group_by(NEIcoal, year)
NEIcoal <- summarize(NEIcoal, Emissions = sum(Emissions, na.rm = TRUE))

# Plot showing the PM2.5 emission from coal combustion related sources  for 1999, 2002, 2005, and 2008
barplot(height = NEIcoal$Emissions, names.arg = NEIcoal$year, main = "Total PM2.5 emissions from coal related sources", xlab = "Year", ylab = "Total PM2.5 emissions from coal related sources")
dev.copy(png, file = "C:/plot4.png")
dev.off()

