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

# Subsetting NEI dataset by SCC for vehicles in Baltimore City and Los Angeles County
NEIvehicles <- NEI[NEI$SCC %in% SCC2, ]
NEIvehicles <- subset(NEIvehicles, fips == "24510" | fips == "06037")
NEIvehicles <- transform(NEIvehicles, fips = factor(fips))
levels(NEIvehicles$fips) <- c("Los Angeles County", "Baltimore City")

# Summing the emissions by year and city
library(dplyr)
NEIvehicles <- group_by(NEIvehicles, year, fips)
NEIvehicles <- summarize(NEIvehicles, Emissions = sum(Emissions, na.rm = TRUE))

# Plot showing comparison of emissions from motor vehicle sources in Baltimore City with those in Los Angeles County for 1999, 2002, 2005 & 2008
library(ggplot2)
g <- ggplot(NEIvehicles, aes(year, Emissions))
p <- g + facet_grid(.~fips) + geom_bar(aes(fill = fips), stat = "identity") + labs(title = "Emissions from vehicles in Baltimore & Los Angeles")
print(p)
dev.copy(png, file = "C:/plot6.png")
dev.off()