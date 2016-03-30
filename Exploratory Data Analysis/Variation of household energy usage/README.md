## Instructions

This assignment uses data from the UC Irvine Machine Learning Repository (http://archive.ics.uci.edu/ml/), a popular repository for machine learning datasets.
In particular, “Individual household electric power consumption Data Set” has been used which is available on the following web site:
Dataset: Electric power consumption (https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip) [20Mb]
Description: Measurements of electric power consumption in one household with a one-minute sampling rate over a period of almost 4 years. Different
electrical quantities and some sub-metering values are available.
The following descriptions of the 9 variables in the dataset are taken from the UCI web site
(https://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption):
1. Date: Date in format dd/mm/yyyy
2. Time: time in format hh:mm:ss
3. Global_active_power: household global minute-averaged active power (in kilowatt)
4. Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
5. Voltage: minute-averaged voltage (in volt)
6. Global_intensity: household global minute-averaged current intensity (in ampere)
7. Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
8. Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumbledrier, a refrigerator and a light.
9. Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.

### Loading the data
* The dataset has 2,075,259 rows and 9 columns. 
* Only the data from the dates 2007-02-01 and 2007-02-02 has been used. 
* You may find it useful to convert the Date and Time variables to Date/Time classes in R using the and functions.
* Note that in this dataset missing values are coded as ?.

### Making Plots 
The overall goal here is simply to examine how household energy usage varies over a 2-day period in February, 2007. 
For each plot you should
* For each scenario, a plot has been constructed and saved to a PNG file with a width of 480 pixels and a height of 480 pixels.
* Each of the plot files have been named as plot1.png, plot2.png, etc.
* A separate R code file (plot1.R, plot2.R, etc.) constructs the corresponding plot. Each code file includes code for reading the data so that the plot can be fully reproduced. Also the code that creates the PNG file has been included.