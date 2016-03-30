# Creating a function to calculate mean of the given pollutant across all monitor IDs
pollutantmean <- function(directory, pollutant, id) {
			# Storing the names of files in a character vector 
			all_datafiles <- list.files(path = directory)
			length(all_datafiles)
			
			# Reading the data from all the files in the directory		
			full_data <- lapply(paste(directory,"/",all_datafiles, sep=""), read.csv)
			class(full_data)
			class(full_data[[1]])
			
			# Creating a function to calculate mean of non-missing pollutant values
			poll_func <- function(x) {
						mean(x[[pollutant]], na.rm = TRUE)				
						}			
			mean_by_monitor <- lapply(full_data[id], poll_func)
			mean_by_monitor <- unlist(mean_by_monitor)
			mean(mean_by_monitor, na.rm = TRUE)
} 

# Path of directory containing all relevant files
specdata <- "C:/Users/P. SYAL/Documents/specdata"
sulfatemean <- pollutantmean(specdata, "sulfate", 1:332)
nitratemean <- pollutantmean(specdata, "nitrate", 1:332)

# Converting list to vector 
sulfatemean <- unlist(sulfatemean)
sulfatemean
nitratemean <- unlist(nitratemean)
nitratemean