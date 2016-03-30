# Creating a function to report the number of completely observed cases in each data file
complete <- function(directory, id) {
			# Storing the names of files in a character vector
			all_datafiles <- list.files(path = directory)
			# Reading the data from all the files in the directory		
			full_data <- lapply(paste(directory,"/",all_datafiles, sep=""), read.csv)
			complete_obs <- function(x) {
							sum(complete.cases(x))
							}
			num_complete_obs <- lapply(full_data[id], complete_obs)
			num_complete_obs <- unlist(num_complete_obs)
			cbind(id, num_complete_obs)
}

# Path of directory containing all relevant files
specdata <- "C:/Users/P. SYAL/Documents/specdata"
complete(specdata, 1:332)