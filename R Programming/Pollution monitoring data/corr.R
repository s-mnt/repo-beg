# Finding the correlations between sulphate and nitrate matter for number of completely observed cases greater than threshold
corr <- function(directory, threshold = 0) {
					# Storing the names of files in a character vector
					all_datafiles <- list.files(path = directory)
					# Reading the data from all the files in the directory		
					full_data <- lapply(paste(directory,"/",all_datafiles, sep=""), read.csv)
					# Using 'complete' created in Part 2
					complete_data <- complete(specdata, 1:332)
					y <- complete_data[ ,"num_complete_obs"] > threshold
					complete_data <- complete_data[y, ]
					subset1 <- full_data[y]	
					
					correlate <- function(x) {
							cor(x[["sulfate"]], x[["nitrate"]], use = "complete.obs")
					}
					sapply(subset1, correlate)
}
# Path of directory containing all relevant files
specdata <- "C:/Users/P. SYAL/Documents/specdata"
cr <- corr(specdata, 0)
cr <- unlist(cr)
head(cr)
summary(cr)