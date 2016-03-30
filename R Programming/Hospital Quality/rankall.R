rankall <- function(outcome, num) {
			fulldata <- read.csv("outcome-of-care-measures.csv")
			state_list <- unique(fulldata[["State"]])		
# For different outcome possibilities
			if (outcome == "heart attack") {
					fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]] <- as.numeric(as.character(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]]))
					mortalityrate <- fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]]
					# Removing rows of the dataset where values for outcome are missing	
					fulldata <- fulldata[(!(is.na(mortalityrate))), ]
					# Sorting alphabetically by Hospital.Name
					fulldata <- fulldata[order(fulldata[["Hospital.Name"]]), ]
					# Sorting by Mortality Rates from Heart attack
					fulldata <- fulldata[order(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]]), ]	
					# Sorting alphabetically by state name
					fulldata <- fulldata[order(fulldata[["State"]]), ]
					
			}	else if (outcome == "heart failure"){
					fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]] <- as.numeric(as.character(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]]))
					mortalityrate <- fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]]
					# Removing rows of the dataset where values for outcome are missing	
					fulldata <- fulldata[(!(is.na(mortalityrate))), ]
					# Sorting alphabetically on Hospital.Name
					fulldata <- fulldata[order(fulldata[["Hospital.Name"]]), ]
					# Sorting by Mortality Rates from Heart failure
					fulldata <- fulldata[order(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]]), ]	
					# Sorting alphabetically by state name
					fulldata <- fulldata[order(fulldata[["State"]]), ]					
			}	else if (outcome == "pneumonia") {
					fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]] <- as.numeric(as.character(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]]))
					mortalityrate <- fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]]
					# Removing rows of the dataset where values for outcome are missing	
					fulldata <- fulldata[(!(is.na(mortalityrate))), ]
					# Sorting alphabetically on Hospital.Name
					fulldata <- fulldata[order(fulldata[["Hospital.Name"]]), ]
					# Sorting by Mortality Rates from Pneumonia
					fulldata <- fulldata[order(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]]), ]
					# Sorting alphabetically by state name
					fulldata <- fulldata[order(fulldata[["State"]]), ]						
			}	else {
					stop("invalid outcome")
			}	
# Ranking the hospitals for each state
			fulldata[["State"]] <- as.character(fulldata[["State"]])
			fulldata[["Hospital.Name"]] <- as.character(fulldata[["Hospital.Name"]])
			sub <- split(fulldata, fulldata[["State"]])
			rank <- function(x) {
				len <- nrow(x)
					if (num == "best") {
					num <- 1
			} 	else if (num == "worst") {
					num <- len
			}	
				x[num, c("Hospital.Name", "State")]		
			}
			z <- sapply(sub, rank)
			z <- t(z)
			z[, "State"] <- labels(z)[[1]]
			z
}

