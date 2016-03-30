# Part 3
# Ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, num) {
			fulldata <- read.csv("outcome-of-care-measures.csv")
# verifying whether 'state' argument is valid 			
			state_list <- unique(fulldata[["State"]])
			if ((sum(state == state_list)) == 0) {
					stop("invalid state")
			}
# Keeping the data for the particular state passed as argument
			fulldata <- fulldata[fulldata[["State"]] == state, ]
# For different outcome possibilities
			if (outcome == "heart attack") {
					fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]] <- as.numeric(as.character(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]]))
					mortalityrate <- fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]]
					# Removing rows of the dataset where values for outcome are missing	
					fulldata <- fulldata[(!(is.na(mortalityrate))), ]
					# Sorting alphabetically on Hospital.Name
					fulldata <- fulldata[order(fulldata[["Hospital.Name"]]), ]
					# Sorting by Mortality Rates from Heart attack
					fulldata <- fulldata[order(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]]), ]					
			}	else if (outcome == "heart failure"){
					fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]] <- as.numeric(as.character(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]]))
					mortalityrate <- fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]]
					# Removing rows of the dataset where values for outcome are missing	
					fulldata <- fulldata[(!(is.na(mortalityrate))), ]
					# Sorting alphabetically on Hospital.Name
					fulldata <- fulldata[order(fulldata[["Hospital.Name"]]), ]
					# Sorting by Mortality Rates from Heart failure
					fulldata <- fulldata[order(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]]), ]					
			}	else if (outcome == "pneumonia") {
					fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]] <- as.numeric(as.character(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]]))
					mortalityrate <- fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]]
					# Removing rows of the dataset where values for outcome are missing	
					fulldata <- fulldata[(!(is.na(mortalityrate))), ]
					# Sorting alphabetically on Hospital.Name
					fulldata <- fulldata[order(fulldata[["Hospital.Name"]]), ]
					# Sorting by Mortality Rates from Pneumonia
					fulldata <- fulldata[order(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]]), ]								
			}	else {
					stop("invalid outcome")
			}	
# Verifying whether num argument is valid
			len <- length(fulldata[["Hospital.Name"]])
			if (num == "best") {
					num <- 1
			} 	else if (num == "worst") {
					num <- len
			}
# Retrieving hospital by rank
			if (num > len) {
					print("NA")
			}	else {		
			fulldata[["Hospital.Name"]] <- as.character(fulldata[["Hospital.Name"]])
			fulldata[["Hospital.Name"]][num]
			}
}