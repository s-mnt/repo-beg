# Part 2
# Finding the best hospital in a state

best <- function(state, outcome) {
			fulldata <- read.csv("outcome-of-care-measures.csv")
# verifying whether 'state' argument is valid 			
			state_list <- unique(fulldata[["State"]])
			if ((sum(state == state_list)) == 0) {
					stop("invalid state")
			}
			if ((sum(state == state_list)) == 0) {
					print("invalid state")
			}
# Keeping the data for the particular state passed as argument
			fulldata <- fulldata[fulldata[["State"]] == state, ]			
# For different outcome possibilities
			if (outcome == "heart attack") {
					fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]] <- as.numeric(as.character(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]]))
					mortalityrate <- fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]]
					# Removing rows of the dataset where values for outcome are missing	
					fulldata <- fulldata[(!(is.na(mortalityrate))), ]
					lowest_mortality <- min(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]])
					fulldata <- fulldata[fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]] == lowest_mortality, ]
					# Sorting alphabetically on Hospital.Name
					fulldata <- fulldata[order(fulldata$Hospital.Name), ]					
			}	else if (outcome == "heart failure"){
					fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]] <- as.numeric(as.character(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]]))
					mortalityrate <- fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]]
					# Removing rows of the dataset where values for outcome are missing	
					fulldata <- fulldata[(!(is.na(mortalityrate))), ]
					lowest_mortality <- min(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]])
					fulldata <- fulldata[fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]] == lowest_mortality, ]
					# Sorting alphabetically on Hospital.Name
					fulldata <- fulldata[order(fulldata$Hospital.Name), ]					
			}	else if (outcome == "pneumonia") {
					fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]] <- as.numeric(as.character(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]]))
					mortalityrate <- fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]]
					# Removing rows of the dataset where values for outcome are missing	
					fulldata <- fulldata[(!(is.na(mortalityrate))), ]
					lowest_mortality <- min(fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]])
					fulldata <- fulldata[fulldata[["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]] == lowest_mortality, ]
					# Sorting alphabetically on Hospital.Name
					fulldata <- fulldata[order(fulldata$Hospital.Name), ]
			}	else {
					stop("invalid outcome")
			}
			fulldata[["Hospital.Name"]] <- as.character(fulldata[["Hospital.Name"]])
			fulldata[["Hospital.Name"]][1]		
}